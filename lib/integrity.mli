(** Filters for leaders that look manipulated or cannot really be copied. Each check yields
    a typed {!Flag.t} with its evidence; {!Policy} says which flags take a leader out of
    the ranking ({!Report.rank}).

    - {b Self trade}: one account on both sides of the same trade (the same trade id, coin,
      time and price, opposite sides).
    - {b Wash trade}: one account's opposite fills in a coin within [wash_window] at the
      same price (within [wash_price_bps]) and size (within [wash_size_tolerance]):
      volume without price risk.
    - {b Counterparty}: two watched accounts on opposite sides of the same trades, at
      least [counterparty_min_share] of one's volume and [counterparty_min_lift] times
      the share random matching among the watched accounts would give. A group of
      leaders trading with each other is the shape of a sybil farm.
    - {b Co-trading}: two accounts whose orders (fills of one coin and side, merged within
      a second) land within [co_window] of each other in the same coin and direction far
      more often than chance. Chance is measured by the same count with one side's times
      shifted circularly by sevenths of the observed span, which keeps each account's
      own rhythm (the check needs a span of at least 20 windows); a pair is linked
      when the observed count is [co_min_lift] times that, at least [co_min_matches]
      orders, and a share [co_min_similarity] of both accounts' orders. Linked accounts
      form clusters (connected components); a "diverse" set of top leaders can be one
      cluster.
    - {b Front-running of followers}: given a flow of trades (market or follower orders),
      the leader's orders are followed within [front_run_horizon] by net same-direction
      flow of at least [front_run_flow_usd] much more often than at shifted times
      (binomial z of at least [front_run_min_z]). Not checked without a flow, or with a
      flow spanning less than 20 horizons.
    - {b Creation burst}: at least [burst_min_accounts] accounts created (or first seen)
      within [burst_window] of the first of them. Informational alone; a warning for a
      member that is also linked to another by counterparty or co-trading flags.
    - {b Off-book price}: fills more than [off_book_bps] outside the reference range at
      that time: the best bid and ask of a recorded quote within [reference_age], or the
      range of other watched accounts' fills in the coin then (never the fill's own
      trade). Fills nobody can see are not prices a follower can get.

    Thresholds are deliberately loose against honest traders: leaders who simply trade
    the same news co-trade a little; the lift over shifted times and the share of orders
    keep that from being a cluster. *)

open! Core

module Kind : sig
  type t =
    | Self_trade
    | Wash_trade
    | Counterparty
    | Co_trading
    | Front_running
    | Creation_burst
    | Off_book_price
  [@@deriving sexp, compare, equal, enumerate]

  (** ["self_trade"], ["wash_trade"], ["counterparty"], ["co_trading"],
      ["front_running"], ["creation_burst"], ["off_book_price"]. *)
  val to_string : t -> string
end

module Example : sig
  type t =
    { time : Time_ns.Alternate_sexp.t
    ; coin : string
    ; px : float
    ; sz : float (** signed *)
    ; note : string
    }
  [@@deriving sexp]
end

module Peer : sig
  type t =
    { account : string
    ; matches : int (** orders of either account matched by the other *)
    ; similarity : float (** matches over both accounts' orders *)
    ; lift : float (** matches over the shifted-time count *)
    }
  [@@deriving sexp]
end

module Evidence : sig
  type t =
    | Self_trade of
        { trades : int
        ; notional : float
        ; share_of_volume : float
        ; examples : Example.t list
        }
    | Wash_trade of
        { pairs : int
        ; notional : float
        ; share_of_volume : float
        ; window_ms : int
        ; examples : Example.t list
        }
    | Counterparty of
        { counterparties : (string * int * float) list
        (** account, shared trades, notional; most trades first *)
        ; trades : int
        ; share_of_volume : float
        ; group : string list (** every account linked to this one through such trades *)
        }
    | Co_trading of
        { cluster : string list
        ; peers : Peer.t list (** direct links, strongest first *)
        }
    | Front_running of
        { orders : int
        ; followed : int
        ; rate : float
        ; base_rate : float (** the same at shifted times *)
        ; z : float
        ; horizon_ms : int
        ; flow_usd : float
        }
    | Creation_burst of
        { accounts : string list
        ; first : Time_ns.Alternate_sexp.t
        ; last : Time_ns.Alternate_sexp.t
        ; linked : bool (** members also linked by counterparty or co-trading flags *)
        }
    | Off_book_price of
        { checked : int
        ; outside : int
        ; share : float
        ; worst_bps : float
        ; examples : Example.t list
        }
  [@@deriving sexp]
end

module Flag : sig
  type t =
    { account : string
    ; kind : Kind.t
    ; severity : Alerts.Severity.t
    ; message : string
    ; evidence : Evidence.t
    }
  [@@deriving sexp]
end

(** A trade print or follower order: [notional] signed, positive for buys. *)
module Flow : sig
  type t =
    { time : Time_ns.t
    ; coin : string
    ; notional : float
    }
end

(** A recorded best bid and ask. *)
module Quote : sig
  type t =
    { time : Time_ns.t
    ; coin : string
    ; bid : float
    ; ask : float
    }
end

module Config : sig
  type t =
    { wash_window : Time_ns.Span.t
    ; wash_price_bps : float
    ; wash_size_tolerance : float
    ; wash_min_pairs : int
    ; wash_min_share : float
    ; counterparty_min_trades : int
    ; counterparty_min_share : float
    ; counterparty_min_lift : float
    ; order_merge : Time_ns.Span.t (** fills of one coin and side this close are one order *)
    ; co_window : Time_ns.Span.t
    ; co_min_matches : int
    ; co_min_similarity : float
    ; co_min_lift : float
    ; front_run_horizon : Time_ns.Span.t
    ; front_run_flow_usd : float
    ; front_run_min_orders : int
    ; front_run_min_z : float
    ; burst_window : Time_ns.Span.t
    ; burst_min_accounts : int
    ; reference_age : Time_ns.Span.t
    ; off_book_bps : float
    ; off_book_min_fills : int
    ; off_book_min_share : float
    }
  [@@deriving sexp]

  (** Wash: 1 s, 1 bp, 10% size, at least 5 pairs and 20% of volume. Counterparty: 10
      trades, 5% of volume and 5 times chance. Orders: fills within 1 s. Co-trading:
      60 s, 20 matches, similarity 0.3, lift 3. Front-running: 5 s, 50k USD, 20 orders,
      z 3. Burst: 1 day, 3 accounts. Off-book: 0.5 s, 200 bps, 3 fills and 10%. *)
  val default : t
end

module Policy : sig
  type t =
    { exclude : Kind.t list (** kinds that take a leader out of the ranking *)
    ; min_severity : Alerts.Severity.t (** from this severity up *)
    }
  [@@deriving sexp]

  (** Every kind from [Warning]: a creation burst alone is [Info] and does not exclude. *)
  val default : t

  (** Never excludes. *)
  val none : t

  (** Why a leader with these flags is out of the ranking; empty when it is not. *)
  val reasons : t -> Flag.t list -> string list
end

(** Flags per account (accounts without any are absent). [created] maps accounts to their
    creation or first-seen time. *)
val check
  :  ?config:Config.t
  -> ?flow:Flow.t list
  -> ?quotes:Quote.t list
  -> ?created:Time_ns.t String.Map.t
  -> History.t list
  -> Flag.t list String.Map.t

(** [{"kind", "severity", "message", "evidence": {...}}]; the evidence's fields as in
    {!Evidence.t}, times in ms. *)
val flag_json : Flag.t -> Yojson.Safe.t
