open! Core
open Leader_scoring

let t0 = Time_ns.of_string_with_utc_offset "2026-01-01T00:00:00Z"
let at ?(h = 0.) d = Time_ns.add t0 (Time_ns.Span.of_hr ((d *. 24.) +. h))
let next_id = ref 0

let fill ?(fee = 0.) ?position_before ?(liquidation = false) ~time ~coin ~px sz : Fill.t =
  incr next_id;
  { time; coin; px; sz; fee; position_before; liquidation; id = !next_id }
;;

let snapshot ~time ~account_value positions : Snapshot.t =
  { time
  ; account_value
  ; positions =
      List.map positions ~f:(fun (coin, size, mark) ->
        { Snapshot.Position.coin
        ; size
        ; entry_px = None
        ; mark_px = Some mark
        ; leverage = None
        })
  }
;;

let history ?(funding = []) ?(snapshots = []) ?(equity_curve = []) fills : History.t =
  { account = "0xabc"
  ; venue = "test"
  ; fetched_at = t0
  ; fills
  ; funding
  ; snapshots
  ; equity_curve
  ; fills_truncated = false
  }
;;

let round x = Float.round_decimal x ~decimal_digits:6
