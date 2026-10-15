(lang dune 3.17)

(name leader-scoring)

(generate_opam_files true)

(license MIT)

(maintainers "winterchill-trading")

(authors "winterchill-trading")

(source
 (github winterchill-trading/leader-scoring))

(package
 (name leader-scoring)
 (synopsis
  "Venue-neutral leader track records, risk metrics, composite and learned scores, copyability and alerts")
 (description
  "Builds an equity curve and per-trade records from a trader's fills, funding and account snapshots; computes documented performance and risk metrics; ranks a universe with a transparent weighted score and an optional gradient-boosted model; estimates follower slippage and capacity from order-book depth; and flags behaviour changes for followers.")
 (tags
  (ocaml oxcaml copy-trading quant))
 (depends
  (ocaml
   (>= 5.2))
  dune
  core
  ppx_jane
  yojson
  market-data
  (core_bench :with-test)))

(package
 (name leader-scoring-hl)
 (synopsis
  "Hyperliquid source for leader-scoring: userFills, userFunding, clearinghouseState and portfolio, read only")
 (tags
  (ocaml oxcaml copy-trading hyperliquid))
 (depends
  (ocaml
   (>= 5.2))
  dune
  core
  core_unix
  async
  ppx_jane
  yojson
  uri
  (leader-scoring
   (= :version))
  hl-protocol
  hl-client))
