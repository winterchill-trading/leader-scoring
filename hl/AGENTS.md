# hl/ (leader-scoring-hl)

The Hyperliquid package: `Hl_source` (read-only `/info` requests, spaced 1.2 s apart,
transport errors retried twice), `Raw` (recordings on disk), `Hl_decode` (answers to
venue-neutral types), `Hl_market` (`l2Book`, `metaAndAssetCtxs`).

- Only `POST /info`. No key, no signature, no exchange endpoint, ever.
- `hl/test/fixtures/` are recordings made with `leader-scoring record`; keep them small
  (trim large funding pages) and read only.
- `userFunding` and `portfolio` are optional (hl-sim lacks them); fills and state are not.

See [../AGENTS.md](../AGENTS.md).
