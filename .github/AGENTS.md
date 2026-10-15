# .github/

`workflows/ci.yml`: the `ox-setup` action with pins fixed-decimal, exchange-core,
parquet, market-data, hl-protocol, venue-net, hl-client (dependency order), then build,
test and the odoc check; `obj-magic` from `infra`. A new org dependency in
`dune-project` needs its pin here in the same commit.

See [../AGENTS.md](../AGENTS.md).
