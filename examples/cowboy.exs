use Mix.Config

config :promexp,
  collector: [
    {"m", 0x0001, {:promexp_collector_erts, :collect_memory, []}},
    {"s", 0x03dd, {:promexp_collector_erts, :collect_statistics, []}},
    {"i", 0x0000, {:promexp_collector_erts, :collect_system_info, []}}
  ]
