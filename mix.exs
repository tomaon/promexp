defmodule Promexp.Mixfile do

  use Mix.Project

  def project do
    [
      app: :promexp,
      description: "Prometheus exporter for Erlang/Elixir",
      version: "0.4.1",

      compilers: [:erlang, :elixir, :app],
      build_path: ".mix",

      elixirc_options: [
        {:warnings_as_errors, true}
      ],

      erlc_options: [
        :warnings_as_errors
      ],

      package: package(),
    ]
  end

  defp package do
    [
      maintainers: ["Tomohiko AONO"],
      licenses: ["Apache-2.0"],
      links: %{
        "GitHub": "https://github.com/tomaon/promexp",
      },
      files: [
        "LICENSE", "README.md", "examples",
        "lib", "mix.exs",
        "elvis.config", "rebar3.config", "src", "test"
      ]
    ]
  end

end
