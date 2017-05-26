defmodule Promexp.Mixfile do

  use Mix.Project

  def project do
    [
      app: :promexp,
      description: "Prometheus exporter for Erlang/Elixir",
      version: "0.5.0",

      compilers: [:erlang, :elixir, :app],
      build_path: ".mix",

      elixirc_options: [
        {:warnings_as_errors, true}
      ],

      erlc_options: [
        :warnings_as_errors
      ],

      deps: deps(),
      package: package(),
    ]
  end

  defp deps do
    [
      {:ex_doc, "~> 0.15.1", only: :dev, runtime: false}
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
        "lib", "mix.exs", "test",
        "elvis.config", "rebar.config", "src"
      ]
    ]
  end

end
