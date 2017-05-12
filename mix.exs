defmodule Promexp.Mixfile do

  use Mix.Project

  def project do
    [
      app: :promexp,
      version: "0.4.0",

      compilers: [:erlang, :elixir, :app],
      build_path: ".mix",

      elixirc_options: [
        {:warnings_as_errors, true}
      ],

      erlc_options: [
        :warnings_as_errors
      ]
    ]
  end

end
