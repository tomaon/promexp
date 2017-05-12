defmodule Promexp.Handler do

  # cowboy: 2.x

  def init(req, state) do
    :promexp.init(req, state)
  end

  # cowboy: 1.x

  def init(type, req, opts) do
    :promexp.init(type, req, opts)
  end

  def handle(req, state) do
    :promexp.handle(req, state)
  end

  # cowboy

  def terminate(reason, req, state) do
    :promexp.terminate(reason, req, state)
  end

end
