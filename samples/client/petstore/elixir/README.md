# OpenapiPetstore

This spec is mainly for testing Petstore server and contains fake endpoints, models. Please do not use this for any other purpose. Special characters: \&quot; \\

## Building

To install the required dependencies and to build the elixir project, run:

```console
mix local.hex --force
mix do deps.get, compile
```

## Installation

If [available in Hex][], the package can be installed by adding `openapi_petstore` to
your list of dependencies in `mix.exs`:

```elixir
def deps do
  [{:openapi_petstore, "~> 1.0.0"}]
end
```

Documentation can be generated with [ExDoc][] and published on [HexDocs][]. Once published, the docs can be found at
[https://hexdocs.pm/openapi_petstore][docs].

## Configuration

You can override the URL of your server (e.g. if you have a separate development and production server in your
configuration files).

```elixir
config :openapi_petstore, base_url: "http://petstore.swagger.io:80/v2"
```

Multiple clients for the same API with different URLs can be created passing different `base_url`s when calling
`OpenapiPetstore.Connection.new/1`:

```elixir
client = OpenapiPetstore.Connection.new(base_url: "http://petstore.swagger.io:80/v2")
```

[exdoc]: https://github.com/elixir-lang/ex_doc
[hexdocs]: https://hexdocs.pm
[available in hex]: https://hex.pm/docs/publish
[docs]: https://hexdocs.pm/openapi_petstore
