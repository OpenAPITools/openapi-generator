defmodule StoreTest do
  use ExUnit.Case, async: true
  alias OpenapiPetstore.Connection
  alias OpenapiPetstore.Api.Store, as: StoreApi

  setup do
    %{connection: Connection.new()}
  end

  test "fetch inventory", %{connection: connection} do
    {:ok, inventory} = StoreApi.get_inventory(connection)

    assert is_map(inventory)
    assert Enum.all?(Map.keys(inventory), &is_binary/1)
    assert Enum.all?(Map.values(inventory), &is_integer/1)
  end
end
