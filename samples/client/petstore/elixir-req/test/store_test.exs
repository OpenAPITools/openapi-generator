defmodule StoreTest do
  use ExUnit.Case, async: true
  alias OpenapiPetstore.Connection
  alias OpenapiPetstore.Api.Store, as: StoreApi
  alias OpenapiPetstore.Model.Order

  setup do
    %{connection: Connection.new()}
  end

  test "fetch inventory", %{connection: connection} do
    {:ok, inventory} = StoreApi.get_inventory(connection)

    assert is_map(inventory)
    assert Enum.all?(Map.keys(inventory), &is_binary/1)
    assert Enum.all?(Map.values(inventory), &is_integer/1)
  end

  test "delete order returns the successful response", %{connection: connection} do
    order = %Order{id: 999, petId: 1, quantity: 1, status: "placed", complete: false}
    assert {:ok, %Order{id: 999}} = StoreApi.place_order(connection, order)

    assert {:ok, %Req.Response{status: status}} = StoreApi.delete_order(connection, "999")
    assert status in 200..299
  end
end
