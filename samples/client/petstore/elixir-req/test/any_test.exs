defmodule AnyTest do
  use ExUnit.Case, async: true
  alias OpenapiPetstore.Model.Any, as: Model

  test "decode all properties (not nil)" do
    assert %Model{
             "@type": "3fa85f64-5717-4562-b3fc-2c963f66afa6"
           }
           |> Model.decode() ==
             %Model{
               "@type": "3fa85f64-5717-4562-b3fc-2c963f66afa6"
             }
  end
end
