defmodule SwaggerPetstore.Api.FakeClassnameTags123 do
  @moduledoc """
  Documentation for SwaggerPetstore.Api.FakeClassnameTags123.
  """

  use Tesla

  plug Tesla.Middleware.BaseUrl, "http://petstore.swagger.io:80/v2"
  plug Tesla.Middleware.JSON

  @doc """
  To test class name in snake case

  
  """
  def test_classname(body) do
    method = [method: :patch]
    url = [url: "/fake_classname_test"]
    query_params = []
    header_params = []
    body_params = [body: body]
    form_params = []
    params = query_params ++ header_params ++ body_params ++ form_params
    opts = []
    options = method ++ url ++ params ++ opts

    request(options)
  end
end
