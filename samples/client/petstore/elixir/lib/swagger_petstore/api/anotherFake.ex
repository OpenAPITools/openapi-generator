defmodule SwaggerPetstore.Api.AnotherFake do
  @moduledoc """
  Documentation for SwaggerPetstore.Api.AnotherFake.
  """

  use Tesla

  plug Tesla.Middleware.BaseUrl, "http://petstore.swagger.io:80/v2"
  plug Tesla.Middleware.JSON

  @doc """
  To test special tags

  To test special tags
  """
  def test_special_tags(body) do
    method = [method: :patch]
    url = [url: "/another-fake/dummy"]
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
