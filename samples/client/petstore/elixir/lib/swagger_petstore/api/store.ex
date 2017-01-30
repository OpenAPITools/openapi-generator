defmodule SwaggerPetstore.Api.Store do
  @moduledoc """
  Documentation for SwaggerPetstore.Api.Store.
  """

  use Tesla

  plug Tesla.Middleware.BaseUrl, "http://petstore.swagger.io/v2"
  plug Tesla.Middleware.JSON

  def delete_order(order_id) do
    method = [method: :delete]
    url = [url: "/store/order/#{order_id}"]
    query_params = []
    header_params = []
    body_params = []
    form_params = []
    params = query_params ++ header_params ++ body_params ++ form_params
    opts = []
    options = method ++ url ++ params ++ opts

    request(options)
  end

  def get_inventory() do
    method = [method: :get]
    url = [url: "/store/inventory"]
    query_params = []
    header_params = []
    body_params = []
    form_params = []
    params = query_params ++ header_params ++ body_params ++ form_params
    opts = []
    options = method ++ url ++ params ++ opts

    request(options)
  end

  def get_order_by_id(order_id) do
    method = [method: :get]
    url = [url: "/store/order/#{order_id}"]
    query_params = []
    header_params = []
    body_params = []
    form_params = []
    params = query_params ++ header_params ++ body_params ++ form_params
    opts = []
    options = method ++ url ++ params ++ opts

    request(options)
  end

  def place_order(body) do
    method = [method: :post]
    url = [url: "/store/order"]
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
