defmodule SwaggerPetstore.Api.Store do
  @moduledoc """
  Documentation for SwaggerPetstore.Api.Store.
  """

  use Tesla

  plug Tesla.Middleware.BaseUrl, "http://petstore.swagger.io:80/v2"
  plug Tesla.Middleware.JSON

  @doc """
  Delete purchase order by ID

  For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors
  """
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

  @doc """
  Returns pet inventories by status

  Returns a map of status codes to quantities
  """
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

  @doc """
  Find purchase order by ID

  For valid response try integer IDs with value &lt;&#x3D; 5 or &gt; 10. Other values will generated exceptions
  """
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

  @doc """
  Place an order for a pet
  """
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
