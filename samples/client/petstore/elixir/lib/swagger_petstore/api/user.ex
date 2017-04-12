defmodule SwaggerPetstore.Api.User do
  @moduledoc """
  Documentation for SwaggerPetstore.Api.User.
  """

  use Tesla

  plug Tesla.Middleware.BaseUrl, "http://petstore.swagger.io:80/v2"
  plug Tesla.Middleware.JSON

  @doc """
  Create user

  This can only be done by the logged in user.
  """
  def create_user(body) do
    method = [method: :post]
    url = [url: "/user"]
    query_params = []
    header_params = []
    body_params = [body: body]
    form_params = []
    params = query_params ++ header_params ++ body_params ++ form_params
    opts = []
    options = method ++ url ++ params ++ opts

    request(options)
  end

  @doc """
  Creates list of users with given input array
  """
  def create_users_with_array_input(body) do
    method = [method: :post]
    url = [url: "/user/createWithArray"]
    query_params = []
    header_params = []
    body_params = [body: body]
    form_params = []
    params = query_params ++ header_params ++ body_params ++ form_params
    opts = []
    options = method ++ url ++ params ++ opts

    request(options)
  end

  @doc """
  Creates list of users with given input array
  """
  def create_users_with_list_input(body) do
    method = [method: :post]
    url = [url: "/user/createWithList"]
    query_params = []
    header_params = []
    body_params = [body: body]
    form_params = []
    params = query_params ++ header_params ++ body_params ++ form_params
    opts = []
    options = method ++ url ++ params ++ opts

    request(options)
  end

  @doc """
  Delete user

  This can only be done by the logged in user.
  """
  def delete_user(username) do
    method = [method: :delete]
    url = [url: "/user/#{username}"]
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
  Get user by user name
  """
  def get_user_by_name(username) do
    method = [method: :get]
    url = [url: "/user/#{username}"]
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
  Logs user into the system
  """
  def login_user(username, password) do
    method = [method: :get]
    url = [url: "/user/login"]
    query_params = [query: [{:"username", username}, {:"password", password}]]
    header_params = []
    body_params = []
    form_params = []
    params = query_params ++ header_params ++ body_params ++ form_params
    opts = []
    options = method ++ url ++ params ++ opts

    request(options)
  end

  @doc """
  Logs out current logged in user session
  """
  def logout_user() do
    method = [method: :get]
    url = [url: "/user/logout"]
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
  Updated user

  This can only be done by the logged in user.
  """
  def update_user(username, body) do
    method = [method: :put]
    url = [url: "/user/#{username}"]
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
