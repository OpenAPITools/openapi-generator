defmodule SwaggerPetstore.Api.Pet do
  @moduledoc """
  Documentation for SwaggerPetstore.Api.Pet.
  """

  use Tesla

  plug Tesla.Middleware.BaseUrl, "http://petstore.swagger.io:80/v2"
  plug Tesla.Middleware.JSON

  @doc """
  Add a new pet to the store
  """
  def add_pet(body) do
    method = [method: :post]
    url = [url: "/pet"]
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
  Deletes a pet
  """
  def delete_pet(pet_id, api_key) do
    method = [method: :delete]
    url = [url: "/pet/#{pet_id}"]
    query_params = []
    header_params = [header: [{:"api_key", api_key}]]
    body_params = []
    form_params = []
    params = query_params ++ header_params ++ body_params ++ form_params
    opts = []
    options = method ++ url ++ params ++ opts

    request(options)
  end

  @doc """
  Finds Pets by status

  Multiple status values can be provided with comma separated strings
  """
  def find_pets_by_status(status) do
    method = [method: :get]
    url = [url: "/pet/findByStatus"]
    query_params = [query: [{:"status", status}]]
    header_params = []
    body_params = []
    form_params = []
    params = query_params ++ header_params ++ body_params ++ form_params
    opts = []
    options = method ++ url ++ params ++ opts

    request(options)
  end

  @doc """
  Finds Pets by tags

  Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
  """
  def find_pets_by_tags(tags) do
    method = [method: :get]
    url = [url: "/pet/findByTags"]
    query_params = [query: [{:"tags", tags}]]
    header_params = []
    body_params = []
    form_params = []
    params = query_params ++ header_params ++ body_params ++ form_params
    opts = []
    options = method ++ url ++ params ++ opts

    request(options)
  end

  @doc """
  Find pet by ID

  Returns a single pet
  """
  def get_pet_by_id(pet_id) do
    method = [method: :get]
    url = [url: "/pet/#{pet_id}"]
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
  Update an existing pet
  """
  def update_pet(body) do
    method = [method: :put]
    url = [url: "/pet"]
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
  Updates a pet in the store with form data
  """
  def update_pet_with_form(pet_id, name, status) do
    method = [method: :post]
    url = [url: "/pet/#{pet_id}"]
    query_params = []
    header_params = []
    body_params = []
    form_params = [body: Enum.map_join([{:"name", name}, {:"status", status}], "&", &("#{elem(&1, 0)}=#{elem(&1, 1)}"))]
    params = query_params ++ header_params ++ body_params ++ form_params
    opts = []
    options = method ++ url ++ params ++ opts

    request(options)
  end

  @doc """
  uploads an image
  """
  def upload_file(pet_id, additional_metadata, file) do
    method = [method: :post]
    url = [url: "/pet/#{pet_id}/uploadImage"]
    query_params = []
    header_params = []
    body_params = []
    form_params = [body: Enum.map_join([{:"additionalMetadata", additional_metadata}, {:"file", file}], "&", &("#{elem(&1, 0)}=#{elem(&1, 1)}"))]
    params = query_params ++ header_params ++ body_params ++ form_params
    opts = []
    options = method ++ url ++ params ++ opts

    request(options)
  end
end
