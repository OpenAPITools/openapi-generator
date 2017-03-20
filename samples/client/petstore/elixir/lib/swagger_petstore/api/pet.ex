defmodule SwaggerPetstore.Api.Pet do
  @moduledoc """
  Documentation for SwaggerPetstore.Api.Pet.
  """

  use Tesla

  plug Tesla.Middleware.BaseUrl, "http://petstore.swagger.io/v2"
  plug Tesla.Middleware.JSON

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
