defmodule SwaggerPetstore.Api.Fake do
  @moduledoc """
  Documentation for SwaggerPetstore.Api.Fake.
  """

  use Tesla

  plug Tesla.Middleware.BaseUrl, "http://petstore.swagger.io/v2"
  plug Tesla.Middleware.JSON

  def test_client_model(body) do
    method = [method: :patch]
    url = [url: "/fake"]
    query_params = []
    header_params = []
    body_params = [body: body]
    form_params = []
    params = query_params ++ header_params ++ body_params ++ form_params
    opts = []
    options = method ++ url ++ params ++ opts

    request(options)
  end

  def test_endpoint_parameters(number, double, pattern_without_delimiter, byte, integer, int32, int64, float, string, binary, date, date_time, password, callback) do
    method = [method: :post]
    url = [url: "/fake"]
    query_params = []
    header_params = []
    body_params = []
    form_params = [body: Enum.map_join([{:"integer", integer}, {:"int32", int32}, {:"int64", int64}, {:"number", number}, {:"float", float}, {:"double", double}, {:"string", string}, {:"patternWithoutDelimiter", pattern_without_delimiter}, {:"byte", byte}, {:"binary", binary}, {:"date", date}, {:"dateTime", date_time}, {:"password", password}, {:"callback", callback}], "&", &("#{elem(&1, 0)}=#{elem(&1, 1)}"))]
    params = query_params ++ header_params ++ body_params ++ form_params
    opts = []
    options = method ++ url ++ params ++ opts

    request(options)
  end

  def test_enum_parameters(enum_form_string_array, enum_form_string, enum_header_string_array, enum_header_string, enum_query_string_array, enum_query_string, enum_query_integer, enum_query_double) do
    method = [method: :get]
    url = [url: "/fake"]
    query_params = [query: [{:"enumQueryStringArray", enum_query_string_array}, {:"enumQueryString", enum_query_string}, {:"enumQueryInteger", enum_query_integer}]]
    header_params = [header: [{:"enumHeaderStringArray", enum_header_string_array}, {:"enumHeaderString", enum_header_string}]]
    body_params = []
    form_params = [body: Enum.map_join([{:"enumFormStringArray", enum_form_string_array}, {:"enumFormString", enum_form_string}, {:"enumQueryDouble", enum_query_double}], "&", &("#{elem(&1, 0)}=#{elem(&1, 1)}"))]
    params = query_params ++ header_params ++ body_params ++ form_params
    opts = []
    options = method ++ url ++ params ++ opts

    request(options)
  end
end
