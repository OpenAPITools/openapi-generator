defmodule RequestBuilderTest do
  use ExUnit.Case, async: true

  alias OpenapiPetstore.Api.Fake
  alias OpenapiPetstore.Connection
  alias OpenapiPetstore.Model.Category
  import OpenapiPetstore.RequestBuilder

  defmodule BinaryBodyAdapter do
    def run(request) do
      send(Process.get(:request_builder_test_pid), {:request_body, request.body})
      {request, %Req.Response{status: 200, body: ""}}
    end
  end

  defmodule MultipartBodyAdapter do
    def run(request) do
      send(Process.get(:request_builder_test_pid), {:multipart_request, request})
      {request, %Req.Response{status: 200, body: ""}}
    end
  end

  test "encodes primitive query lists like Tesla" do
    request =
      %{}
      |> method(:get)
      |> url("/pet/findByStatus")
      |> add_param(:query, :status, ["available", "pending"])
      |> finalize_request()

    assert request.url ==
             "/pet/findByStatus?status%5B%5D=available&status%5B%5D=pending"
  end

  test "encodes nested query values like Tesla" do
    request =
      %{}
      |> method(:get)
      |> url("/search")
      |> add_param(:query, :filter, %{status: ["available", "pending"]})
      |> finalize_request()

    assert request.url ==
             "/search?filter%5Bstatus%5D%5B%5D=available&filter%5Bstatus%5D%5B%5D=pending"
  end

  test "flattens form values before Req encodes them" do
    request = add_param(%{}, :form, :status, ["available", "pending"])

    assert request.form == [{"status[]", "available"}, {"status[]", "pending"}]
  end

  test "keeps binary bodies unchanged" do
    body = <<0, 1, 2, 255>>

    assert add_param(%{}, :body, :body, body) == %{body: body}
  end

  test "does not add a body or content type for a nil body" do
    request = add_param(%{}, :body, :body, nil)

    refute Map.has_key?(request, :body)

    refute Enum.any?(Map.get(request, :headers, []), fn {key, _value} -> key == "content-type" end)
  end

  test "renders cookie params in a single cookie header" do
    request =
      %{}
      |> add_param(:cookie, :first, "one")
      |> add_param(:cookie, :second, "two")
      |> finalize_request()

    assert request.headers == [{"cookie", "first=one; second=two"}]
  end

  test "appends cookie params to a caller-supplied cookie header" do
    request =
      %{}
      |> add_param(:headers, :cookie, "existing=zero")
      |> add_param(:cookie, :first, "one")
      |> add_param(:cookie, :second, "two")
      |> finalize_request()

    assert request.headers == [{"cookie", "existing=zero; first=one; second=two"}]
  end

  test "serializes non-exploded array cookie values" do
    request =
      %{}
      |> add_param(:cookie, :id, [3, 4, 5])
      |> finalize_request()

    assert request.headers == [{"cookie", "id=3,4,5"}]
  end

  test "serializes exploded array cookie values" do
    request =
      %{}
      |> add_param(:cookie_exploded, :id, [3, 4, 5])
      |> finalize_request()

    assert request.headers == [{"cookie", "id=3; id=4; id=5"}]
  end

  test "serializes keyword-list cookie values" do
    request =
      %{}
      |> add_param(:cookie, :id, [role: "admin", firstName: "Alex"])
      |> finalize_request()

    assert request.headers == [{"cookie", "id=role,admin,firstName,Alex"}]
  end

  test "serializes exploded object cookie values" do
    request =
      %{}
      |> add_param(:cookie_exploded, :id, [role: "admin", firstName: "Alex"])
      |> finalize_request()

    assert request.headers == [{"cookie", "role=admin; firstName=Alex"}]
  end

  test "serializes generated model structs as object cookie values" do
    category = %Category{id: 1, name: "toys"}

    request =
      %{}
      |> add_param(:cookie, :category, category)
      |> finalize_request()

    assert request.headers == [{"cookie", "category=id,1,name,toys"}]
  end

  test "serializes Date and DateTime cookie values with their scalar forms" do
    date = Date.from_iso8601!("2024-01-02")
    {:ok, date_time, 0} = DateTime.from_iso8601("2024-01-02T03:04:05Z")

    request =
      %{}
      |> add_param(:cookie_exploded, :date, date)
      |> add_param(:cookie_exploded, :date_time, date_time)
      |> finalize_request()

    assert request == %{
             headers: [{"cookie", "date=#{to_string(date)}; date_time=#{to_string(date_time)}"}]
           }
  end

  test "serializes scalar and list cookie values together" do
    request =
      %{}
      |> add_param(:cookie, :token, "abc")
      |> add_param(:cookie, :id, [3, 4, 5])
      |> finalize_request()

    assert request.headers == [{"cookie", "token=abc; id=3,4,5"}]
  end

  test "serializes scalar header values unchanged" do
    request = add_param(%{}, :headers, :x_token, "abc")

    assert request == %{headers: [{"x_token", "abc"}]}
  end

  test "serializes array header values as a comma-joined binary" do
    request = add_param(%{}, :headers, :x_ids, [3, 4, 5])
    [{_, value}] = request.headers

    assert value == "3,4,5"
    assert is_binary(value)
    refute value == <<3, 4, 5>>
  end

  test "serializes map header values in deterministic key order" do
    request = add_param(%{}, :headers, :x_metadata, %{"k2" => "v2", "k1" => "v1"})

    assert request == %{headers: [{"x_metadata", "k1,v1,k2,v2"}]}
  end

  test "does not prepend a separator for an empty caller-supplied cookie header" do
    request =
      %{}
      |> add_param(:headers, :cookie, "  ")
      |> add_param(:cookie, :id, 3)
      |> finalize_request()

    assert request.headers == [{"cookie", "id=3"}]
  end

  test "raises for nested parameter values" do
    assert_raise ArgumentError, ~r/nested parameter values/, fn ->
      add_param(%{}, :cookie, :id, [[1, 2]])
    end
  end

  test "returns an unmapped 2xx response as success" do
    response = %Req.Response{status: 204}

    assert evaluate_response({:ok, response}, []) == {:ok, response}
  end

  test "returns an unmapped 5xx response as an error" do
    response = %Req.Response{status: 500}

    assert evaluate_response({:ok, response}, []) == {:error, response}
  end

  test "encodes optional multipart files as file parts" do
    path = Path.join(System.tmp_dir!(), "request-builder-#{System.unique_integer([:positive])}")
    File.write!(path, "file contents")
    on_exit(fn -> File.rm(path) end)

    request = add_optional_params(%{form_multipart: []}, %{file: :file}, file: path)

    assert [{"file", {%File.Stream{}, options}}] = request.form_multipart
    assert options[:filename] == Path.basename(path)
    assert options[:content_type] == "application/octet-stream"
  end

  test "sends raw binary bodies unchanged in multipart operations" do
    body = <<0, 1, 2, 255>>

    request =
      %{}
      |> method(:post)
      |> url("/multipart")
      |> multipart()
      |> add_param(:body, :body, body)
      |> finalize_request()
      |> Enum.into([])

    Process.put(:request_builder_test_pid, self())

    connection = Connection.new(req_options: [adapter: MultipartBodyAdapter])

    assert {:ok, %Req.Response{status: 200}} = Connection.request(connection, request)
    assert_receive {:multipart_request, sent_request}

    wire_body = IO.iodata_to_binary(sent_request.body)
    assert :binary.match(wire_body, body) != :nomatch
    assert :binary.match(wire_body, "content-type: application/octet-stream") != :nomatch
  end

  test "keeps response decoding disabled when requested Req options enable it" do
    connection = Connection.new(req_options: [decode_body: true])

    assert connection.options[:decode_body] == false
  end

  test "generated binary body operation sends bytes unchanged" do
    body = <<0, 1, 2, 255>>

    Process.put(:request_builder_test_pid, self())

    connection = Connection.new(req_options: [adapter: BinaryBodyAdapter])

    assert {:ok, %Req.Response{status: 200}} = Fake.test_body_with_binary(connection, body)
    assert_receive {:request_body, ^body}
  end
end
