defmodule RequestBuilderTest do
  use ExUnit.Case, async: true

  alias OpenapiPetstore.RequestBuilder
  alias OpenapiPetstore.Model.Category

  test "renders multiple cookie parameters and merges a caller-supplied header" do
    request =
      %{headers: [{"cookie", "caller=provided"}]}
      |> RequestBuilder.add_param(:cookie, :first, "one")
      |> RequestBuilder.add_param(:cookie, :second, "two")
      |> RequestBuilder.finalize_request()

    assert request == %{headers: [{"cookie", "caller=provided; first=one; second=two"}]}
  end

  test "serializes non-exploded array cookie values" do
    request =
      %{}
      |> RequestBuilder.add_param(:cookie, :id, [3, 4, 5])
      |> RequestBuilder.finalize_request()

    assert request == %{headers: [{"cookie", "id=3,4,5"}]}
  end

  test "serializes exploded array cookie values" do
    request =
      %{}
      |> RequestBuilder.add_param(:cookie_exploded, :id, [3, 4, 5])
      |> RequestBuilder.finalize_request()

    assert request == %{headers: [{"cookie", "id=3; id=4; id=5"}]}
  end

  test "serializes keyword-list cookie values" do
    request =
      %{}
      |> RequestBuilder.add_param(:cookie, :id, role: "admin", firstName: "Alex")
      |> RequestBuilder.finalize_request()

    assert request == %{headers: [{"cookie", "id=role,admin,firstName,Alex"}]}
  end

  test "serializes exploded object cookie values" do
    request =
      %{}
      |> RequestBuilder.add_param(:cookie_exploded, :id, role: "admin", firstName: "Alex")
      |> RequestBuilder.finalize_request()

    assert request == %{headers: [{"cookie", "role=admin; firstName=Alex"}]}
  end

  test "serializes generated model structs as object cookie values" do
    category = %Category{id: 1, name: "toys"}

    request =
      %{}
      |> RequestBuilder.add_param(:cookie, :category, category)
      |> RequestBuilder.finalize_request()

    assert request == %{headers: [{"cookie", "category=id,1,name,toys"}]}
  end

  test "serializes Date values nested in an object cookie" do
    date = Date.from_iso8601!("2026-01-01")

    request =
      %{}
      |> RequestBuilder.add_param(:cookie_exploded, :metadata, %{expires: date})
      |> RequestBuilder.finalize_request()

    assert request == %{headers: [{"cookie", "expires=#{to_string(date)}"}]}
  end

  test "serializes Date and DateTime cookie values with their scalar forms" do
    date = Date.from_iso8601!("2024-01-02")
    {:ok, date_time, 0} = DateTime.from_iso8601("2024-01-02T03:04:05Z")

    request =
      %{}
      |> RequestBuilder.add_param(:cookie_exploded, :date, date)
      |> RequestBuilder.add_param(:cookie_exploded, :date_time, date_time)
      |> RequestBuilder.finalize_request()

    assert request == %{
             headers: [{"cookie", "date=#{to_string(date)}; date_time=#{to_string(date_time)}"}]
           }
  end

  test "serializes scalar and list cookie values together" do
    request =
      %{}
      |> RequestBuilder.add_param(:cookie, :token, "abc")
      |> RequestBuilder.add_param(:cookie, :id, [3, 4, 5])
      |> RequestBuilder.finalize_request()

    assert request == %{headers: [{"cookie", "token=abc; id=3,4,5"}]}
  end

  test "serializes scalar header values unchanged" do
    request = RequestBuilder.add_param(%{}, :headers, :x_token, "abc")

    assert request == %{headers: [{"x_token", "abc"}]}
  end

  test "serializes a scalar header charlist as a string" do
    request = RequestBuilder.add_param(%{}, :headers, :x_token, ~c"abc")

    assert request == %{headers: [{"x_token", "abc"}]}
  end

  test "serializes array header values as a comma-joined binary" do
    request = RequestBuilder.add_param(%{}, :headers_form, :x_ids, [3, 4, 5])
    [{_, value}] = request.headers

    assert value == "3,4,5"
    assert is_binary(value)
    refute value == <<3, 4, 5>>
  end

  test "serializes map header values in deterministic key order" do
    request =
      RequestBuilder.add_param(%{}, :headers_form, :x_metadata, %{"k2" => "v2", "k1" => "v1"})

    assert request == %{headers: [{"x_metadata", "k1,v1,k2,v2"}]}
  end

  test "serializes non-exploded object header values in deterministic key order" do
    request =
      RequestBuilder.add_param(%{}, :headers_form, :x_metadata, role: "admin", firstName: "Alex")

    assert request == %{headers: [{"x_metadata", "role,admin,firstName,Alex"}]}
  end

  test "serializes exploded object header values in deterministic key order" do
    request =
      RequestBuilder.add_param(
        %{},
        :headers_form_exploded,
        :x_metadata,
        role: "admin",
        firstName: "Alex"
      )

    assert request == %{headers: [{"x_metadata", "role=admin,firstName=Alex"}]}
  end

  test "serializes Date values in an array header" do
    dates = [Date.from_iso8601!("2026-01-01"), Date.from_iso8601!("2026-01-02")]

    request = RequestBuilder.add_param(%{}, :headers_form, :x_dates, dates)

    assert request == %{headers: [{"x_dates", "2026-01-01,2026-01-02"}]}
  end

  test "serializes DateTime values in an exploded object header" do
    {:ok, date_time, 0} = DateTime.from_iso8601("2026-01-01T03:04:05Z")

    request =
      RequestBuilder.add_param(%{}, :headers_form_exploded, :x_metadata, %{k: date_time})

    assert request == %{headers: [{"x_metadata", "k=#{to_string(date_time)}"}]}
  end

  test "serializes model structs as object headers" do
    category = %Category{id: 1, name: "toys"}

    request = RequestBuilder.add_param(%{}, :headers_form, :x_category, category)

    assert request == %{headers: [{"x_category", "id,1,name,toys"}]}
  end

  test "serializes Date and DateTime headers with their scalar forms" do
    date = Date.from_iso8601!("2024-01-02")
    {:ok, date_time, 0} = DateTime.from_iso8601("2024-01-02T03:04:05Z")

    request =
      %{}
      |> RequestBuilder.add_param(:headers, :date, date)
      |> RequestBuilder.add_param(:headers, :date_time, date_time)

    assert request == %{
             headers: [{"date", to_string(date)}, {"date_time", to_string(date_time)}]
           }
  end

  test "does not prepend a separator for an empty caller-supplied cookie header" do
    request =
      %{headers: [{"cookie", "  "}]}
      |> RequestBuilder.add_param(:cookie, :id, 3)
      |> RequestBuilder.finalize_request()

    assert request == %{headers: [{"cookie", "id=3"}]}
  end

  test "raises for nested cookie values" do
    assert_raise ArgumentError, ~r/nested parameter values/, fn ->
      RequestBuilder.add_param(%{}, :cookie, :id, [[1, 2]])
    end
  end

  test "raises for nested header values" do
    assert_raise ArgumentError, ~r/nested parameter values/, fn ->
      RequestBuilder.add_param(%{}, :headers_form, :id, [[1, 2]])
    end
  end

  test "raises for a list nested inside an object" do
    assert_raise ArgumentError, ~r/nested parameter values/, fn ->
      RequestBuilder.add_param(%{}, :cookie, :id, %{tags: [1, 2]})
    end

    assert_raise ArgumentError, ~r/nested parameter values/, fn ->
      RequestBuilder.add_param(%{}, :headers_form_exploded, :id, %{tags: [1, 2]})
    end
  end

  test "returns an unmapped 2xx response as successful" do
    env = %Tesla.Env{status: 204}

    assert RequestBuilder.evaluate_response({:ok, env}, []) == {:ok, env}
  end

  test "returns an unmapped 5xx response as an error" do
    env = %Tesla.Env{status: 500}

    assert RequestBuilder.evaluate_response({:ok, env}, []) == {:error, env}
  end

  test "multipart file and form field accumulate into one Tesla.Multipart body" do
    path = Path.join(System.tmp_dir!(), "rb_multipart_#{System.unique_integer([:positive])}.txt")
    File.write!(path, "hello")

    try do
      request =
        RequestBuilder.add_optional_params(
          %{},
          %{:additionalMetadata => :multipart_form, :file => :file},
          additionalMetadata: "meta",
          file: path
        )

      assert %Tesla.Multipart{} = request.body
      names = Enum.map(request.body.parts, & &1.dispositions[:name])
      assert "additionalMetadata" in names
      assert "file" in names
    after
      File.rm(path)
    end
  end

  test "multipart form fields encode structured values as JSON and scalars as text" do
    request =
      RequestBuilder.add_optional_params(
        %{},
        %{:meta => :multipart_form, :count => :multipart_form, :when => :multipart_form},
        meta: %{"a" => 1},
        count: 42,
        when: ~D[2026-01-02]
      )

    parts = Map.new(request.body.parts, &{&1.dispositions[:name], &1})

    assert parts["meta"].body == ~s({"a":1})
    assert parts["meta"].headers == [{:"Content-Type", "application/json"}]
    assert parts["count"].body == "42"
    assert parts["count"].headers == []
    assert parts["when"].body == "2026-01-02"
  end
end
