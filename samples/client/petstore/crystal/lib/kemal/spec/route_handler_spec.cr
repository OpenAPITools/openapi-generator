require "./spec_helper"

describe "Kemal::RouteHandler" do
  it "routes" do
    get "/" do
      "hello"
    end
    request = HTTP::Request.new("GET", "/")
    client_response = call_request_on_app(request)
    client_response.body.should eq("hello")
  end

  it "routes with long response body" do
    long_response_body = "string" * 10_000

    get "/" do
      long_response_body
    end
    request = HTTP::Request.new("GET", "/")
    client_response = call_request_on_app(request)
    client_response.body.should eq(long_response_body)
  end

  it "routes should only return strings" do
    get "/" do
      100
    end
    request = HTTP::Request.new("GET", "/")
    client_response = call_request_on_app(request)
    client_response.body.should eq("")
  end

  it "routes request with query string" do
    get "/" do |env|
      "hello #{env.params.query["message"]}"
    end
    request = HTTP::Request.new("GET", "/?message=world")
    client_response = call_request_on_app(request)
    client_response.body.should eq("hello world")
  end

  it "routes request with multiple query strings" do
    get "/" do |env|
      "hello #{env.params.query["message"]} time #{env.params.query["time"]}"
    end
    request = HTTP::Request.new("GET", "/?message=world&time=now")
    client_response = call_request_on_app(request)
    client_response.body.should eq("hello world time now")
  end

  it "route parameter has more precedence than query string arguments" do
    get "/:message" do |env|
      "hello #{env.params.url["message"]}"
    end
    request = HTTP::Request.new("GET", "/world?message=coco")
    client_response = call_request_on_app(request)
    client_response.body.should eq("hello world")
  end

  it "parses simple JSON body" do
    post "/" do |env|
      name = env.params.json["name"]
      age = env.params.json["age"]
      "Hello #{name} Age #{age}"
    end

    json_payload = {"name": "Serdar", "age": 26}
    request = HTTP::Request.new(
      "POST",
      "/",
      body: json_payload.to_json,
      headers: HTTP::Headers{"Content-Type" => "application/json"},
    )
    client_response = call_request_on_app(request)
    client_response.body.should eq("Hello Serdar Age 26")
  end

  it "parses JSON with string array" do
    post "/" do |env|
      skills = env.params.json["skills"].as(Array)
      "Skills #{skills.each.join(',')}"
    end

    json_payload = {"skills": ["ruby", "crystal"]}
    request = HTTP::Request.new(
      "POST",
      "/",
      body: json_payload.to_json,
      headers: HTTP::Headers{"Content-Type" => "application/json"},
    )
    client_response = call_request_on_app(request)
    client_response.body.should eq("Skills ruby,crystal")
  end

  it "parses JSON with json object array" do
    post "/" do |env|
      skills = env.params.json["skills"].as(Array)
      skills_from_languages = skills.map do |skill|
        skill["language"]
      end
      "Skills #{skills_from_languages.each.join(',')}"
    end

    json_payload = {"skills": [{"language": "ruby"}, {"language": "crystal"}]}
    request = HTTP::Request.new(
      "POST",
      "/",
      body: json_payload.to_json,
      headers: HTTP::Headers{"Content-Type" => "application/json"},
    )

    client_response = call_request_on_app(request)
    client_response.body.should eq("Skills ruby,crystal")
  end

  it "can process HTTP HEAD requests for defined GET routes" do
    get "/" do
      "Hello World from GET"
    end
    request = HTTP::Request.new("HEAD", "/")
    client_response = call_request_on_app(request)
    client_response.status_code.should eq(200)
  end

  it "redirects user to provided url" do
    get "/" do |env|
      env.redirect "/login"
    end
    request = HTTP::Request.new("GET", "/")
    client_response = call_request_on_app(request)
    client_response.status_code.should eq(302)
    client_response.body.should eq("")
    client_response.headers.has_key?("Location").should eq(true)
  end

  it "redirects with body" do
    get "/" do |env|
      env.redirect "/login", body: "Redirecting to /login"
    end
    request = HTTP::Request.new("GET", "/")
    client_response = call_request_on_app(request)
    client_response.status_code.should eq(302)
    client_response.body.should eq("Redirecting to /login")
    client_response.headers.has_key?("Location").should eq(true)
  end
end
