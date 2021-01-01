require "../../spec_helper"

class HTTP::Client::DigestAuth
  # Hardcode nonce value for specs
  private def make_cnonce
    "9ea5ff3bd34554a4165bbdc1df91dcff"
  end
end

uri = URI.parse("http://www.example.com/")
cnonce = ""
header = ""
expected = [] of String

da = HTTP::Client::DigestAuth.new

describe "HTTP::Client::DigestAuth" do
  Spec.before_each do
    uri = URI.parse("http://www.example.com/")
    uri.user = "user"
    uri.password = "password"

    cnonce = "9ea5ff3bd34554a4165bbdc1df91dcff"

    header = [
      "Digest qop=\"auth\"",
      "realm=\"www.example.com\"",
      "nonce=\"4107baa081a592a6021660200000cd6c5686ff5f579324402b374d83e2c9\"",
    ].join ", "

    expected = [
      "Digest username=\"user\"",
      "realm=\"www.example.com\"",
      "algorithm=MD5",
      "uri=\"/\"",
      "nonce=\"4107baa081a592a6021660200000cd6c5686ff5f579324402b374d83e2c9\"",
      "response=\"67be92a5e7b38d08679957db04f5da04\"",
      "qop=auth",
      "nc=00000000",
      "cnonce=\"9ea5ff3bd34554a4165bbdc1df91dcff\"",
    ]

    da = HTTP::Client::DigestAuth.new
  end

  it "test auth_header" do
    (da.auth_header(uri, header, "GET")).should eq(expected.join(", "))

    expected[7] = "nc=00000001"
    expected[5] = "response=\"1f5f0cd1588690c1303737f081c0b9bb\""

    (da.auth_header(uri, header, "GET")).should eq(expected.join(", "))
  end

  it "test auth_header iis" do
    expected[6] = "qop=\"auth\""

    (da.auth_header(uri, header, "GET", true)).should eq(expected.join(", "))
  end

  it "test auth_header no qop" do
    header = header.sub(" qop=\"auth\",", "")

    expected[5] = "response=\"32f6ca1631ccf7c42a8075deff44e470\""
    expected.delete("qop=auth")
    expected.delete("cnonce=\"9ea5ff3bd34554a4165bbdc1df91dcff\"")
    expected.delete("nc=00000000")

    (da.auth_header(uri, header, "GET")).should eq(expected.join(", "))
  end

  it "test auth_header opaque" do
    expected << "opaque=\"5ccc069c403ebaf9f0171e9517f40e41\""
    header = header + "opaque=\"5ccc069c403ebaf9f0171e9517f40e41\""

    (da.auth_header(uri, header, "GET")).should eq(expected.join(", "))
  end

  it "test auth_header post" do
    expected[5] = "response=\"d82219e1e5430b136bbae1670fa51d48\""

    (da.auth_header(uri, header, "POST")).should eq(expected.join(", "))
  end

  it "test auth_header sess" do
    header = header + ", algorithm=MD5-sess"

    expected[2] = "algorithm=MD5-sess"
    expected[5] = "response=\"c22c5bd9112a86ca78ddc1ae772daeeb\""

    (da.auth_header(uri, header, "GET")).should eq(expected.join(", "))
  end

  it "test auth_header sha1" do
    expected[2] = "algorithm=SHA1"
    expected[5] = "response=\"2cb62fc18f7b0ebdc34543f896bb77686b4115e4\""

    header = header + "algorithm=SHA1"

    (da.auth_header(uri, header, "GET")).should eq(expected.join(", "))
  end

  it "test auth_header unknown algorithm" do
    header = header + "algorithm=bogus"

    expect_raises HTTP::Client::DigestAuth::Error, "unknown algorithm \"bogus\"" do
      da.auth_header(uri, header, "GET")
    end
  end

  it "test auth_header quoted algorithm" do
    header = header + "algorithm=\"MD5\""

    (da.auth_header(uri, header, "GET")).should eq(expected.join(", "))
  end
end
