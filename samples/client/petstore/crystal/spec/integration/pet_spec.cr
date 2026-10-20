require "../spec_helper"

# Integration tests for the Pet resource sub-client.
#
# These exercise the generated client end-to-end against a live Petstore
# server. The CI workflow (.github/workflows/samples-crystal.yaml) starts a
# swaggerapi/petstore service container, reachable at http://localhost/v2 (the
# default below). Override PETSTORE_HOST / PETSTORE_SCHEME to run elsewhere.
#
# Hand-maintained: NOT produced by the generator. The generated specs under
# spec/api are structural-only and must never hit the network; this file
# provides the real round-trip coverage and is protected from regeneration
# via .openapi-generator-ignore.
Spectator.describe "Pet integration" do
  it "persists a pet and reads it back by id" do
    client = Petstore::Client.new(
      host: ENV.fetch("PETSTORE_HOST", "localhost"),
      scheme: ENV.fetch("PETSTORE_SCHEME", "http"),
    )

    pet_id = 90_000_i64
    pet = Petstore::Pet.new(
      name: "crystal",
      photo_urls: Set(String).new,
      id: pet_id,
    )

    client.pet.create(pet)

    result = client.pet.get(pet_id).value
    expect(result.id).to eq pet_id
    expect(result.name).to eq "crystal"
    expect(result.photo_urls).to eq Set(String).new
    expect(result.status).to be_nil
  end
end
