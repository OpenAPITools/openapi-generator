data "petstore_pet" "example" {
  name       = "fido"
  photo_urls = "https://example.com/fido.jpg"
}

output "pet_status" {
  value = data.petstore_pet.example.status
}
