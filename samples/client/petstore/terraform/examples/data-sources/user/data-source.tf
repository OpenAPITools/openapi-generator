data "petstore_user" "example" {
  username = "johndoe"
}

output "user_email" {
  value = data.petstore_user.example.email
}
