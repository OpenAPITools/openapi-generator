terraform {
  required_providers {
    petstore = {
      source = "registry.terraform.io/example/petstore-addpet"
    }
  }
}

provider "petstore" {
  endpoint = "http://petstore.swagger.io/v2"
  # api_key  = "your-api-key"
  # token    = "your-bearer-token"
}
