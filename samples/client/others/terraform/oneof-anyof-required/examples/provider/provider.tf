terraform {
  required_providers {
    oneof = {
      source = "registry.terraform.io/example/oneof-anyof"
    }
  }
}

provider "oneof" {
  endpoint = "http://localhost"
  # api_key  = "your-api-key"
  # token    = "your-bearer-token"
}
