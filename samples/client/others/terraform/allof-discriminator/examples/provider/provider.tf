terraform {
  required_providers {
    allof = {
      source = "registry.terraform.io/example/allof"
    }
  }
}

provider "allof" {
  endpoint = "http://localhost"
  # api_key  = "your-api-key"
  # token    = "your-bearer-token"
}
