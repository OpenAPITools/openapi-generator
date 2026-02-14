terraform {
  required_providers {
    oneof = {
      source = "registry.terraform.io/example/oneof-disc"
    }
  }
}

provider "oneof" {
  endpoint = "http://localhost"
  # api_key  = "your-api-key"
  # token    = "your-bearer-token"
}
