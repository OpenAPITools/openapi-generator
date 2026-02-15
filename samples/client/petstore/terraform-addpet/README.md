# Terraform Provider petstore

This Terraform provider was generated using [OpenAPI Generator](https://openapi-generator.tech).

## Requirements

- [Terraform](https://www.terraform.io/downloads.html) >= 1.0
- [Go](https://golang.org/doc/install) >= 1.24

## Building The Provider

1. Clone the repository
2. Enter the repository directory
3. Build the provider using the Go `install` command:

```shell
go install
```

## Using the provider

```hcl
terraform {
  required_providers {
    petstore = {
      source = "registry.terraform.io/example/petstore-addpet"
    }
  }
}

provider "petstore" {
  endpoint = "http://petstore.swagger.io/v2"
}
```

## Developing the Provider

If you wish to work on the provider, you'll first need [Go](http://www.golang.org) installed on your machine (see [Requirements](#requirements) above).

To compile the provider, run `go install`. This will build the provider and put the provider binary in the `$GOPATH/bin` directory.

For local development and testing, add a `dev_overrides` block to your `~/.terraformrc` so that Terraform uses the locally built binary instead of fetching from the registry:

```hcl
provider_installation {
  dev_overrides {
    "registry.terraform.io/example/petstore-addpet" = "${GOPATH}/bin"
  }
  direct {}
}
```

With `dev_overrides` configured, you do not need to run `terraform init` for the provider.

To generate or update documentation, run `go generate`.

In order to run the full suite of Acceptance tests, run `make testacc`.

```shell
make testacc
```
