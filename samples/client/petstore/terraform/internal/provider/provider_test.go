package provider

import (
	"github.com/hashicorp/terraform-plugin-framework/providerserver"
	"github.com/hashicorp/terraform-plugin-go/tfprotov6"
)

// testAccProtoV6ProviderFactories are used to instantiate the provider during
// acceptance testing. The factory function is called for every Terraform CLI
// command executed to create a provider server to which the CLI can reattach.
var testAccProtoV6ProviderFactories = map[string]func() (tfprotov6.ProviderServer, error){
	"petstore": providerserver.NewProtocol6WithError(New("test")()),
}

const testAccProviderConfig = `
provider "petstore" {
  endpoint = "http://petstore.swagger.io/v2"
}
`
