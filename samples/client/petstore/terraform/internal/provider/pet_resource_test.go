package provider

import (
	"fmt"
	"testing"

	"github.com/hashicorp/terraform-plugin-testing/helper/resource"
)

func TestAccPetResource_basic(t *testing.T) {
	resource.Test(t, resource.TestCase{
		ProtoV6ProviderFactories: testAccProtoV6ProviderFactories,
		Steps: []resource.TestStep{
			// Create and Read testing
			{
				Config: testAccPetResourceConfig("fido", "available"),
				Check: resource.ComposeAggregateTestCheckFunc(
					resource.TestCheckResourceAttr("petstore_pet.test", "name", "fido"),
					resource.TestCheckResourceAttr("petstore_pet.test", "status", "available"),
					resource.TestCheckResourceAttrSet("petstore_pet.test", "id"),
				),
			},
			// ImportState testing
			{
				ResourceName:      "petstore_pet.test",
				ImportState:       true,
				ImportStateVerify: true,
			},
			// Delete testing is automatically handled by TestCase
		},
	})
}

func TestAccPetResource_statusPending(t *testing.T) {
	resource.Test(t, resource.TestCase{
		ProtoV6ProviderFactories: testAccProtoV6ProviderFactories,
		Steps: []resource.TestStep{
			{
				Config: testAccPetResourceConfig("buddy", "pending"),
				Check: resource.ComposeAggregateTestCheckFunc(
					resource.TestCheckResourceAttr("petstore_pet.test", "name", "buddy"),
					resource.TestCheckResourceAttr("petstore_pet.test", "status", "pending"),
				),
			},
		},
	})
}

func testAccPetResourceConfig(name string, status string) string {
	return fmt.Sprintf(`
%s

resource "petstore_pet" "test" {
  name       = %[2]q
  status     = %[3]q
  photo_urls = ["https://example.com/photo.jpg"]
}
`, testAccProviderConfig, name, status)
}
