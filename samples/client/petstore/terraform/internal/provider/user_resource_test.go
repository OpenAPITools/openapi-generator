package provider

import (
	"fmt"
	"testing"

	"github.com/hashicorp/terraform-plugin-testing/helper/resource"
)

func TestAccUserResource_basic(t *testing.T) {
	resource.Test(t, resource.TestCase{
		ProtoV6ProviderFactories: testAccProtoV6ProviderFactories,
		Steps: []resource.TestStep{
			// Create and Read testing
			{
				Config: testAccUserResourceConfig("johndoe", "John", "Doe", "john@example.com"),
				Check: resource.ComposeAggregateTestCheckFunc(
					resource.TestCheckResourceAttr("petstore_user.test", "username", "johndoe"),
					resource.TestCheckResourceAttr("petstore_user.test", "first_name", "John"),
					resource.TestCheckResourceAttr("petstore_user.test", "last_name", "Doe"),
					resource.TestCheckResourceAttr("petstore_user.test", "email", "john@example.com"),
				),
			},
			// ImportState testing
			{
				ResourceName:      "petstore_user.test",
				ImportState:       true,
				ImportStateVerify: true,
				// password is sensitive and may not be returned on read
				ImportStateVerifyIgnore: []string{"password"},
			},
			// Update testing
			{
				Config: testAccUserResourceConfig("johndoe", "Jane", "Doe", "jane@example.com"),
				Check: resource.ComposeAggregateTestCheckFunc(
					resource.TestCheckResourceAttr("petstore_user.test", "username", "johndoe"),
					resource.TestCheckResourceAttr("petstore_user.test", "first_name", "Jane"),
					resource.TestCheckResourceAttr("petstore_user.test", "email", "jane@example.com"),
				),
			},
			// Delete testing is automatically handled by TestCase
		},
	})
}

func TestAccUserResource_allFields(t *testing.T) {
	resource.Test(t, resource.TestCase{
		ProtoV6ProviderFactories: testAccProtoV6ProviderFactories,
		Steps: []resource.TestStep{
			{
				Config: testAccUserResourceAllFieldsConfig(),
				Check: resource.ComposeAggregateTestCheckFunc(
					resource.TestCheckResourceAttr("petstore_user.test", "username", "testuser"),
					resource.TestCheckResourceAttr("petstore_user.test", "first_name", "Test"),
					resource.TestCheckResourceAttr("petstore_user.test", "last_name", "User"),
					resource.TestCheckResourceAttr("petstore_user.test", "email", "test@example.com"),
					resource.TestCheckResourceAttr("petstore_user.test", "phone", "555-1234"),
					resource.TestCheckResourceAttr("petstore_user.test", "user_status", "1"),
				),
			},
		},
	})
}

func testAccUserResourceConfig(username, firstName, lastName, email string) string {
	return fmt.Sprintf(`
%s

resource "petstore_user" "test" {
  username   = %[2]q
  first_name = %[3]q
  last_name  = %[4]q
  email      = %[5]q
  password   = "secret123"
}
`, testAccProviderConfig, username, firstName, lastName, email)
}

func testAccUserResourceAllFieldsConfig() string {
	return fmt.Sprintf(`
%s

resource "petstore_user" "test" {
  username    = "testuser"
  first_name  = "Test"
  last_name   = "User"
  email       = "test@example.com"
  password    = "secret123"
  phone       = "555-1234"
  user_status = 1
}
`, testAccProviderConfig)
}
