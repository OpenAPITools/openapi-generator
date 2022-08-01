package main

import (
	"github.com/GIT_USER_ID/GIT_REPO_ID/handlers"
	"github.com/labstack/echo/v4"
	"github.com/labstack/echo/v4/middleware"
)

func main() {
	e := echo.New()

    //todo: handle the error!
	c, _ := handlers.NewContainer()

	// Middleware
	e.Use(middleware.Logger())
	e.Use(middleware.Recover())


	// AddPet - Add a new pet to the store
	e.POST("/v2/pet", c.AddPet)

	// DeletePet - Deletes a pet
	e.DELETE("/v2/pet/:petId", c.DeletePet)

	// FindPetsByStatus - Finds Pets by status
	e.GET("/v2/pet/findByStatus", c.FindPetsByStatus)

	// FindPetsByTags - Finds Pets by tags (deprecated)
	e.GET("/v2/pet/findByTags", c.FindPetsByTags)

	// GetPetById - Find pet by ID
	e.GET("/v2/pet/:petId", c.GetPetById)

	// UpdatePet - Update an existing pet
	e.PUT("/v2/pet", c.UpdatePet)

	// UpdatePetWithForm - Updates a pet in the store with form data
	e.POST("/v2/pet/:petId", c.UpdatePetWithForm)

	// UploadFile - uploads an image
	e.POST("/v2/pet/:petId/uploadImage", c.UploadFile)

	// DeleteOrder - Delete purchase order by ID
	e.DELETE("/v2/store/order/:orderId", c.DeleteOrder)

	// GetInventory - Returns pet inventories by status
	e.GET("/v2/store/inventory", c.GetInventory)

	// GetOrderById - Find purchase order by ID
	e.GET("/v2/store/order/:orderId", c.GetOrderById)

	// PlaceOrder - Place an order for a pet
	e.POST("/v2/store/order", c.PlaceOrder)

	// CreateUser - Create user
	e.POST("/v2/user", c.CreateUser)

	// CreateUsersWithArrayInput - Creates list of users with given input array
	e.POST("/v2/user/createWithArray", c.CreateUsersWithArrayInput)

	// CreateUsersWithListInput - Creates list of users with given input array
	e.POST("/v2/user/createWithList", c.CreateUsersWithListInput)

	// DeleteUser - Delete user
	e.DELETE("/v2/user/:username", c.DeleteUser)

	// GetUserByName - Get user by user name
	e.GET("/v2/user/:username", c.GetUserByName)

	// LoginUser - Logs user into the system
	e.GET("/v2/user/login", c.LoginUser)

	// LogoutUser - Logs out current logged in user session
	e.GET("/v2/user/logout", c.LogoutUser)

	// UpdateUser - Updated user
	e.PUT("/v2/user/:username", c.UpdateUser)


	// Start server
	e.Logger.Fatal(e.Start(":8080"))
}