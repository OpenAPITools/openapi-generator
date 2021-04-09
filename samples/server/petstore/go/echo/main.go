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
	

	e.POST("/v2/pet", c.AddPet)

	e.DELETE("/v2/pet/:petId", c.DeletePet)

	e.GET("/v2/pet/findByStatus", c.FindPetsByStatus)

	e.GET("/v2/pet/findByTags", c.FindPetsByTags)

	e.GET("/v2/pet/:petId", c.GetPetById)

	e.PUT("/v2/pet", c.UpdatePet)

	e.POST("/v2/pet/:petId", c.UpdatePetWithForm)

	e.POST("/v2/pet/:petId/uploadImage", c.UploadFile)

	e.DELETE("/v2/store/order/:orderId", c.DeleteOrder)

	e.GET("/v2/store/inventory", c.GetInventory)

	e.GET("/v2/store/order/:orderId", c.GetOrderById)

	e.POST("/v2/store/order", c.PlaceOrder)

	e.POST("/v2/user", c.CreateUser)

	e.POST("/v2/user/createWithArray", c.CreateUsersWithArrayInput)

	e.POST("/v2/user/createWithList", c.CreateUsersWithListInput)

	e.DELETE("/v2/user/:username", c.DeleteUser)

	e.GET("/v2/user/:username", c.GetUserByName)

	e.GET("/v2/user/login", c.LoginUser)

	e.GET("/v2/user/logout", c.LogoutUser)

	e.PUT("/v2/user/:username", c.UpdateUser)


	// Start server
	e.Logger.Fatal(e.Start(":8080"))
}