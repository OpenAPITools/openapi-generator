package petstoreserver

import (
	"net/http"
	"fmt"
	"github.com/gorilla/mux"
)

type Route struct {
	Name        string
	Method      string
	Pattern     string
	HandlerFunc http.HandlerFunc
}

type Routes []Route

func NewRouter() *mux.Router {
	router := mux.NewRouter().StrictSlash(true)
	for _, route := range routes {
		var handler http.Handler
		handler = route.HandlerFunc
		handler = Logger(handler, route.Name)

		router.
			Methods(route.Method).
			Path(route.Pattern).
			Name(route.Name).
			Handler(handler)
	}

	return router
}

func Index(w http.ResponseWriter, r *http.Request) {
	fmt.Fprintf(w, "Hello World!")
}

var routes = Routes{
	Route{
		"Index",
		"GET",
		"/",
		Index,
	},

	Route{
		"AddPet",
		"POST",
		"/pet",
		AddPet,
	},

	Route{
		"DeletePet",
		"DELETE",
		"/pet/{petId}",
		DeletePet,
	},

	Route{
		"FindPetsByStatus",
		"GET",
		"/pet/findByStatus",
		FindPetsByStatus,
	},

	Route{
		"FindPetsByTags",
		"GET",
		"/pet/findByTags",
		FindPetsByTags,
	},

	Route{
		"GetPetById",
		"GET",
		"/pet/{petId}",
		GetPetById,
	},

	Route{
		"UpdatePet",
		"PUT",
		"/pet",
		UpdatePet,
	},

	Route{
		"UpdatePetWithForm",
		"POST",
		"/pet/{petId}",
		UpdatePetWithForm,
	},

	Route{
		"UploadFile",
		"POST",
		"/pet/{petId}/uploadImage",
		UploadFile,
	},

	Route{
		"DeleteOrder",
		"DELETE",
		"/store/order/{orderId}",
		DeleteOrder,
	},

	Route{
		"GetInventory",
		"GET",
		"/store/inventory",
		GetInventory,
	},

	Route{
		"GetOrderById",
		"GET",
		"/store/order/{orderId}",
		GetOrderById,
	},

	Route{
		"PlaceOrder",
		"POST",
		"/store/order",
		PlaceOrder,
	},

	Route{
		"CreateUser",
		"POST",
		"/user",
		CreateUser,
	},

	Route{
		"CreateUsersWithArrayInput",
		"POST",
		"/user/createWithArray",
		CreateUsersWithArrayInput,
	},

	Route{
		"CreateUsersWithListInput",
		"POST",
		"/user/createWithList",
		CreateUsersWithListInput,
	},

	Route{
		"DeleteUser",
		"DELETE",
		"/user/{username}",
		DeleteUser,
	},

	Route{
		"GetUserByName",
		"GET",
		"/user/{username}",
		GetUserByName,
	},

	Route{
		"LoginUser",
		"GET",
		"/user/login",
		LoginUser,
	},

	Route{
		"LogoutUser",
		"GET",
		"/user/logout",
		LogoutUser,
	},

	Route{
		"UpdateUser",
		"PUT",
		"/user/{username}",
		UpdateUser,
	},

}