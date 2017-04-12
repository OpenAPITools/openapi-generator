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
		"/v2/",
		Index,
	},

	Route{
		"AddPet",
		"POST",
		"/v2/pet",
		AddPet,
	},

	Route{
		"DeletePet",
		"DELETE",
		"/v2/pet/{petId}",
		DeletePet,
	},

	Route{
		"FindPetsByStatus",
		"GET",
		"/v2/pet/findByStatus",
		FindPetsByStatus,
	},

	Route{
		"FindPetsByTags",
		"GET",
		"/v2/pet/findByTags",
		FindPetsByTags,
	},

	Route{
		"GetPetById",
		"GET",
		"/v2/pet/{petId}",
		GetPetById,
	},

	Route{
		"UpdatePet",
		"PUT",
		"/v2/pet",
		UpdatePet,
	},

	Route{
		"UpdatePetWithForm",
		"POST",
		"/v2/pet/{petId}",
		UpdatePetWithForm,
	},

	Route{
		"UploadFile",
		"POST",
		"/v2/pet/{petId}/uploadImage",
		UploadFile,
	},

	Route{
		"DeleteOrder",
		"DELETE",
		"/v2/store/order/{orderId}",
		DeleteOrder,
	},

	Route{
		"GetInventory",
		"GET",
		"/v2/store/inventory",
		GetInventory,
	},

	Route{
		"GetOrderById",
		"GET",
		"/v2/store/order/{orderId}",
		GetOrderById,
	},

	Route{
		"PlaceOrder",
		"POST",
		"/v2/store/order",
		PlaceOrder,
	},

	Route{
		"CreateUser",
		"POST",
		"/v2/user",
		CreateUser,
	},

	Route{
		"CreateUsersWithArrayInput",
		"POST",
		"/v2/user/createWithArray",
		CreateUsersWithArrayInput,
	},

	Route{
		"CreateUsersWithListInput",
		"POST",
		"/v2/user/createWithList",
		CreateUsersWithListInput,
	},

	Route{
		"DeleteUser",
		"DELETE",
		"/v2/user/{username}",
		DeleteUser,
	},

	Route{
		"GetUserByName",
		"GET",
		"/v2/user/{username}",
		GetUserByName,
	},

	Route{
		"LoginUser",
		"GET",
		"/v2/user/login",
		LoginUser,
	},

	Route{
		"LogoutUser",
		"GET",
		"/v2/user/logout",
		LogoutUser,
	},

	Route{
		"UpdateUser",
		"PUT",
		"/v2/user/{username}",
		UpdateUser,
	},

}
