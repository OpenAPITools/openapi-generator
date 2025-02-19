#tag Class
Public Class Mock
	#tag Method, Flags = &h0
		Sub testPetApi(basePath As String)
		  Dim error As New XojoOpenAPIClientSynchronous.XojoOpenAPIClientSynchronousException(0, "No Error")
		  Dim tmpDownloadFile As FolderItem = GetTemporaryFolderItem
		  
		  Dim api As New XojoOpenAPIClientSynchronous.APIs.PetApi
		  api.BasePath = basePath
		  api.BasicAuthUser = "user"
		  api.BasicAuthPassword = "password"
		  api.UseHTTPS = false
		  
		  
		  // Operation addPet
		  // Add a new pet to the store
		  // - parameter localOutStatus: Information on whether the operation succeeded.
		  // - parameter localOutData: On success, contains the data returned by the API.
		  // - parameter pet: (body) Pet object that needs to be added to the store 
		  //
		  // - POST /pet
		  // - 
		  // - defaultResponse: Nil
		  //
		  // - OAuth:
		  //   - type: oauth2
		  //   - name: petstore_auth
		  //
		  Dim AddPetpetModel As New XojoOpenAPIClientSynchronous.Models.Pet
		  AddPetpetModel.name = "doggie"
		  
		  Dim AddPetData As XojoOpenAPIClientSynchronous.Models.Pet
		  If api.AddPet(error, AddPetData, AddPetpetModel) Then
		    Print("[+] PetApi.AddPet successful.")
		  Else
		    Print("[-] PetApi.AddPet unsuccessful.")
			Print("    " + " | ErrorNumber: " + Str(error.ErrorNumber) + " | HTTP Status: " + Str(error.HTTPCode) + " | SocketCode: " + Str(error.SocketCode) + " | Message: " + error.Message)
		  End If
		  
		  
		  // Operation deletePet
		  // Deletes a pet
		  // - parameter localOutStatus: Information on whether the operation succeeded.
		  // - parameter petId: (path) Pet id to delete 
		  // - parameter apiKey: (header)  (optional, default to Sample)
		  //
		  // - DELETE /pet/{petId}
		  // - 
		  //
		  // - OAuth:
		  //   - type: oauth2
		  //   - name: petstore_auth
		  //
		  If api.DeletePet(error, 789, "apiKey_example") Then
		    Print("[+] PetApi.DeletePet successful.")
		  Else
		    Print("[-] PetApi.DeletePet unsuccessful.")
			Print("    " + " | ErrorNumber: " + Str(error.ErrorNumber) + " | HTTP Status: " + Str(error.HTTPCode) + " | SocketCode: " + Str(error.SocketCode) + " | Message: " + error.Message)
		  End If
		  
		  
		  // Operation findPetsByStatus
		  // Finds Pets by status
		  // - parameter localOutStatus: Information on whether the operation succeeded.
		  // - parameter localOutData: On success, contains the data returned by the API.
		  // - parameter status: (query) Status values that need to be considered for filter 
		  //
		  // - GET /pet/findByStatus
		  // - Multiple status values can be provided with comma separated strings
		  // - defaultResponse: Nil
		  //
		  // - OAuth:
		  //   - type: oauth2
		  //   - name: petstore_auth
		  //
		  Dim FindPetsByStatusstatusArray() As String
		  Dim FindPetsByStatusData() As XojoOpenAPIClientSynchronous.Models.Pet
		  If api.FindPetsByStatus(error, FindPetsByStatusData, XojoOpenAPIClientSynchronous.APIs.PetApi.StatusEnum_FindPetsByStatus.Available) Then
		    Print("[+] PetApi.FindPetsByStatus successful.")
		  Else
		    Print("[-] PetApi.FindPetsByStatus unsuccessful.")
			Print("    " + " | ErrorNumber: " + Str(error.ErrorNumber) + " | HTTP Status: " + Str(error.HTTPCode) + " | SocketCode: " + Str(error.SocketCode) + " | Message: " + error.Message)
		  End If
		  
		  
		  // Operation findPetsByTags
		  // Finds Pets by tags
		  // - parameter localOutStatus: Information on whether the operation succeeded.
		  // - parameter localOutData: On success, contains the data returned by the API.
		  // - parameter tags: (query) Tags to filter by 
		  //
		  // - GET /pet/findByTags
		  // - Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
		  // - defaultResponse: Nil
		  //
		  // - OAuth:
		  //   - type: oauth2
		  //   - name: petstore_auth
		  //
		  Dim FindPetsByTagstagsArray() As String
		  Dim FindPetsByTagsData() As XojoOpenAPIClientSynchronous.Models.Pet
		  If api.FindPetsByTags(error, FindPetsByTagsData, FindPetsByTagstagsArray) Then
		    Print("[+] PetApi.FindPetsByTags successful.")
		  Else
		    Print("[-] PetApi.FindPetsByTags unsuccessful.")
			Print("    " + " | ErrorNumber: " + Str(error.ErrorNumber) + " | HTTP Status: " + Str(error.HTTPCode) + " | SocketCode: " + Str(error.SocketCode) + " | Message: " + error.Message)
		  End If
		  
		  
		  // Operation getPetById
		  // Find pet by ID
		  // - parameter localOutStatus: Information on whether the operation succeeded.
		  // - parameter localOutData: On success, contains the data returned by the API.
		  // - parameter petId: (path) ID of pet to return 
		  //
		  // - GET /pet/{petId}
		  // - Returns a single pet
		  // - defaultResponse: Nil
		  //
		  // - API Key:
		  //   - type: apiKey api_key (HEADER)
		  //   - name: api_key
		  //
		  Dim GetPetByIdData As XojoOpenAPIClientSynchronous.Models.Pet
		  If api.GetPetById(error, GetPetByIdData, 789) Then
		    Print("[+] PetApi.GetPetById successful.")
		  Else
		    Print("[-] PetApi.GetPetById unsuccessful.")
			Print("    " + " | ErrorNumber: " + Str(error.ErrorNumber) + " | HTTP Status: " + Str(error.HTTPCode) + " | SocketCode: " + Str(error.SocketCode) + " | Message: " + error.Message)
		  End If
		  
		  
		  // Operation updatePet
		  // Update an existing pet
		  // - parameter localOutStatus: Information on whether the operation succeeded.
		  // - parameter localOutData: On success, contains the data returned by the API.
		  // - parameter pet: (body) Pet object that needs to be added to the store 
		  //
		  // - PUT /pet
		  // - 
		  // - defaultResponse: Nil
		  //
		  // - OAuth:
		  //   - type: oauth2
		  //   - name: petstore_auth
		  //
		  Dim UpdatePetpetModel As New XojoOpenAPIClientSynchronous.Models.Pet
		  UpdatePetpetModel.name = "doggie"
		  
		  Dim UpdatePetData As XojoOpenAPIClientSynchronous.Models.Pet
		  If api.UpdatePet(error, UpdatePetData, UpdatePetpetModel) Then
		    Print("[+] PetApi.UpdatePet successful.")
		  Else
		    Print("[-] PetApi.UpdatePet unsuccessful.")
			Print("    " + " | ErrorNumber: " + Str(error.ErrorNumber) + " | HTTP Status: " + Str(error.HTTPCode) + " | SocketCode: " + Str(error.SocketCode) + " | Message: " + error.Message)
		  End If
		  
		  
		  // Operation updatePetWithForm
		  // Updates a pet in the store with form data
		  // - parameter localOutStatus: Information on whether the operation succeeded.
		  // - parameter petId: (path) ID of pet that needs to be updated 
		  // - parameter name: (form) Updated name of the pet (optional, default to Sample)
		  // - parameter status: (form) Updated status of the pet (optional, default to Sample)
		  //
		  // - POST /pet/{petId}
		  // - 
		  //
		  // - OAuth:
		  //   - type: oauth2
		  //   - name: petstore_auth
		  //
		  If api.UpdatePetWithForm(error, 789, "name_example", "status_example") Then
		    Print("[+] PetApi.UpdatePetWithForm successful.")
		  Else
		    Print("[-] PetApi.UpdatePetWithForm unsuccessful.")
			Print("    " + " | ErrorNumber: " + Str(error.ErrorNumber) + " | HTTP Status: " + Str(error.HTTPCode) + " | SocketCode: " + Str(error.SocketCode) + " | Message: " + error.Message)
		  End If
		  
		  
		  // Operation uploadFile
		  // uploads an image
		  // - parameter localOutStatus: Information on whether the operation succeeded.
		  // - parameter localOutData: On success, contains the data returned by the API.
		  // - parameter petId: (path) ID of pet to update 
		  // - parameter additionalMetadata: (form) Additional data to pass to server (optional, default to Sample)
		  // - parameter Escapedfile: (form) file to upload (optional, default to Sample)
		  //
		  // - POST /pet/{petId}/uploadImage
		  // - 
		  // - defaultResponse: Nil
		  //
		  // - OAuth:
		  //   - type: oauth2
		  //   - name: petstore_auth
		  //
		  Dim UploadFileData As XojoOpenAPIClientSynchronous.Models.ApiResponse
		  If api.UploadFile(error, UploadFileData, 789, "additionalMetadata_example", GetTemporaryFolderItem) Then
		    Print("[+] PetApi.UploadFile successful.")
		  Else
		    Print("[-] PetApi.UploadFile unsuccessful.")
			Print("    " + " | ErrorNumber: " + Str(error.ErrorNumber) + " | HTTP Status: " + Str(error.HTTPCode) + " | SocketCode: " + Str(error.SocketCode) + " | Message: " + error.Message)
		  End If
		  
		End Sub
	#tag EndMethod
	#tag Method, Flags = &h0
		Sub testStoreApi(basePath As String)
		  Dim error As New XojoOpenAPIClientSynchronous.XojoOpenAPIClientSynchronousException(0, "No Error")
		  Dim tmpDownloadFile As FolderItem = GetTemporaryFolderItem
		  
		  Dim api As New XojoOpenAPIClientSynchronous.APIs.StoreApi
		  api.BasePath = basePath
		  api.BasicAuthUser = "user"
		  api.BasicAuthPassword = "password"
		  api.UseHTTPS = false
		  
		  
		  // Operation deleteOrder
		  // Delete purchase order by ID
		  // - parameter localOutStatus: Information on whether the operation succeeded.
		  // - parameter orderId: (path) ID of the order that needs to be deleted 
		  //
		  // - DELETE /store/order/{orderId}
		  // - For valid response try integer IDs with value < 1000. Anything above 1000 or nonintegers will generate API errors
		  //
		  //
		  If api.DeleteOrder(error, "orderId_example") Then
		    Print("[+] StoreApi.DeleteOrder successful.")
		  Else
		    Print("[-] StoreApi.DeleteOrder unsuccessful.")
			Print("    " + " | ErrorNumber: " + Str(error.ErrorNumber) + " | HTTP Status: " + Str(error.HTTPCode) + " | SocketCode: " + Str(error.SocketCode) + " | Message: " + error.Message)
		  End If
		  
		  
		  // Operation getInventory
		  // Returns pet inventories by status
		  // - parameter localOutStatus: Information on whether the operation succeeded.
		  // - parameter localOutData: On success, contains the data returned by the API.
		  //
		  // - GET /store/inventory
		  // - Returns a map of status codes to quantities
		  // - defaultResponse: Nil
		  //
		  // - API Key:
		  //   - type: apiKey api_key (HEADER)
		  //   - name: api_key
		  //
		  Dim GetInventoryData As Dictionary
		  If api.GetInventory(error, GetInventoryData) Then
		    Print("[+] StoreApi.GetInventory successful.")
		  Else
		    Print("[-] StoreApi.GetInventory unsuccessful.")
			Print("    " + " | ErrorNumber: " + Str(error.ErrorNumber) + " | HTTP Status: " + Str(error.HTTPCode) + " | SocketCode: " + Str(error.SocketCode) + " | Message: " + error.Message)
		  End If
		  
		  
		  // Operation getOrderById
		  // Find purchase order by ID
		  // - parameter localOutStatus: Information on whether the operation succeeded.
		  // - parameter localOutData: On success, contains the data returned by the API.
		  // - parameter orderId: (path) ID of pet that needs to be fetched 
		  //
		  // - GET /store/order/{orderId}
		  // - For valid response try integer IDs with value <= 5 or > 10. Other values will generate exceptions
		  // - defaultResponse: Nil
		  //
		  //
		  Dim GetOrderByIdData As XojoOpenAPIClientSynchronous.Models.Order
		  If api.GetOrderById(error, GetOrderByIdData, 789) Then
		    Print("[+] StoreApi.GetOrderById successful.")
		  Else
		    Print("[-] StoreApi.GetOrderById unsuccessful.")
			Print("    " + " | ErrorNumber: " + Str(error.ErrorNumber) + " | HTTP Status: " + Str(error.HTTPCode) + " | SocketCode: " + Str(error.SocketCode) + " | Message: " + error.Message)
		  End If
		  
		  
		  // Operation placeOrder
		  // Place an order for a pet
		  // - parameter localOutStatus: Information on whether the operation succeeded.
		  // - parameter localOutData: On success, contains the data returned by the API.
		  // - parameter order: (body) order placed for purchasing the pet 
		  //
		  // - POST /store/order
		  // - 
		  // - defaultResponse: Nil
		  //
		  //
		  Dim PlaceOrderorderModel As New XojoOpenAPIClientSynchronous.Models.Order
		  
		  Dim PlaceOrderData As XojoOpenAPIClientSynchronous.Models.Order
		  If api.PlaceOrder(error, PlaceOrderData, PlaceOrderorderModel) Then
		    Print("[+] StoreApi.PlaceOrder successful.")
		  Else
		    Print("[-] StoreApi.PlaceOrder unsuccessful.")
			Print("    " + " | ErrorNumber: " + Str(error.ErrorNumber) + " | HTTP Status: " + Str(error.HTTPCode) + " | SocketCode: " + Str(error.SocketCode) + " | Message: " + error.Message)
		  End If
		  
		End Sub
	#tag EndMethod
	#tag Method, Flags = &h0
		Sub testUserApi(basePath As String)
		  Dim error As New XojoOpenAPIClientSynchronous.XojoOpenAPIClientSynchronousException(0, "No Error")
		  Dim tmpDownloadFile As FolderItem = GetTemporaryFolderItem
		  
		  Dim api As New XojoOpenAPIClientSynchronous.APIs.UserApi
		  api.BasePath = basePath
		  api.BasicAuthUser = "user"
		  api.BasicAuthPassword = "password"
		  api.UseHTTPS = false
		  
		  
		  // Operation createUser
		  // Create user
		  // - parameter localOutStatus: Information on whether the operation succeeded.
		  // - parameter user: (body) Created user object 
		  //
		  // - POST /user
		  // - This can only be done by the logged in user.
		  //
		  // - API Key:
		  //   - type: apiKey api_key (HEADER)
		  //   - name: api_key
		  //
		  Dim CreateUseruserModel As New XojoOpenAPIClientSynchronous.Models.User
		  
		  If api.CreateUser(error, CreateUseruserModel) Then
		    Print("[+] UserApi.CreateUser successful.")
		  Else
		    Print("[-] UserApi.CreateUser unsuccessful.")
			Print("    " + " | ErrorNumber: " + Str(error.ErrorNumber) + " | HTTP Status: " + Str(error.HTTPCode) + " | SocketCode: " + Str(error.SocketCode) + " | Message: " + error.Message)
		  End If
		  
		  
		  // Operation createUsersWithArrayInput
		  // Creates list of users with given input array
		  // - parameter localOutStatus: Information on whether the operation succeeded.
		  // - parameter user: (body) List of user object 
		  //
		  // - POST /user/createWithArray
		  // - 
		  //
		  // - API Key:
		  //   - type: apiKey api_key (HEADER)
		  //   - name: api_key
		  //
		  Dim CreateUsersWithArrayInputuserArray() As XojoOpenAPIClientSynchronous.Models.User
		  If api.CreateUsersWithArrayInput(error, CreateUsersWithArrayInputuserArray) Then
		    Print("[+] UserApi.CreateUsersWithArrayInput successful.")
		  Else
		    Print("[-] UserApi.CreateUsersWithArrayInput unsuccessful.")
			Print("    " + " | ErrorNumber: " + Str(error.ErrorNumber) + " | HTTP Status: " + Str(error.HTTPCode) + " | SocketCode: " + Str(error.SocketCode) + " | Message: " + error.Message)
		  End If
		  
		  
		  // Operation createUsersWithListInput
		  // Creates list of users with given input array
		  // - parameter localOutStatus: Information on whether the operation succeeded.
		  // - parameter user: (body) List of user object 
		  //
		  // - POST /user/createWithList
		  // - 
		  //
		  // - API Key:
		  //   - type: apiKey api_key (HEADER)
		  //   - name: api_key
		  //
		  Dim CreateUsersWithListInputuserArray() As XojoOpenAPIClientSynchronous.Models.User
		  If api.CreateUsersWithListInput(error, CreateUsersWithListInputuserArray) Then
		    Print("[+] UserApi.CreateUsersWithListInput successful.")
		  Else
		    Print("[-] UserApi.CreateUsersWithListInput unsuccessful.")
			Print("    " + " | ErrorNumber: " + Str(error.ErrorNumber) + " | HTTP Status: " + Str(error.HTTPCode) + " | SocketCode: " + Str(error.SocketCode) + " | Message: " + error.Message)
		  End If
		  
		  
		  // Operation deleteUser
		  // Delete user
		  // - parameter localOutStatus: Information on whether the operation succeeded.
		  // - parameter username: (path) The name that needs to be deleted 
		  //
		  // - DELETE /user/{username}
		  // - This can only be done by the logged in user.
		  //
		  // - API Key:
		  //   - type: apiKey api_key (HEADER)
		  //   - name: api_key
		  //
		  If api.DeleteUser(error, "username_example") Then
		    Print("[+] UserApi.DeleteUser successful.")
		  Else
		    Print("[-] UserApi.DeleteUser unsuccessful.")
			Print("    " + " | ErrorNumber: " + Str(error.ErrorNumber) + " | HTTP Status: " + Str(error.HTTPCode) + " | SocketCode: " + Str(error.SocketCode) + " | Message: " + error.Message)
		  End If
		  
		  
		  // Operation getUserByName
		  // Get user by user name
		  // - parameter localOutStatus: Information on whether the operation succeeded.
		  // - parameter localOutData: On success, contains the data returned by the API.
		  // - parameter username: (path) The name that needs to be fetched. Use user1 for testing. 
		  //
		  // - GET /user/{username}
		  // - 
		  // - defaultResponse: Nil
		  //
		  //
		  Dim GetUserByNameData As XojoOpenAPIClientSynchronous.Models.User
		  If api.GetUserByName(error, GetUserByNameData, "username_example") Then
		    Print("[+] UserApi.GetUserByName successful.")
		  Else
		    Print("[-] UserApi.GetUserByName unsuccessful.")
			Print("    " + " | ErrorNumber: " + Str(error.ErrorNumber) + " | HTTP Status: " + Str(error.HTTPCode) + " | SocketCode: " + Str(error.SocketCode) + " | Message: " + error.Message)
		  End If
		  
		  
		  // Operation loginUser
		  // Logs user into the system
		  // - parameter localOutStatus: Information on whether the operation succeeded.
		  // - parameter localOutData: On success, contains the data returned by the API.
		  // - parameter username: (query) The user name for login 
		  // - parameter password: (query) The password for login in clear text 
		  //
		  // - GET /user/login
		  // - 
		  // - defaultResponse: Sample
		  //
		  //
		  // - responseHeaders: [Set-Cookie(String), X-Rate-Limit(Integer), X-Expires-After(Date)]
		  Dim LoginUserData As String
		  If api.LoginUser(error, LoginUserData, "username_example", "password_example") Then
		    Print("[+] UserApi.LoginUser successful.")
		  Else
		    Print("[-] UserApi.LoginUser unsuccessful.")
			Print("    " + " | ErrorNumber: " + Str(error.ErrorNumber) + " | HTTP Status: " + Str(error.HTTPCode) + " | SocketCode: " + Str(error.SocketCode) + " | Message: " + error.Message)
		  End If
		  
		  
		  // Operation logoutUser
		  // Logs out current logged in user session
		  // - parameter localOutStatus: Information on whether the operation succeeded.
		  //
		  // - GET /user/logout
		  // - 
		  //
		  // - API Key:
		  //   - type: apiKey api_key (HEADER)
		  //   - name: api_key
		  //
		  If api.LogoutUser(error) Then
		    Print("[+] UserApi.LogoutUser successful.")
		  Else
		    Print("[-] UserApi.LogoutUser unsuccessful.")
			Print("    " + " | ErrorNumber: " + Str(error.ErrorNumber) + " | HTTP Status: " + Str(error.HTTPCode) + " | SocketCode: " + Str(error.SocketCode) + " | Message: " + error.Message)
		  End If
		  
		  
		  // Operation updateUser
		  // Updated user
		  // - parameter localOutStatus: Information on whether the operation succeeded.
		  // - parameter username: (path) name that need to be deleted 
		  // - parameter user: (body) Updated user object 
		  //
		  // - PUT /user/{username}
		  // - This can only be done by the logged in user.
		  //
		  // - API Key:
		  //   - type: apiKey api_key (HEADER)
		  //   - name: api_key
		  //
		  Dim UpdateUseruserModel As New XojoOpenAPIClientSynchronous.Models.User
		  
		  If api.UpdateUser(error, "username_example", UpdateUseruserModel) Then
		    Print("[+] UserApi.UpdateUser successful.")
		  Else
		    Print("[-] UserApi.UpdateUser unsuccessful.")
			Print("    " + " | ErrorNumber: " + Str(error.ErrorNumber) + " | HTTP Status: " + Str(error.HTTPCode) + " | SocketCode: " + Str(error.SocketCode) + " | Message: " + error.Message)
		  End If
		  
		End Sub
	#tag EndMethod


	#tag Method, Flags = &h21
		Private Function FromRFC3339(stringRepresentation As String) As Date

		  Dim d As New Xoson.DateIntermediate(stringRepresentation)

		  return New Date(d.year, d.month, d.day, d.hour, d.minute, d.second, 0.0)
		End Function
	#tag EndMethod


	#tag ViewBehavior
		#tag ViewProperty
			Name="Name"
			Visible=true
			Group="ID"
			InitialValue=""
			Type="String"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Index"
			Visible=true
			Group="ID"
			InitialValue="-2147483648"
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Super"
			Visible=true
			Group="ID"
			InitialValue=""
			Type="String"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Left"
			Visible=true
			Group="Position"
			InitialValue="0"
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Top"
			Visible=true
			Group="Position"
			InitialValue="0"
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="useHTTPS"
			Visible=false
			Group="Behavior"
			InitialValue="true"
			Type="Boolean"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="port"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="host"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="String"
			EditorType="MultiLineEditor"
		#tag EndViewProperty
		#tag ViewProperty
			Name="BasePath"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="String"
			EditorType="MultiLineEditor"
		#tag EndViewProperty
	#tag EndViewBehavior
End Class
#tag EndClass
