#tag Class
Protected Class UserApi
	#tag Method, Flags = &h0
		Function CreateUser(ByRef localOutStatus As XojoOpenAPIClientSynchronous.XojoOpenAPIClientSynchronousException, user As XojoOpenAPIClientSynchronous.Models.User) As Boolean
		  // Operation createUser
		  // Create user
		  // - parameter localOutStatus: Information on whether the operation succeeded.
		  // - parameter user: (body) Created user object 
		  //
		  // 
		  //
		  // - POST /user
		  // - This can only be done by the logged in user.
		  //
		  // - API Key:
		  //   - type: apiKey api_key (HEADER)
		  //   - name: api_key
		  //
		  
		  Dim localVarHTTPSocket As New HTTPSecureSocket
		  Me.PrivateFuncPrepareSocket(localVarHTTPSocket)
		  localVarHTTPSocket.SetRequestContent(Xoson.toJSON(user), "application/json")
		  
		  If me.ApiKeyapi_key = "" Then Raise New XojoOpenAPIClientSynchronous.XojoOpenAPIClientSynchronousException(kErrorCannotAuthenticate, "API key is unset. Please assign a value to `UserApi.ApiKeyapi_key` before invoking `UserApi.CreateUser()`.")
		  
		  localVarHTTPSocket.SetRequestHeader(EncodeURLComponent("api_key"), EncodeURLComponent(me.ApiKeyapi_key))
		  


		  Dim localVarPath As String = "/user"
		  
		  
		  
		  localVarHTTPSocket.Yield = True 'recommended in synchronous mode
		  Dim localVarContent As String = localVarHTTPSocket.SendRequest("POST", Me.BasePath + localVarPath, XojoOpenAPIClientSynchronous.kAPITimeoutSeconds)
		  #Pragma Unused localVarContent
		  
		  Dim localVarSocketError As Integer = localVarHTTPSocket.ErrorCode
		  If localVarSocketError <> 0 Then
		    localVarHTTPSocket.Close
		    localOutStatus = New XojoOpenAPIClientSynchronous.XojoOpenAPIClientSynchronousException(localVarSocketError)
		    Return False
		  End If
		  
		  Dim localVarHttpStatus As Integer = localVarHTTPSocket.HTTPStatusCode
		  
		  localOutStatus = New XojoOpenAPIClientSynchronous.XojoOpenAPIClientSynchronousException(localVarHttpStatus, "", "")

		  localVarHTTPSocket.Close
		  
		  Return True
		End Function
	#tag EndMethod





	#tag Method, Flags = &h0
		Function CreateUsersWithArrayInput(ByRef localOutStatus As XojoOpenAPIClientSynchronous.XojoOpenAPIClientSynchronousException, user() As XojoOpenAPIClientSynchronous.Models.User) As Boolean
		  // Operation createUsersWithArrayInput
		  // Creates list of users with given input array
		  // - parameter localOutStatus: Information on whether the operation succeeded.
		  // - parameter user: (body) List of user object 
		  //
		  // 
		  //
		  // - POST /user/createWithArray
		  // - 
		  //
		  // - API Key:
		  //   - type: apiKey api_key (HEADER)
		  //   - name: api_key
		  //
		  
		  Dim localVarHTTPSocket As New HTTPSecureSocket
		  Me.PrivateFuncPrepareSocket(localVarHTTPSocket)
		  localVarHTTPSocket.SetRequestContent(Xoson.toJSON(user), "application/json")
		  
		  If me.ApiKeyapi_key = "" Then Raise New XojoOpenAPIClientSynchronous.XojoOpenAPIClientSynchronousException(kErrorCannotAuthenticate, "API key is unset. Please assign a value to `UserApi.ApiKeyapi_key` before invoking `UserApi.CreateUsersWithArrayInput()`.")
		  
		  localVarHTTPSocket.SetRequestHeader(EncodeURLComponent("api_key"), EncodeURLComponent(me.ApiKeyapi_key))
		  


		  Dim localVarPath As String = "/user/createWithArray"
		  
		  
		  
		  localVarHTTPSocket.Yield = True 'recommended in synchronous mode
		  Dim localVarContent As String = localVarHTTPSocket.SendRequest("POST", Me.BasePath + localVarPath, XojoOpenAPIClientSynchronous.kAPITimeoutSeconds)
		  #Pragma Unused localVarContent
		  
		  Dim localVarSocketError As Integer = localVarHTTPSocket.ErrorCode
		  If localVarSocketError <> 0 Then
		    localVarHTTPSocket.Close
		    localOutStatus = New XojoOpenAPIClientSynchronous.XojoOpenAPIClientSynchronousException(localVarSocketError)
		    Return False
		  End If
		  
		  Dim localVarHttpStatus As Integer = localVarHTTPSocket.HTTPStatusCode
		  
		  localOutStatus = New XojoOpenAPIClientSynchronous.XojoOpenAPIClientSynchronousException(localVarHttpStatus, "", "")

		  localVarHTTPSocket.Close
		  
		  Return True
		End Function
	#tag EndMethod





	#tag Method, Flags = &h0
		Function CreateUsersWithListInput(ByRef localOutStatus As XojoOpenAPIClientSynchronous.XojoOpenAPIClientSynchronousException, user() As XojoOpenAPIClientSynchronous.Models.User) As Boolean
		  // Operation createUsersWithListInput
		  // Creates list of users with given input array
		  // - parameter localOutStatus: Information on whether the operation succeeded.
		  // - parameter user: (body) List of user object 
		  //
		  // 
		  //
		  // - POST /user/createWithList
		  // - 
		  //
		  // - API Key:
		  //   - type: apiKey api_key (HEADER)
		  //   - name: api_key
		  //
		  
		  Dim localVarHTTPSocket As New HTTPSecureSocket
		  Me.PrivateFuncPrepareSocket(localVarHTTPSocket)
		  localVarHTTPSocket.SetRequestContent(Xoson.toJSON(user), "application/json")
		  
		  If me.ApiKeyapi_key = "" Then Raise New XojoOpenAPIClientSynchronous.XojoOpenAPIClientSynchronousException(kErrorCannotAuthenticate, "API key is unset. Please assign a value to `UserApi.ApiKeyapi_key` before invoking `UserApi.CreateUsersWithListInput()`.")
		  
		  localVarHTTPSocket.SetRequestHeader(EncodeURLComponent("api_key"), EncodeURLComponent(me.ApiKeyapi_key))
		  


		  Dim localVarPath As String = "/user/createWithList"
		  
		  
		  
		  localVarHTTPSocket.Yield = True 'recommended in synchronous mode
		  Dim localVarContent As String = localVarHTTPSocket.SendRequest("POST", Me.BasePath + localVarPath, XojoOpenAPIClientSynchronous.kAPITimeoutSeconds)
		  #Pragma Unused localVarContent
		  
		  Dim localVarSocketError As Integer = localVarHTTPSocket.ErrorCode
		  If localVarSocketError <> 0 Then
		    localVarHTTPSocket.Close
		    localOutStatus = New XojoOpenAPIClientSynchronous.XojoOpenAPIClientSynchronousException(localVarSocketError)
		    Return False
		  End If
		  
		  Dim localVarHttpStatus As Integer = localVarHTTPSocket.HTTPStatusCode
		  
		  localOutStatus = New XojoOpenAPIClientSynchronous.XojoOpenAPIClientSynchronousException(localVarHttpStatus, "", "")

		  localVarHTTPSocket.Close
		  
		  Return True
		End Function
	#tag EndMethod





	#tag Method, Flags = &h0
		Function DeleteUser(ByRef localOutStatus As XojoOpenAPIClientSynchronous.XojoOpenAPIClientSynchronousException, username As String) As Boolean
		  // Operation deleteUser
		  // Delete user
		  // - parameter localOutStatus: Information on whether the operation succeeded.
		  // - parameter username: (path) The name that needs to be deleted 
		  //
		  // 
		  //
		  // - DELETE /user/{username}
		  // - This can only be done by the logged in user.
		  //
		  // - API Key:
		  //   - type: apiKey api_key (HEADER)
		  //   - name: api_key
		  //
		  
		  Dim localVarHTTPSocket As New HTTPSecureSocket
		  Me.PrivateFuncPrepareSocket(localVarHTTPSocket)
		  
		  
		  If me.ApiKeyapi_key = "" Then Raise New XojoOpenAPIClientSynchronous.XojoOpenAPIClientSynchronousException(kErrorCannotAuthenticate, "API key is unset. Please assign a value to `UserApi.ApiKeyapi_key` before invoking `UserApi.DeleteUser()`.")
		  
		  localVarHTTPSocket.SetRequestHeader(EncodeURLComponent("api_key"), EncodeURLComponent(me.ApiKeyapi_key))
		  


		  Dim localVarPath As String = "/user/{username}"
		  
		  Dim localVarPathStringusername As String = username
		  
		  localVarPath = localVarPath.ReplaceAllB("{username}", localVarPathStringusername)
		  
		  
		  localVarHTTPSocket.Yield = True 'recommended in synchronous mode
		  Dim localVarContent As String = localVarHTTPSocket.SendRequest("DELETE", Me.BasePath + localVarPath, XojoOpenAPIClientSynchronous.kAPITimeoutSeconds)
		  #Pragma Unused localVarContent
		  
		  Dim localVarSocketError As Integer = localVarHTTPSocket.ErrorCode
		  If localVarSocketError <> 0 Then
		    localVarHTTPSocket.Close
		    localOutStatus = New XojoOpenAPIClientSynchronous.XojoOpenAPIClientSynchronousException(localVarSocketError)
		    Return False
		  End If
		  
		  Dim localVarHttpStatus As Integer = localVarHTTPSocket.HTTPStatusCode
		  
		  localOutStatus = New XojoOpenAPIClientSynchronous.XojoOpenAPIClientSynchronousException(localVarHttpStatus, "", "")

		  localVarHTTPSocket.Close
		  
		  Return True
		End Function
	#tag EndMethod





	#tag Method, Flags = &h0
		Function GetUserByName(ByRef localOutStatus As XojoOpenAPIClientSynchronous.XojoOpenAPIClientSynchronousException, ByRef localOutData As XojoOpenAPIClientSynchronous.Models.User, username As String) As Boolean
		  // Operation getUserByName
		  // Get user by user name
		  // - parameter localOutStatus: Information on whether the operation succeeded.
		  // - parameter localOutData: On success, contains the data returned by the API.
		  // - parameter username: (path) The name that needs to be fetched. Use user1 for testing. 
		  //
		  // 
		  //
		  // - GET /user/{username}
		  // - 
		  // - defaultResponse: Nil
		  //
		  //
		  
		  Dim localVarHTTPSocket As New HTTPSecureSocket
		  Me.PrivateFuncPrepareSocket(localVarHTTPSocket)
		  
		  
		  


		  Dim localVarPath As String = "/user/{username}"
		  
		  Dim localVarPathStringusername As String = username
		  
		  localVarPath = localVarPath.ReplaceAllB("{username}", localVarPathStringusername)
		  
		  
		  localVarHTTPSocket.Yield = True 'recommended in synchronous mode
		  Dim localVarContent As String = localVarHTTPSocket.SendRequest("GET", Me.BasePath + localVarPath, XojoOpenAPIClientSynchronous.kAPITimeoutSeconds)
		  
		  Dim localVarSocketError As Integer = localVarHTTPSocket.ErrorCode
		  If localVarSocketError <> 0 Then
		    localVarHTTPSocket.Close
		    localOutStatus = New XojoOpenAPIClientSynchronous.XojoOpenAPIClientSynchronousException(localVarSocketError)
		    Return False
		  End If
		  
		  Dim localVarHttpStatus As Integer = localVarHTTPSocket.HTTPStatusCode
		  Dim localVarHeaders As InternetHeaders = localVarHTTPSocket.PageHeaders
		  
		  localOutStatus = New XojoOpenAPIClientSynchronous.XojoOpenAPIClientSynchronousException(localVarHttpStatus, "", localVarContent)

		  localVarHTTPSocket.Close
		  
		  Return GetUserByNamePrivateFuncDeserializeResponse(localVarHTTPStatus, localVarHeaders, localOutStatus, localVarContent, localOutData)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function GetUserByNamePrivateFuncDeserializeResponse(HTTPStatus As Integer, Headers As InternetHeaders, error As XojoOpenAPIClientSynchronous.XojoOpenAPIClientSynchronousException, Content As String, ByRef outData As XojoOpenAPIClientSynchronous.Models.User) As Boolean
		  Dim contentType As String = Headers.Value("Content-Type")
		  Dim contentEncoding As TextEncoding = XojoOpenAPIClientSynchronous.EncodingFromContentType(contentType)
		  Content = DefineEncoding(Content, contentEncoding)
		  
		  If HTTPStatus > 199 and HTTPStatus < 300 then
		    If contentType.LeftB(16) = "application/json" then
		      
			  outData = New XojoOpenAPIClientSynchronous.Models.User
			  Try
		        Xoson.fromJSON(outData, Content.toText())

		      Catch e As JSONException
		        error.Message = error.Message + " with JSON parse exception: " + e.Message
		        error.ErrorNumber = kErrorInvalidJSON
		        Return False
		        
		      Catch e As Xojo.Data.InvalidJSONException
		        error.Message = error.Message + " with Xojo.Data.JSON parse exception: " + e.Message
		        error.ErrorNumber = kErrorInvalidJSON
		        Return False
		        
		      Catch e As Xoson.XosonException
		        error.Message = error.Message + " with Xoson parse exception: " + e.Message
		        error.ErrorNumber = kErrorXosonProblem
		        Return False

		      End Try
		      
		      
		    ElseIf contentType.LeftB(19) = "multipart/form-data" then
		      error.Message = "Unsupported media type: " + contentType
		      error.ErrorNumber = kErrorUnsupportedMediaType
		      Return False

		    ElseIf contentType.LeftB(33) = "application/x-www-form-urlencoded" then
		      error.Message = "Unsupported media type: " + contentType
		      error.ErrorNumber = kErrorUnsupportedMediaType
		      Return False

		    Else
		      error.Message = "Unsupported media type: " + contentType
		      error.ErrorNumber = kErrorUnsupportedMediaType
		      Return False

		    End If
		  Else
		    error.Message = error.Message + ". " + Content
			error.ErrorNumber = kErrorHTTPFail
		    Return False
		  End If
		  
		  Return True
		End Function
	#tag EndMethod




	#tag Method, Flags = &h0
		Function LoginUser(ByRef localOutStatus As XojoOpenAPIClientSynchronous.XojoOpenAPIClientSynchronousException, ByRef localOutData As String, username As String, password As String) As Boolean
		  // Operation loginUser
		  // Logs user into the system
		  // - parameter localOutStatus: Information on whether the operation succeeded.
		  // - parameter localOutData: On success, contains the data returned by the API.
		  // - parameter username: (query) The user name for login 
		  // - parameter password: (query) The password for login in clear text 
		  //
		  // 
		  //
		  // - GET /user/login
		  // - 
		  // - defaultResponse: Sample
		  //
		  //
		  // - responseHeaders: [Set-Cookie(String), X-Rate-Limit(Integer), X-Expires-After(Date)]
		  
		  Dim localVarHTTPSocket As New HTTPSecureSocket
		  Me.PrivateFuncPrepareSocket(localVarHTTPSocket)
		  
		  Dim localVarQueryParams As String = "?"
		  localVarQueryParams = localVarQueryParams + EncodeURLComponent("username") + "=" + EncodeURLComponent(username)
		  
		  localVarQueryParams = localVarQueryParams + "&" + EncodeURLComponent("password") + "=" + EncodeURLComponent(password)
		  

		  


		  Dim localVarPath As String = "/user/login"
		  
		  
		  
		  localVarHTTPSocket.Yield = True 'recommended in synchronous mode
		  Dim localVarContent As String = localVarHTTPSocket.SendRequest("GET", Me.BasePath + localVarPath + localVarQueryParams, XojoOpenAPIClientSynchronous.kAPITimeoutSeconds)
		  
		  Dim localVarSocketError As Integer = localVarHTTPSocket.ErrorCode
		  If localVarSocketError <> 0 Then
		    localVarHTTPSocket.Close
		    localOutStatus = New XojoOpenAPIClientSynchronous.XojoOpenAPIClientSynchronousException(localVarSocketError)
		    Return False
		  End If
		  
		  Dim localVarHttpStatus As Integer = localVarHTTPSocket.HTTPStatusCode
		  Dim localVarHeaders As InternetHeaders = localVarHTTPSocket.PageHeaders
		  
		  localOutStatus = New XojoOpenAPIClientSynchronous.XojoOpenAPIClientSynchronousException(localVarHttpStatus, "", localVarContent)

		  localVarHTTPSocket.Close
		  
		  Return LoginUserPrivateFuncDeserializeResponse(localVarHTTPStatus, localVarHeaders, localOutStatus, localVarContent, localOutData)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function LoginUserPrivateFuncDeserializeResponse(HTTPStatus As Integer, Headers As InternetHeaders, error As XojoOpenAPIClientSynchronous.XojoOpenAPIClientSynchronousException, Content As String, ByRef outData As String) As Boolean
		  Dim contentType As String = Headers.Value("Content-Type")
		  Dim contentEncoding As TextEncoding = XojoOpenAPIClientSynchronous.EncodingFromContentType(contentType)
		  Content = DefineEncoding(Content, contentEncoding)
		  
		  If HTTPStatus > 199 and HTTPStatus < 300 then
		    If contentType.LeftB(16) = "application/json" then
		      outData = Content
		      
		      
		    ElseIf contentType.LeftB(19) = "multipart/form-data" then
		      error.Message = "Unsupported media type: " + contentType
		      error.ErrorNumber = kErrorUnsupportedMediaType
		      Return False

		    ElseIf contentType.LeftB(33) = "application/x-www-form-urlencoded" then
		      error.Message = "Unsupported media type: " + contentType
		      error.ErrorNumber = kErrorUnsupportedMediaType
		      Return False

		    Else
		      error.Message = "Unsupported media type: " + contentType
		      error.ErrorNumber = kErrorUnsupportedMediaType
		      Return False

		    End If
		  Else
		    error.Message = error.Message + ". " + Content
			error.ErrorNumber = kErrorHTTPFail
		    Return False
		  End If
		  
		  Return True
		End Function
	#tag EndMethod




	#tag Method, Flags = &h0
		Function LogoutUser(ByRef localOutStatus As XojoOpenAPIClientSynchronous.XojoOpenAPIClientSynchronousException) As Boolean
		  // Operation logoutUser
		  // Logs out current logged in user session
		  // - parameter localOutStatus: Information on whether the operation succeeded.
		  //
		  // 
		  //
		  // - GET /user/logout
		  // - 
		  //
		  // - API Key:
		  //   - type: apiKey api_key (HEADER)
		  //   - name: api_key
		  //
		  
		  Dim localVarHTTPSocket As New HTTPSecureSocket
		  Me.PrivateFuncPrepareSocket(localVarHTTPSocket)
		  
		  
		  If me.ApiKeyapi_key = "" Then Raise New XojoOpenAPIClientSynchronous.XojoOpenAPIClientSynchronousException(kErrorCannotAuthenticate, "API key is unset. Please assign a value to `UserApi.ApiKeyapi_key` before invoking `UserApi.LogoutUser()`.")
		  
		  localVarHTTPSocket.SetRequestHeader(EncodeURLComponent("api_key"), EncodeURLComponent(me.ApiKeyapi_key))
		  


		  Dim localVarPath As String = "/user/logout"
		  
		  
		  
		  localVarHTTPSocket.Yield = True 'recommended in synchronous mode
		  Dim localVarContent As String = localVarHTTPSocket.SendRequest("GET", Me.BasePath + localVarPath, XojoOpenAPIClientSynchronous.kAPITimeoutSeconds)
		  #Pragma Unused localVarContent
		  
		  Dim localVarSocketError As Integer = localVarHTTPSocket.ErrorCode
		  If localVarSocketError <> 0 Then
		    localVarHTTPSocket.Close
		    localOutStatus = New XojoOpenAPIClientSynchronous.XojoOpenAPIClientSynchronousException(localVarSocketError)
		    Return False
		  End If
		  
		  Dim localVarHttpStatus As Integer = localVarHTTPSocket.HTTPStatusCode
		  
		  localOutStatus = New XojoOpenAPIClientSynchronous.XojoOpenAPIClientSynchronousException(localVarHttpStatus, "", "")

		  localVarHTTPSocket.Close
		  
		  Return True
		End Function
	#tag EndMethod





	#tag Method, Flags = &h0
		Function UpdateUser(ByRef localOutStatus As XojoOpenAPIClientSynchronous.XojoOpenAPIClientSynchronousException, username As String, user As XojoOpenAPIClientSynchronous.Models.User) As Boolean
		  // Operation updateUser
		  // Updated user
		  // - parameter localOutStatus: Information on whether the operation succeeded.
		  // - parameter username: (path) name that need to be deleted 
		  // - parameter user: (body) Updated user object 
		  //
		  // 
		  //
		  // - PUT /user/{username}
		  // - This can only be done by the logged in user.
		  //
		  // - API Key:
		  //   - type: apiKey api_key (HEADER)
		  //   - name: api_key
		  //
		  
		  Dim localVarHTTPSocket As New HTTPSecureSocket
		  Me.PrivateFuncPrepareSocket(localVarHTTPSocket)
		  localVarHTTPSocket.SetRequestContent(Xoson.toJSON(user), "application/json")
		  
		  If me.ApiKeyapi_key = "" Then Raise New XojoOpenAPIClientSynchronous.XojoOpenAPIClientSynchronousException(kErrorCannotAuthenticate, "API key is unset. Please assign a value to `UserApi.ApiKeyapi_key` before invoking `UserApi.UpdateUser()`.")
		  
		  localVarHTTPSocket.SetRequestHeader(EncodeURLComponent("api_key"), EncodeURLComponent(me.ApiKeyapi_key))
		  


		  Dim localVarPath As String = "/user/{username}"
		  
		  Dim localVarPathStringusername As String = username
		  
		  localVarPath = localVarPath.ReplaceAllB("{username}", localVarPathStringusername)
		  
		  
		  localVarHTTPSocket.Yield = True 'recommended in synchronous mode
		  Dim localVarContent As String = localVarHTTPSocket.SendRequest("PUT", Me.BasePath + localVarPath, XojoOpenAPIClientSynchronous.kAPITimeoutSeconds)
		  #Pragma Unused localVarContent
		  
		  Dim localVarSocketError As Integer = localVarHTTPSocket.ErrorCode
		  If localVarSocketError <> 0 Then
		    localVarHTTPSocket.Close
		    localOutStatus = New XojoOpenAPIClientSynchronous.XojoOpenAPIClientSynchronousException(localVarSocketError)
		    Return False
		  End If
		  
		  Dim localVarHttpStatus As Integer = localVarHTTPSocket.HTTPStatusCode
		  
		  localOutStatus = New XojoOpenAPIClientSynchronous.XojoOpenAPIClientSynchronousException(localVarHttpStatus, "", "")

		  localVarHTTPSocket.Close
		  
		  Return True
		End Function
	#tag EndMethod







	#tag Method, Flags = &h21
		Private Function AuthenticationRequired(Realm As String, Headers As InternetHeaders, ByRef Name As String, ByRef Password As String) As Boolean
		  #Pragma Unused Realm
		  #Pragma Unused Headers
		  Name = Me.BasicAuthUser
		  Password = Me.BasicAuthPassword
		  Return True
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub PrivateFuncPrepareSocket(socket As HTTPSecureSocket)
		  socket.Secure = Me.useHTTPS
		  socket.ConnectionType = SSLSocket.TLSv12
		  socket.Port = Me.Port
		  socket.RequestHeaders.Delete("Accept")
		  socket.RequestHeaders.AppendHeader("Accept", "text/plain")
		  socket.RequestHeaders.AppendHeader("Accept", "application/json")
		  socket.RequestHeaders.AppendHeader("Content-Type", "application/json")

		  If Me.AdditionalHeaders <> Nil Then
		    For Each HeaderName As Variant In Me.AdditionalHeaders.Keys
		      Dim headerValueS As Variant = additionalHeaders.Value(HeaderName)
		      If headerValueS.IsArray Then
		        If headerValueS.ArrayElementType = Variant.TypeString Then
		          Dim values() As String = headerValueS
		          For Each value As String In values
		            socket.RequestHeaders.AppendHeader(HeaderName, value)
		          Next
		        Else
		          Raise New XojoOpenAPIClientSynchronous.XojoOpenAPIClientSynchronousException(kErrorInternal, "AdditionalHeaders only support Strings and String arrays as values.")
		        End If
		      Else
		        socket.RequestHeaders.AppendHeader(HeaderName, headerValueS.StringValue)
		      End If
		    Next
		  End If
		End Sub
	#tag EndMethod



	#tag Property, Flags = &h0
		AdditionalHeaders As Dictionary
	#tag EndProperty

	#tag Property, Flags = &h0
		ApiKeyapi_key As String
	#tag EndProperty

	#tag Property, Flags = &h0
		BasePath As String = "http://petstore.swagger.io/v2"
	#tag EndProperty

	#tag Property, Flags = &h0
		BasicAuthPassword As String
	#tag EndProperty

	#tag Property, Flags = &h0
		BasicAuthUser As String
	#tag EndProperty

	#tag Property, Flags = &h0
		Host As String = ""
	#tag EndProperty

	#tag Property, Flags = &h0
		Port As Integer
	#tag EndProperty

	#tag Property, Flags = &h0
		UseHTTPS As Boolean = true
	#tag EndProperty


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
			Name="BasePath"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="String"
			EditorType="MultiLineEditor"
		#tag EndViewProperty
		#tag ViewProperty
			Name="BasicAuthUser"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="String"
			EditorType="MultiLineEditor"
		#tag EndViewProperty
		#tag ViewProperty
			Name="BasicAuthPassword"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="String"
			EditorType="MultiLineEditor"
		#tag EndViewProperty
		#tag ViewProperty
			Name="UseHTTPS"
			Visible=false
			Group="Behavior"
			InitialValue="true"
			Type="Boolean"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Port"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Host"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="String"
			EditorType="MultiLineEditor"
		#tag EndViewProperty
	#tag EndViewBehavior
End Class
#tag EndClass
