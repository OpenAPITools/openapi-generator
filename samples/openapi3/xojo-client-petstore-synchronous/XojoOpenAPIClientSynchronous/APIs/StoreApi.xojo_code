#tag Class
Protected Class StoreApi
	#tag Method, Flags = &h0
		Function DeleteOrder(ByRef localOutStatus As XojoOpenAPIClientSynchronous.XojoOpenAPIClientSynchronousException, orderId As String) As Boolean
		  // Operation deleteOrder
		  // Delete purchase order by ID
		  // - parameter localOutStatus: Information on whether the operation succeeded.
		  // - parameter orderId: (path) ID of the order that needs to be deleted 
		  //
		  // 
		  //
		  // - DELETE /store/order/{orderId}
		  // - For valid response try integer IDs with value < 1000. Anything above 1000 or nonintegers will generate API errors
		  //
		  //
		  
		  Dim localVarHTTPSocket As New HTTPSecureSocket
		  Me.PrivateFuncPrepareSocket(localVarHTTPSocket)
		  
		  
		  


		  Dim localVarPath As String = "/store/order/{orderId}"
		  
		  Dim localVarPathStringorderId As String = orderId
		  
		  localVarPath = localVarPath.ReplaceAllB("{orderId}", localVarPathStringorderId)
		  
		  
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
		Function GetInventory(ByRef localOutStatus As XojoOpenAPIClientSynchronous.XojoOpenAPIClientSynchronousException, ByRef localOutData As Dictionary) As Boolean
		  // Operation getInventory
		  // Returns pet inventories by status
		  // - parameter localOutStatus: Information on whether the operation succeeded.
		  // - parameter localOutData: On success, contains the data returned by the API.
		  //
		  // 
		  //
		  // - GET /store/inventory
		  // - Returns a map of status codes to quantities
		  // - defaultResponse: Nil
		  //
		  // - API Key:
		  //   - type: apiKey api_key (HEADER)
		  //   - name: api_key
		  //
		  
		  Dim localVarHTTPSocket As New HTTPSecureSocket
		  Me.PrivateFuncPrepareSocket(localVarHTTPSocket)
		  
		  
		  If me.ApiKeyapi_key = "" Then Raise New XojoOpenAPIClientSynchronous.XojoOpenAPIClientSynchronousException(kErrorCannotAuthenticate, "API key is unset. Please assign a value to `StoreApi.ApiKeyapi_key` before invoking `StoreApi.GetInventory()`.")
		  
		  localVarHTTPSocket.SetRequestHeader(EncodeURLComponent("api_key"), EncodeURLComponent(me.ApiKeyapi_key))
		  


		  Dim localVarPath As String = "/store/inventory"
		  
		  
		  
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
		  
		  Return GetInventoryPrivateFuncDeserializeResponse(localVarHTTPStatus, localVarHeaders, localOutStatus, localVarContent, localOutData)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function GetInventoryPrivateFuncDeserializeResponse(HTTPStatus As Integer, Headers As InternetHeaders, error As XojoOpenAPIClientSynchronous.XojoOpenAPIClientSynchronousException, Content As String, ByRef outData As Dictionary) As Boolean
		  Dim contentType As String = Headers.Value("Content-Type")
		  Dim contentEncoding As TextEncoding = XojoOpenAPIClientSynchronous.EncodingFromContentType(contentType)
		  Content = DefineEncoding(Content, contentEncoding)
		  
		  If HTTPStatus > 199 and HTTPStatus < 300 then
		    If contentType.LeftB(16) = "application/json" then
		      
			  outData = New Dictionary
			  Try
		        outData = ParseJSON(Content)

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
		Function GetOrderById(ByRef localOutStatus As XojoOpenAPIClientSynchronous.XojoOpenAPIClientSynchronousException, ByRef localOutData As XojoOpenAPIClientSynchronous.Models.Order, orderId As Int64) As Boolean
		  // Operation getOrderById
		  // Find purchase order by ID
		  // - parameter localOutStatus: Information on whether the operation succeeded.
		  // - parameter localOutData: On success, contains the data returned by the API.
		  // - parameter orderId: (path) ID of pet that needs to be fetched 
		  //
		  // 
		  //
		  // - GET /store/order/{orderId}
		  // - For valid response try integer IDs with value <= 5 or > 10. Other values will generate exceptions
		  // - defaultResponse: Nil
		  //
		  //
		  
		  Dim localVarHTTPSocket As New HTTPSecureSocket
		  Me.PrivateFuncPrepareSocket(localVarHTTPSocket)
		  
		  
		  


		  Dim localVarPath As String = "/store/order/{orderId}"
		  
		  Dim localVarPathStringorderId As String = orderId.ToString
		  
		  localVarPath = localVarPath.ReplaceAllB("{orderId}", localVarPathStringorderId)
		  
		  
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
		  
		  Return GetOrderByIdPrivateFuncDeserializeResponse(localVarHTTPStatus, localVarHeaders, localOutStatus, localVarContent, localOutData)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function GetOrderByIdPrivateFuncDeserializeResponse(HTTPStatus As Integer, Headers As InternetHeaders, error As XojoOpenAPIClientSynchronous.XojoOpenAPIClientSynchronousException, Content As String, ByRef outData As XojoOpenAPIClientSynchronous.Models.Order) As Boolean
		  Dim contentType As String = Headers.Value("Content-Type")
		  Dim contentEncoding As TextEncoding = XojoOpenAPIClientSynchronous.EncodingFromContentType(contentType)
		  Content = DefineEncoding(Content, contentEncoding)
		  
		  If HTTPStatus > 199 and HTTPStatus < 300 then
		    If contentType.LeftB(16) = "application/json" then
		      
			  outData = New XojoOpenAPIClientSynchronous.Models.Order
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
		Function PlaceOrder(ByRef localOutStatus As XojoOpenAPIClientSynchronous.XojoOpenAPIClientSynchronousException, ByRef localOutData As XojoOpenAPIClientSynchronous.Models.Order, order As XojoOpenAPIClientSynchronous.Models.Order) As Boolean
		  // Operation placeOrder
		  // Place an order for a pet
		  // - parameter localOutStatus: Information on whether the operation succeeded.
		  // - parameter localOutData: On success, contains the data returned by the API.
		  // - parameter order: (body) order placed for purchasing the pet 
		  //
		  // 
		  //
		  // - POST /store/order
		  // - 
		  // - defaultResponse: Nil
		  //
		  //
		  
		  Dim localVarHTTPSocket As New HTTPSecureSocket
		  Me.PrivateFuncPrepareSocket(localVarHTTPSocket)
		  localVarHTTPSocket.SetRequestContent(Xoson.toJSON(order), "application/json")
		  
		  


		  Dim localVarPath As String = "/store/order"
		  
		  
		  
		  localVarHTTPSocket.Yield = True 'recommended in synchronous mode
		  Dim localVarContent As String = localVarHTTPSocket.SendRequest("POST", Me.BasePath + localVarPath, XojoOpenAPIClientSynchronous.kAPITimeoutSeconds)
		  
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
		  
		  Return PlaceOrderPrivateFuncDeserializeResponse(localVarHTTPStatus, localVarHeaders, localOutStatus, localVarContent, localOutData)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function PlaceOrderPrivateFuncDeserializeResponse(HTTPStatus As Integer, Headers As InternetHeaders, error As XojoOpenAPIClientSynchronous.XojoOpenAPIClientSynchronousException, Content As String, ByRef outData As XojoOpenAPIClientSynchronous.Models.Order) As Boolean
		  Dim contentType As String = Headers.Value("Content-Type")
		  Dim contentEncoding As TextEncoding = XojoOpenAPIClientSynchronous.EncodingFromContentType(contentType)
		  Content = DefineEncoding(Content, contentEncoding)
		  
		  If HTTPStatus > 199 and HTTPStatus < 300 then
		    If contentType.LeftB(16) = "application/json" then
		      
			  outData = New XojoOpenAPIClientSynchronous.Models.Order
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
