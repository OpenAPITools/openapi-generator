#tag Class
Protected Class PetApi
	#tag Method, Flags = &h0
		Function AddPet(ByRef localOutStatus As XojoOpenAPIClientSynchronous.XojoOpenAPIClientSynchronousException, ByRef localOutData As XojoOpenAPIClientSynchronous.Models.Pet, pet As XojoOpenAPIClientSynchronous.Models.Pet) As Boolean
		  // Operation addPet
		  // Add a new pet to the store
		  // - parameter localOutStatus: Information on whether the operation succeeded.
		  // - parameter localOutData: On success, contains the data returned by the API.
		  // - parameter pet: (body) Pet object that needs to be added to the store 
		  //
		  // 
		  //
		  // - POST /pet
		  // - 
		  // - defaultResponse: Nil
		  //
		  // - OAuth:
		  //   - type: oauth2
		  //   - name: petstore_auth
		  //
		  
		  Dim localVarHTTPSocket As New HTTPSecureSocket
		  Me.PrivateFuncPrepareSocket(localVarHTTPSocket)
		  localVarHTTPSocket.SetRequestContent(Xoson.toJSON(pet), "application/json")
		  
		  
		  


		  Dim localVarPath As String = "/pet"
		  
		  
		  
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
		  
		  Return AddPetPrivateFuncDeserializeResponse(localVarHTTPStatus, localVarHeaders, localOutStatus, localVarContent, localOutData)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function AddPetPrivateFuncDeserializeResponse(HTTPStatus As Integer, Headers As InternetHeaders, error As XojoOpenAPIClientSynchronous.XojoOpenAPIClientSynchronousException, Content As String, ByRef outData As XojoOpenAPIClientSynchronous.Models.Pet) As Boolean
		  Dim contentType As String = Headers.Value("Content-Type")
		  Dim contentEncoding As TextEncoding = XojoOpenAPIClientSynchronous.EncodingFromContentType(contentType)
		  Content = DefineEncoding(Content, contentEncoding)
		  
		  If HTTPStatus > 199 and HTTPStatus < 300 then
		    If contentType.LeftB(16) = "application/json" then
		      
			  outData = New XojoOpenAPIClientSynchronous.Models.Pet
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
		Function DeletePet(ByRef localOutStatus As XojoOpenAPIClientSynchronous.XojoOpenAPIClientSynchronousException, petId As Int64, Optional apiKey As Xoson.O.OptionalString) As Boolean
		  // Operation deletePet
		  // Deletes a pet
		  // - parameter localOutStatus: Information on whether the operation succeeded.
		  // - parameter petId: (path) Pet id to delete 
		  // - parameter apiKey: (header)  (optional, default to Sample)
		  //
		  // 
		  //
		  // - DELETE /pet/{petId}
		  // - 
		  //
		  // - OAuth:
		  //   - type: oauth2
		  //   - name: petstore_auth
		  //
		  
		  Dim localVarHTTPSocket As New HTTPSecureSocket
		  Me.PrivateFuncPrepareSocket(localVarHTTPSocket)
		  
		  		  If apiKey <> nil Then localVarHTTPSocket.SetRequestHeader(EncodeURLComponent("api_key"), EncodeURLComponent(apiKey))
		  
		  
		  


		  Dim localVarPath As String = "/pet/{petId}"
		  
		  Dim localVarPathStringpetId As String = petId.ToString
		  
		  localVarPath = localVarPath.ReplaceAllB("{petId}", localVarPathStringpetId)
		  
		  
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
		Function FindPetsByStatus(ByRef localOutStatus As XojoOpenAPIClientSynchronous.XojoOpenAPIClientSynchronousException, localOutData() As XojoOpenAPIClientSynchronous.Models.Pet, status() As StatusEnum_FindPetsByStatus) As Boolean
		  // Operation findPetsByStatus
		  // Finds Pets by status
		  // - parameter localOutStatus: Information on whether the operation succeeded.
		  // - parameter localOutData: On success, contains the data returned by the API.
		  // - parameter status: (query) Status values that need to be considered for filter 
		  //
		  // 
		  //
		  // - GET /pet/findByStatus
		  // - Multiple status values can be provided with comma separated strings
		  // - defaultResponse: Nil
		  //
		  // - OAuth:
		  //   - type: oauth2
		  //   - name: petstore_auth
		  //
		  
		  Dim localVarHTTPSocket As New HTTPSecureSocket
		  Me.PrivateFuncPrepareSocket(localVarHTTPSocket)
		  
		  Dim localVarQueryParams As String = "?"
		  
		  Dim localVarQueryStringsstatus() As String
		  For Each localVarItemstatus As StatusEnum_FindPetsByStatus in status
		    Dim encodedParameter As String = EncodeURLComponent(StatusEnum_FindPetsByStatusToString(localVarItemstatus))
		    localVarQueryStringsstatus.Append(encodedParameter)
		  Next
		  
		  Dim localVarQueryStringstatus As String
		  Select Case "form"
		    Case "form"
			  localVarQueryStringstatus = "status=" + Join(localVarQueryStringsstatus, ",")
		    Case "spaceDelimited"
		      localVarQueryStringstatus = "status=" + Join(localVarQueryStringsstatus, " ")
		    Case "pipeDelimited"
		      localVarQueryStringstatus = "status=" + Join(localVarQueryStringsstatus, "|")
		    Case "deepObject"
		      Raise New XojoOpenAPIClientSynchronous.XojoOpenAPIClientSynchronousException(kErrorUnsupportedFeature, "deepObject query parameters are not supported")
		  End Select
		  If localVarQueryStringsstatus.Ubound() > -1 Then localVarQueryParams = localVarQueryParams + EncodeURLComponent("status") + "=" + EncodeURLComponent(localVarQueryStringstatus)

		  
		  


		  Dim localVarPath As String = "/pet/findByStatus"
		  
		  
		  
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
		  
		  Return FindPetsByStatusPrivateFuncDeserializeResponse(localVarHTTPStatus, localVarHeaders, localOutStatus, localVarContent, localOutData)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function FindPetsByStatusPrivateFuncDeserializeResponse(HTTPStatus As Integer, Headers As InternetHeaders, error As XojoOpenAPIClientSynchronous.XojoOpenAPIClientSynchronousException, Content As String, outData() As XojoOpenAPIClientSynchronous.Models.Pet) As Boolean
		  Dim contentType As String = Headers.Value("Content-Type")
		  Dim contentEncoding As TextEncoding = XojoOpenAPIClientSynchronous.EncodingFromContentType(contentType)
		  Content = DefineEncoding(Content, contentEncoding)
		  
		  If HTTPStatus > 199 and HTTPStatus < 300 then
		    If contentType.LeftB(16) = "application/json" then
		      
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
		Private Function StatusEnum_FindPetsByStatusToString(value As StatusEnum_FindPetsByStatus) As String
		  Select Case value
		    
		    Case StatusEnum_FindPetsByStatus.Available
		      Return "available"
		    Case StatusEnum_FindPetsByStatus.Pending
		      Return "pending"
		    Case StatusEnum_FindPetsByStatus.Sold
		      Return "sold"
		    
		  End Select
		  Return ""
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function FindPetsByTags(ByRef localOutStatus As XojoOpenAPIClientSynchronous.XojoOpenAPIClientSynchronousException, localOutData() As XojoOpenAPIClientSynchronous.Models.Pet, tags() As String) As Boolean
		  // Operation findPetsByTags
		  // Finds Pets by tags
		  // - parameter localOutStatus: Information on whether the operation succeeded.
		  // - parameter localOutData: On success, contains the data returned by the API.
		  // - parameter tags: (query) Tags to filter by 
		  //
		  // 
		  //
		  // - GET /pet/findByTags
		  // - Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
		  // - defaultResponse: Nil
		  //
		  // - OAuth:
		  //   - type: oauth2
		  //   - name: petstore_auth
		  //
		  
		  Dim localVarHTTPSocket As New HTTPSecureSocket
		  Me.PrivateFuncPrepareSocket(localVarHTTPSocket)
		  
		  Dim localVarQueryParams As String = "?"
		  
		  Dim localVarQueryStringstags() As String
		  For Each localVarItemtags As String in tags
		    Dim encodedParameter As String = EncodeURLComponent(localVarItemtags)
		    localVarQueryStringstags.Append(encodedParameter)
		  Next
		  
		  Dim localVarQueryStringtags As String
		  Select Case "form"
		    Case "form"
			  localVarQueryStringtags = "inner=" + Join(localVarQueryStringstags, ",")
		    Case "spaceDelimited"
		      localVarQueryStringtags = "inner=" + Join(localVarQueryStringstags, " ")
		    Case "pipeDelimited"
		      localVarQueryStringtags = "inner=" + Join(localVarQueryStringstags, "|")
		    Case "deepObject"
		      Raise New XojoOpenAPIClientSynchronous.XojoOpenAPIClientSynchronousException(kErrorUnsupportedFeature, "deepObject query parameters are not supported")
		  End Select
		  If localVarQueryStringstags.Ubound() > -1 Then localVarQueryParams = localVarQueryParams + EncodeURLComponent("inner") + "=" + EncodeURLComponent(localVarQueryStringtags)

		  
		  


		  Dim localVarPath As String = "/pet/findByTags"
		  
		  
		  
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
		  
		  Return FindPetsByTagsPrivateFuncDeserializeResponse(localVarHTTPStatus, localVarHeaders, localOutStatus, localVarContent, localOutData)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function FindPetsByTagsPrivateFuncDeserializeResponse(HTTPStatus As Integer, Headers As InternetHeaders, error As XojoOpenAPIClientSynchronous.XojoOpenAPIClientSynchronousException, Content As String, outData() As XojoOpenAPIClientSynchronous.Models.Pet) As Boolean
		  Dim contentType As String = Headers.Value("Content-Type")
		  Dim contentEncoding As TextEncoding = XojoOpenAPIClientSynchronous.EncodingFromContentType(contentType)
		  Content = DefineEncoding(Content, contentEncoding)
		  
		  If HTTPStatus > 199 and HTTPStatus < 300 then
		    If contentType.LeftB(16) = "application/json" then
		      
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
		Function GetPetById(ByRef localOutStatus As XojoOpenAPIClientSynchronous.XojoOpenAPIClientSynchronousException, ByRef localOutData As XojoOpenAPIClientSynchronous.Models.Pet, petId As Int64) As Boolean
		  // Operation getPetById
		  // Find pet by ID
		  // - parameter localOutStatus: Information on whether the operation succeeded.
		  // - parameter localOutData: On success, contains the data returned by the API.
		  // - parameter petId: (path) ID of pet to return 
		  //
		  // 
		  //
		  // - GET /pet/{petId}
		  // - Returns a single pet
		  // - defaultResponse: Nil
		  //
		  // - API Key:
		  //   - type: apiKey api_key (HEADER)
		  //   - name: api_key
		  //
		  
		  Dim localVarHTTPSocket As New HTTPSecureSocket
		  Me.PrivateFuncPrepareSocket(localVarHTTPSocket)
		  
		  
		  If me.ApiKeyapi_key = "" Then Raise New XojoOpenAPIClientSynchronous.XojoOpenAPIClientSynchronousException(kErrorCannotAuthenticate, "API key is unset. Please assign a value to `PetApi.ApiKeyapi_key` before invoking `PetApi.GetPetById()`.")
		  
		  localVarHTTPSocket.SetRequestHeader(EncodeURLComponent("api_key"), EncodeURLComponent(me.ApiKeyapi_key))
		  


		  Dim localVarPath As String = "/pet/{petId}"
		  
		  Dim localVarPathStringpetId As String = petId.ToString
		  
		  localVarPath = localVarPath.ReplaceAllB("{petId}", localVarPathStringpetId)
		  
		  
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
		  
		  Return GetPetByIdPrivateFuncDeserializeResponse(localVarHTTPStatus, localVarHeaders, localOutStatus, localVarContent, localOutData)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function GetPetByIdPrivateFuncDeserializeResponse(HTTPStatus As Integer, Headers As InternetHeaders, error As XojoOpenAPIClientSynchronous.XojoOpenAPIClientSynchronousException, Content As String, ByRef outData As XojoOpenAPIClientSynchronous.Models.Pet) As Boolean
		  Dim contentType As String = Headers.Value("Content-Type")
		  Dim contentEncoding As TextEncoding = XojoOpenAPIClientSynchronous.EncodingFromContentType(contentType)
		  Content = DefineEncoding(Content, contentEncoding)
		  
		  If HTTPStatus > 199 and HTTPStatus < 300 then
		    If contentType.LeftB(16) = "application/json" then
		      
			  outData = New XojoOpenAPIClientSynchronous.Models.Pet
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
		Function UpdatePet(ByRef localOutStatus As XojoOpenAPIClientSynchronous.XojoOpenAPIClientSynchronousException, ByRef localOutData As XojoOpenAPIClientSynchronous.Models.Pet, pet As XojoOpenAPIClientSynchronous.Models.Pet) As Boolean
		  // Operation updatePet
		  // Update an existing pet
		  // - parameter localOutStatus: Information on whether the operation succeeded.
		  // - parameter localOutData: On success, contains the data returned by the API.
		  // - parameter pet: (body) Pet object that needs to be added to the store 
		  //
		  // 
		  //
		  // - PUT /pet
		  // - 
		  // - defaultResponse: Nil
		  //
		  // - OAuth:
		  //   - type: oauth2
		  //   - name: petstore_auth
		  //
		  
		  Dim localVarHTTPSocket As New HTTPSecureSocket
		  Me.PrivateFuncPrepareSocket(localVarHTTPSocket)
		  localVarHTTPSocket.SetRequestContent(Xoson.toJSON(pet), "application/json")
		  
		  
		  


		  Dim localVarPath As String = "/pet"
		  
		  
		  
		  localVarHTTPSocket.Yield = True 'recommended in synchronous mode
		  Dim localVarContent As String = localVarHTTPSocket.SendRequest("PUT", Me.BasePath + localVarPath, XojoOpenAPIClientSynchronous.kAPITimeoutSeconds)
		  
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
		  
		  Return UpdatePetPrivateFuncDeserializeResponse(localVarHTTPStatus, localVarHeaders, localOutStatus, localVarContent, localOutData)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function UpdatePetPrivateFuncDeserializeResponse(HTTPStatus As Integer, Headers As InternetHeaders, error As XojoOpenAPIClientSynchronous.XojoOpenAPIClientSynchronousException, Content As String, ByRef outData As XojoOpenAPIClientSynchronous.Models.Pet) As Boolean
		  Dim contentType As String = Headers.Value("Content-Type")
		  Dim contentEncoding As TextEncoding = XojoOpenAPIClientSynchronous.EncodingFromContentType(contentType)
		  Content = DefineEncoding(Content, contentEncoding)
		  
		  If HTTPStatus > 199 and HTTPStatus < 300 then
		    If contentType.LeftB(16) = "application/json" then
		      
			  outData = New XojoOpenAPIClientSynchronous.Models.Pet
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
		Function UpdatePetWithForm(ByRef localOutStatus As XojoOpenAPIClientSynchronous.XojoOpenAPIClientSynchronousException, petId As Int64, Optional name As Xoson.O.OptionalString, Optional status As Xoson.O.OptionalString) As Boolean
		  // Operation updatePetWithForm
		  // Updates a pet in the store with form data
		  // - parameter localOutStatus: Information on whether the operation succeeded.
		  // - parameter petId: (path) ID of pet that needs to be updated 
		  // - parameter name: (form) Updated name of the pet (optional, default to Sample)
		  // - parameter status: (form) Updated status of the pet (optional, default to Sample)
		  //
		  // 
		  //
		  // - POST /pet/{petId}
		  // - 
		  //
		  // - OAuth:
		  //   - type: oauth2
		  //   - name: petstore_auth
		  //
		  
		  Dim localVarHTTPSocket As New HTTPSecureSocket
		  Me.PrivateFuncPrepareSocket(localVarHTTPSocket)
		  Dim localVarFormParams As New Dictionary
		  If name <> nil Then localVarFormParams.Value("name") = name
If status <> nil Then localVarFormParams.Value("status") = status
		  If localVarFormParams.Count > 0 Then localVarHTTPSocket.SetFormData(localVarFormParams)
		  
		  
		  
		  


		  Dim localVarPath As String = "/pet/{petId}"
		  
		  Dim localVarPathStringpetId As String = petId.ToString
		  
		  localVarPath = localVarPath.ReplaceAllB("{petId}", localVarPathStringpetId)
		  
		  
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
		Function UploadFile(ByRef localOutStatus As XojoOpenAPIClientSynchronous.XojoOpenAPIClientSynchronousException, ByRef localOutData As XojoOpenAPIClientSynchronous.Models.ApiResponse, petId As Int64, Optional additionalMetadata As Xoson.O.OptionalString, Optional Escapedfile As FolderItem) As Boolean
		  // Operation uploadFile
		  // uploads an image
		  // - parameter localOutStatus: Information on whether the operation succeeded.
		  // - parameter localOutData: On success, contains the data returned by the API.
		  // - parameter petId: (path) ID of pet to update 
		  // - parameter additionalMetadata: (form) Additional data to pass to server (optional, default to Sample)
		  // - parameter Escapedfile: (form) file to upload (optional, default to Sample)
		  //
		  // 
		  //
		  // - POST /pet/{petId}/uploadImage
		  // - 
		  // - defaultResponse: Nil
		  //
		  // - OAuth:
		  //   - type: oauth2
		  //   - name: petstore_auth
		  //
		  
		  Dim localVarHTTPSocket As New HTTPSecureSocket
		  Me.PrivateFuncPrepareSocket(localVarHTTPSocket)
		  Dim localVarFormParams As New Dictionary
		  If additionalMetadata <> nil Then localVarFormParams.Value("additionalMetadata") = additionalMetadata
If Escapedfile <> nil Then localVarFormParams.Value("file") = Escapedfile.LocalFuncSerializeFile
		  If localVarFormParams.Count > 0 Then localVarHTTPSocket.SetFormData(localVarFormParams)
		  
		  
		  
		  


		  Dim localVarPath As String = "/pet/{petId}/uploadImage"
		  
		  Dim localVarPathStringpetId As String = petId.ToString
		  
		  localVarPath = localVarPath.ReplaceAllB("{petId}", localVarPathStringpetId)
		  
		  
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
		  
		  Return UploadFilePrivateFuncDeserializeResponse(localVarHTTPStatus, localVarHeaders, localOutStatus, localVarContent, localOutData)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function UploadFilePrivateFuncDeserializeResponse(HTTPStatus As Integer, Headers As InternetHeaders, error As XojoOpenAPIClientSynchronous.XojoOpenAPIClientSynchronousException, Content As String, ByRef outData As XojoOpenAPIClientSynchronous.Models.ApiResponse) As Boolean
		  Dim contentType As String = Headers.Value("Content-Type")
		  Dim contentEncoding As TextEncoding = XojoOpenAPIClientSynchronous.EncodingFromContentType(contentType)
		  Content = DefineEncoding(Content, contentEncoding)
		  
		  If HTTPStatus > 199 and HTTPStatus < 300 then
		    If contentType.LeftB(16) = "application/json" then
		      
			  outData = New XojoOpenAPIClientSynchronous.Models.ApiResponse
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

	#tag Enum, Name = StatusEnum_FindPetsByStatus, Type = Integer, Flags = &h0
		
        Available
        Pending
        Sold
		
	#tag EndEnum


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
