#tag Interface
Protected Interface PetApiCallbackHandler
	#tag Method, Flags = &h0
		Sub AddPetCallback(status As OpenAPIClient.OpenAPIClientException, Optional data As OpenAPIClient.Models.Pet)
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub DeletePetCallback(status As OpenAPIClient.OpenAPIClientException)
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub FindPetsByStatusCallback(status As OpenAPIClient.OpenAPIClientException, data() As OpenAPIClient.Models.Pet)
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub FindPetsByTagsCallback(status As OpenAPIClient.OpenAPIClientException, data() As OpenAPIClient.Models.Pet)
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub GetPetByIdCallback(status As OpenAPIClient.OpenAPIClientException, Optional data As OpenAPIClient.Models.Pet)
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub UpdatePetCallback(status As OpenAPIClient.OpenAPIClientException, Optional data As OpenAPIClient.Models.Pet)
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub UpdatePetWithFormCallback(status As OpenAPIClient.OpenAPIClientException)
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub UploadFileCallback(status As OpenAPIClient.OpenAPIClientException, Optional data As OpenAPIClient.Models.ApiResponse)
		  
		End Sub
	#tag EndMethod




	#tag ViewBehavior
		#tag ViewProperty
			Name="Name"
			Visible=true
			Group="ID"
			Type="String"
		#tag EndViewProperty
		#tag ViewProperty
			Name="Index"
			Visible=true
			Group="ID"
			InitialValue="-2147483648"
			Type="Integer"
		#tag EndViewProperty
		#tag ViewProperty
			Name="Super"
			Visible=true
			Group="ID"
			Type="String"
		#tag EndViewProperty
		#tag ViewProperty
			Name="Left"
			Visible=true
			Group="Position"
			InitialValue="0"
			Type="Integer"
		#tag EndViewProperty
		#tag ViewProperty
			Name="Top"
			Visible=true
			Group="Position"
			InitialValue="0"
			Type="Integer"
		#tag EndViewProperty
	#tag EndViewBehavior
End Interface
#tag EndInterface
