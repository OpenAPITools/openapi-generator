#tag Interface
Protected Interface StoreApiCallbackHandler
	#tag Method, Flags = &h0
		Sub DeleteOrderCallback(status As OpenAPIClient.OpenAPIClientException)
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub GetInventoryCallback(status As OpenAPIClient.OpenAPIClientException, Optional data As Dictionary)
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub GetOrderByIdCallback(status As OpenAPIClient.OpenAPIClientException, Optional data As OpenAPIClient.Models.Order)
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub PlaceOrderCallback(status As OpenAPIClient.OpenAPIClientException, Optional data As OpenAPIClient.Models.Order)
		  
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
