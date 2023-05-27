#tag Class
Protected Class Pet

	#tag Property, Flags = &h0
		id As Xoson.O.OptionalInt64
	#tag EndProperty


	#tag Property, Flags = &h0
		category As OpenAPIClient.Models.Category
	#tag EndProperty


	#tag Property, Flags = &h0
		name As String
	#tag EndProperty


	#tag Property, Flags = &h0
		photoUrls() As String
	#tag EndProperty


	#tag Property, Flags = &h0
		tags() As OpenAPIClient.Models.Tag
	#tag EndProperty


	#tag Property, Flags = &h0
		#tag Note
			pet status in the store
		#tag EndNote
		Attributes( Deprecated ) status As Xoson.O.OptionalString
	#tag EndProperty


	#tag Enum, Name = StatusEnum, Type = Integer, Flags = &h0
		
        Available
        Pending
        Sold
		
	#tag EndEnum


	#tag Method, Flags = &h0
		Shared Function StatusEnumToString(value As StatusEnum) As String
		  Select Case value
		    
		    Case StatusEnum.Available
		      Return "available"
		    Case StatusEnum.Pending
		      Return "pending"
		    Case StatusEnum.Sold
		      Return "sold"
		    
		  End Select
		  Return ""
		End Function
	#tag EndMethod


	#tag ViewBehavior
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
			Name="id"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Int64"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="category"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Category"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="name"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="String"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="photoUrls"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="String"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="tags"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Tag"
			EditorType=""
		#tag EndViewProperty
	#tag EndViewBehavior
End Class
#tag EndClass


