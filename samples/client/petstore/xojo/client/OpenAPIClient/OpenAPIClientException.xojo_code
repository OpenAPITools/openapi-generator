#tag Class
Protected Class OpenAPIClientException
Inherits RuntimeException
	#tag Method, Flags = &h0
		Sub Constructor(errorNumber As Integer, message As String)
		  // Construct an error related to the library.
		  'Super.Constructor
		  Me.Message = message
		  Me.ErrorNumber = errorNumber
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Constructor(HTTPCode As Integer, message As String, content As String)
		  // Construct an error related to an HTTP response.
		  'Super.Constructor

		  Me.HTTPCode = HTTPCode
		  If message = "" Then
		    Me.Message = "HTTP Status " + Str(HTTPCode)
		  Else
		    Me.Message = message
		  End If

		  If HTTPCode < 200 Or HTTPCode > 299 Then
		    Me.ErrorNumber = kErrorHTTPFail
		    Me.OriginalResponseBody = content
		  End If
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Constructor(socketCode As Integer)
		  // Construct an error related to a socket.
		  'Super.Constructor
		  Me.SocketCode = socketCode
		  Me.Message = MessageForSocketErrorCode(socketCode)

		  If socketCode <> 0 Then Me.ErrorNumber = kErrorSocketFail
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub Constructor()
		  // Disallow construction of empty results.
		  'Super.Constructor
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function MessageForSocketErrorCode(code As Integer) As String
		  If code = 0 Then
		    Return "No error occurred."
		  ElseIf code = 100 Then
		    Return "There was an error opening and initializing the drivers."
		  ElseIf code = 101 Then
		    Return "This error code is no longer used."
		  ElseIf code = 102 Then
		    Return "This code means that you lost your connection."
		  ElseIf code = 103 Then
		    Return "The socket was unable to resolve the address that was specified."
		  ElseIf code = 104 Then
		    Return "This error code is no longer used."
		  ElseIf code = 105 Then
		    Return "The address is currently in use."
		  ElseIf code = 106 Then
		    Return "This is an invalid state error, which means that the socket is not in the proper state to be doing a certain operation."
		  ElseIf code = 107 Then
		    Return "This error means that the port you specified is invalid."
		  ElseIf code = 108 Then
		    Return "This error indicates that your application has run out of memory."
		  End If
		  
		  Return "An unknown socket error " + Str(code) + " occurred."
		End Function
	#tag EndMethod


	#tag ComputedProperty, Flags = &h0
		#tag Getter
			Get
			  Return "OpenAPIClientException | ErrorNumber: " + Str(Me.ErrorNumber) + " | HTTP Status: " + Str(Me.HTTPCode) + " | SocketCode: " + Str(Me.SocketCode) + " | Message: " + Me.Message
			End Get
		#tag EndGetter
		Description As String
	#tag EndComputedProperty

	#tag Property, Flags = &h0
		HTTPCode As Integer = -1
	#tag EndProperty

	#tag ComputedProperty, Flags = &h0
		#tag Getter
			Get
			  Return ErrorNumber <> 0
			End Get
		#tag EndGetter
		IsError As Boolean
	#tag EndComputedProperty

	#tag Property, Flags = &h0
		OriginalResponseBody As String
	#tag EndProperty

	#tag Property, Flags = &h0
		SocketCode As Integer = 0
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
			Name="HTTPCode"
			Visible=false
			Group="Behavior"
			InitialValue="-1"
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Message"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="String"
			EditorType="MultiLineEditor"
		#tag EndViewProperty
		#tag ViewProperty
			Name="SocketCode"
			Visible=false
			Group="Behavior"
			InitialValue="-1"
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="IsError"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Boolean"
			EditorType=""
		#tag EndViewProperty
	#tag EndViewBehavior
End Class
#tag EndClass
