#tag Module
Protected Module OpenAPIClient
	#tag Method, Flags = &h21
		Private Function BoolFromString(s As String) As Boolean
		  If s = "true" Then
		    Return True
		  ElseIf s = "false" Then
		    Return False
		  ElseIf s = "0" Then
		    Return True
		  Else
		    Return False
		  End If
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function EncodingFromContentType(contentType As String) As TextEncoding
		  // Extract the encoding from the contentType header.
		  // Defaults to Encodings.UTF8.
		  
		  Dim contentEncoding As TextEncoding = nil
		  
		  Dim components() As String = contentType.SplitB(";")
		  For Each component As String In components
		    Dim directive() As String = component.SplitB("=")
		    If Ubound(directive) > 0 And directive(0).RightB(7) = "charset" Then
		      contentEncoding = GetInternetTextEncoding(directive(1))
		      If contentEncoding <> Nil Then Return contentEncoding
		    End If
		  Next
		  
		  Return Encodings.UTF8
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function FromRFC3339(Extends stringRepresentation As String) As Date

		  Dim d As New Xoson.DateIntermediate(stringRepresentation)

		  Return New Date(d.year, d.month, d.day, d.hour, d.minute, d.second, 0.0)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function FromRFC3339(Extends stringRepresentation As String) As Xojo.Core.Date

		  Dim d As New Xoson.DateIntermediate(stringRepresentation)

		  Return New Xojo.Core.Date(d.year, d.month, d.day, d.hour, d.minute, d.second, d.millisecond * 1000, new Xojo.Core.TimeZone("Etc/UTC"))
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function LocalFuncSerializeFile(Extends file As FolderItem) As String
		  If file <> Nil Then
		    Try
		      Dim bs As BinaryStream = BinaryStream.Open(file, False)
		      // read the whole binaryStream
		      Return bs.Read(bs.Length)
		      
		    Catch e As IOException
		      Dim error As New OpenAPIClient.OpenAPIClientException(kErrorFileWrite, e.Message)
		      error.ErrorNumber = e.ErrorNumber
		      Raise error
		    End Try
		  Else
		    Dim error As New OpenAPIClient.OpenAPIClientException(kErrorInternal, "Nil parameter ")
		    Raise error
		  End If
		  
		  Return ""
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function ToRFC3339(Extends d As Date) As String
		  Dim utcDate As New Date(d)
		  utcDate.GMTOffset = 0

		  Dim mb As New MemoryBlock(24)
		  mb.StringValue(0,4) = Format(utcDate.Year, "0000")
		  mb.Byte(4) = &h2D
		  mb.StringValue(5,2) = Format(utcDate.Month, "00")
		  mb.Byte(7) = &h2D
		  mb.StringValue(8,2) = Format(utcDate.Day, "00")
		  mb.Byte(10) = &h54
		  mb.StringValue(11,2) = Format(utcDate.Hour, "00")
		  mb.Byte(13) = &h3A
		  mb.StringValue(14,2) = Format(utcDate.Minute, "00")
		  mb.Byte(16) = &h3A
		  mb.StringValue(17,2) = Format(utcDate.Second, "00")
		  mb.Byte(19) = &h2E
		  mb.StringValue(20,3) = "000" 'milliseconds are not available in Date
		  mb.Byte(23) = &h5A
		  Return DefineEncoding(mb.StringValue(0, 24), Encodings.UTF8)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function ToRFC3339(Extends d As Xojo.Core.Date) As String
		  Dim utcDate As New Xojo.Core.Date(d.SecondsFrom1970, New Xojo.Core.TimeZone("Etc/UTC"))

		  Dim mb As New MemoryBlock(24)
		  mb.StringValue(0,4) = Format(utcDate.Year, "0000")
		  mb.Byte(4) = &h2D
		  mb.StringValue(5,2) = Format(utcDate.Month, "00")
		  mb.Byte(7) = &h2D
		  mb.StringValue(8,2) = Format(utcDate.Day, "00")
		  mb.Byte(10) = &h54
		  mb.StringValue(11,2) = Format(utcDate.Hour, "00")
		  mb.Byte(13) = &h3A
		  mb.StringValue(14,2) = Format(utcDate.Minute, "00")
		  mb.Byte(16) = &h3A
		  mb.StringValue(17,2) = Format(utcDate.Second, "00")
		  mb.Byte(19) = &h2E
		  mb.StringValue(20,3) = Format(utcDate.Nanosecond / 1000, "000")
		  mb.Byte(23) = &h5A
		  Return DefineEncoding(mb.StringValue(0, 24), Encodings.UTF8)
		End Function
	#tag EndMethod


	#tag Constant, Name = kErrorInternal, Type = Double, Dynamic = False, Default = \"-10", Scope = Protected
	#tag EndConstant

	#tag Constant, Name = kErrorFileWrite, Type = Double, Dynamic = False, Default = \"-8", Scope = Protected
	#tag EndConstant

	#tag Constant, Name = kErrorCannotAuthenticate, Type = Double, Dynamic = False, Default = \"-7", Scope = Protected
	#tag EndConstant

	#tag Constant, Name = kErrorUnsupportedFeature, Type = Double, Dynamic = False, Default = \"-6", Scope = Protected
	#tag EndConstant

	#tag Constant, Name = kErrorSocketFail, Type = Double, Dynamic = False, Default = \"-5", Scope = Protected
	#tag EndConstant

	#tag Constant, Name = kErrorHTTPFail, Type = Double, Dynamic = False, Default = \"-4", Scope = Protected
	#tag EndConstant

	#tag Constant, Name = kErrorUnsupportedMediaType, Type = Double, Dynamic = False, Default = \"-3", Scope = Protected
	#tag EndConstant

	#tag Constant, Name = kErrorXosonProblem, Type = Double, Dynamic = False, Default = \"-2", Scope = Protected
	#tag EndConstant

	#tag Constant, Name = kErrorInvalidJSON, Type = Double, Dynamic = False, Default = \"-1", Scope = Protected
	#tag EndConstant

	#tag Constant, Name = kAPITimeoutSeconds, Type = Double, Dynamic = False, Default = \"5", Scope = Private
	#tag EndConstant

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
	#tag EndViewBehavior
End Module
#tag EndModule
