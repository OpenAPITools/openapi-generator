#tag Class
Protected Class App
Inherits ConsoleApplication
	#tag Event
		Function Run(args() as String) As Integer
		  #Pragma Unused args
		  Dim m As New Mock
		  m.testPetApi("http://localhost:4010") 'original basePath: http://petstore.swagger.io/v2
		  m.testStoreApi("http://localhost:4010") 'original basePath: http://petstore.swagger.io/v2
		  m.testUserApi("http://localhost:4010") 'original basePath: http://petstore.swagger.io/v2
		End Function
	#tag EndEvent


	#tag ViewBehavior
	#tag EndViewBehavior
End Class
#tag EndClass
