package test
{
	import com.adobe.serialization.json.JSON;
    import com.wordnik.swagger.common.ApiInvoker;
	import com.wordnik.swagger.common.ApiUserCredentials;
	import com.wordnik.swagger.common.SwaggerApi;
	import com.wordnik.swagger.event.ApiClientEvent;
	import com.wordnik.swagger.event.Response;

	import flash.desktop.NativeApplication;
	import flash.events.Event;
	import flash.events.EventDispatcher;
	import flash.events.IEventDispatcher;
	import flash.events.IOErrorEvent;
	import flash.filesystem.File;
	import flash.filesystem.FileMode;
	import flash.filesystem.FileStream;
	import flash.net.URLLoader;
	import flash.net.URLRequest;
	import flash.system.System;
	import flash.utils.describeType;
	import flash.utils.getDefinitionByName;
	
	import flexunit.framework.TestCase;
	
	import mx.core.ClassFactory;
	import mx.rpc.events.FaultEvent;
	import mx.utils.StringUtil;
	
	public class TestExecutor extends TestCase
	{
		private var urlReq:URLRequest;
		private var urlLdr:URLLoader;
		
		private var apiInvoker: ApiInvoker;

		private const MODEL_INFO_URL:String ="testData.json";
		private const TIME_OUT:int = 5000;
		
		private var testData: Object;
				

		public function testApiMethod():void{
			initializeRequest();
			urlLdr.addEventListener(Event.COMPLETE, addAsync(executeTest, TIME_OUT));
			urlLdr.addEventListener(IOErrorEvent.IO_ERROR, executeTest);
			urlLdr.load(urlReq);		
		}
		
		private function initializeRequest():void {
			/* Initialize the URLRequest object with the URL to the file of name/value pairs. */
			urlReq = new URLRequest(MODEL_INFO_URL);
			/* Initialize the URLLoader object, assign the various event listeners, and load the specified URLRequest object. */
			urlLdr = new URLLoader();
		}
		
		private function checkAndLoadModelXml(event:Event):Object {
			var ldr:URLLoader = urlLdr;//event.currentTarget as URLLoader;
			assertTrue("Test data info not found ", ldr.data != null);
			
			var testData:Object = JSON.decode(ldr.data);
			//var classList:XML = new XML(ldr.data);
			assertTrue("Test data could not be loaded as xml ", testData != null);
			return testData;
		}
				
		private function executeTest(event:Event):void {
			testData = checkAndLoadModelXml(event);
			//figure out class and method to execute
			var className: String = getServiceName(testData.resource);
			var methodName: String = testData.methodName;
			var servicePackageName: String = testData.apiPackageName;
			var fullClassName: String = servicePackageName + "." + className;
			
			var apiUrl: String = testData.apiUrl;
			var apiHostName : String = apiUrl.substring(apiUrl.indexOf("//")+2, 
				apiUrl.indexOf("/",apiUrl.indexOf("//")+2) );
			var apiPath: String = apiUrl.substring(apiUrl.indexOf(apiHostName)+ apiHostName.length, 
				apiUrl.indexOf("/", apiUrl.indexOf(apiHostName)+ apiHostName.length + 1));
			var useProxyServer: Boolean = false;
			if(testData.useProxyServer != null){
				useProxyServer = testData.useProxyServer == "true" ? true : false;
			}
			var params: Array;
			//execute the test
			
			var classRef:Class;
			try{
				classRef = getDefinitionByName(fullClassName) as Class;
			}
			catch(error: ReferenceError){
				var classFailure: Response = new Response(false, null, "Api Class not found");
				writeToFile( JSON.encode(classFailure) );	
			}
			
			var apiCredentials: ApiUserCredentials = new ApiUserCredentials(testData.apiKey,
				testData.authToken, apiHostName, -1, apiPath, testData.proxyServerUrl);
			
			var apiInstance : * = new classRef(apiCredentials);
			apiInstance.useProxyServer(useProxyServer);
			apiInstance.addEventListener(methodName, addAsync(onApiCallResponse, TIME_OUT, {} , apiTimeOutHandler) );
			apiInstance.addEventListener(ApiClientEvent.FAILURE_EVENT, onApiCallFault );
			
			var queryAndPathParams: Object = new Object();
			
			queryAndPathParams = testData.queryAndPathParams;
			
			params = getArgumentsForTestCaseExecution(methodName, apiInstance,  
				queryAndPathParams, testData.postData, className, testData.resource);
			if(apiInstance.hasOwnProperty(methodName)){
				var method:Function = apiInstance[methodName];
				var returnValue:* = method.apply(apiInstance, params);
			}
			else{
				//write out error
				var failureResponse: Response = new Response(false, null, "Method not found");
				writeToFile( JSON.encode(failureResponse) );
			}			
			
			//write out test data result to json file 
		}
		
		private function onApiCallResponse(event: ApiClientEvent, tokenObject: Object = null): void{
			var result: Response = event.response;
			trace("writing to file");			
			
			writeToFile( JSON.encode(result) );						
		}
		
		private function onApiCallFault(event:FaultEvent):void {
			var failureResponse: Response = new Response(false, null, "Method invocation failure");
			writeToFile( JSON.encode(failureResponse) );			
		}
		
		private function writeToFile(data: String) : void {
			var localFile: File = File.documentsDirectory.resolvePath("testOutput.json");
			var localFileStream:FileStream = new FileStream();
			
			localFileStream.open(localFile, FileMode.WRITE);
			
			localFileStream.writeMultiByte( data, "utf-8");				
			localFileStream.close();				
			trace(data);
			applicationExit();
		}
		
		private function apiTimeOutHandler(o: Object):void {
			//fail("test timed out");
			trace("Execution timed out");
			var failureResponse: Response = new Response(false, null, "Method Execution timed out");
			writeToFile( JSON.encode(failureResponse) );						
		}		
		
		/**
		 * Generate name of the service from resource path.
		 *
		 * Example: if input is /user.json the generated name for this path will be UserAPI
		 * If the input is /user.json/{userId}, the service name will still be generated as UserAPI
		 *
		 * @param resourcePath
		 * @return
		 */
		private function getServiceName(resourcePath: String): String {
			var className:String = null;
			var index: int = resourcePath.indexOf(".");
			if(index >= 0) {
				var resourceName: String = resourcePath.substring(1,index);
				className = applyClassNamingPolicy(resourceName);
			}else{
				var paths: Array = resourcePath.split("/");
				for each(var path: String in paths) {
					if(path != null && path.length > 0) {
						className = applyClassNamingPolicy(path);
						break;
					}
				}
			}
			return className+ "API";
		}
		
		private function getArgumentsForTestCaseExecution(methodName: String, apiObject: Object, queryAndPathParameters: Object,
			postData: String, className: String, resourcePath: String): Array{
		
			var result: Array;
			//get the xml data for the type
			var classAsXML: XML = describeType(apiObject);
			//get the parameters for the method
			
			var argNamesArray: Array = [];
			var argTypesArray: Array = [];
			
			var list: XMLList = classAsXML.method;
			var methodXml: XML;
			var paramDefn: XML
			var currentMethodName: String;
			var methodParams: XMLList;
			var methodArgumentNames: XMLList;
			var argumentNames: String;
			
			for each (methodXml in list) {
				//get the names and types for the parameters
				currentMethodName = methodXml.@name.toString();
				if(methodName == currentMethodName){
					methodParams = methodXml.parameter;
					for each(paramDefn in methodParams){
						argTypesArray.push(paramDefn.@type.toString());		
					}					
					methodArgumentNames = methodXml.metadata.(@name == "MethodArgumentNames");
					if(methodArgumentNames.length() > 0){
						argumentNames = methodArgumentNames.arg[0].@value.toString();
						argNamesArray = argNamesArray.concat(argumentNames.split(","));
					}					
					break;
				}
			}	
			
			if(argNamesArray != null && argNamesArray.length > 0){
				result = [];
				//get the value of the input type parameter
				var inputClassName: String = getInputObjectName(className, resourcePath);
				var argName: String;
				var argType: String;
				var argumentValue: Object;
				for (var i: Number=0 ; i< argNamesArray.length ; i++){ 
					argName = StringUtil.trim( argNamesArray[i].toString() );
					argType = argTypesArray[i].toString();
					//if the parameter type is of collated input type
					if(argType == inputClassName){
						//create an object of type input model and populate it
						argumentValue = populateInputModelObject(argTypesArray[i], queryAndPathParameters);
					}
					//if it is a primitive type then
					else if( isPrimitiveType(argType) ){					
						//get the value from the queryAndPathParameters
						argumentValue =  queryAndPathParameters[argName] ;
					}
					//if it is a POST param
					else if( argName == "postData"){
						//convert from JSON to object ? of type ?
						if(postData.charAt(0) == "\"" && postData.charAt(postData.length - 1) == "\""){
							postData = postData.slice(1, postData.length);
						}
						argumentValue = JSON.decode( postData.toString() );
						argumentValue = mapToFlexObjects(argumentValue, argType);
					}
					else if(true){
					//???some times input can be list of primitives for supporting multivalued values. however test case sends the input as comma separated values
						//???so we need to convert comma separated string into JSON list format
						argumentValue = queryAndPathParameters[argName].toString().split(",");
					}					
					result.push(argumentValue);
				}
			}
			
			return result;	
		}
		
		/**
		 * Converts an instance of type 'Object' to a type of 'argType' 
		 */ 
		private function mapToFlexObjects(obj:Object, argType: String):Object {
			var fullClassName: String = argType.replace("::",".");			
			var classRef:Class;
			try{
				classRef = getDefinitionByName(fullClassName) as Class;
			}
			catch(error: ReferenceError){
				var classFailure: Response = new Response(false, null, "Api Class not found");
				writeToFile( JSON.encode(classFailure) );	
			}
			var returnObject : * = new classRef();
			
			var propertyMap:XML = describeType(returnObject);
			var propertyTypeClass:Class;
			
			for each (var property:XML in propertyMap.variable) {
				if ((obj as Object).hasOwnProperty(property.@name)) {
					propertyTypeClass = getDefinitionByName(property.@type) as Class;
					if (obj[property.@name] is (propertyTypeClass)) {
						returnObject[property.@name] = obj[property.@name];
					}
					
					if(property.@type == "Date"){
						var dateValue:Date = new Date();
						dateValue.setTime( Date.parse( obj[property.@name] ) );
						returnObject[property.@name] = dateValue;	
					}
					
					if( !isPrimitiveType( property.@type )){
						try{
							var complexTypeObject: Object = mapToFlexObjects( obj[property.@name], property.@type );
							returnObject[property.@name] = complexTypeObject;
						}
						catch(error: Error){
							var mapToFlexFailure: Response = new Response(false, null, "Post data object could not be created");
							writeToFile( JSON.encode(mapToFlexFailure) );								
						}
					}
				}
			}
			
			return returnObject;
		}		
		
		private function isPrimitiveType(type: String): Boolean {
			if(type == "String" || type == "int" || type == "integer" || type == "double" ||
				type == "boolean" || type == "float" || type == "long" || type == "Number" ){
				return true;
			}
			return false;
		}
		
		/**
		 * Converts the first character of the input into upper case .
		 * Example: If the input is word, the return value will be Word
		 * @param input
		 * @return
		 */
		private function applyClassNamingPolicy(input: String): String {
			if(input != null && input.length > 0) {
				return input.substring(0,1).toUpperCase() + input.substring(1);
			}else{
				throw new Error("Error converting input to first letter caps becuase of null or empty input");
			}
		}
				
		private function getInputObjectName(serviceName: String, resourcePath: String): String {			
			//Since service name has API at the end remove that format he name
			var inputobjectName: String = serviceName.substring(0, serviceName.length - 3);
			
			var pathElements: Array = resourcePath.split("/");
			var urlPath: String = "";
			if(pathElements != null){
				for each(var pathElement: String in pathElements){
					if(pathElement != null && pathElement.length > 0) {
						var position: int = pathElement.indexOf("{");
						if(position < 0) {
							inputobjectName = inputobjectName + applyClassNamingPolicy( pathElement ) + "Input";
						}
					}
				}
			}
			return inputobjectName;
		}
		
		/**
		 * Populates the swagger input model object.
		 *
		 * Input model is created when number of inputs to a method exceed certain limit.
		 * @param inputDefinitions
		 * @return
		 */
		private function populateInputModelObject(swaggerInputClassName: String, inputDefinitions: Object): Object {
			var inputModelObjectClass: Class = getDefinitionByName(swaggerInputClassName) as Class;
			var inputObject: Object = new inputModelObjectClass();
			
			for(var attributeName: String in inputDefinitions){
				if(inputObject.hasOwnProperty(attributeName)){
					inputObject[attributeName] = inputDefinitions[attributeName];
				}	
			}
			
			return inputObject;
		} 
		
		public function applicationExit():void {
			var exitingEvent:Event = new Event(Event.EXITING, false, true);
			NativeApplication.nativeApplication.dispatchEvent(exitingEvent);
			if (!exitingEvent.isDefaultPrevented()) {
				NativeApplication.nativeApplication.exit();
			}
		}		
		
		
//		/**
//		 * Gets the list of input query and path parameters and post data vlues and covenrt them to arguments that
//		 * can be used for calling the method. This logic will be different in each driver language depends on how method
//		 * input arguments are created.
//		 */
//		private function populateArgumentsForTestCaseExecution(methodToExecute: Function, queryAndPathParameters: Object, 
//															   postData: String, serviceName: String, resourcePath: String): Array {
//				MethodArgumentNames argNames = methodToExecute.getAnnotation(MethodArgumentNames.class);
//				String[] argNamesArray = null;
//				if(argNames != null && argNames.value().length() > 0) {
//					argNamesArray = argNames.value().split(",");
//				}
//				Class[] argTypesArray = methodToExecute.getParameterTypes();
//				Object output = null;
//				String inputClassName = namingPolicyProvider.getInputObjectName(serviceName, resourcePath);
//				
//				if(argNamesArray != null && argNamesArray.length > 0){
//					Object[] arguments = new Object[argNamesArray.length];
//					
//					for(int i=0; i < argNamesArray.length; i++){
//						Object argument = null;
//						//if the method takes input model instead of individual arguments, convert individual arguments into input model object
//						if(argTypesArray[i].getName().equalsIgnoreCase(inputClassName)){
//							argument = populateInputModelObject(argTypesArray[i], queryAndPathParameters);
//						}else if(datatypeMppingProvider.isPrimitiveType(argTypesArray[i].getName())){
//							argument = queryAndPathParameters.get(argNamesArray[i].trim());
//						}else if (argNamesArray[i].trim().equals(APITestRunner.POST_PARAM_NAME)){
//							argument = APITestRunner.convertJSONStringToObject(postData, argTypesArray[i]);
//						}else{
//							//some times input can be list of primitives for supporting multivalued values. however test case sends the input as comma separated values
//							//so we need to convert comma separated string into JSON list format
//							if(List.class.isAssignableFrom(argTypesArray[i]) && !queryAndPathParameters.get(argNamesArray[i].trim()).startsWith("[")){
//								String listInput= "[";
//								int x = 0;
//								String[] values = queryAndPathParameters.get(argNamesArray[i].trim()).split(",");
//								for(String value : values){
//									if(x > 0){listInput = listInput + ",";}
//									listInput = listInput + "\""+ value + "\"";
//									x++;
//								}
//								listInput = listInput + "]";
//								argument = APITestRunner.convertJSONStringToObject(listInput, argTypesArray[i]);
//							}else{
//								argument = APITestRunner.convertJSONStringToObject(queryAndPathParameters.get(argNamesArray[i].trim()), argTypesArray[i]);
//							}
//						}
//						arguments[i] = argument;
//					}
//					return arguments;
//				}
//				return null;
//			}
									
		
		
				
	}
}