package io.swagger.exception
{
	public class ApiErrorCodes
	{
		/**
		 * System exception.
		 */
		public static const SYSTEM_EXCEPTION: Number = 0;
		
		/**
		 * With Arguments as current key. 
		 */
		public static const API_KEY_NOT_VALID: Number = 1000;
		/**
		 * With arguments as current token value
		 */
		public static const AUTH_TOKEN_NOT_VALID: Number = 1001;
		/**
		 * With arguments as input JSON and output class anme
		 */
		public static const ERROR_CONVERTING_JSON_TO_JAVA: Number = 1002;
		/**
		 * With arguments as JAVA class name
		 */
		public static const ERROR_CONVERTING_JAVA_TO_JSON: Number = 1003;
		
		public static const ERROR_FROM_WEBSERVICE_CALL: Number = 1004;
		/**
		 * With arguments as current API server name
		 */
		public static const API_SERVER_NOT_VALID: Number = 1005;
		
	}
}