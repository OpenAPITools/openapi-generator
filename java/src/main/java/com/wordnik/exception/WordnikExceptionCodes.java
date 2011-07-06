package com.wordnik.exception;

/**
 * Lists all the possible exception codes 
 * @author ramesh
 *
 */
public interface WordnikExceptionCodes {

    /**
     * System exception.
     */
    public static final int SYSTEM_EXCEPTION = 0;

	/**
	 * With Arguments as current key. 
	 */
	public static final int API_KEY_NOT_VALID = 1000;
	/**
	 * With arguments as current token value
	 */
	public static final int AUTH_TOKEN_NOT_VALID = 1001;
	/**
	 * With arguments as input JSON and output class anme
	 */
	public static final int ERROR_CONVERTING_JSON_TO_JAVA = 1002;
	/**
	 * With arguments as JAVA class name
	 */
	public static final int ERROR_CONVERTING_JAVA_TO_JSON = 1003;
    
	public static final int ERROR_FROM_WEBSERVICE_CALL = 1004;
	/**
	 * With arguments as current API server name
	 */
	public static final int API_SERVER_NOT_VALID = 1005;
	
}
