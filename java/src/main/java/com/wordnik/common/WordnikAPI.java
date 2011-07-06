package com.wordnik.common;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.logging.Logger;

import javax.ws.rs.core.MultivaluedMap;

import org.codehaus.jackson.map.ObjectMapper;
import org.codehaus.jackson.map.DeserializationConfig.Feature;
import org.codehaus.jackson.map.SerializationConfig;
import org.codehaus.jackson.type.TypeReference;

import com.sun.jersey.api.client.Client;
import com.sun.jersey.api.client.ClientResponse;
import com.sun.jersey.api.client.WebResource;
import com.sun.jersey.api.client.WebResource.Builder;
import com.sun.jersey.api.client.filter.LoggingFilter;
import com.wordnik.exception.WordnikAPIException;
import com.wordnik.exception.WordnikExceptionCodes;


/**
 * Provides way to initialize the communication with Wordnik API server. 
 * This is also a Base class for all API classes 
 * @author ramesh
 *
 */
public class WordnikAPI {

	private static String apiServer = "http://api.wordnik.com/v4";
	private static String apiKey = "";
	private static boolean loggingEnabled;
	private static Logger logger = null;
	
	protected static String POST = "POST";
	protected static String GET = "GET";
	protected static String PUT = "PUT";
	protected static String DELETE = "DELETE";
	protected static ObjectMapper mapper = new ObjectMapper();
	static{
        mapper.getDeserializationConfig().set(Feature.FAIL_ON_UNKNOWN_PROPERTIES, false);
        mapper.getSerializationConfig().set(SerializationConfig.Feature.FAIL_ON_EMPTY_BEANS, false);
        mapper.configure(SerializationConfig.Feature.WRITE_NULL_PROPERTIES, false);
        mapper.configure(SerializationConfig.Feature.WRITE_DATES_AS_TIMESTAMPS, false);
	}

	/**
	 * Initializes the API communication with required inputs. 
	 * @param apiKey provide the key provided as part of registration
	 * @param apiServer Sets the URL for the API server. It is defaulted to the server 
	 * 					used while building the driver. This value should be provided while testing the APIs against 
	 * 					test servers or if there is any changes in production server URLs.
	 * @param enableLogging This will enable the logging using Jersey logging filter. Refer the following documentation 
	 * 						for more details. {@link LoggingFilter}. Default output is sent to system.out. 
	 * 						Create a logger ({@link Logger} class and set using setLogger method.
	 */
	public static void initialize(String apiKey, String apiServer, boolean enableLogging) {
		setApiKey(apiKey);
		if(apiServer != null && apiServer.length() > 0) {
			if(apiServer.substring(apiServer.length()-1).equals("/")){
				apiServer = apiServer.substring(0, apiServer.length()-1);
			}
			setApiServer(apiServer);
		}
		loggingEnabled = enableLogging;
	}
	
	/**
	 * Set the logger instance used for Jersey logging. 
	 * @param aLogger
	 */
	public static void setLogger(Logger aLogger) {
		logger = aLogger; 
	}
	
	/**
	 * Gets the API key used for server communication. 
	 * This value is set using initialize method. 
	 * @return
	 */
	public static String getApiKey() {
		return apiKey;
	}

	private static void setApiKey(String apiKey) {
		WordnikAPI.apiKey = apiKey;
	}

	/**
	 * Sets the URL for the API server. It is defaulted to the server used while building the driver.
	 * @return 
	 */
	private static String getApiServer() {
		return apiServer;
	}

	private static void setApiServer(String server) {
		WordnikAPI.apiServer = server;
	}
	
	
	
	/**
	 * Invokes the API and returns the response as json string.
	 * This is an internal method called by individual APIs for communication. It sets the required HTTP headers
	 * based on API key and auth token.   
	 * @param authToken - token that is received as part of authentication call. This is only needed for the calls that are secure.
	 * @param resourceURL - URL for the rest resource
	 * @param method - Method we should use for communicating to the back end. 
	 * @param postObject - if the method is POST, provide the object that should be sent as part of post request.
	 * @return JSON response of the API call. 
	 * @throws WordnikAPIException if the call to API server fails. 
	 */
	protected static String invokeAPI(String authToken, String resourceURL, String method, Map<String,
            String> queryParams, Object postObject) throws WordnikAPIException {


        Client apiClient = Client.create();
        
        //check for app key and server values
        if(getApiKey() == null || getApiKey().length() == 0) {
        	String[] args = {getApiKey()};
        	throw new WordnikAPIException(WordnikExceptionCodes.API_KEY_NOT_VALID, args);
        }
        if(getApiServer() == null || getApiServer().length() == 0) {
        	String[] args = {getApiServer()};
        	throw new WordnikAPIException(WordnikExceptionCodes.API_SERVER_NOT_VALID, args);
        }
        //initialize the logger if needed
        if(loggingEnabled) {
        	if(logger == null) {
        		apiClient.addFilter(new LoggingFilter());
        	}else{
        		apiClient.addFilter(new LoggingFilter(logger));
        	}
        }
        
        //make the communication
		resourceURL = getApiServer() + resourceURL;
		if(queryParams.keySet().size() > 0){
			int i=0;
			for(String paramName : queryParams.keySet()){
				String symbol = "&";
				if(i==0){
					symbol = "?";
				}
				resourceURL = resourceURL + symbol + paramName + "=" + queryParams.get(paramName);
				i++;
			}
		}
        WebResource aResource = apiClient.resource(resourceURL);
 //       aResource.queryParams(queryParams);
        
        //set the required HTTP headers
        Builder builder = aResource.type("application/json");
        builder.header("api_key", getApiKey());
        if(authToken != null){
        	builder.header("auth_token", authToken);
        }
        ClientResponse clientResponse = null;
        if(method.equals(GET)) {
        	clientResponse =  builder.get(ClientResponse.class);
        }else if (method.equals(POST)) {
        	clientResponse =  builder.post(ClientResponse.class, serialize(postObject));
        }else if (method.equals(PUT)) {
        	clientResponse =  builder.put(ClientResponse.class, serialize(postObject));
        }else if (method.equals(DELETE)) {
        	clientResponse =  builder.delete(ClientResponse.class);
        }
        
        //process the response
        if(clientResponse.getClientResponseStatus() == ClientResponse.Status.OK) {
	        String response = clientResponse.getEntity(String.class);
			return response;
        }else{
        	int responseCode = clientResponse.getClientResponseStatus().getStatusCode() ;
        	throw new WordnikAPIException(responseCode, clientResponse.getEntity(String.class));
        }
	}
	
	/**
	 * De-serialize the object from String to object of type input class name.
	 * @param response
	 * @param inputClassName
	 * @return
	 */
	public static Object deserialize(String response, Class inputClassName) throws WordnikAPIException {
        try {
            System.out.println("Input :::::" + response);
            Object responseObject = mapper.readValue(response, inputClassName);
            return responseObject;
        } catch (IOException ioe) {
        	String[] args = new String[]{response, inputClassName.toString()};
            throw new WordnikAPIException(WordnikExceptionCodes.ERROR_CONVERTING_JSON_TO_JAVA, args, "Error in coversting response json value to java object : " + ioe.getMessage(), ioe);
        }
	}


	/**
	 * serialize the object from String to input object. 
	 * @param input
	 * @return
	 */
	public static String serialize(Object input) throws WordnikAPIException {
        try {
        	if(input != null) {
	            return mapper.writeValueAsString(input);
        	}else{
        		return "";
        	}
        } catch (IOException ioe) {
            throw new WordnikAPIException(WordnikExceptionCodes.ERROR_CONVERTING_JAVA_TO_JSON, "Error in coverting input java to json : " + ioe.getMessage(), ioe);
        }
	}	
}
