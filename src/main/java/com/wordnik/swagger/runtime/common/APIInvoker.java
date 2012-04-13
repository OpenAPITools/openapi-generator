/**
 *  Copyright 2011 Wordnik, Inc.
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package com.wordnik.swagger.runtime.common;

import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.lang.String;
import java.net.URLEncoder;
import java.util.Map;
import java.util.List;
import java.util.HashMap;
import java.util.logging.Logger;

import javax.ws.rs.core.MultivaluedMap;

import com.wordnik.swagger.runtime.exception.APIException;
import com.wordnik.swagger.runtime.exception.APIExceptionCodes;
import org.codehaus.jackson.map.DeserializationConfig;
import org.codehaus.jackson.map.ObjectMapper;
import org.codehaus.jackson.map.DeserializationConfig.Feature;
import org.codehaus.jackson.map.SerializationConfig;
import org.codehaus.jackson.type.TypeReference;

import com.sun.jersey.api.client.Client;
import com.sun.jersey.api.client.ClientResponse;
import com.sun.jersey.api.client.WebResource;
import com.sun.jersey.api.client.WebResource.Builder;
import com.sun.jersey.api.client.filter.LoggingFilter;


/**
 * Provides method to initialize the api server settings and also handles the logic related to invoking the API server
 * along with serealizing and deserializing input and output responses.
 *
 * This is also a Base class for all API classes
 *
 * @author ramesh
 *
 */
public class APIInvoker {

	private String apiServer = "http://api.wordnik.com/v4";
	private SecurityHandler securityHandler = null;
	private static boolean loggingEnabled;
	private static Logger logger = null;
	private static APIInvoker apiInvoker = null;
    private static Client apiClient = null;

	protected static String POST = "POST";
	protected static String GET = "GET";
	protected static String PUT = "PUT";
	protected static String DELETE = "DELETE";
	public static ObjectMapper mapper = new ObjectMapper();
	static{
        mapper.configure(DeserializationConfig.Feature.FAIL_ON_UNKNOWN_PROPERTIES, false);
        mapper.configure(SerializationConfig.Feature.WRITE_NULL_PROPERTIES, false);
        mapper.configure(SerializationConfig.Feature.WRITE_DATES_AS_TIMESTAMPS, false);
        mapper.configure(SerializationConfig.Feature.FAIL_ON_EMPTY_BEANS, false);
        apiClient = Client.create();
	}

	/**
	 * Initializes the API communication with required inputs. 
	 * @param securityHandler security handler responsible for populating necessary security invocation while making API server calls
	 * @param apiServer Sets the URL for the API server. It is defaulted to the server 
	 * 					used while building the driver. This value should be provided while testing the APIs against 
	 * 					test servers or if there is any changes in production server URLs.
	 * @param enableLogging This will enable the logging using Jersey logging filter. Refer the following documentation 
	 * 						for more details. {@link com.sun.jersey.api.client.filter.LoggingFilter}. Default output is sent to system.out.
	 * 						Create a logger ({@link java.util.logging.Logger} class and set using setLogger method.
	 */
	public static APIInvoker initialize(SecurityHandler securityHandler, String apiServer, boolean enableLogging) {
        APIInvoker invoker = new APIInvoker();
		invoker.setSecurityHandler(securityHandler);
		if(apiServer != null && apiServer.length() > 0) {
			if(apiServer.substring(apiServer.length()-1).equals("/")){
				apiServer = apiServer.substring(0, apiServer.length()-1);
			}
			invoker.setApiServer(apiServer);
		}
		invoker.setLoggingEnable(enableLogging);
        //initialize the logger if needed
        if(loggingEnabled && apiInvoker == null) {
        	if(logger == null) {
        		apiClient.addFilter(new LoggingFilter());
        	}else{
        		apiClient.addFilter(new LoggingFilter(logger));
        	}
        }
        apiInvoker = invoker;
        return invoker;
	}

    /**
     * Returns lst initialized API invoker
     * @return
     */
    public static APIInvoker getApiInvoker(){
        return apiInvoker;
    }

	/**
	 * Set the logger instance used for Jersey logging. 
	 * @param aLogger
	 */
	public void setLogger(Logger aLogger) {
		logger = aLogger; 
	}
	
	/**
	 * Gets the API key used for server communication. 
	 * This value is set using initialize method. 
	 * @return
	 */
	public SecurityHandler setSecurityHandler() {
		return securityHandler;
	}

	private void setSecurityHandler(SecurityHandler aSecurityHandler) {
		securityHandler = aSecurityHandler;
	}

	/**
	 * Sets the URL for the API server. It is defaulted to the server used while building the driver.
	 * @return 
	 */
	private String getApiServer() {
		return apiServer;
	}

	public void setApiServer(String server) {
		apiServer = server;
	}
	
	
	
	/**
	 * Invokes the API and returns the response as json string.
     *
	 * This is an internal method called by individual APIs for communication. It sets the required security information
     * based ons ecuroty handler
	 *
	 * @param resourceURL - URL for the rest resource
	 * @param method - Method we should use for communicating to the back end. 
	 * @param postData - if the method is POST, provide the object that should be sent as part of post request.
	 * @return JSON response of the API call. 
	 * @throws com.wordnik.swagger.runtime.exception.APIException if the call to API server fails.
	 */
	public String invokeAPI(String resourceURL, String method, Map<String,
            String> queryParams, Object postData, Map<String, String> headerParams) throws APIException {


        //check for app server values
        if(getApiServer() == null || getApiServer().length() == 0) {
        	String[] args = {getApiServer()};
        	throw new APIException(APIExceptionCodes.API_SERVER_NOT_VALID, args);
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
        Map<String, String> headerMap = new HashMap<String, String>();
        if(securityHandler != null){
            securityHandler.populateSecurityInfo(resourceURL, headerMap);
        }
        WebResource aResource = apiClient.resource(resourceURL);


        //set the required HTTP headers
        Builder builder = aResource.type("application/json");
        for(String key : headerMap.keySet()){
            builder.header(key, headerMap.get(key));
        }
        if(headerParams != null){
            for(String key : headerParams.keySet()){
                builder.header(key, headerParams.get(key));
            }
        }

        ClientResponse clientResponse = null;
        if(method.equals(GET)) {
        	clientResponse =  builder.get(ClientResponse.class);
        }else if (method.equals(POST)) {
        	clientResponse =  builder.post(ClientResponse.class, serialize(postData));
        }else if (method.equals(PUT)) {
        	clientResponse =  builder.put(ClientResponse.class, serialize(postData));
        }else if (method.equals(DELETE)) {
        	clientResponse =  builder.delete(ClientResponse.class);
        }
        
        //process the response
        if(clientResponse.getClientResponseStatus() == ClientResponse.Status.OK) {
	        String response = clientResponse.getEntity(String.class);
			return response;
        }else{
        	int responseCode = clientResponse.getClientResponseStatus().getStatusCode() ;
        	throw new APIException(responseCode, clientResponse.getEntity(String.class));
        }
	}
	
	/**
	 * De-serialize the object from String to object of type input class name.
	 * @param response
	 * @param inputClassName
	 * @return
	 */
	public static Object deserialize(String response, Class inputClassName) throws APIException {
        try {
            if(inputClassName.isAssignableFrom(String.class)){
                return response;
            } else if (inputClassName.isAssignableFrom(Integer.class)){
                return new Integer(response);
            } else if (inputClassName.isAssignableFrom(Boolean.class)){
                return new Boolean(response);
            } else if (inputClassName.isAssignableFrom(Long.class)){
                return new Long(response);
            } else if (inputClassName.isAssignableFrom(Double.class)){
                return new Double(response);
            } else{
                Object responseObject = mapper.readValue(response, inputClassName);
                return responseObject;
            }
        } catch (IOException ioe) {
        	String[] args = new String[]{response, inputClassName.toString()};
            throw new APIException(APIExceptionCodes.ERROR_CONVERTING_JSON_TO_JAVA, args, "Error in coversting response json value to java object : " + ioe.getMessage(), ioe);
        }
	}


	/**
	 * serialize the object from String to input object. 
	 * @param input
	 * @return
	 */
	public static String serialize(Object input) throws APIException {
        try {
        	if(input != null) {
		    if (input instanceof String) {
			return (String)input;
		    } else {
			return mapper.writeValueAsString(input);
		    }
        	}else{
        		return "{}";
        	}
        } catch (IOException ioe) {
            throw new APIException(APIExceptionCodes.ERROR_CONVERTING_JAVA_TO_JSON, "Error in coverting input java to json : " + ioe.getMessage(), ioe);
        }
	}


    /**
     * Overloaded method for returning the path value
     * For a string value an empty value is returned if the value is null
     * @param value
     * @return
     */
    public static String toPathValue(String value) {
        value = (value == null) ? "" : value;
        return encode(value);
    }

    /**
     * Overloaded method for returning a path value
     * For a list of objects a comma separated string is returned
     * @param objects
     * @return
     */
    public static String toPathValue(List objects) {
        StringBuilder out = new StringBuilder();
        String output = "";
        for(Object o: objects){
            out.append(o.toString());
            out.append(",");
        }
        if(out.indexOf(",") != -1) {
            output = out.substring(0, out.lastIndexOf(",") );
        }
        return encode(output);
    }

    private static String encode(String value){
        try{
            return URLEncoder.encode(value, "utf-8").replaceAll("\\+", "%20");
        }catch(UnsupportedEncodingException uee){
            throw new RuntimeException(uee.getMessage());
        }
    }

    public boolean isLoggingEnable() {
        return loggingEnabled;
    }

    public void setLoggingEnable(boolean enabled) {
        loggingEnabled = enabled;
    }

}
