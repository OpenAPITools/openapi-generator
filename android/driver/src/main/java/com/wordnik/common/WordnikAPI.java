package com.wordnik.common;

import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.net.URISyntaxException;
import java.net.URLEncoder;
import java.util.Map;
import java.util.logging.Logger;


import org.apache.http.HttpException;
import org.apache.http.HttpResponse;
import org.apache.http.HttpStatus;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.*;
import org.apache.http.entity.StringEntity;
import org.apache.http.impl.client.DefaultHttpClient;
import org.apache.http.protocol.HTTP;
import org.apache.http.util.EntityUtils;
import org.codehaus.jackson.map.ObjectMapper;
import org.codehaus.jackson.map.DeserializationConfig.Feature;

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

    public static final String WORDNIK_HEADER_NAME = "api_key";
	
	protected static String POST = "POST";
	protected static String GET = "GET";
	protected static String PUT = "PUT";
	protected static String DELETE = "DELETE";
	protected static ObjectMapper mapper = new ObjectMapper();
	static{
        mapper.getDeserializationConfig().set(Feature.FAIL_ON_UNKNOWN_PROPERTIES, false);
	}

	/**
	 * Initializes the API communication with required inputs. 
	 * @param apiKey provide the key provided as part of registration
	 * @param apiServer Sets the URL for the API server. It is defaulted to the server 
	 * 					used while building the driver. This value should be provided while testing the APIs against 
	 * 					test servers or if there is any changes in production server URLs.
	 * @param enableLogging This will enable the logging using Jersey logging filter. Refer the following documentation 
	 * 						for more details. {@link //LoggingFilter}. Default output is sent to system.out.
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
	private static String getApiKey() {
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
     * @throws com.wordnik.exception.WordnikAPIException if the call to API server fails.
     */
    protected static String invokeAPI(String authToken, String resourceURL, String method,
                                             Map<String, String> queryParams, Object postObject)
            throws WordnikAPIException {
        String responseString = null;
        try {
            //check for app key and server values
            if(getApiKey() == null || getApiKey().length() == 0) {
                String[] args = {getApiKey()};
                throw new WordnikAPIException(WordnikExceptionCodes.API_KEY_NOT_VALID, args);
            }
            if(getApiServer() == null || getApiServer().length() == 0) {
                String[] args = {getApiServer()};
                throw new WordnikAPIException(WordnikExceptionCodes.API_SERVER_NOT_VALID, args);
            }

            String url = getApiServer() + resourceURL + getQueryParams(queryParams);

            HttpUriRequest request = null;
            if(method.equals(GET)) {
                request = new HttpGet(url);
            } else if (method.equals(POST)) {
                request = new HttpPost(url);
                if(postObject != null) {
                    StringEntity entity = new StringEntity(serialize(postObject));
                    ((HttpPost) request).setEntity(entity);
                }
            } else if (method.equals(PUT)) {
                request = new HttpPut(url);
                if(postObject != null) {
                    StringEntity entity = new StringEntity(serialize(postObject));
                    ((HttpPut) request).setEntity(entity);
                }
            } else if (method.equals(DELETE)) {
                request = new HttpDelete(url);
            } else {
                throw new WordnikAPIException(WordnikExceptionCodes.ERROR_FROM_WEBSERVICE_CALL, "Http method not valid - " + method);
            }

            request.setHeader("Content-type", "application/json");
            request.addHeader(WORDNIK_HEADER_NAME, getApiKey());
            if(authToken != null){
                request.addHeader("auth_token", authToken);
            }

            final HttpClient hc = new DefaultHttpClient();
            final HttpResponse resp = hc.execute(request);
            final int status = resp.getStatusLine().getStatusCode();
            if (status == HttpStatus.SC_OK) {
                if (resp.getStatusLine().getStatusCode() == HttpStatus.SC_OK) {
                    responseString = EntityUtils.toString(resp.getEntity(), HTTP.UTF_8);
                } else {
                    throw new WordnikAPIException(resp.getStatusLine().getStatusCode(), EntityUtils.toString(resp.getEntity(), HTTP.UTF_8));
                }
            } else {
                throw new WordnikAPIException(status, EntityUtils.toString(resp.getEntity(), HTTP.UTF_8));
            }
        } catch (Exception e) {
            throw new WordnikAPIException(WordnikExceptionCodes.ERROR_FROM_WEBSERVICE_CALL, e.getMessage(), e);
        }

        return responseString;
    }

    private static String getQueryParams(Map<String, String> queryParams) {
        if(queryParams == null || queryParams.size() == 0) {
            return "";
        }
        StringBuilder buf = new StringBuilder();
        buf.append("?");

        for(String key : queryParams.keySet()) {
            String value = queryParams.get(key);

            try {
                value = URLEncoder.encode(value, "UTF-8");
            } catch (UnsupportedEncodingException e) {
                e.printStackTrace();
            }

            if(buf.length() > 1) {
                buf.append("&");
            }

            buf.append(key).append("=").append(value);
        }

        return buf.toString();
    }

	/**
	 * De-serialize the object from String to input object. 
	 * @param response
	 * @param inputClassName
	 * @return
	 */
	public static Object deserialize(String response, Class inputClassName) throws WordnikAPIException {
        try {
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
