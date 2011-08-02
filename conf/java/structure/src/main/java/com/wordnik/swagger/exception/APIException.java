package com.wordnik.swagger.exception;

import com.sun.jersey.api.client.ClientResponse;
import org.codehaus.jackson.annotate.JsonAutoDetect;
import org.codehaus.jackson.annotate.JsonCreator;
import org.codehaus.jackson.annotate.JsonMethod;
import org.codehaus.jackson.annotate.JsonProperty;

/**
 * Exception that is thrown if there are any issues in invoking Wordnik API.
 * 
 * Each exception carries a code and message. Code can be either HTTP error response code {@link com.sun.jersey.api.client.ClientResponse.Status}
 * or The list of possible Wordnik exception code that are listed in the interface {@link APIExceptionCodes}.
 * User: ramesh
 * Date: 3/31/11
 * Time: 9:27 AM
 */
public class APIException extends Exception {

    private String message;

    private int code;

    private String[] args;

    @JsonCreator
    public APIException() {
    }

    public APIException(String message) {
        super(message);
    }

    public APIException(int code) {
        this.code = code;
    }

    public APIException(int code, String message, Throwable t) {
    	super(message, t);
    	this.message = message;
        this.code = code;
    }

    public APIException(int code, String[] args, String message, Throwable t) {
    	super(message, t);
    	this.message = message;
        this.code = code;
        this.args = args;
    }

    public APIException(int code, String message) {
    	super(message);
    	this.message = message;
        this.code = code;
    }

    public APIException(int code, String[] args, String message) {
    	super(message);
    	this.message = message;
        this.code = code;
        this.args = args;
    }
    
    public APIException(int code, String[] args) {
        this.code = code;
        this.args = args;
    }
    
    public String getMessage() {
        return message;
    }

    public void setMessage(String message) {
        this.message = message;
    }

    public int getCode() {
        return code;
    }

    public void setCode(int code) {
        this.code = code;
    }
}
