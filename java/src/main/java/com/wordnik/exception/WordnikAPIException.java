package com.wordnik.exception;

import java.util.List;

import com.sun.jersey.api.client.ClientResponse;
import org.codehaus.jackson.annotate.JsonAutoDetect;
import org.codehaus.jackson.annotate.JsonCreator;
import org.codehaus.jackson.annotate.JsonMethod;
import org.codehaus.jackson.annotate.JsonProperty;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

/**
 * Exception that is thrown if there are any issues in invoking Wordnik API.
 * 
 * Each exception carries a code and message. Code can be either HTTP error response code {@link ClientResponse.Status}
 * or The list of possible Wordnik exception code that are listed in the interface {@link WordnikExceptionCodes}. 
 * User: ramesh
 * Date: 3/31/11
 * Time: 9:27 AM
 */
public class WordnikAPIException extends Exception {

    private String message;

    private int code;

    private String[] args;

    @JsonCreator
    public WordnikAPIException() {
    }

    public WordnikAPIException(String message) {
        super(message);
    }

    public WordnikAPIException(int code) {
        this.code = code;
    }

    public WordnikAPIException(int code, String message, Throwable t) {
    	super(message, t);
    	this.message = message;
        this.code = code;
    }

    public WordnikAPIException(int code, String[] args, String message, Throwable t) {
    	super(message, t);
    	this.message = message;
        this.code = code;
        this.args = args;
    }

    public WordnikAPIException(int code, String message) {
    	super(message);
    	this.message = message;
        this.code = code;
    }

    public WordnikAPIException(int code, String[] args, String message) {
    	super(message);
    	this.message = message;
        this.code = code;
        this.args = args;
    }
    
    public WordnikAPIException(int code, String[] args) {
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
