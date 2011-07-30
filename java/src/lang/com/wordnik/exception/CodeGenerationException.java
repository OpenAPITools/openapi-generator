package com.wordnik.exception;

/**
 * Exception raised while generating code for java driver. 
 * User: ramesh
 * Date: 3/31/11
 * Time: 9:29 AM
 */
public class CodeGenerationException extends RuntimeException {

    private String message;

    public CodeGenerationException(String message){
        this.message = message;
    }

    public String getMessage() {
        return message;
    }

    public void setMessage(String message) {
        this.message = message;
    }
}

