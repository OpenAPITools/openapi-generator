package org.openapitools.api;

@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJerseyServerCodegen")
public class ApiException extends Exception{
    private int code;
    public ApiException (int code, String msg) {
        super(msg);
        this.code = code;
    }
}
