package org.openapitools.api;

@.annotation.Generated(value = "org.openapitools.codegen.languages.JavaMSF4JServerCodegen")
public class NotFoundException extends ApiException {
    private int code;
    public NotFoundException (int code, String msg) {
        super(code, msg);
        this.code = code;
    }
}
