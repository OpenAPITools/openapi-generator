package io.swagger.generator.util;

import java.util.List;

public class ValidationException extends Exception {
    private int code;
    private String msg;
    private List<ValidationMessage> errors;

    public ValidationException(String msg) {
        super(msg);
    }

    public int getCode() {
        return code;
    }

    public void setCode(int code) {
        this.code = code;
    }

    public String getMessage() {
        return msg;
    }

    public void setMessage(String msg) {
        this.msg = msg;
    }
}