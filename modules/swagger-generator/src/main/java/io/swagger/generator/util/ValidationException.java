package io.swagger.generator.util;

public class ValidationException extends Exception {
    private static final long serialVersionUID = 6861195361018260380L;
    private int code;
    private String msg;

    public ValidationException(String msg) {
        super(msg);
    }

    public int getCode() {
        return code;
    }

    public void setCode(int code) {
        this.code = code;
    }

    @Override
    public String getMessage() {
        return msg;
    }

    public void setMessage(String msg) {
        this.msg = msg;
    }
}