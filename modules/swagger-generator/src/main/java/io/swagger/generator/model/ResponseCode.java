package io.swagger.generator.model;

public class ResponseCode {
    private String code;
    private String link;

    public ResponseCode() {
    }

    public ResponseCode(String code, String link) {
        setCode(code);
        setLink(link);
    }

    public String getCode() {
        return code;
    }

    public void setCode(String code) {
        this.code = code;
    }

    public String getLink() {
        return link;
    }

    public void setLink(String link) {
        this.link = link;
    }
}