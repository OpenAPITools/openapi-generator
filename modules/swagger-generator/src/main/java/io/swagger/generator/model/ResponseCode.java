package io.swagger.generator.model;

import io.swagger.annotations.ApiModelProperty;

public class ResponseCode {
    private String code;
    private String link;

    public ResponseCode() {
    }

    public ResponseCode(String code, String link) {
        setCode(code);
        setLink(link);
    }

    @ApiModelProperty(value = "File download code", example = "d40029be-eda6-4d62-b1ef-d05e2e91a72a")
    public String getCode() {
        return code;
    }

    public void setCode(String code) {
        this.code = code;
    }

    @ApiModelProperty(value = "URL for fetching the generated client", example = "http://generator.swagger.io:80/api/gen/download/d40029be-eda6-4d62-b1ef-d05e2e91a72a")
    public String getLink() {
        return link;
    }

    public void setLink(String link) {
        this.link = link;
    }
}