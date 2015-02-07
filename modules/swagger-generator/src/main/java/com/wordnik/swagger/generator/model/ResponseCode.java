package com.wordnik.swagger.generator.model;

public class ResponseCode {
  private String code;

  public String getCode() {
    return code;
  }

  public void setCode(String code) {
    this.code = code;
  }

  public ResponseCode() {}

  public ResponseCode(String code) {
    setCode(code);
  }
}