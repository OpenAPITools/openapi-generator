package com.wordnik.swagger.codegen;

public enum CodegenModelType {

  PARAMETER(CodegenParameter.class),
  OPERATION(CodegenOperation.class);

  private final Class<?> defaultImplementation;

  private CodegenModelType(Class<?> defaultImplementation) {
    this.defaultImplementation = defaultImplementation;
  }

  public Class<?> getDefaultImplementation() {
    return defaultImplementation;
  }
}
