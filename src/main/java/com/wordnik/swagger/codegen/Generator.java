package com.wordnik.swagger.codegen;

import com.wordnik.swagger.models.Swagger;

public interface Generator {
  Generator config(CodegenConfig config);
  void generate(Swagger swagger);
}