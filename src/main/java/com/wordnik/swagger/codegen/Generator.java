package com.wordnik.swagger.codegen;

import com.wordnik.swagger.models.Swagger;

public interface Generator {
  Generator opts(ClientOptInput opts);
  void generate();
}