package io.swagger.codegen;

import io.swagger.models.Swagger;

import java.io.File;
import java.util.List;

public interface Generator {
  Generator opts(ClientOptInput opts);
  List<File> generate();
}