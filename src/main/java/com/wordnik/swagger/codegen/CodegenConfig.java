package com.wordnik.swagger.codegen;

import com.wordnik.swagger.models.*;

import java.util.*;

public interface CodegenConfig {
  Map<String, Object> additionalProperties();
  String apiPackage();
  String apiFileFolder();
  String fileSuffix();
  String templateDir();
  String modelFileFolder();
  String modelPackage();
  String toApiName(String name);
  String toModelName(String name);

  Set<String> reservedWords();

  CodegenModel fromModel(String name, Model model);
  CodegenOperation fromOperation(String resourcePath, String httpMethod, Operation operation);
  Set<String> defaultIncludes();
  Map<String, String> typeMapping();
  Map<String, String> importMapping();
  Map<String, String> apiTemplateFiles();
  Map<String, String> modelTemplateFiles();

  String toApiFilename(String name);
  String toModelFilename(String name);
  String toModelImport(String name);
}
