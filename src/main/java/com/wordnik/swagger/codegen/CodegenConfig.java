package com.wordnik.swagger.codegen;

import com.wordnik.swagger.models.*;
import com.wordnik.swagger.models.properties.*;

import java.util.*;

public interface CodegenConfig {
  Map<String, Object> additionalProperties();
  String apiPackage();
  String apiFileFolder();
  String fileSuffix();
  String outputFolder();
  String templateDir();
  String modelFileFolder();
  String modelPackage();
  String toApiName(String name);
  String toModelName(String name);
  String toParamName(String name);
  String escapeReservedWord(String name);
  String getTypeDeclaration(Property p);
  String getTypeDeclaration(String name);

  Set<String> reservedWords();

  List<SupportingFile> supportingFiles();

  void setOutputDir(String dir);
  String getOutputDir();

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
  String toApiImport(String name);
}
