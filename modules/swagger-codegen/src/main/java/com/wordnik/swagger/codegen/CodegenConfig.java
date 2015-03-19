package com.wordnik.swagger.codegen;

import com.wordnik.swagger.models.*;
import com.wordnik.swagger.models.auth.SecuritySchemeDefinition;
import com.wordnik.swagger.models.properties.*;

import java.util.*;

public interface CodegenConfig {
  CodegenType getTag();
  String getName();
  String getHelp();
  Map<String, Object> additionalProperties();
  String apiPackage();
  String apiFileFolder();
  String fileSuffix();
  String outputFolder();
  String templateDir();
  String modelFileFolder();
  String modelPackage();
  String toApiName(String name);
  String toApiVarName(String name);
  String toModelName(String name);
  String toParamName(String name);
  String escapeReservedWord(String name);
  String getTypeDeclaration(Property p);
  String getTypeDeclaration(String name);
  void processOpts();
  String generateExamplePath(String path, Operation operation);

  Set<String> reservedWords();

  List<SupportingFile> supportingFiles();

  void setOutputDir(String dir);
  String getOutputDir();

  CodegenModel fromModel(String name, Model model);
  CodegenOperation fromOperation(String resourcePath, String httpMethod, Operation operation, Map<String, Model> definitions);
  List<CodegenSecurity> fromSecurity(Map<String, SecuritySchemeDefinition> schemes);

  Set<String> defaultIncludes();
  Map<String, String> typeMapping();
  Map<String, String> instantiationTypes();
  Map<String, String> importMapping();
  Map<String, String> apiTemplateFiles();
  Map<String, String> modelTemplateFiles();
  void processSwagger(Swagger swagger);

  String toApiFilename(String name);
  String toModelFilename(String name);
  String toModelImport(String name);
  String toApiImport(String name);
  void addOperationToGroup(String tag, String resourcePath, Operation operation, CodegenOperation co, Map<String, List<CodegenOperation>> operations);
  Map<String, Object> postProcessModels(Map<String, Object> objs);
  Map<String, Object> postProcessOperations(Map<String, Object> objs);
  Map<String, Object> postProcessSupportingFileData(Map<String, Object> objs);
}
