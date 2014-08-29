package com.wordnik.swagger.codegen;

import com.wordnik.swagger.models.ModelImpl;

import java.util.*;

public interface CodegenConfig {
  CodegenModel fromModel(String name, ModelImpl model);
  String modelPackage();
  Set<String> defaultIncludes();
  Map<String, String> typeMapping();
  Set<String> reservedWords();
  Map<String, String> importMapping();
  String apiPackage();
  String fileSuffix();
  String templateDir();
  Map<String, String> modelTemplateFiles();
  String toModelFilename(String name);
  String modelFileFolder();
}
