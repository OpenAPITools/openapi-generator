package com.wordnik.swagger.codegen.languages;

import com.wordnik.swagger.codegen.*;
import com.wordnik.swagger.models.properties.*;

import java.util.*;
import java.io.File;

public class CSharpClientCodegen extends DefaultCodegen implements CodegenConfig {
  protected String invokerPackage = "io.swagger.client";
  protected String groupId = "io.swagger";
  protected String artifactId = "swagger-client";
  protected String artifactVersion = "1.0.0";
  protected String sourceFolder = "src/main/csharp";

  public CodegenType getTag() {
    return CodegenType.CLIENT;
  }

  public String getName() {
    return "csharp";
  }

  public String getHelp() {
    return "Generates a CSharp client library.";
  }

  public CSharpClientCodegen() {
    super();
    outputFolder = "generated-code/csharp";
    modelTemplateFiles.put("model.mustache", ".cs");
    apiTemplateFiles.put("api.mustache", ".cs");
    templateDir = "csharp";
    apiPackage = "io.swagger.Api";
    modelPackage = "io.swagger.Model";

    reservedWords = new HashSet<String> (
      Arrays.asList(
        "abstract", "as", "base", "bool", "break", "byte", "case", "catch", "char", "checked", "class", "const", "continue", "decimal", "default", "delegate", "do", "double", "else", "enum", "event", "explicit", "extern", "false", "finally", "fixed", "float", "for", "foreach", "goto", "if", "implicit", "in", "int", "interface", "internal", "is", "lock", "long", "namespace", "new", "null", "object", "operator", "out", "override", "params", "private", "protected", "public", "readonly", "ref", "return", "sbyte", "sealed", "short", "sizeof", "stackalloc", "static", "string", "struct", "switch", "this", "throw", "true", "try", "typeof", "uint", "ulong", "unchecked", "unsafe", "ushort", "using", "virtual", "void", "volatile", "while")
    );

    additionalProperties.put("invokerPackage", invokerPackage);

    supportingFiles.add(new SupportingFile("apiInvoker.mustache", 
      (sourceFolder + File.separator + invokerPackage).replace(".", java.io.File.separator), "ApiInvoker.cs"));
    supportingFiles.add(new SupportingFile("apiException.mustache", 
      (sourceFolder + File.separator + invokerPackage).replace(".", java.io.File.separator), "ApiException.cs"));
    supportingFiles.add(new SupportingFile("Newtonsoft.Json.dll", "bin", "Newtonsoft.Json.dll"));
    supportingFiles.add(new SupportingFile("compile.mustache", "", "compile.bat"));

    languageSpecificPrimitives = new HashSet<String>(
      Arrays.asList(
        "String",
        "boolean",
        "Boolean",
        "Double",
        "Integer",
        "Long",
        "Float",
        "Object")
      );
    instantiationTypes.put("array", "List");
    instantiationTypes.put("map", "Dictionary");

    typeMapping = new HashMap<String, String>();
    typeMapping.put("boolean", "bool?");
    typeMapping.put("integer", "int?");
    typeMapping.put("float", "float?");
    typeMapping.put("long", "long?");
    typeMapping.put("double", "double?");
    typeMapping.put("number", "double?");
    typeMapping.put("Date", "DateTime");
    typeMapping.put("file", "byte[]");
    typeMapping.put("array", "List");
    typeMapping.put("map", "Dictionary");

  }

  @Override
  public String escapeReservedWord(String name) {
    return "_" + name;
  }

  @Override
  public String apiFileFolder() {
    return outputFolder + "/" + sourceFolder + "/" + apiPackage().replace('.', File.separatorChar);
  }

  public String modelFileFolder() {
    return outputFolder + "/" + sourceFolder + "/" + modelPackage().replace('.', File.separatorChar);
  }

  @Override
  public String toVarName(String name) {
    // replace - with _ e.g. created-at => created_at
    name = name.replaceAll("-", "_");

    // if it's all uppper case, do nothing
    if (name.matches("^[A-Z_]*$"))
      return name;

    // camelize the variable name
    // pet_id => PetId
    name = camelize(name);

    // for reserved word or word starting with number, append _
    if(reservedWords.contains(name) || name.matches("^\\d.*"))
      name = escapeReservedWord(name);

    return name;
  }

  @Override
  public String toParamName(String name) {
    // should be the same as variable name
    return toVarName(name);
  }

  @Override
  public String toModelName(String name) {
    // model name cannot use reserved keyword, e.g. return
    if(reservedWords.contains(name))
      throw new RuntimeException(name + " (reserved word) cannot be used as a model name");

    // camelize the model name
    // phone_number => PhoneNumber
    return camelize(name);
  }

  @Override
  public String toModelFilename(String name) {
    // should be the same as the model name
    return toModelName(name);
  }


  @Override
  public String getTypeDeclaration(Property p) {
    if(p instanceof ArrayProperty) {
      ArrayProperty ap = (ArrayProperty) p;
      Property inner = ap.getItems();
      return getSwaggerType(p) + "<" + getTypeDeclaration(inner) + ">";
    }
    else if (p instanceof MapProperty) {
      MapProperty mp = (MapProperty) p;
      Property inner = mp.getAdditionalProperties();

      return getSwaggerType(p) + "<String, " + getTypeDeclaration(inner) + ">";
    }
    return super.getTypeDeclaration(p);
  }

  @Override
  public String getSwaggerType(Property p) {
    String swaggerType = super.getSwaggerType(p);
    String type = null;
    if(typeMapping.containsKey(swaggerType)) {
      type = typeMapping.get(swaggerType);
      if(languageSpecificPrimitives.contains(type))
        return type;
    }
    else
      type = swaggerType;
    return type;
  }
}
