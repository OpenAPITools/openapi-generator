package com.wordnik.swagger.codegen.languages;

import com.wordnik.swagger.codegen.*;
import com.wordnik.swagger.util.Json;
import com.wordnik.swagger.models.properties.*;

import java.util.*;
import java.io.File;

public class PhpClientCodegen extends DefaultCodegen implements CodegenConfig {
  protected String invokerPackage = "com.wordnik.client";
  protected String groupId = "com.wordnik";
  protected String artifactId = "swagger-client";
  protected String artifactVersion = "1.0.0";
  // protected String sourceFolder = "";

  public String getName() {
    return "php";
  }

  public String getHelp() {
    return "Generates a PHP client library.";
  }

  public PhpClientCodegen() {
    super();
    outputFolder = "generated-code/php";
    modelTemplateFiles.put("model.mustache", ".php");
    apiTemplateFiles.put("api.mustache", ".php");
    templateDir = "php";

    typeMapping.clear();
    languageSpecificPrimitives.clear();

    reservedWords = new HashSet<String> (
      Arrays.asList(
        "int")
    );

    additionalProperties.put("invokerPackage", invokerPackage);
    additionalProperties.put("groupId", groupId);
    additionalProperties.put("artifactId", artifactId);
    additionalProperties.put("artifactVersion", artifactVersion);

    languageSpecificPrimitives.add("array");
    languageSpecificPrimitives.add("string");

    typeMapping.put("long", "int");
    typeMapping.put("integer", "int");
    typeMapping.put("Array", "array");
    typeMapping.put("String", "string");
    typeMapping.put("List", "array");

    supportingFiles.add(new SupportingFile("Swagger.mustache", "", "Swagger.php"));
  }

  @Override
  public String escapeReservedWord(String name) {
    return "_" + name;
  }

  @Override
  public String apiFileFolder() {
    return outputFolder + "/" + apiPackage().replaceAll("\\.", "/");
  }

  public String modelFileFolder() {
    return outputFolder + "/" + modelPackage().replaceAll("\\.", "/");
  }

  @Override
  public String getTypeDeclaration(Property p) {
    if(p instanceof ArrayProperty) {
      ArrayProperty ap = (ArrayProperty) p;
      Property inner = ap.getItems();
      return getSwaggerType(p) + "[" + getTypeDeclaration(inner) + "]";
    }
    else if (p instanceof MapProperty) {
      MapProperty mp = (MapProperty) p;
      Property inner = mp.getAdditionalProperties();
      return getSwaggerType(p) + "[String, " + getTypeDeclaration(inner) + "]";
    }
    return super.getTypeDeclaration(p);
  }

  @Override
  public String getSwaggerType(Property p) {
    String swaggerType = super.getSwaggerType(p);
    String type = null;
    if(typeMapping.containsKey(swaggerType)) {
      type = typeMapping.get(swaggerType);
      if(languageSpecificPrimitives.contains(type)) {
        return type;
      }
    }
    else
      type = swaggerType;
    if(type == null)
      return null;
    return type;
  }
}

/*
package com.wordnik.swagger.codegen

import com.wordnik.swagger.codegen.model._

import java.io.File

object BasicPHPGenerator extends BasicPHPGenerator {
  def main(args: Array[String]) = generateClient(args)
}

class BasicPHPGenerator extends BasicGenerator {
  // template used for models
  modelTemplateFiles += "model.mustache" -> ".php"

  // template used for models
  apiTemplateFiles += "api.mustache" -> ".php"

  // location of templates
  override def templateDir = "php"

  // where to write generated code
  override def destinationDir = "generated-code/php"

  // package for models
  override def modelPackage: Option[String] = Some("models")

  // package for apis
  override def apiPackage: Option[String] = Some("")

  // file suffix
  override def fileSuffix = ".php"

  // reserved words which need special quoting
  // These will all be object properties, in which context we don't need
  // to worry about escaping them for PHP.
  override def reservedWords = Set()

  // import/require statements for specific datatypes
  override def importMapping = Map()


 // response classes
  override def processResponseClass(responseClass: String): Option[String] = {
    typeMapping.contains(responseClass) match {
      case true => Some(typeMapping(responseClass))
      case false => {
        responseClass match {
          case "void" => None
          case e: String => {
            responseClass.startsWith("List") match {
              case true => Some("array")
              case false => Some(responseClass)
            }
          }
        }
      }
    }
  }


  override def processResponseDeclaration(responseClass: String): Option[String] = {
    typeMapping.contains(responseClass) match {
      case true => Some(typeMapping(responseClass))
      case false => {
        responseClass match {
          case "void" => None
          case e: String => {
            responseClass.startsWith("List") match {
              case true => {
                val responseSubClass = responseClass.dropRight(1).substring(5)
                typeMapping.contains(responseSubClass) match {
                  case true => Some("array[" + typeMapping(responseSubClass) + "]")
                  case false => Some("array[" + responseSubClass + "]")
                }
              }
              case false => Some(responseClass)
            }
          }
        }
      }
    }
  }
  override def typeMapping = Map(
    "string" -> "string",
    "str" -> "string",
    "int" -> "int",
    "float" -> "float",
    "long" -> "int",
    "double" -> "float",
    "Array" -> "array",
    "boolean" -> "bool",
    "Date" -> "DateTime"
    )

  override def toDeclaredType(dt: String): String = {
    val declaredType = typeMapping.getOrElse(dt, dt)
    declaredType.startsWith("Array") match {
      case true => {
        val innerType = dt.dropRight(1).substring(6)
        typeMapping.contains(innerType) match {
          case true => "array[" + typeMapping(innerType) + "]"
          case false => "array[" + innerType + "]"
        }
      }
      case _ => declaredType
    }
  }

  override def toDeclaration(obj: ModelProperty) = {
    var declaredType = toDeclaredType(obj.`type`)

    declaredType match {
      case "Array" => declaredType = "array"
      case e: String => {
        e
      }
    }

    val defaultValue = toDefaultValue(declaredType, obj)
    declaredType match {
      case "array" => {
        val inner = {
          obj.items match {
            case Some(items) => items.ref.getOrElse(items.`type`)
            case _ => {
              println("failed on " + declaredType + ", " + obj)
              throw new Exception("no inner type defined")
            }
          }
        }
        declaredType += "[" + toDeclaredType(inner) + "]"
        "array"
      }
      case _ =>
    }
    (declaredType, defaultValue)
  }

  // supporting classes
  override def supportingFiles = List(
    ("Swagger.mustache", destinationDir + File.separator + apiPackage.get,
     "Swagger.php")
  )
}

*/