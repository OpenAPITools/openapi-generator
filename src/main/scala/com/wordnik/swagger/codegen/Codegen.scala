/**
 *  Copyright 2014 Wordnik, Inc.
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package com.wordnik.swagger.codegen

import com.wordnik.swagger.codegen.model._
import com.wordnik.swagger.codegen.util.CoreUtils
import com.wordnik.swagger.codegen.language.CodegenConfig
import com.wordnik.swagger.codegen.spec.SwaggerSpec._

import org.json4s.jackson.JsonMethods._
import org.json4s.jackson.Serialization.write

import org.fusesource.scalate._
import org.fusesource.scalate.layout.DefaultLayoutStrategy
import org.fusesource.scalate.mustache._
import org.fusesource.scalate.support.ScalaCompiler

import java.io.{ File, FileWriter, InputStream }

import org.apache.commons.io.FileUtils

import scala.io.Source
import scala.collection.mutable.{ HashMap, ListBuffer, HashSet }
import scala.collection.JavaConversions._

object Codegen {
  val templates = new HashMap[String, (String, (TemplateEngine, Template))]
}

class Codegen(config: CodegenConfig) {
  implicit val formats = SwaggerSerializers.formats("1.2")

  def compileTemplate(templateFile: String, rootDir: Option[File] = None, engine: Option[TemplateEngine] = None): (String, (TemplateEngine, Template)) = {
    val engine = new TemplateEngine(rootDir orElse Some(new File(".")))
    val srcName = config.templateDir + "/" + templateFile
    val srcStream = {
      getClass.getClassLoader.getResourceAsStream(srcName) match {
        case is: java.io.InputStream => is
        case _ => {
          val f = new java.io.File(srcName)
          if (!f.exists) throw new Exception("Missing template: " + srcName)
          else new java.io.FileInputStream(f)
        }
      }
    }
    val template = engine.compile(
      TemplateSource.fromText(config.templateDir + File.separator + templateFile,
        Source.fromInputStream(srcStream).mkString))
    (srcName, engine -> template)
  }

  def rawAllowableValuesToString(v: AllowableValues) = {
    v match {
      case av: AllowableListValues => {
        av
      }
      case av: AllowableRangeValues => {
        av
      }
      case _ => None
    }
  }


  def allowableValuesToString(v: AllowableValues) = {
    v match {
      case av: AllowableListValues => {
        Some(av.values.mkString("LIST[", ",", "]"))
      }
      case av: AllowableRangeValues => {
        Some("RANGE[" + av.min + "," + av.max + "]")
      }
      case _ => None
    }
  }

  def apiToMap(path: String, operation: Operation): Map[String, AnyRef] = {
    var bodyParam: Option[String] = None
    var queryParams = new ListBuffer[AnyRef]
    val pathParams = new ListBuffer[AnyRef]
    val headerParams = new ListBuffer[AnyRef]
    val bodyParams = new ListBuffer[AnyRef]
    val formParams = new ListBuffer[AnyRef]
    var paramList = new ListBuffer[HashMap[String, AnyRef]]
    var errorList = new ListBuffer[HashMap[String, AnyRef]]
    var bodyParamRequired: Option[String] = Some("true")

    if (operation.responseMessages != null) {
      operation.responseMessages.foreach(param => {
        val params = new HashMap[String, AnyRef]
        params += "code" -> param.code.toString()
        params += "reason" -> param.message
        if (!param.responseModel.isEmpty) 
          params += "responseModel" -> param.responseModel
        params += "hasMore" -> "true"
        errorList += params
      })
    }

    if (operation.parameters != null) {
      operation.parameters.foreach(param => {
        val params = new HashMap[String, AnyRef]
        params += (param.paramType + "Parameter") -> "true"
        params += "type" -> param.paramType
        params += "defaultValue" -> config.toDefaultValue(param.dataType, param.defaultValue.getOrElse(""))
        params += "swaggerDataType" -> param.dataType
        params += "description" -> param.description
        params += "hasMore" -> "true"
        params += "allowMultiple" -> param.allowMultiple.toString

        if(param.dataType == "File") params += "isFile" -> "true"
        else params += "notFile" -> "true"

        val u = param.dataType.indexOf("[") match {
          case -1 => config.toDeclaredType(param.dataType)
          case n: Int => {
            val ComplexTypeMatcher = "(.*)\\[(.*)\\].*".r
            val ComplexTypeMatcher(container, basePart) = param.dataType
            config.toDeclaredType(container + "[" + config.toDeclaredType(basePart) + "]")
          }
        }

        params += "dataType" -> u
        params += "getter" -> config.toGetter(param.name, u)
        params += "setter" -> config.toSetter(param.name, u)

        param.allowableValues match {
          case a: AllowableValues => params += "allowableValues" -> allowableValuesToString(a)
          case _ =>
        }

        if (param.required) {
          params += "required" -> "true"
        } else {
          params += "optional" -> "true"
        }
        param.paramType match {
          case "body" => {
            params += "paramName" -> "body"
            params += "baseName" -> "body"
            if (!param.required) {
              bodyParamRequired = None
            }

            bodyParam = Some("body")
            bodyParams += params.clone
          }
          case "path" => {
            params += "paramName" -> config.toVarName(param.name)
            params += "baseName" -> param.name
            params += "required" -> "true"
            params -= "optional"
            pathParams += params.clone
          }
          case "query" => {
            params += "paramName" -> config.toVarName(param.name)
            params += "baseName" -> param.name
            queryParams += params.clone
          }
          case "header" => {
            params += "paramName" -> config.toVarName(param.name)
            params += "baseName" -> param.name
            headerParams += params.clone
          }
          case "form" => {
            params += "paramName" -> config.toVarName(param.name)
            params += "baseName" -> param.name
            formParams += params.clone
          }
          case x @ _ => throw new Exception("Unknown parameter type: " + x)
        }
        paramList += params
      })
    }

    val requiredParams = new ListBuffer[HashMap[String, AnyRef]]
    paramList.filter(p => p.contains("required") && p("required") == "true").foreach(param => {
      requiredParams += (param.clone += "hasMore" -> "true")
    })
    requiredParams.size match {
      case 0 =>
      case _ => requiredParams.last.asInstanceOf[HashMap[String, String]] -= "hasMore"
    }

    headerParams.size match {
      case 0 =>
      case _ => headerParams.last.asInstanceOf[HashMap[String, String]] -= "hasMore"
    }

    queryParams.size match {
      case 0 =>
      case _ => queryParams.last.asInstanceOf[HashMap[String, String]] -= "hasMore"
    }

    pathParams.size match {
      case 0 =>
      case _ => pathParams.last.asInstanceOf[HashMap[String, String]] -= "hasMore"
    }
    errorList.size match{
      case 0 =>
      case _ => errorList.last.asInstanceOf[HashMap[String, String]] -= "hasMore"
      }

    val sp = {
      val lb = new ListBuffer[AnyRef]
      paramList.foreach(i => {
        i += "secondaryParam" -> "true"
        i("defaultValue") match {
          case Some(e) =>
          case None => lb += i
        }
      })
      paramList.foreach(i => {
        i("defaultValue") match {
          case Some(e) => lb += i
          case None =>
        }
      })
      lb.toList
    }

    paramList.size match {
      case 0 =>
      case _ => {
        sp.head.asInstanceOf[HashMap[String, String]] -= "secondaryParam"
        sp.last.asInstanceOf[HashMap[String, String]] -= "hasMore"
      }
    }

    val writeMethods = Set("POST", "PUT", "PATCH")
    val properties =
      HashMap[String, AnyRef](
        "path" -> path,
        "nickname" -> config.toMethodName(operation.nickname),
        "summary" -> operation.summary,
        "notes" -> operation.notes,
        "deprecated" -> operation.`deprecated`,
        "bodyParam" -> bodyParam,
        "bodyParamRequired" -> bodyParamRequired,
        "emptyBodyParam" -> (if (writeMethods contains operation.method.toUpperCase) "{}" else ""),
        "allParams" -> sp,
        "bodyParams" -> bodyParams.toList,
        "pathParams" -> pathParams.toList,
        "queryParams" -> queryParams.toList,
        "headerParams" -> headerParams.toList,
        "formParams" -> formParams.toList,
        "requiredParams" -> requiredParams.toList,
        "errorList" -> errorList,
        "httpMethod" -> operation.method.toUpperCase,
        "httpMethodLowerCase" -> operation.method.toLowerCase,
        operation.method.toLowerCase -> "true")
    if (0 < operation.consumes.length) {
      val o = new ListBuffer[Map[String, String]]
      for(i <- 0 until operation.consumes.length) {
        val m = new HashMap[String, String]
        if(i < (operation.consumes.length - 1))
          m += "hasMore" -> "true"
        m += "mediaType" -> operation.consumes(i)
        o += m.toMap
      }
      properties += "consumes" -> o.toList
    } else {
      properties += "consumes" -> List(Map("mediaType" -> "application/json"))
    }
    if (0 < operation.produces.length) {
      val o = new ListBuffer[Map[String, String]]
      for(i <- 0 until operation.produces.length) {
        val m = new HashMap[String, String]
        if((i + 1) < operation.produces.length)
          m += "hasMore" -> "true"
        m += "mediaType" -> operation.produces(i)
        o += m.toMap
      }
      properties += "produces" -> o.toList
    } else {
      properties += "produces" -> List(Map("mediaType" -> "application/json"))
    }
    if (requiredParams.size > 0) properties += "requiredParamCount" -> requiredParams.size.toString
    operation.responseClass.indexOf("[") match {
      case -1 => {
        val baseType = operation.responseClass
        properties += "returnType" -> config.processResponseDeclaration(baseType)
        properties += "returnBaseType" -> config.processResponseClass(baseType)
        properties += "returnSimpleType" -> "true"
        properties += "returnTypeIsPrimitive" -> {
          (config.languageSpecificPrimitives.contains(baseType) || primitives.contains(baseType)) match {
            case true => Some("true")
            case _ => None
          }
        }
      }
      case n: Int => {
        val ComplexTypeMatcher = ".*\\[(.*)\\].*".r
        val ComplexTypeMatcher(basePart) = operation.responseClass

        properties += "returnType" -> config.processResponseDeclaration(operation.responseClass.replaceAll(basePart, config.processResponseClass(basePart).get))
        properties += "returnContainer" -> config.processResponseClass(operation.responseClass.substring(0, n))
        properties += "returnBaseType" -> config.processResponseClass(basePart)
        properties += "returnTypeIsPrimitive" -> {
          (config.languageSpecificPrimitives.contains(basePart) || primitives.contains(basePart)) match {
            case true => Some("true")
            case _ => None
          }
        }
      }
    }
    config.processApiMap(properties.toMap)
  }

  def modelToMap(className: String, model: Model): Map[String, AnyRef] = {
    val data: HashMap[String, AnyRef] =
      HashMap(
        "classname" -> config.toModelName(className),
        "className" -> config.toModelName(className),
        "classVarName" -> config.toVarName(className), // suggested name of object created from this class
        "modelPackage" -> config.modelPackage,
        "description" -> model.description,
        "modelJson" -> writeJson(model),
        "newline" -> "\n")

    val l = new ListBuffer[AnyRef]

    val imports = new HashSet[AnyRef]
    model.properties.map(prop => {
      val propertyDocSchema = prop._2
      val dt = propertyDocSchema.`type`

      var baseType = dt
      // import the object inside the container
      if (propertyDocSchema.items != null && !config.typeMapping.contains(dt)) {
        // import the container
        imports += Map("import" -> dt)
        propertyDocSchema.items match {
          case Some(items) => baseType = items.ref.getOrElse(items.`type`)
          case _ =>
        }
      }
      baseType = config.typeMapping.contains(baseType) match {
        case true => config.typeMapping(baseType)
        case false => {
          // imports += Map("import" -> config.toDeclaredType(baseType))
          baseType
        }
      }
      (config.defaultIncludes ++ config.languageSpecificPrimitives).toSet.contains(baseType) match {
        case true =>
        case _ => {
          imports += Map("import" -> baseType)
        }
      }

      val isList = (if (isListType(propertyDocSchema.`type`)) true else None)
      val isMap = (if (isMapType(propertyDocSchema.`type`)) true else None)
      val isNotContainer = if (!isListType(propertyDocSchema.`type`) && !isMapType(propertyDocSchema.`type`)) true else None
      val isContainer = if (isListType(propertyDocSchema.`type`) || isMapType(propertyDocSchema.`type`)) true else None

      val properties =
        HashMap(
          "name" -> config.toVarName(prop._1),
          "nameSingular" -> {
            val name = config.toVarName(prop._1)
            if (name.endsWith("s") && name.length > 1) name.substring(0, name.length - 1) else name
          },
          "baseType" -> {
            if (primitives.contains(baseType))
              baseType
            else
              config.modelPackage match {
                case Some(p) => p + "." + baseType
                case _ => baseType
              }
          },
          "baseTypeVarName" -> config.toVarName(baseType),
          "baseName" -> prop._1,
          "datatype" -> config.toDeclaration(propertyDocSchema)._1,
          "defaultValue" -> config.toDeclaration(propertyDocSchema)._2,
          "description" -> propertyDocSchema.description,
          "notes" -> propertyDocSchema.description,
          "allowableValues" -> rawAllowableValuesToString(propertyDocSchema.allowableValues),
          (if(propertyDocSchema.required) "required" else "isNotRequired") -> "true",
          "getter" -> config.toGetter(prop._1, config.toDeclaration(propertyDocSchema)._1),
          "setter" -> config.toSetter(prop._1, config.toDeclaration(propertyDocSchema)._1),
          "isList" -> isList,
          "isMap" -> isMap,
          "isContainer" -> isContainer,
          "isNotContainer" -> isNotContainer,
          "hasMore" -> "true")
      (config.languageSpecificPrimitives.contains(baseType) || primitives.contains(baseType)) match {
        case true => properties += "isPrimitiveType" -> "true"
        case _ => properties += "complexType" -> config.toModelName(baseType)
      }
      l += properties
    })
    if(l.size > 0) {
      val last = l.last.asInstanceOf[HashMap[String, String]]
      last.remove("hasMore")
    }
    data += "vars" -> l
    data += "imports" -> imports.toSet
    config.processModelMap(data.toMap)
  }

  /**
   * gets an input stream from resource or file
   */
  def getInputStream(path: String): InputStream = {
    getClass.getClassLoader.getResourceAsStream(path) match {
      case is: InputStream => is
      case _ => new java.io.FileInputStream(path)
    }
  }

  def writeJson(m: AnyRef): String = {
    Option(System.getProperty("modelFormat")) match {
      case Some(e) if e =="1.1" => write1_1(m)
      case _ => pretty(render(parse(write(m))))
    }
  }

  def write1_1(m: AnyRef): String = {
    implicit val formats = SwaggerSerializers.formats("1.1")
    write(m)
  }

  def writeSupportingClasses2(
    apiBundle: List[Map[String, AnyRef]],
    modelsMap: List[Map[String, AnyRef]],
    apiVersion: String): Seq[File] = {



    val b = new HashMap[String, HashMap[String, AnyRef]]
    modelsMap.foreach(m => {
      if(m.contains("models")) {
        val f = m("models").asInstanceOf[List[Map[String, AnyRef]]]

        f.foreach(g => {
          val e = new HashMap[String, AnyRef]
          val model = g("model").asInstanceOf[Map[String, AnyRef]]
          e ++= model
          e += "hasMoreModels" -> "true"

          b += model("classVarName").toString -> e
        })
      }  
    })
    val models = new ListBuffer[HashMap[String, AnyRef]]

    val keys = b.keys
    var count = 0
    b.values.foreach(v => {
      models += v
      count += 1
      if(count != keys.size) {
        v += "hasMoreModels" -> "true"
      }
      else {
        v.remove("hasMoreModels")
      }
    })

    val f = Map("model" -> models)
    val rootDir: Option[File] = Some(new File("."))
    val engine = new TemplateEngine(rootDir orElse Some(new File(".")))

    val data = Map(
      "invokerPackage" -> config.invokerPackage,
      "package" -> config.packageName,
      "modelPackage" -> config.modelPackage,
      "apiPackage" -> config.apiPackage,
      "apiInfo" -> Map("apis" -> apiBundle),
      "models" -> f,
      "apiVersion" -> apiVersion) ++ config.additionalParams

    val outputFiles = config.supportingFiles map { file =>
      val supportingFile = file._1
      val outputDir = file._2
      val destFile = file._3

      val outputFile = new File(outputDir + File.separator + destFile)
      val outputFolder = outputFile.getParent
      new File(outputFolder).mkdirs

      if (supportingFile.endsWith(".mustache")) {
        val output = {
          val (resourceName, (_, template)) = compileTemplate(supportingFile, rootDir, Some(engine))
          engine.layout(resourceName, template, data.toMap)
        }
        val fw = new FileWriter(outputFile, false)
        fw.write(output + "\n")
        fw.close()
        println("wrote " + outputFile.getPath())
      } else {
        val file = new File(config.templateDir + File.separator + supportingFile)
        if (file.isDirectory()) {
          // copy the whole directory
          FileUtils.copyDirectory(file, new File(outputDir))
          println("copied directory " + supportingFile)
        } else {
          val is = getInputStream(config.templateDir + File.separator + supportingFile)
          val parentDir = outputFile.getParentFile()
          if (parentDir != null && !parentDir.exists) {
            println("making directory: " + parentDir.toString + ": " + parentDir.mkdirs)
          }
          FileUtils.copyInputStreamToFile(is, outputFile)
          println("copied " + outputFile.getPath())
          is.close
        }
      }
      outputFile
    }
    //a shutdown method will be added to scalate in an upcoming release
    engine.compiler.shutdown()
    outputFiles
  }
  
  protected def isListType(dt: String) = isCollectionType(dt, "List") || isCollectionType(dt, "Array") || isCollectionType(dt, "Set")

  protected def isMapType(dt: String) = isCollectionType(dt, "Map")

  protected def isCollectionType(dt: String, str: String) = {
    if (dt.equals(str))
      true
    else
      dt.indexOf("[") match {
        case -1 => false
        case n: Int => {
          if (dt.substring(0, n) == str) {
            true
          } else false
        }
      }
  }
}
