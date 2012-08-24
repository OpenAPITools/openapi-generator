package com.wordnik.swagger.codegen

import com.wordnik.swagger.core._

import com.wordnik.swagger.codegen.util.{ CoreUtils, ScalaJsonUtil }
import com.wordnik.swagger.codegen.language.CodegenConfig
import com.wordnik.swagger.codegen.spec.SwaggerSpec._

import org.fusesource.scalate._
import org.fusesource.scalate.layout.DefaultLayoutStrategy
import org.fusesource.scalate.mustache._
import org.fusesource.scalate.support.ScalaCompiler

import java.io.File
import java.io.FileWriter

import scala.io.Source
import scala.collection.mutable.{ HashMap, ListBuffer, HashSet }
import scala.collection.JavaConversions._
import org.apache.commons.io.FileUtils

object Codegen {
  val templates = new HashMap[String, (TemplateEngine, Template)]
}

class Codegen(config: CodegenConfig) {
  val m = ScalaJsonUtil.getJsonMapper

  def generateSource(bundle: Map[String, AnyRef], templateFile: String): String = {
    val allImports = new HashSet[String]
    val includedModels = new HashSet[String]
    val modelList = new ListBuffer[Map[String, AnyRef]]
    val models = bundle("models")
    models match {
      case e: List[Tuple2[String, DocumentationSchema]] => {
        e.foreach(m => {
          includedModels += m._1
          val modelMap = modelToMap(m._1, m._2)
          modelMap.getOrElse("imports", None) match {
            case im: Set[Map[String, String]] => im.foreach(m => m.map(e => allImports += e._2))
            case None =>
          }
          modelList += modelMap
        })
      }
      case None =>
    }

    val modelData = Map[String, AnyRef]("model" -> modelList.toList)
    val operationList = new ListBuffer[Map[String, AnyRef]]
    val classNameToOperationList = new HashMap[String, ListBuffer[AnyRef]]

    val apis = bundle("apis")
    apis match {
      case a: Map[String, List[(String, DocumentationOperation)]] => {
        a.map(op => {
          val classname = op._1
          val ops = op._2
          for ((apiPath, operation) <- ops) {
            val opList = classNameToOperationList.getOrElse(classname, {
              val lb = new ListBuffer[AnyRef]
              classNameToOperationList += classname -> lb
              lb
            })
            opList += apiToMap(apiPath, operation)
            CoreUtils.extractModelNames(operation).foreach(i => allImports += i)
          }
        })
      }
      case None =>
    }

    val f = new ListBuffer[AnyRef]
    classNameToOperationList.map(m => f += Map("classname" -> m._1, "operation" -> m._2))

    val imports = new ListBuffer[Map[String, String]]
    val importScope = config.modelPackage match {
      case Some(s) => s + "."
      case None => ""
    }
    // do the mapping before removing primitives!
    allImports.foreach(i => includedModels.contains(i) match {
      case false => {
        config.importMapping.containsKey(i) match {
          case true => imports += Map("import" -> config.importMapping(i))
          case false =>
        }
      }
      case true =>
    })
    allImports --= config.defaultIncludes
    allImports --= primitives
    allImports --= containers
    allImports.foreach(i => includedModels.contains(i) match {
      case false => {
        config.importMapping.containsKey(i) match {
          case true =>
          case false => imports += Map("import" -> (importScope + i))
        }
      }
      case true => // no need to add the model
    })

    val rootDir = new java.io.File(".")
    val engineData = Codegen.templates.getOrElse(templateFile, {
      val engine = new TemplateEngine(Some(rootDir))
      val srcName = config.templateDir + File.separator + templateFile
      val srcStream = getClass.getClassLoader.getResourceAsStream(srcName)
      if (srcStream == null)
        throw new Exception("Missing template: " + srcName)
      println("pre-compile")
      val template = engine.compile(
        TemplateSource.fromText(config.templateDir + File.separator + templateFile,
          Source.fromInputStream(srcStream).mkString))
      val t = Tuple2(engine, template)
      Codegen.templates += templateFile -> t
      t
    })

    val engine = engineData._1
    val template = engineData._2

    var data = Map[String, AnyRef](
      "package" -> bundle("package"),
      "invokerPackage" -> bundle("invokerPackage"),
      "imports" -> imports,
      "operations" -> f,
      "models" -> modelData,
      "basePath" -> bundle.getOrElse("basePath", ""))

    var output = engine.layout(config.templateDir + File.separator + templateFile, template, data.toMap)
    //a shutdown method will be added to scalate in an upcoming release
    engine.compiler.asInstanceOf[ScalaCompiler].compiler.askShutdown
    output
  }

  def extractImportsFromApi(operation: Tuple2[String, DocumentationOperation]) = {
    val imports = new ListBuffer[AnyRef]
    val modelNames = CoreUtils.extractModelNames(operation._2)

    modelNames.foreach(modelName => {
      // apply mapings, packages for generated code
      val qualifiedModel = (config.importMapping.contains(modelName) match {
        case true => config.importMapping(modelName)
        case false => {
          config.modelPackage match {
            case Some(p) => p + "." + modelName
            case None => modelName
          }
        }
      })
      imports += Map("import" -> qualifiedModel)
    })
    imports.toSet
  }

  def apiToMap(path: String, op: DocumentationOperation): Map[String, AnyRef] = {
    var bodyParam: Option[String] = None

    var queryParams = new ListBuffer[AnyRef]
    val pathParams = new ListBuffer[AnyRef]
    val headerParams = new ListBuffer[AnyRef]
    var paramList = new ListBuffer[HashMap[String, AnyRef]]

    if (op.getParameters != null) {
      op.getParameters.foreach(param => {
        val params = new HashMap[String, AnyRef]
        params += "type" -> param.paramType
        params += "defaultValue" -> config.toDefaultValue(param.dataType, param.defaultValue)
        params += "dataType" -> config.toDeclaredType(param.dataType)
        params += "description" -> param.description
        params += "hasMore" -> "true"
        if (!param.required) {
          params += "optional" -> "true"
        }
        param.paramType match {
          case "body" => {
            params += "paramName" -> "body"
            params += "baseName" -> "body"
            param.required match {
              case true => params += "required" -> "true"
              case _ =>
            }
            bodyParam = Some("body")
          }
          case "path" => {
            params += "paramName" -> config.toVarName(param.name)
            params += "baseName" -> param.name
            params += "required" -> "true"
            pathParams += params.clone
          }
          case "query" => {
            params += "paramName" -> config.toVarName(param.name)
            params += "baseName" -> param.name
            params += "required" -> param.required.toString
            queryParams += params.clone
          }
          case "header" => {
            params += "paramName" -> config.toVarName(param.name)
            params += "baseName" -> param.name
            params += "required" -> param.required.toString
            headerParams += params.clone
          }
          case x @ _ => throw new Exception("Unknown parameter type: " + x)
        }
        paramList += params
      })
    }

    val requiredParams = new ListBuffer[HashMap[String, AnyRef]]
    paramList.filter(p => p.contains("required") && p("required") == "true").foreach(param => {
      requiredParams += HashMap(
        "paramName" -> param("paramName"),
        "defaultValue" -> param("defaultValue"),
        "baseName" -> param("baseName"),
        "hasMore" -> "true")
    })
    requiredParams.size match {
      case 0 =>
      case _ => requiredParams.last.asInstanceOf[HashMap[String, String]] -= "hasMore"
    }

    queryParams.size match {
      case 0 =>
      case _ => queryParams.last.asInstanceOf[HashMap[String, String]] -= "hasMore"
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
        sp.first.asInstanceOf[HashMap[String, String]] -= "secondaryParam"
        sp.last.asInstanceOf[HashMap[String, String]] -= "hasMore"
      }
    }

    val properties =
      HashMap[String, AnyRef](
        "path" -> path,
        "nickname" -> config.toMethodName(op.nickname),
        "summary" -> op.summary,
        "notes" -> op.notes,
        "deprecated" -> op.deprecated,
        "bodyParam" -> bodyParam,
        "allParams" -> sp,
        "pathParams" -> pathParams,
        "queryParams" -> queryParams,
        "headerParams" -> headerParams,
        "requiredParams" -> requiredParams,
        "httpMethod" -> op.httpMethod.toUpperCase,
        op.httpMethod.toLowerCase -> "true")
    if (requiredParams.size > 0) properties += "requiredParamCount" -> requiredParams.size.toString
    op.responseClass.indexOf("[") match {
      case -1 => {
        val baseType = op.responseClass
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
        val ComplexTypeMatcher(basePart) = op.responseClass
        properties += "returnType" -> config.processResponseDeclaration(op.responseClass)
        properties += "returnContainer" -> (op.responseClass.substring(0, n))
        properties += "returnBaseType" -> Some(basePart)
        properties += "returnTypeIsPrimitive" -> {
          (config.languageSpecificPrimitives.contains(basePart) || primitives.contains(basePart)) match {
            case true => Some("true")
            case _ => None
          }
        }
      }
    }

    properties.toMap
  }

  def modelToMap(className: String, model: DocumentationSchema): Map[String, AnyRef] = {
    val data: HashMap[String, AnyRef] =
      HashMap(
        "classname" -> className,
        "classVarName" -> config.toVarName(className), // suggested name of object created from this class
        "modelPackage" -> config.modelPackage,
        "newline" -> "\n")

    val l = new ListBuffer[AnyRef]

    val imports = new HashSet[AnyRef]
    model.properties.map(prop => {
      val propertyDocSchema = prop._2
      val dt = propertyDocSchema.getType

      var baseType = dt
      // import the object inside the container
      if (propertyDocSchema.items != null) {
        // import the container
        imports += Map("import" -> dt)
        if (propertyDocSchema.items.ref != null) baseType = propertyDocSchema.items.ref
        else if (propertyDocSchema.items.getType != null) baseType = propertyDocSchema.items.getType
      }
      baseType = config.typeMapping.contains(baseType) match {
        case true => config.typeMapping(baseType)
        case false => imports += Map("import" -> config.typeMapping.getOrElse(baseType, baseType)); baseType
      }

      val isList = (if (isListType(propertyDocSchema.getType)) true else None)
      val isMap = (if (isMapType(propertyDocSchema.getType)) true else None)
      val isNotContainer = if (!isListType(propertyDocSchema.getType) && !isMapType(propertyDocSchema.getType)) true else None
      val isContainer = if (isListType(propertyDocSchema.getType) || isMapType(propertyDocSchema.getType)) true else None

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
          "notes" -> propertyDocSchema.notes,
          "required" -> propertyDocSchema.required.toString,
          "getter" -> config.toGetter(prop._1, config.toDeclaration(propertyDocSchema)._1),
          "setter" -> config.toSetter(prop._1, config.toDeclaration(propertyDocSchema)._1),
          "isList" -> isList,
          "isMap" -> isMap,
          "isContainer" -> isContainer,
          "isNotContainer" -> isNotContainer,
          "hasMore" -> "true")

      (config.languageSpecificPrimitives.contains(baseType) || primitives.contains(baseType)) match {
        case true => properties += "isPrimitiveType" -> "true"
        case _ => properties += "complexType" -> baseType
      }

      l += properties
    })
    l.size match {
      case 0 =>
      case _ => l.last.asInstanceOf[HashMap[String, String]] -= "hasMore"
    }
    data += "vars" -> l
    data += "imports" -> imports.toSet
    data.toMap
  }

  def writeSupportingClasses = {
    val rootDir = new java.io.File(".")
    val engine = new TemplateEngine(Some(rootDir))

    val data: HashMap[String, String] =
      HashMap(
        "package" -> config.packageName)

    config.supportingFiles.map(file => {
      val srcTemplate = file._1
      val outputDir = file._2
      val destFile = file._3

      val outputFilename = outputDir.replaceAll("\\.", File.separator) + File.separator + destFile
      val outputFolder = new File(outputFilename).getParent
      new File(outputFolder).mkdirs

      if (srcTemplate.endsWith(".mustache")) {
        val output = {
          val resourceName = config.templateDir + File.separator + srcTemplate
          val resourceStream = getClass.getClassLoader.getResourceAsStream(resourceName)
          if (resourceStream == null)
            throw new Exception("Resource not found: " + resourceName)
          val template = engine.compile(
            TemplateSource.fromText(resourceName,
              Source.fromInputStream(resourceStream).mkString))
          engine.layout(resourceName, template, data.toMap)
        }
        val fw = new FileWriter(outputFilename, false)
        fw.write(output + "\n")
        fw.close()
        println("wrote " + outputFilename)
      } else {
        val is = getClass.getClassLoader
          .getResourceAsStream(config.templateDir + File.separator + srcTemplate)
        FileUtils.copyInputStreamToFile(is, new File(outputFilename))
        println("copied " + outputFilename)
        is.close
      }
    })
    //a shutdown method will be added to scalate in an upcoming release
    engine.compiler.asInstanceOf[ScalaCompiler].compiler.askShutdown
  }

  protected def isListType(dt: String) = isCollectionType(dt, "List") || isCollectionType(dt, "Array")

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
        case _ => false
      }
  }
}
