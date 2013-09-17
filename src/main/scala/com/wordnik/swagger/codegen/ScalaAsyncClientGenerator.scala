package com.wordnik.swagger.codegen

import scala.collection.mutable
import java.io.{File, FileWriter}
import com.wordnik.swagger.model._
import scala.collection.mutable.{HashMap, ListBuffer}
import language.CodegenConfig
import scala.io.Source
import org.json4s.jackson.Serialization._
import org.fusesource.scalate.{Template, TemplateSource, TemplateEngine}
import org.apache.commons.io.FileUtils
import com.wordnik.swagger.codegen.util.{CoreUtils, ApiExtractor, ResourceExtractor}
import com.wordnik.swagger.codegen.spec.SwaggerSpecValidator
import mojolly.inflector.InflectorImports._
import org.rogach.scallop.{ScallopConf, Scallop}
import scala.annotation.switch

case class SwaggerApi(
             clientName: String,
             resourceUrl: String,
             packageName: String,
             apiTemplates: Map[String, String] = Map("api.mustache" -> ".scala"),
             modelTemplates: Map[String, String] = Map("model.mustache" -> ".scala"),
             apiKey: Option[String] = None,
             baseUrl: Option[String] = None,
             excludedApis: Set[String] = Set.empty,
             excludedModels: Set[String] = Set.empty,
             excludedModelPackages: Set[String] = Set.empty,
             defaultImports: Map[String, String] = Map.empty)
case class SwaggerGenConfig(
             api: SwaggerApi,
             templateDir: File,
             codeDir: File,
             projectRoot: File,
             defaultIncludes: Set[String] = Set.empty,
             typeMapping: Map[String, String] = Map.empty,
             defaultImports: Map[String, String] = Map.empty,
             excludedModelPackages: Set[String] = Set.empty)
object AsycnClientGeneratorConf {
  val appBanner: String = """
        |
        |
        |  .--.--.
        | /  /    '.
        ||  :  /`. /         .---.                                        __  ,-.
        |;  |  |--`         /. ./|           ,----._,. ,----._,.        ,' ,'/ /|
        ||  :  ;_        .-'-. ' | ,--.--.  /   /  ' //   /  ' /  ,---. '  | |' |
        | \  \    `.    /___/ \: |/       \|   :     |   :     | /     \|  |   ,'
        |  `----.   \.-'.. '   ' .--.  .-. |   | .\  |   | .\  ./    /  '  :  /
        |  __ \  \  /___/ \:     '\__\/: . .   ; ';  .   ; ';  .    ' / |  | '
        | /  /`--'  .   \  ' .\   ," .--.; '   .   . '   .   . '   ;   /;  : |
        |'--'.     / \   \   ' \ /  /  ,.  |`---`-'| |`---`-'| '   |  / |  , ;
        |  `--'---'   \   \  |--;  :   .'   .'__/\_: |.'__/\_: |   :    |---'
        |              \   \ |  |  ,     .-.|   :    :|   :    :\   \  /
        |               '---"    `--`---'    \   \  /  \   \  /  `----'
        |                                     `--`-'    `--`-'
        |
        |         Swagger Codegen, Reverb Technologies Inc. (c) 2009-2013
        |      For more info, visit: https://developers.helloreverb.com/swagger/
      """.stripMargin
}
class AsycnClientGeneratorConf(arguments: Seq[String]) extends ScallopConf(arguments) {

  val name = opt[String](required = true, descr = "The name of the generated client.")
  val `package` = opt[String](default = Some("com.wordnik.swagger.client.async"), descr = "The package for the generated code.")
  val resourceUrl = trailArg[String](descr = "The url to use for fetching the swagger spec from. This can be a http(s) url or a file path.")
  val baseUrl = opt[String](descr = "The url to use when you want to override the base url provided by the resource url json.")
  val apiKey = opt[String](required = false, descr = "An optional api key to use when calling the swagger api")
  val templateDir = opt[String](descr = "The directory that contains the templates for use in this generator", default = Some("asyncscala"))
  val codeDir = opt[String](descr = "The directory to use as base for generating code files, this will contain the generated scala files.", default = Some("src/main/scala"), hidden = true)
  val projectRoot = opt[String](descr = "The directory to use as project dir, this will receive the build files (*.sbt, *.pom)", default = Some("."))

  mainOptions = Seq(resourceUrl, name)

  banner("""
           |Usage: scala-async.sh [OPTION] spec-url
           |
           |The scala-async tool generates a swagger api client, using async-http-client
           |and stdlib futures.
           |
           |Options:
           |
         """.stripMargin)

  footer("\nFor more information, visit https://developers.helloreverb.com/swagger/")
}
object ScalaAsyncClientGenerator extends App {

  val appBanner: String = AsycnClientGeneratorConf.appBanner

  val opts = new AsycnClientGeneratorConf(if (args.nonEmpty) args else Array("--help"))
  val rootDir = new File(opts.projectRoot())
  val codeDir = {
    val cd = opts.codeDir()
    if (cd.startsWith("/")) new File(cd)
    else new File(rootDir, cd)
  }
  val resUrl = {
    val r = opts.resourceUrl()
    if (!r.startsWith("http") && !r.startsWith("file")) sys.props("fileMap") = r
    r
  }
  val baseUrl = opts.baseUrl.get
  val cfg = SwaggerGenConfig(
    api = SwaggerApi(opts.name(), resUrl, opts.`package`(), apiKey = opts.apiKey.get, baseUrl = baseUrl),
    templateDir = new File(opts.templateDir()),
    codeDir = new File(rootDir, opts.codeDir()),
    projectRoot = rootDir
  )

  val generator = new ScalaAsyncClientGenerator(cfg)
  println(appBanner)
  generator.generateClient(Array.empty)
}

class AsyncClientCodegen(clientName: String, config: CodegenConfig, rootDir: Option[File] = None) extends Codegen(config) {
  override def writeSupportingClasses(apis: Map[(String, String), List[(String, Operation)]], models: Map[String, Model]) = {
    val engine = new TemplateEngine(rootDir orElse Some(new File(".")))

    val apiList = new ListBuffer[Map[String, AnyRef]]

    apis.map(a => {
      apiList += Map(
        "name" -> a._1._2,
        "filename" -> config.toApiFilename(a._1._2),
        "className" -> config.toApiName(a._1._2),
        "basePath" -> a._1._1,
        "operations" -> {
          (for (t <- a._2) yield { Map("operation" -> t._2, "path" -> t._1) }).toList
        })
    })



    val modelList = new ListBuffer[HashMap[String, AnyRef]]

    models.foreach(m => {
      val json = write(m._2)

      modelList += HashMap(
        "modelName" -> m._1,
        "model" -> m._2,
        "filename" -> config.toModelFilename(m._1),
        "modelJson" -> json,
        "hasMore" -> "true")
    })
    modelList.size match {
      case 0 =>
      case _ => modelList.last.asInstanceOf[HashMap[String, String]] -= "hasMore"
    }

    val data =
      Map(
        "clientName" -> clientName.underscore.pascalize,
        "projectName" -> clientName.underscore.dasherize,
        "package" -> config.packageName,
        "modelPackage" -> config.modelPackage,
        "apiPackage" -> config.apiPackage,
        "apis" -> apiList,
        "models" -> modelList)

    config.supportingFiles.map(file => {
      val supportingFile = file._1
      val outputDir = file._2
      val destFile = file._3

      val outputFilename = outputDir.replaceAll("\\.", File.separator) + File.separator + destFile
      val outputFolder = new File(outputFilename).getParent
      new File(outputFolder).mkdirs

      if (supportingFile.endsWith(".mustache")) {
        val output = {
          val (resourceName, (_, template)) = compileTemplate(supportingFile, rootDir, Some(engine))
          engine.layout(resourceName, template, data.toMap)
        }
        val fw = new FileWriter(outputFilename, false)
        fw.write(output + "\n")
        fw.close()
        println("wrote " + outputFilename)
      } else {
        val file = new File(config.templateDir + File.separator + supportingFile)
        if(file.isDirectory) {
          // copy the whole directory
          FileUtils.copyDirectory(file, new File(outputDir))
          println("copied directory " + supportingFile)
        } else {
          val is = getInputStream(config.templateDir + File.separator + supportingFile)
          val outputFile = new File(outputFilename)
          val parentDir = new File(outputFile.getParent)
          if (parentDir != null && !parentDir.exists) {
            println("making directory: " + parentDir.toString + ": " + parentDir.mkdirs)
          }
          FileUtils.copyInputStreamToFile(is, new File(outputFilename))
          println("copied " + outputFilename)
          is.close
        }
      }
    })
    //a shutdown method will be added to scalate in an upcoming release

    engine.compiler.shutdown()
  }

  override protected def compileTemplate(templateFile: String, rootDir: Option[File] = None, engine: Option[TemplateEngine] = None): (String, (TemplateEngine, Template)) = {
    val eng = engine getOrElse new TemplateEngine(rootDir orElse Some(new File(".")))
    val rn = config.templateDir + File.separator + templateFile
    val rrn = "asyncscala" + File.separator + templateFile
    val resourceName = if (new File(rn).exists) rn else rrn
    val is = getInputStream(resourceName)
    if (is == null)
      throw new Exception("Missing template: " + resourceName)

    val template = eng.compile(TemplateSource.fromText(resourceName,Source.fromInputStream(is).mkString))
    (resourceName, eng -> template)
  }
}

class ScalaAsyncClientGenerator(cfg: SwaggerGenConfig) extends BasicGenerator {
  private[this] val pascalizedClientName = cfg.api.clientName.underscore.pascalize

  override val packageName: String = cfg.api.packageName
  override val templateDir: String = cfg.templateDir.getPath
  override val destinationDir: String = cfg.codeDir.getPath
  override val fileSuffix: String = ".scala"
  override val modelPackage: Option[String] = Some(packageName + ".model")
  override val apiPackage: Option[String] = Some(packageName + ".apis")


  override val reservedWords: Set[String] =
    Set(
      "abstract",
      "case",
      "catch",
      "class",
      "def",
      "do",
      "else",
      "extends",
      "false",
      "final",
      "finally",
      "for",
      "forSome",
      "if",
      "implicit",
      "import",
      "lazy",
      "match",
      "new",
      "null",
      "object",
      "override",
      "package",
      "private",
      "protected",
      "return",
      "sealed",
      "super",
      "this",
      "throw",
      "trait",
      "try",
      "true",
      "type",
      "val",
      "var",
      "while",
      "with",
      "yield")
  override val importMapping = Map(
      "Date" -> "java.util.Date",
      "File" -> "java.io.File"
    ) ++ cfg.defaultImports ++ cfg.api.defaultImports
  override val typeMapping = Map(
      "array" -> "List",
      "boolean" -> "Boolean",
      "string" -> "String",
      "int" -> "Int",
      "long" -> "Long",
      "float" -> "Float",
      "byte" -> "Byte",
      "short" -> "Short",
      "char" -> "Char",
      "long" -> "Long",
      "double" -> "Double",
      "object" -> "Any",
      "file" -> "File") ++ cfg.typeMapping

  override val defaultIncludes = Set(
      "Int",
      "String",
      "Long",
      "Short",
      "Char",
      "Byte",
      "Float",
      "Double",
      "Boolean",
      "AnyRef",
      "Any") ++  cfg.defaultIncludes ++ cfg.api.excludedModels

  override def supportingFiles = List(
    ("client.mustache", destinationDir + "/" + cfg.api.packageName.replace('.', '/'), (pascalizedClientName +".scala")),
    ("sbt.mustache", cfg.projectRoot.getPath, "swagger-client.sbt")
  )

  modelTemplateFiles ++= cfg.api.modelTemplates
  apiTemplateFiles ++= cfg.api.apiTemplates


  codegen = new AsyncClientCodegen(cfg.api.clientName, this, Some(cfg.projectRoot))


  override def getBasePath(host: String, basePath: String): String =
    cfg.api.baseUrl.getOrElse(super.getBasePath(host, basePath))

  override def generateClient(args: Array[String]) = {

    val host = cfg.api.resourceUrl
    val authorization = {
      val apiKey = cfg.api.apiKey
      if(apiKey != None) 
        Some(ApiKeyValue("api_key", "query", apiKey.get))
      else 
        None
    }

    val doc = {
      try {
        ResourceExtractor.fetchListing(getResourcePath(host), authorization)
      } catch {
        case e: Exception => throw new Exception("unable to read from " + host, e)
      }
    }

    implicit val basePath = getBasePath(host, doc.basePath)

    val apiReferences = doc.apis
    if (apiReferences == null)
      throw new Exception("No APIs specified by resource")
    val apis = ApiExtractor.fetchApiListings(doc.swaggerVersion, basePath, apiReferences, authorization)

    new SwaggerSpecValidator(doc, apis).validate()

    val allModels = new mutable.HashMap[String, Model]
    val operations = extractApiOperations(apis, allModels)
    val operationMap = groupOperationsToFiles(operations)
    val modelBundle = prepareModelMap(allModels.toMap)
    val modelFiles = bundleToSource(modelBundle, modelTemplateFiles.toMap)

    modelFiles.map(m => {
      val filename = m._1

      val file = new java.io.File(filename)
      file.getParentFile().mkdirs

      val fw = new FileWriter(filename, false)
      fw.write(m._2 + "\n")
      fw.close()
      println("wrote model " + filename)
    })



    val apiBundle = prepareApiBundle(operationMap.toMap)
    val apiFiles = bundleToSource(apiBundle, apiTemplateFiles.toMap)

    apiFiles.map(m => {
      val filename = m._1

      val file = new java.io.File(filename)
      file.getParentFile().mkdirs

      val fw = new FileWriter(filename, false)
      fw.write(m._2 + "\n")
      fw.close()
      println("wrote api " + filename)
    })

    codegen.writeSupportingClasses(operationMap, allModels.toMap)
  }



  override def extractApiOperations(apiListings: List[ApiListing], allModels: mutable.HashMap[String, Model] )(implicit basePath:String) = {
    val output = new mutable.ListBuffer[(String, String, Operation)]
    apiListings.foreach(apiDescription => {
      val basePath = apiDescription.basePath
      val resourcePath = apiDescription.resourcePath
      if(apiDescription.apis != null) {
        apiDescription.apis.foreach(api => {
          for ((apiPath, operation) <- ApiExtractor.extractApiOperations(basePath, api)) {
            output += ((basePath, apiPath, operation))
          }
        })
      }
      output.map(op => processApiOperation(op._2, op._3))
      allModels ++= CoreUtils.extractApiModels(apiDescription, defaultIncludes, typeMapping)
    })
    output.toList
  }

  override def toModelName(name: String) = toDeclaredType(name.pascalize)

  override def toApiName(name: String) = {
    name.replaceAll("\\{","").replaceAll("\\}", "") match {
      case s: String if(s.length > 0) => s.underscore.pascalize + "Client"
      case _ => "Client"
    }
  }
//
//  override def nameFromPath(apiPath: String) = resourceNameFromFullPath(apiPath)
//
//
//  override def resourceNameFromFullPath(apiPath: String) =
//    apiPath.split('/').head.split('.').head

  /**
   * creates a map of models and properties needed to write source
   */
  override def prepareModelMap(models: Map[String, Model]): List[Map[String, AnyRef]] = {
    for {
      (name, schema) <- (models -- defaultIncludes).toList
      if !(cfg.excludedModelPackages ++ cfg.api.excludedModelPackages).exists(schema.qualifiedType.startsWith)
    } yield {
      Map(
        "name" -> toModelName(name),
        "className" -> name,
        "filename" -> toModelFilename(name),
        "apis" -> None,
        "models" -> List((name, schema)),
        "package" -> modelPackage,
        "invokerPackage" -> invokerPackage,
        "outputDirectory" -> (destinationDir + File.separator + modelPackage.getOrElse("").replaceAll("\\.", File.separator)),
        "newline" -> "\n")
    }
  }

  override def prepareApiBundle(apiMap: Map[(String, String), List[(String, Operation)]] ): List[Map[String, AnyRef]] = {
    for {
      ((basePath, name), operationList) <- apiMap.toList
      className = toApiName(name)
    } yield {
      Map(
        "baseName" -> name,
        "filename" -> toApiFilename(name),
        "name" -> toApiName(name),
        "className" -> className,
        "basePath" -> basePath,
        "package" -> apiPackage,
        "invokerPackage" -> invokerPackage,
        "apis" -> Map(className -> operationList.toList),
        "models" -> None,
        "outputDirectory" -> (destinationDir + File.separator + apiPackage.getOrElse("").replaceAll("\\.", File.separator)),
        "newline" -> "\n")
    }
  }

  override def bundleToSource(bundle:List[Map[String, AnyRef]], templates: Map[String, String]): List[(String, String)] = {
    bundle.foldLeft(List.empty[(String, String)]) { (acc, m) =>
      templates.foldLeft(acc) { (out, tem) =>
        val (file, suffix) = tem
        (m("outputDirectory").toString + File.separator + m("filename").toString + suffix) -> codegen.generateSource(m, file) :: acc
      }
    }
  }

  override def generateAndWrite(bundle: Map[String, AnyRef], templateFile: String) = {
    val output = codegen.generateSource(bundle, templateFile)
    val outputDir = new File(bundle("outputDirectory").asInstanceOf[String])
    outputDir.mkdirs

    val filename = outputDir + File.separator + bundle("filename")
    val fw = new FileWriter(filename, false)
    fw.write(output + "\n")
    fw.close()
    println("wrote " + filename)
  }

  override def groupOperationsToFiles(operations: List[(String, String, Operation)]): Map[(String, String), List[(String, Operation)]] = {
    val opMap = new mutable.HashMap[(String, String), mutable.ListBuffer[(String, Operation)]]
    for ((basePath, apiPath, operation) <- operations) {
      val className = resourceNameFromFullPath(apiPath)
      if (!cfg.api.excludedApis.exists(_.equalsIgnoreCase(className))) {
        val listToAddTo = opMap.getOrElse((basePath, className), {
          val l = new mutable.ListBuffer[(String, Operation)]
          opMap += (basePath, className) -> l
          l
        })
        listToAddTo += (apiPath -> operation)
      }
    }
    opMap.map(m => (m._1, m._2.toList)).toMap
  }


  // response classes--if you don't want a response class, override and set to None
  override def processResponseClass(responseClass: String): Option[String] = {
    responseClass match {
      case "void" => None //Some("Unit")
      case e: String => Some(toDeclaredType(e))
    }
  }

  override def processResponseDeclaration(responseClass: String): Option[String] = {
    responseClass match {
      case "void" => None //Some("Unit")
      case e: String => Some(toDeclaredType(e))
    }
  }

  override def toDeclaredType(dt: String): String = {
    val declaredType = (dt.indexOf("[")) match {
      case -1 => dt
      case n: Int => {
        if (dt.substring(0, n).toLowerCase == "array") {
          val dtt = dt.substring(n + 1, dt.length - 1)
          "List[%s]".format(typeMapping.getOrElse(dtt, dtt))
        } else dt
      }
    }
    typeMapping.getOrElse(declaredType, declaredType)
  }

  override def toDeclaration(obj: ModelProperty): (String, String) = {
    obj.`type` match {
      case "Array" | "array" =>  makeContainerType(obj, "List")
      case "Set" | "set" => makeContainerType(obj, "Set")
      case e: String => (toDeclaredType(e), toDefaultValue(e, obj))
    }
  }


  private def makeContainerType(obj: ModelProperty, container: String): (String, String) = {
    val inner = {
      obj.items match {
        case Some(items) => items.ref.getOrElse(items.`type`)
        case _ => throw new Exception("no inner type defined")
      }
    }
    val e = "%s[%s]" format (container, toDeclaredType(inner))
    (e, toDefaultValue(inner, obj))
  }

  // escape keywords
  override def escapeReservedWord(word: String) = "`" + word + "`"

}
