package com.wordnik.swagger.model

import com.wordnik.swagger.codegen.spec.ValidationMessage
import legacy.LegacySerializers

import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods._
import org.json4s.jackson.Serialization.{read, write}

import scala.collection.mutable.{ListBuffer, LinkedHashMap}

object SwaggerSerializers {
  import ValidationMessage._

  val jsonSchemaTypeMap = Map(
    // simple types
    ("integer", "int32") -> "int",
    ("integer", "int64") -> "long",
    ("number", "float") -> "float",
    ("number", "double") -> "double",
    ("string", "byte") -> "byte",
    ("string", "date") -> "Date",
    ("string", "date-time") -> "Date",

    // containers
    ("array", "") -> "Array",
    ("set", "") -> "Set"
  )

  def toJsonSchema(name: String, `type`: String): JObject = {
    `type` match {
      case "int"       => (name -> "integer") ~ ("format" -> "int32")
      case "long"      => (name -> "integer") ~ ("format" -> "int64")
      case "float"     => (name -> "number")  ~ ("format" -> "float")
      case "double"    => (name -> "number")  ~ ("format" -> "double")
      case "string"    => (name -> "string")  ~ ("format" -> JNothing)
      case "byte"      => (name -> "string")  ~ ("format" -> "byte")
      case "boolean"   => (name -> "boolean") ~ ("format" -> JNothing)
      case "Date"      => (name -> "string")  ~ ("format" -> "date-time")
      case "date"      => (name -> "string")  ~ ("format" -> "date")
      case "date-time" => (name -> "string")  ~ ("format" -> "date-time")
      case "Array"     => (name -> "array")
      case _           => {
        val ComplexTypeMatcher = "([a-zA-Z]*)\\[([a-zA-Z\\.\\-]*)\\].*".r
        `type` match {
          case ComplexTypeMatcher(container, value) => 
            toJsonSchemaContainer(container) ~ {
              ("items" -> {if(isSimpleType(value))
                  toJsonSchema("type", value)
                else
                  toJsonSchema("$ref", value)})
            }
          case _ => (name -> `type`)    ~ ("format" -> JNothing)
        }
      }
    }
  }

  def toJsonSchemaContainer(name: String): JObject = {
    name match {
      case "List"      => ("type" -> "array")   ~ ("format" -> JNothing)
      case "Array"     => ("type" -> "array")   ~ ("format" -> JNothing)
      case "Set"       => ("type" -> "array")   ~ ("uniqueItems" -> true)
      case _           => ("type" -> JNothing)
    }
  }

  def isSimpleType(name: String) = {
    Set("int", "long", "float", "double", "string", "byte", "boolean", "Date", "date", "date-time", "array", "set").contains(name)
  }

  def formats(version: String) = {
    version match {
      case "1.1" => LegacySerializers.formats
      case "1.2" => {
        DefaultFormats + 
          new ModelSerializer + 
          new ModelPropertySerializer +
          new ModelRefSerializer + 
          new AllowableValuesSerializer + 
          new ParameterSerializer +
          new OperationSerializer +
          new ResponseMessageSerializer +
          new ApiDescriptionSerializer +
          new ApiListingReferenceSerializer +
          new ResourceListingSerializer +
          new ApiListingSerializer
      }
      case _ => throw new IllegalArgumentException("%s is not a valid Swagger version".format(version))
    }
  }

  def validationMessages = ValidationMessage.validationMessages

  def !!(element: AnyRef, elementType: String, elementId: String, message: String, level: String = ERROR) {
    val msg = new ValidationMessage(element, elementType, elementId, message, level)
    ValidationMessage.validationMessages += msg
  }

  class ApiListingSerializer extends CustomSerializer[ApiListing](implicit formats => ({
    case json =>
      val authorizations = (json \ "authorizations").extractOpt[Map[String, AuthorizationType]] match {
        case Some(m) => m.values.toList
        case _ => List.empty
      }
      ApiListing(
        (json \ "apiVersion").extractOrElse({
          !!(json, RESOURCE, "apiVersion", "missing required field", ERROR)
          ""
        }),
        (json \ "swaggerVersion").extractOrElse({
          !!(json, RESOURCE, "swaggerVersion", "missing required field", ERROR)
          ""
        }),
        (json \ "basePath").extractOrElse({
          !!(json, RESOURCE, "basePath", "missing required field", ERROR)
          ""
        }),
        (json \ "resourcePath").extractOrElse({
          !!(json, RESOURCE, "resourcePath", "missing recommended field", WARNING)
          ""
        }),
        (json \ "produces").extract[List[String]],
        (json \ "consumes").extract[List[String]],
        (json \ "protocols").extract[List[String]],
        authorizations,
        (json \ "apis").extract[List[ApiDescription]],
        (json \ "models").extractOpt[Map[String, Model]],
        (json \ "description").extractOpt[String],
        (json \ "position").extractOrElse(0)
      )
    }, {
      case x: ApiListing =>
      ("apiVersion" -> x.apiVersion) ~
      ("resourcePath" -> x.resourcePath) ~
      ("swaggerVersion" -> x.swaggerVersion) ~
      ("basePath" -> x.basePath) ~
      ("apis" -> {
        x.apis match {
          case e: List[ApiDescription] if (e.size > 0) => Extraction.decompose(e)
          case _ => JNothing
        }
      }) ~
      ("models" -> {
        x.models match {
          case Some(e) if (e.size > 0) => Extraction.decompose(e)
          case _ => JNothing
        }
      })
    }
  ))

  class ResourceListingSerializer extends CustomSerializer[ResourceListing](implicit formats => ({
    case json =>

      val apis = (json \ "apis").extract[List[ApiListingReference]]

      ResourceListing(
        (json \ "apiVersion").extractOrElse({
          !!(json, RESOURCE_LISTING, "apiVersion", "missing required field", ERROR)
          ""
        }),
        (json \ "swaggerVersion").extractOrElse({
          !!(json, RESOURCE_LISTING, "swaggerVersion", "missing required field", ERROR)
          ""
        }),
        "",
        apis.filter(a => a.path != "" && a.path != null)
      )
    }, {
      case x: ResourceListing =>
      ("apiVersion" -> x.apiVersion) ~
      ("swaggerVersion" -> x.swaggerVersion) ~
      ("apis" -> {
        x.apis match {
          case e: List[ApiListingReference] if (e.size > 0) => Extraction.decompose(e)
          case _ => JNothing
        }
      })
    }
  ))

  class ApiListingReferenceSerializer extends CustomSerializer[ApiListingReference](implicit formats => ({
    case json =>
      ApiListingReference(
        (json \ "path").extractOrElse({
          !!(json, RESOURCE, "path", "missing required field", ERROR)
          ""
        }),
        (json \ "description").extractOpt[String]
      )
    }, {
      case x: ApiListingReference =>
      ("path" -> x.path) ~
      ("description" -> x.description)
    }
  ))

  class ApiDescriptionSerializer extends CustomSerializer[ApiDescription](implicit formats => ({
    case json =>
      ApiDescription(
        (json \ "path").extractOrElse({
          !!(json, RESOURCE_LISTING, "path", "missing required field", ERROR)
          ""
        }),
        (json \ "description").extractOpt[String],
        (json \ "operations").extract[List[Operation]]
      )
    }, {
      case x: ApiDescription =>
      ("path" -> x.path) ~
      ("description" -> x.description) ~
      ("operations" -> {
        x.operations match {
          case e:List[Operation] if(e.size > 0) => Extraction.decompose(e)
          case _ => JNothing
        }
      })
    }
  ))

  class ResponseMessageSerializer extends CustomSerializer[ResponseMessage](implicit formats => ({
    case json =>
      ResponseMessage(
        (json \ "code").extractOrElse({
          !!(json, ERROR, "code", "missing required field", ERROR)
          0
        }),
        (json \ "message").extractOrElse({
          !!(json, ERROR, "reason", "missing required field", ERROR)
          ""
        })
      )
    }, {
      case x: ResponseMessage =>
      ("code" -> x.code) ~
      ("message" -> x.message)
    }
  ))

  class OperationSerializer extends CustomSerializer[Operation](implicit formats => ({
    case json =>

      val authorizations = (json \ "authorizations").extractOpt[Map[String, AuthorizationType]] match {
        case Some(m) => m.values.toList
        case _ => List.empty
      }
      val t =  SwaggerSerializers.jsonSchemaTypeMap.getOrElse(
            ((json \ "type").extractOrElse(""), (json \ "format").extractOrElse(""))
          , (json \ "type").extractOrElse(""))

      val inner = {
        val items = new scala.collection.mutable.HashSet[String]
        val map = new scala.collection.mutable.HashMap[String, String]
        (json \ "items") match {
          case JObject(e) => {
            for(a <- e) {
              a._2 match {
                case e: JString => map += a._1 -> e.s
                case _ =>
              }
            }
            val `type` = map.getOrElse("type", "")
            val format = map.getOrElse("format", "")
            if(map.contains("$ref")) {
              Some(map("$ref"))
            }
            else
              Option(jsonSchemaTypeMap.getOrElse((`type`,format), `type`))
          }
          case _ => None
        }
      }
      val responseClass = inner match {
        case Some(a) => "%s[%s]".format(t, a)
        case _ => t
      }

      if(responseClass == "" || responseClass == null){
        !!(json, OPERATION, "responseClass", "missing required field", ERROR)
      }

      Operation(
        (json \ "httpMethod").extractOrElse(
          (json \ "method").extractOrElse({
            !!(json, OPERATION, "method", "missing required field", ERROR)
            ""
          })
        ),
        (json \ "summary").extract[String],
        (json \ "notes").extractOrElse(""),
        responseClass,
        (json \ "nickname").extractOrElse({
          !!(json, OPERATION, "nickname", "missing required field", ERROR)
          ""
        }),
        (json \ "position").extractOrElse(0),
        (json \ "produces").extract[List[String]],
        (json \ "consumes").extract[List[String]],
        (json \ "protocols").extract[List[String]],
        authorizations,
        (json \ "parameters").extract[List[Parameter]],
        (json \ "responseMessages").extract[List[ResponseMessage]],
        (json \ "deprecated").extractOpt[String]
      )
    }, {
      case x: Operation =>

      val ComplexTypeMatcher = "([a-zA-Z]*)\\[([a-zA-Z\\.\\-]*)\\].*".r
      val output = x.responseClass match {
        case ComplexTypeMatcher(container, value) => 
          toJsonSchemaContainer(container) ~ {
            ("items" -> {if(isSimpleType(value))
                toJsonSchema("type", value)
              else
                toJsonSchema("$ref", value)})
          }
        case _ => toJsonSchema("type", x.responseClass)    ~ ("format" -> JNothing)
      }

      ("method" -> x.method) ~
      ("summary" -> x.summary) ~
      ("notes" -> x.notes) ~
      output ~
      ("nickname" -> x.nickname) ~
      ("parameters" -> Extraction.decompose(x.parameters)) ~
      ("responseMessages" -> {
        x.responseMessages match {
          case e: List[ResponseMessage] if(e.size > 0) => Extraction.decompose(e)
          case _ => JNothing
        }
      }) ~
      ("deprecated" -> x.`deprecated`)
    }
  ))

  class ParameterSerializer extends CustomSerializer[Parameter](formats => ({
    case json =>
      implicit val fmts: Formats = formats

      val output = new ListBuffer[String]
      (json \ "enum") match {
        case JArray(entries) => entries.map {
          case e: JInt => output += e.num.toString
          case e: JBool => output += e.value.toString
          case e: JString => output += e.s
          case e: JDouble => output += e.num.toString
          case _ =>
        }
        case _ =>
      }
      val allowableValues = {
        if(output.size > 0) AllowableListValues(output.toList)
        else {
          val min = (json \ "min") match {
            case e: JObject => e.extract[String]
            case e: JString => e.s
            case e: JInt => e.num.toString
            case e: JDouble => e.num.toString          
            case _ => ""
          }
          val max = (json \ "max") match {
            case e: JObject => e.extract[String]
            case e: JString => e.s
            case e: JInt => e.num.toString
            case e: JDouble => e.num.toString          
            case _ => ""
          }
          if(min != "" && max != "")
            AllowableRangeValues(min, max)
          else
            AnyAllowableValues
        }
      }

      val t =  SwaggerSerializers.jsonSchemaTypeMap.getOrElse(
            ((json \ "type").extractOrElse(""), (json \ "format").extractOrElse(""))
          , (json \ "type").extractOrElse(""))

      val inner = {
        val items = new scala.collection.mutable.HashSet[String]
        val map = new scala.collection.mutable.HashMap[String, String]
        (json \ "items") match {
          case JObject(e) => {
            for(a <- e) {
              a._2 match {
                case e: JString => map += a._1 -> e.s
                case _ =>
              }
            }
            val `type` = map.getOrElse("type", "")
            val format = map.getOrElse("format", "")
            if(map.contains("$ref")) {
              Some(map("$ref"))
            }
            else
              Option(jsonSchemaTypeMap.getOrElse((`type`,format), `type`))
          }
          case _ => None
        }
      }
      val `type` = inner match {
        case Some(a) => "%s[%s]".format(t, a)
        case _ => t
      }
      Parameter(
        name = (json \ "name").extractOrElse({
          !!(json, OPERATION_PARAM, "reason", "missing parameter name", WARNING)
          ""
        }),
        description = (json \ "description").extractOpt[String],
        defaultValue = (json \ "defaultValue") match {
          case e:JInt => Some(e.num.toString)
          case e:JBool => Some(e.value.toString)
          case e:JString => Some(e.s)
          case e:JDouble => Some(e.num.toString)
          case _ => None
        },
        required = (json \ "required") match {
          case e:JString => e.s.toBoolean
          case e:JBool => e.value
          case _ => false
        },
        allowMultiple = (json \ "allowMultiple").extractOrElse(false),
        dataType = `type`,
        allowableValues = allowableValues,
        paramType = (json \ "paramType").extractOrElse({
          !!(json, OPERATION_PARAM, "paramType", "missing required field", ERROR)
          ""
        })
      )
    }, {
      case x: Parameter =>
      implicit val fmts = formats
      val output = ("name" -> x.name) ~
      ("description" -> x.description) ~
      ("defaultValue" -> x.defaultValue) ~
      ("required" -> x.required) ~
      ("allowMultiple" -> x.allowMultiple) ~
      toJsonSchema("type", x.dataType) ~
      ("paramType" -> x.paramType)

      x.allowableValues match {
        case AllowableListValues(values, "LIST") => 
          output ~ ("enum" -> Extraction.decompose(values))
        case AllowableRangeValues(min, max)  => 
          output ~ ("minimum" -> min) ~ ("maximum" -> max)
        case _ => output
      }
    }
  ))

  class ModelSerializer extends CustomSerializer[Model](implicit formats => ({
    case json =>
      val output = new LinkedHashMap[String, ModelProperty]
      val required = (json \ "required").extract[Set[String]]
      json \ "properties" match {
        case JObject(entries) => {
          entries.map({
            case (key, value) => {
              val prop = value.extract[ModelProperty]
              if(required.contains(key))
                output += key -> prop.copy(required = true)
              else
                output += key -> prop
            }
          })
        }
        case _ =>
      }

      Model(
        (json \ "id").extractOrElse({
          !!(json, MODEL, "id", "missing required field", ERROR)
          ""
        }),
        (json \ "name").extractOrElse(""),
        (json \ "qualifiedType").extractOrElse((json \ "id").extractOrElse("")),
        output,
        (json \ "description").extractOpt[String]
      )
    }, {
    case x: Model =>
      val required: List[String] = (for((name, prop) <- x.properties) yield {
        if(prop.required) Some(name)
        else None
      }).flatten.toList

      ("id" -> x.id) ~
      ("name" -> x.name) ~
      ("required" -> (required.size match {
        case 0 => JNothing
        case _ => Extraction.decompose(required)
      })) ~
      ("properties" -> {
        (x.properties: @unchecked) match {
          case e: LinkedHashMap[String, ModelProperty] => Extraction.decompose(e.toMap)
          case _ => JNothing
        }
      })
    }
  ))

  class ModelPropertySerializer extends CustomSerializer[ModelProperty] (implicit formats => ({
    case json =>
      val `type` = (json \ "$ref") match {
        case e: JString => e.s
        case _ => {
          // convert the jsonschema types into swagger types.  Note, this logic will move elsewhere soon
          val t = SwaggerSerializers.jsonSchemaTypeMap.getOrElse(
            ((json \ "type").extractOrElse(""), (json \ "format").extractOrElse(""))
          , (json \ "type").extractOrElse(""))
          val isUnique = (json \ "uniqueItems") match {
            case e: JBool => e.value
            case e: JString => e.s.toBoolean
            case _ => false
          }
          if(t == "Array" && isUnique) "Set"
          else t
        }
      }

      val output = new ListBuffer[String]
      json \ "enum" match {
        case JArray(entries) => entries.map {
          case e: JInt => output += e.num.toString
          case e: JBool => output += e.value.toString
          case e: JString => output += e.s
          case e: JDouble => output += e.num.toString
          case _ =>
        }
        case _ =>
      }
      val allowableValues = {
        if(output.size > 0) AllowableListValues(output.toList)
        else {
          val min = (json \ "min") match {
            case e: JObject => e.extract[String]
            case _ => ""
          }
          val max = (json \ "max") match {
            case e: JObject => e.extract[String]
            case _ => ""
          }
          if(min != "" && max != "")
            AllowableRangeValues(min, max)
          else
            AnyAllowableValues
        }
      }
      ModelProperty(
        `type` = `type`,
        `qualifiedType` = (json \ "qualifiedType").extractOpt[String].getOrElse(`type`),
        required = (json \ "required") match {
          case e:JString => e.s.toBoolean
          case e:JBool => e.value
          case _ => false
        },
        description = (json \ "description").extractOpt[String],
        allowableValues = allowableValues,
        items = {
          (json \ "items").extractOpt[ModelRef] match {
            case Some(e: ModelRef) if(e.`type` != null || e.ref != None) => Some(e)
            case _ => None
          }
        }
      )
    }, {
    case x: ModelProperty =>
      val output = toJsonSchema("type", x.`type`) ~
      ("description" -> x.description) ~
      ("items" -> Extraction.decompose(x.items))

      x.allowableValues match {
        case AllowableListValues(values, "LIST") => 
          output ~ ("enum" -> Extraction.decompose(values))
        case AllowableRangeValues(min, max)  => 
          output ~ ("minimum" -> min) ~ ("maximum" -> max)
        case _ => output
      }
    }
  ))

  class ModelRefSerializer extends CustomSerializer[ModelRef](implicit formats => ({
    case json =>

      val `type` = (json \ "type") match {
        case e: JString => e.s
        case _ => ""
      }
      val format = (json \ "format") match {
        case e: JString => e.s
        case _ => ""
      }
      val jsonSchemaType = jsonSchemaTypeMap.getOrElse((`type`, format), `type`)

      ModelRef(
        jsonSchemaType match {
          case e: String if(e != "") => e
          case _ => null
        },
        (json \ "$ref").extractOpt[String]
      )
    }, {
      case x: ModelRef =>
      ("type" -> {
        x.`type` match {
          case e:String => Some(e)
          case _ => None
        }
      }) ~
      ("$ref" -> x.ref)
    }
  ))

  class AllowableValuesSerializer extends CustomSerializer[AllowableValues](implicit formats => ({
    case json =>
      json \ "valueType" match {
        case JString(x) if x.equalsIgnoreCase("list") => {
          val output = new ListBuffer[String]
          val properties = (json \ "values") match {
            case JArray(entries) => entries.map {
              case e:JInt => output += e.num.toString
              case e:JBool => output += e.value.toString
              case e:JString => output += e.s
              case e:JDouble => output += e.num.toString
              case _ =>
            }
            case _ =>
          }
          AllowableListValues(output.toList)
        }
        case JString(x) if x.equalsIgnoreCase("range") =>
          AllowableRangeValues((json \ "min").extract[String], (json \ "max").extract[String])
        case _ => AnyAllowableValues
      }
    }, {
      case AllowableListValues(values, "LIST") => 
        ("valueType" -> "LIST") ~ ("values" -> Extraction.decompose(values))
      case AllowableRangeValues(min, max)  => 
        ("valueType" -> "RANGE") ~ ("min" -> min) ~ ("max" -> max)
    }
  ))
}
