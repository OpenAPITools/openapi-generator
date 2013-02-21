package com.wordnik.swagger.model

import com.wordnik.swagger.codegen.spec.ValidationMessage

import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods._
import org.json4s.jackson.Serialization.{read, write}

import scala.collection.mutable.{ListBuffer, LinkedHashMap}

object SwaggerSerializers {
  import ValidationMessage._

  implicit val formats = DefaultFormats + 
    new ModelSerializer + 
    new ModelPropertySerializer +
    new ModelRefSerializer + 
    new AllowableValuesSerializer + 
    new ParameterSerializer +
    new OperationSerializer +
    new ErrorResponseSerializer +
    new ApiDescriptionSerializer +
    new ApiListingReferenceSerializer +
    new ResourceListingSerializer +
    new ApiListingSerializer

  val validationMessages = ListBuffer.empty[ValidationMessage]

  def !!(element: AnyRef, elementType: String, elementId: String, message: String, level: String = ERROR) {
    val msg = new ValidationMessage(element, elementType, elementId, message, level)
    validationMessages += msg
  }

  class ApiListingSerializer extends CustomSerializer[ApiListing](formats => ({
    case json =>
      implicit val fmts: Formats = formats
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
        (json \ "apis").extract[List[ApiDescription]],
        (json \ "models").extract[Map[String, Model]]
      )
    }, {
      case x: ApiListing =>
      implicit val fmts = formats
      ("apiVersion" -> x.apiVersion) ~
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
          case e: Map[String, Model] if (e.size > 0) => Extraction.decompose(e)
          case _ => JNothing
        }
      })
    }
  ))

  class ResourceListingSerializer extends CustomSerializer[ResourceListing](formats => ({
    case json =>
      implicit val fmts: Formats = formats
      ResourceListing(
        (json \ "apiVersion").extractOrElse({
          !!(json, RESOURCE_LISTING, "apiVersion", "missing required field", ERROR)
          ""
        }),
        (json \ "swaggerVersion").extractOrElse({
          !!(json, RESOURCE_LISTING, "swaggerVersion", "missing required field", ERROR)
          ""
        }),
        (json \ "basePath").extractOrElse({
          !!(json, RESOURCE_LISTING, "basePath", "missing required field", ERROR)
          ""
        }),
        (json \ "apis").extract[List[ApiListingReference]]
      )
    }, {
      case x: ResourceListing =>
      implicit val fmts = formats
      ("apiVersion" -> x.apiVersion) ~
      ("swaggerVersion" -> x.swaggerVersion) ~
      ("basePath" -> x.basePath) ~
      ("apis" -> {
        x.apis match {
          case e: List[ApiListingReference] if (e.size > 0) => Extraction.decompose(e)
          case _ => JNothing
        }
      })
    }
  ))

  class ApiListingReferenceSerializer extends CustomSerializer[ApiListingReference](formats => ({
    case json =>
      implicit val fmts: Formats = formats
      ApiListingReference(
        (json \ "path").extractOrElse({
          !!(json, RESOURCE, "path", "missing required field", ERROR)
          ""
        }),
        (json \ "description").extractOrElse("")
      )
    }, {
      case x: ApiListingReference =>
      implicit val fmts = formats
      ("path" -> x.path) ~
      ("description" -> x.description)
    }
  ))

  class ApiDescriptionSerializer extends CustomSerializer[ApiDescription](formats => ({
    case json =>
      implicit val fmts: Formats = formats
      ApiDescription(
        (json \ "path").extractOrElse({
          !!(json, RESOURCE_LISTING, "path", "missing required field", ERROR)
          ""
        }),
        (json \ "description").extractOrElse(""),
        (json \ "operations").extract[List[Operation]]
      )
    }, {
      case x: ApiDescription =>
      implicit val fmts = formats
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

  class ErrorResponseSerializer extends CustomSerializer[ErrorResponse](formats => ({
    case json =>
      implicit val fmts: Formats = formats
      ErrorResponse(
        (json \ "code").extractOrElse({
          !!(json, ERROR, "code", "missing required field", ERROR)
          0
        }),
        (json \ "reason").extractOrElse({
          !!(json, ERROR, "reason", "missing required field", ERROR)
          ""
        })
      )
    }, {
      case x: ErrorResponse =>
      implicit val fmts = formats
      ("code" -> x.code) ~
      ("reason" -> x.reason)
    }
  ))

  class OperationSerializer extends CustomSerializer[Operation](formats => ({
    case json =>
      implicit val fmts: Formats = formats
      Operation(
        (json \ "httpMethod").extractOrElse({
          !!(json, OPERATION, "httpMethod", "missing required field", ERROR)
          ""
        }),
        (json \ "summary").extract[String],
        (json \ "notes").extractOrElse(""),
        (json \ "responseClass").extractOrElse({
          !!(json, OPERATION, "responseClass", "missing required field", ERROR)
          ""
        }),
        (json \ "nickname").extractOrElse({
          !!(json, OPERATION, "nickname", "missing required field", ERROR)
          ""
        }),
        (json \ "parameters").extract[List[Parameter]],
        (json \ "errorResponses").extract[List[ErrorResponse]],
        (json \ "deprecated").extractOpt[String]
      )
    }, {
      case x: Operation =>
      implicit val fmts = formats
      ("httpMethod" -> x.httpMethod) ~
      ("summary" -> x.summary) ~
      ("notes" -> x.notes) ~
      ("responseClass" -> x.responseClass) ~
      ("nickname" -> x.nickname) ~
      ("parameters" -> Extraction.decompose(x.parameters)) ~
      ("errorResponses" -> {
        x.errorResponses match {
          case e: List[ErrorResponse] if(e.size > 0) => Extraction.decompose(e)
          case _ => JNothing
        }
      }) ~
      ("deprecated" -> x.`deprecated`)
    }
  ))

  class ParameterSerializer extends CustomSerializer[Parameter](formats => ({
    case json =>
      implicit val fmts: Formats = formats
      Parameter(
        (json \ "name").extractOrElse({
          !!(json, OPERATION_PARAM, "reason", "missing parameter name", WARNING)
          ""
        }),
        (json \ "description").extractOrElse({
          !!(json, OPERATION_PARAM, "description", "missing recommended field", WARNING)
          ""
        }),
        (json \ "defaultValue") match {
          case e:JInt => e.num.toString
          case e:JBool => e.value.toString
          case e:JString => e.s
          case e:JDouble => e.num.toString
          case _ => ""
        },
        (json \ "required") match {
          case e:JString => e.s.toBoolean
          case e:JBool => e.value
          case _ => false
        },
        (json \ "allowMultiple").extractOrElse(false),
        (json \ "dataType").extractOrElse({
          !!(json, OPERATION_PARAM, "dataType", "missing required field", ERROR)
          ""
        }),
        (json \ "allowableValues").extract[AllowableValues],
        (json \ "paramType").extractOrElse({
          !!(json, OPERATION_PARAM, "paramType", "missing required field", ERROR)
          ""
        })
      )
    }, {
      case x: Parameter =>
      implicit val fmts = formats
      ("name" -> x.name) ~
      ("description" -> x.description) ~
      ("defaultValue" -> x.defaultValue) ~
      ("required" -> x.required) ~
      ("allowMultiple" -> x.allowMultiple) ~
      ("dataType" -> x.dataType) ~
      ("allowableValues" -> {
        x.allowableValues match {
          case Any => JNothing // don't serialize when not a concrete type
          case e:AllowableValues => Extraction.decompose(x.allowableValues)
          case _ => JNothing
        }
      }) ~
      ("paramType" -> x.paramType)
    }
  ))

  class ModelSerializer extends CustomSerializer[Model](formats => ({
    case json =>
      implicit val fmts: Formats = formats
      val output = new LinkedHashMap[String, ModelProperty]
      val properties = (json \ "properties") match {
        case JObject(entries) => {
          entries.map({
            case (key, value) => output += key -> value.extract[ModelProperty]
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
        output,
        (json \ "description").extractOpt[String]
      )
    }, {
    case x: Model =>
      implicit val fmts = formats
      ("id" -> x.id) ~
      ("name" -> x.name) ~
      ("properties" -> {
        x.properties match {
          case e: LinkedHashMap[String, ModelProperty] => Extraction.decompose(e.toMap)
          case _ => JNothing
        }
      })
    }
  ))

  class ModelPropertySerializer extends CustomSerializer[ModelProperty] (formats => ({
    case json =>
      implicit val fmts: Formats = formats
      ModelProperty(
        `type` = (json \ "type").extractOrElse(""),
        (json \ "required") match {
          case e:JString => e.s.toBoolean
          case e:JBool => e.value
          case _ => false
        },
        description = (json \ "description").extractOpt[String],
        allowableValues = (json \ "allowableValues").extract[AllowableValues],
        items = {
          (json \ "items").extractOpt[ModelRef] match {
            case Some(e: ModelRef) if(e.`type` != null || e.ref != None) => Some(e)
            case _ => None
          }
        }
      )
    }, {
    case x: ModelProperty =>
      implicit val fmts = formats
      ("type" -> x.`type`) ~
      ("required" -> x.required) ~
      ("description" -> x.description) ~
      ("allowableValues" -> {
        x.allowableValues match {
          case Any => JNothing // don't serialize when not a concrete type
          case e:AllowableValues => Extraction.decompose(x.allowableValues)
          case _ => JNothing
        }
      }) ~
      ("items" -> Extraction.decompose(x.items))
    }
  ))

  class ModelRefSerializer extends CustomSerializer[ModelRef](formats => ({
    case json =>
      implicit val fmts: Formats = formats
      ModelRef(
        (json \ "type").extractOrElse(null: String),
        (json \ "$ref").extractOpt[String]
      )
    }, {
      case x: ModelRef =>
      implicit val fmts = formats
      ("type" -> {
        x.`type` match {
          case e:String => Some(e)
          case _ => None
        }
      }) ~
      ("$ref" -> x.ref)
    }
  ))

  class AllowableValuesSerializer extends CustomSerializer[AllowableValues](formats => ({
    case json =>
      implicit val fmts: Formats = formats
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
        case _ => Any
      }
    }, {
      case AllowableListValues(values, "LIST") => 
        implicit val fmts = formats
        ("valueType" -> "LIST") ~ ("values" -> Extraction.decompose(values))
      case AllowableRangeValues(min, max)  => 
        ("valueType" -> "RANGE") ~ ("min" -> min) ~ ("max" -> max)
    }
  ))
}