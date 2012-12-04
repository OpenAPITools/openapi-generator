package com.wordnik.swagger.model

import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods._
import org.json4s.native.Serialization.{read, write}

import scala.collection.mutable.HashMap
import scala.collection.JavaConverters._

object SwaggerSerializers {
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
    new ResourceListingSerializer

  class ResourceListingSerializer extends CustomSerializer[ResourceListing](formats => ({
    case json =>
      implicit val fmts: Formats = formats
      ResourceListing(
        (json \ "apiVersion").extract[String],
        (json \ "swaggerVersion").extract[String],
        (json \ "basePath").extract[String],
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
        (json \ "path").extract[String],
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
        (json \ "path").extract[String],
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
        (json \ "code").extract[Int],
        (json \ "reason").extract[String]
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
        (json \ "httpMethod").extract[String],
        (json \ "summary").extract[String],
        (json \ "notes").extract[String],
        (json \ "responseClass").extract[String],
        (json \ "nickname").extract[String],
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
        (json \ "name").extract[String],
        (json \ "description").extract[String],
        (json \ "defaultValue").extract[String],
        (json \ "required").extractOrElse(false),
        (json \ "allowMultiple").extractOrElse(false),
        (json \ "dataType").extract[String],
        (json \ "allowableValues").extract[AllowableValuesFoo],
        (json \ "paramType").extract[String]
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
          case e:AllowableValuesFoo => Extraction.decompose(x.allowableValues)
          case _ => JNothing
        }
      }) ~
      ("paramType" -> x.paramType)
    }
  ))

  class ModelSerializer extends CustomSerializer[Model](formats => ({
    case json =>
      implicit val fmts: Formats = formats
      val output = new HashMap[String, ModelProperty]
      val properties = (json \ "properties") match {
        case JObject(entries) => {
          entries.map({
            case (key, value) => output += key -> value.extract[ModelProperty]
          })
        }
        case _ =>
      }

      Model(
        (json \ "id").extract[String],
        (json \ "name").extract[String],
        output.asJava,
        (json \ "description").extractOpt[String]
      )
    }, {
    case x: Model =>
      implicit val fmts = formats
      ("id" -> x.id) ~
      ("name" -> x.name) ~
      ("properties" -> {
        x.properties match {
          case e:java.util.Map[String, ModelProperty] => Extraction.decompose(e.asScala.toMap)
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
        required = ((json \ "required").extractOrElse(false)),
        description = (json \ "description").extractOpt[String],
        allowableValues = (json \ "allowableValues").extract[AllowableValuesFoo],
        items = (json \ "items").extractOpt[ModelRef]
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
          case e:AllowableValuesFoo => Extraction.decompose(x.allowableValues)
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
        (json \ "$ref").extract[String],
        (json \ "type").extract[String]
      )
    }, {
      case x: ModelRef =>
      implicit val fmts = formats
      ("$ref" -> x.ref) ~
      ("type" -> x.`type`)
    }
  ))

  class AllowableValuesSerializer extends CustomSerializer[AllowableValuesFoo](formats => ({
    case json =>
      implicit val fmts: Formats = formats
      json \ "valueType" match {
        case JString(x) if x.equalsIgnoreCase("list") =>
          AllowableListValues((json \ "values").extract[List[String]])
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