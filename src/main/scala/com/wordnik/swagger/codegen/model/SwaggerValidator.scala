package com.wordnik.swagger.codegen.model

import scala.collection.mutable.ListBuffer

case class ValidationError (
  path: String,
  message: String,
  severity: String
)

object SwaggerValidator {
  val ERROR = "ERROR"
  val WARNING = "WARNING"

  def validate (resource: ResourceListing): List[ValidationError] = {
    val errors = new ListBuffer[ValidationError]
    
    if(resource.apiVersion == "")
      errors += ValidationError("resourceListing", "apiVersion", ERROR)
    if(resource.swaggerVersion == "")
      errors += ValidationError("resourceListing", "apiVersion", ERROR)
    for(api <- resource.apis)
      validate(api, errors, "resourceListing")
    errors.toList
  }

  def validate(ref: ApiListingReference, errors: ListBuffer[ValidationError], parent: String): Unit = {
    if(ref.path == "")
      errors += ValidationError(parent + ":api", "path", ERROR)
  }

  def validate(api: ApiListing, errors: ListBuffer[ValidationError]): Unit = {
    if(api.swaggerVersion == "")
      errors += ValidationError("apiDeclaration", "swaggerVersion", ERROR)
    if(api.apiVersion == "")
      errors += ValidationError("apiDeclaration", "apiVersion", ERROR)
    if(api.basePath == "")
      errors += ValidationError("apiDeclaration", "basePath", ERROR)
    if(api.resourcePath == "")
      errors += ValidationError("apiDeclaration", "resourcePath", ERROR)

    for(a <- api.apis) {
      validate(a, errors, api.resourcePath)
    }

    api.models match {
      case Some(m) => for((name, model) <- m) {
        validate(model, errors, api.resourcePath)
      }
      case None =>
    }
  }

  def validate(model: Model, errors: ListBuffer[ValidationError], parent: String): Unit = {
    if(model.id == "")
      errors += ValidationError(parent + ":model", "id", ERROR)
  }

  def validate(desc: ApiDescription, errors: ListBuffer[ValidationError], parent: String): Unit = {
    if(desc.path == "")
      errors += ValidationError(parent + ":api", "path", ERROR)
    for(op <- desc.operations)
      validate(op, errors, parent + ":" + desc.path)
  }

  def validate(op: Operation, errors: ListBuffer[ValidationError], parent: String): Unit = {
    if(op.method == "")
      errors += ValidationError(parent + ":operation", "method", ERROR)
    if(op.nickname == "")
      errors += ValidationError(parent + ":" + op.method, "nickname", WARNING)
    if(op.responseClass == "")
      errors += ValidationError(parent + ":" + op.method, "responseClass", ERROR)
    for(resp <- op.responseMessages)
      validate(resp, errors, parent)
    for(param <- op.parameters)
      validate(param, errors, parent)
  }

  def validate(param: Parameter, errors: ListBuffer[ValidationError], parent: String): Unit = {
    if(param.name == "")
      errors += ValidationError("Parameter", "name", ERROR)
    if(param.paramType == "")
      errors += ValidationError("Parameter", "paramType", ERROR)
  }

  def validate(resp: ResponseMessage, errors: ListBuffer[ValidationError], parent: String): Unit = {
    if(resp.code == 0)
      errors += ValidationError("ResponseMessage", "code", ERROR)
    if(resp.message == 0)
      errors += ValidationError("ResponseMessage", "message", ERROR)
  }
}