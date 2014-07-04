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
      errors += ValidationError("resourceListing", "apiVersion is required", ERROR)
    if(resource.swaggerVersion == "")
      errors += ValidationError("resourceListing", "apiVersion is required", ERROR)
    for(api <- resource.apis)
      validate(api, errors, "resourceListing")
    errors.toList
  }

  def validate(ref: ApiListingReference, errors: ListBuffer[ValidationError], parent: String): Unit = {
    if(ref.path == "")
      errors += ValidationError(parent + ":[api]", "path is required", ERROR)
  }

  def validate(api: ApiListing, errors: ListBuffer[ValidationError]): Unit = {
    if(api.swaggerVersion == "")
      errors += ValidationError("apiDeclaration", "swaggerVersion is required", ERROR)
    if(api.apiVersion == "")
      errors += ValidationError("apiDeclaration", "apiVersion is required", ERROR)
    if(api.basePath == "")
      errors += ValidationError("apiDeclaration", "basePath is required", ERROR)
    if(api.resourcePath == "")
      errors += ValidationError("apiDeclaration", "resourcePath is required", ERROR)

    val name = {
      if(api.resourcePath == "") "[unknown]"
      else api.resourcePath
    }
    for(a <- api.apis) {
      validate(a, errors, name)
    }

    api.models match {
      case Some(m) => for((name, model) <- m) {
        validate(model, errors, name)
      }
      case None =>
    }
  }

  def validate(model: Model, errors: ListBuffer[ValidationError], parent: String): Unit = {
    if(model.id == "")
      errors += ValidationError(parent + ":[model]", "id is required", ERROR)
  }

  def validate(desc: ApiDescription, errors: ListBuffer[ValidationError], parent: String): Unit = {
    if(desc.path == "")
      errors += ValidationError(parent + ":[api]", "path is required", ERROR)
    for(op <- desc.operations)
      validate(op, errors, parent + ":" + desc.path)
  }

  def validate(op: Operation, errors: ListBuffer[ValidationError], parent: String): Unit = {
    if(op.method == "")
      errors += ValidationError(parent + ":[operation]", "method is required", ERROR)
    if(op.nickname == "")
      errors += ValidationError(parent + ":" + op.method, "nickname is recommended", WARNING)
    if(op.responseClass == "")
      errors += ValidationError(parent + ":" + op.method, "responseClass is required", ERROR)
    for(resp <- op.responseMessages)
      validate(resp, errors, parent)
    for(param <- op.parameters)
      validate(param, errors, parent)
  }

  def validate(param: Parameter, errors: ListBuffer[ValidationError], parent: String): Unit = {
    val name = if(param.name == "")
      "[unknown]"
    else
      param.name

    if(param.name == "")
      errors += ValidationError(parent + ":[parameter]", "name is required", ERROR)    
    if(param.paramType == "")
      errors += ValidationError(parent + name, "paramType is required", ERROR)
    if(param.dataType == "")
      errors += ValidationError(parent + name, "type is required", ERROR)
  }

  def validate(resp: ResponseMessage, errors: ListBuffer[ValidationError], parent: String): Unit = {
    if(resp.code == 0)
      errors += ValidationError(parent + ":[responseMessage]", "code is required", ERROR)
    if(resp.message == 0)
      errors += ValidationError(parent + ":[responseMessage]", "message is required", ERROR)
  }
}