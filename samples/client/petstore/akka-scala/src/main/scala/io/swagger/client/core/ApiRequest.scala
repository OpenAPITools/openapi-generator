package io.swagger.client.core

sealed trait ResponseState
object ResponseState {
  case object Success extends ResponseState
  case object Error extends ResponseState
}

case class ApiRequest[U](
                       // required fields
                       method: ApiMethod,
                       basePath: String,
                       operationPath: String,
                       contentType: String,

                       // optional fields
                       responses: Map[Int, (Manifest[_], ResponseState)] = Map.empty,
                       bodyParam: Option[Any] = None,
                       formParams: Map[String, Any] = Map.empty,
                       pathParams: Map[String, Any] = Map.empty,
                       queryParams: Map[String, Any] = Map.empty,
                       headerParams: Map[String, Any] = Map.empty,
                       credentials: Seq[Credentials] = List.empty) {

  def withCredentials(cred: Credentials) = copy[U](credentials = credentials :+ cred)

  def withApiKey(key: ApiKeyValue, keyName: String, location: ApiKeyLocation) = withCredentials(ApiKeyCredentials(key, keyName, location))

  def withSuccessResponse[T](code: Int)(implicit m: Manifest[T]) = copy[U](responses = responses + (code -> (m, ResponseState.Success)))

  def withErrorResponse[T](code: Int)(implicit m: Manifest[T]) = copy[U](responses = responses + (code -> (m, ResponseState.Error)))

  def withDefaultSuccessResponse[T](implicit m: Manifest[T]) = withSuccessResponse[T](0)

  def withDefaultErrorResponse[T](implicit m: Manifest[T]) = withErrorResponse[T](0)

  def responseForCode(statusCode: Int): Option[(Manifest[_], ResponseState)] = responses.get(statusCode) orElse responses.get(0)

  def withoutBody() = copy[U](bodyParam = None)

  def withBody(body: Any) = copy[U](bodyParam = Some(body))

  def withFormParam(name: String, value: Any) = copy[U](formParams = formParams + (name -> value))

  def withPathParam(name: String, value: Any) = copy[U](pathParams = pathParams + (name -> value))

  def withQueryParam(name: String, value: Any) = copy[U](queryParams = queryParams + (name -> value))

  def withHeaderParam(name: String, value: Any) = copy[U](headerParams = headerParams + (name -> value))
}
