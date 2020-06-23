package org.openapitools.client.infrastructure

internal enum class ResponseType {
    Success, Informational, Redirection, ClientError, ServerError
}

internal interface Response

internal abstract class ApiInfrastructureResponse<T>(val responseType: ResponseType): Response {
    abstract val statusCode: Int
    abstract val headers: Map<String,List<String>>
}

internal class Success<T>(
    val data: T,
    override val statusCode: Int = -1,
    override val headers: Map<String, List<String>> = mapOf()
): ApiInfrastructureResponse<T>(ResponseType.Success)

internal class Informational<T>(
    val statusText: String,
    override val statusCode: Int = -1,
    override val headers: Map<String, List<String>> = mapOf()
) : ApiInfrastructureResponse<T>(ResponseType.Informational)

internal class Redirection<T>(
    override val statusCode: Int = -1,
    override val headers: Map<String, List<String>> = mapOf()
) : ApiInfrastructureResponse<T>(ResponseType.Redirection)

internal class ClientError<T>(
    val message: String? = null,
    val body: Any? = null,
    override val statusCode: Int = -1,
    override val headers: Map<String, List<String>> = mapOf()
) : ApiInfrastructureResponse<T>(ResponseType.ClientError)

internal class ServerError<T>(
    val message: String? = null,
    val body: Any? = null,
    override val statusCode: Int = -1,
    override val headers: Map<String, List<String>>
): ApiInfrastructureResponse<T>(ResponseType.ServerError)