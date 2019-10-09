package org.openapitools.client.infrastructure

public enum class ResponseType {
    Success, Informational, Redirection, ClientError, ServerError
}

public abstract class ApiInfrastructureResponse<T>(val responseType: ResponseType) {
    abstract val statusCode: Int
    abstract val headers: Map<String,List<String>>
}

public class Success<T>(
    val data: T,
    override val statusCode: Int = -1,
    override val headers: Map<String, List<String>> = mapOf()
): ApiInfrastructureResponse<T>(ResponseType.Success)

public class Informational<T>(
    val statusText: String,
    override val statusCode: Int = -1,
    override val headers: Map<String, List<String>> = mapOf()
) : ApiInfrastructureResponse<T>(ResponseType.Informational)

public class Redirection<T>(
    override val statusCode: Int = -1,
    override val headers: Map<String, List<String>> = mapOf()
) : ApiInfrastructureResponse<T>(ResponseType.Redirection)

public class ClientError<T>(
    val body: Any? = null,
    override val statusCode: Int = -1,
    override val headers: Map<String, List<String>> = mapOf()
) : ApiInfrastructureResponse<T>(ResponseType.ClientError)

public class ServerError<T>(
    val message: String? = null,
    val body: Any? = null,
    override val statusCode: Int = -1,
    override val headers: Map<String, List<String>>
): ApiInfrastructureResponse<T>(ResponseType.ServerError)