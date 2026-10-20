package org.openapitools.client.infrastructure

public enum class ResponseType {
    Success, Informational, Redirection, ClientError, ServerError
}

public interface Response

public abstract class ApiResponse<T>(public val responseType: ResponseType): Response {
    public abstract val statusCode: Int
    public abstract val headers: Map<String,List<String>>
}

public class Success<T>(
    public val data: T,
    override val statusCode: Int = -1,
    override val headers: Map<String, List<String>> = mapOf()
): ApiResponse<T>(ResponseType.Success)

public class Informational<T>(
    public val statusText: String,
    override val statusCode: Int = -1,
    override val headers: Map<String, List<String>> = mapOf()
) : ApiResponse<T>(ResponseType.Informational)

public class Redirection<T>(
    override val statusCode: Int = -1,
    override val headers: Map<String, List<String>> = mapOf()
) : ApiResponse<T>(ResponseType.Redirection)

public class ClientError<T>(
    public val message: String? = null,
    public val body: Any? = null,
    override val statusCode: Int = -1,
    override val headers: Map<String, List<String>> = mapOf()
) : ApiResponse<T>(ResponseType.ClientError)

public class ServerError<T>(
    public val message: String? = null,
    public val body: Any? = null,
    override val statusCode: Int = -1,
    override val headers: Map<String, List<String>> = mapOf()
): ApiResponse<T>(ResponseType.ServerError)
