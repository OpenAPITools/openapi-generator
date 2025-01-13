@file:Suppress("unused")
package org.openapitools.client.infrastructure

import java.lang.RuntimeException

public open class ClientException(message: kotlin.String? = null, public val statusCode: Int = -1, public val response: Response? = null) : RuntimeException(message) {

    public companion object {
        private const val serialVersionUID: Long = 123L
    }
}

public open class ServerException(message: kotlin.String? = null, public val statusCode: Int = -1, public val response: Response? = null) : RuntimeException(message) {

    public companion object {
        private const val serialVersionUID: Long = 456L
    }
}
