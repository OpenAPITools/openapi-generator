@file:Suppress("unused")
package org.openapitools.client.infrastructure

import java.lang.RuntimeException

internal open class ClientException(message: kotlin.String? = null, val statusCode: Int = -1, val response: Response? = null) : RuntimeException(message) {

    internal companion object {
        private const val serialVersionUID: Long = 123L
    }
}

internal open class ServerException(message: kotlin.String? = null, val statusCode: Int = -1, val response: Response? = null) : RuntimeException(message) {

    internal companion object {
        private const val serialVersionUID: Long = 456L
    }
}