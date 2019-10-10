@file:Suppress("unused")
package org.openapitools.client.infrastructure

import java.lang.RuntimeException

internal open class ClientException : RuntimeException {

    /**
     * Constructs an [ClientException] with no detail message.
     */
    constructor() : super()

    /**
     * Constructs an [ClientException] with the specified detail message.

     * @param   message   the detail message.
     */
    constructor(message: kotlin.String) : super(message)

    internal companion object {
        private const val serialVersionUID: Long = 123L
    }
}

internal open class ServerException : RuntimeException {

    /**
     * Constructs an [ServerException] with no detail message.
     */
    constructor() : super()

    /**
     * Constructs an [ServerException] with the specified detail message.

     * @param   message   the detail message.
     */
    constructor(message: kotlin.String) : super(message)

    internal companion object {
        private const val serialVersionUID: Long = 456L
    }
}