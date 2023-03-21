package org.openapitools.client.infrastructure

import io.ktor.client.statement.HttpResponse

/**
 * Provides an extension to evaluation whether the response is a 1xx code
 */
val HttpResponse.isInformational : Boolean get() = this.status.value in 100..199

/**
 * Provides an extension to evaluation whether the response is a 3xx code
 */
@Suppress("EXTENSION_SHADOWED_BY_MEMBER")
val HttpResponse.isRedirect : Boolean get() = this.status.value in 300..399

/**
 * Provides an extension to evaluation whether the response is a 4xx code
 */
val HttpResponse.isClientError : Boolean get() = this.status.value in 400..499

/**
 * Provides an extension to evaluation whether the response is a 5xx (Standard) through 9999 (non-standard) code
 */
val HttpResponse.isServerError : Boolean get() = this.status.value in 500..9999
