package org.openapitools.client.infrastructure

fun String.toBase64() = encodeToByteArray().encodeBase64()
