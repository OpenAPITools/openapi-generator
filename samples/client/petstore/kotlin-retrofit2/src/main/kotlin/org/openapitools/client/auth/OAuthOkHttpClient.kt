package org.openapitools.client.auth

import java.io.IOException

import org.apache.oltu.oauth2.client.HttpClient
import org.apache.oltu.oauth2.client.request.OAuthClientRequest
import org.apache.oltu.oauth2.client.response.OAuthClientResponse
import org.apache.oltu.oauth2.client.response.OAuthClientResponseFactory
import org.apache.oltu.oauth2.common.exception.OAuthProblemException
import org.apache.oltu.oauth2.common.exception.OAuthSystemException

import okhttp3.OkHttpClient
import okhttp3.Request
import okhttp3.MediaType.Companion.toMediaTypeOrNull
import okhttp3.RequestBody


class OAuthOkHttpClient(
        private var client: OkHttpClient
) : HttpClient {

    constructor() : this(OkHttpClient())

    @Throws(OAuthSystemException::class, OAuthProblemException::class)
    override fun <T : OAuthClientResponse?> execute(
            request: OAuthClientRequest, 
            headers: Map<String, String>?,
            requestMethod: String, 
            responseClass: Class<T>?): T {

        var mediaType = "application/json".toMediaTypeOrNull()
        val requestBuilder = Request.Builder().url(request.locationUri)

        headers?.forEach { entry ->
            if (entry.key.equals("Content-Type", true)) {
                mediaType = entry.value.toMediaTypeOrNull()
            } else {
                requestBuilder.addHeader(entry.key, entry.value)
            }
        }

        val body: RequestBody? = if (request.body != null) RequestBody.create(mediaType, request.body) else null
        requestBuilder.method(requestMethod, body)

        try {
            val response = client.newCall(requestBuilder.build()).execute()
            return OAuthClientResponseFactory.createCustomResponse(
                    response.body?.string(), 
                    response.body?.contentType()?.toString(),
                    response.code,
                    responseClass)
        } catch (e: IOException) {
            throw OAuthSystemException(e)
        }
    }

    override fun shutdown() {
        // Nothing to do here
    }

}
