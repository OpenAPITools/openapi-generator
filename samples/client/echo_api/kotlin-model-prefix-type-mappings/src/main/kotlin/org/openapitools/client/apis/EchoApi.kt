package org.openapitools.client.apis

import org.openapitools.client.infrastructure.CollectionFormats.*
import retrofit2.http.*
import retrofit2.Response
import okhttp3.RequestBody
import com.google.gson.annotations.SerializedName


interface EchoApi {
    /**
     * GET echo/string-escaping/{$paramName}
     * Test $-in-path-param escaping
     * Tests that path params with $dollar, backslash \\ and quote \&quot; are properly escaped
     * Responses:
     *  - 200: ok
     *
     * @param dollarParamName 
     * @param filterDollarType Filter with $dollar in description and comment-close *_/ (optional, default to "default$Value with \\ and \"")
     * @return [kotlin.String]
     */
    @GET("echo/string-escaping/{\$paramName}")
    suspend fun testsEchoStringEscapingParamName(@Path("\$paramName") dollarParamName: kotlin.String, @Query("filter\$Type") filterDollarType: kotlin.String? = "default\$Value with \\ and \""): Response<kotlin.String>

}
