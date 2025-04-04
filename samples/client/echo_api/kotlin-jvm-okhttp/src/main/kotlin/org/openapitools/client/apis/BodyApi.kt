/**
 *
 * Please note:
 * This class is auto generated by OpenAPI Generator (https://openapi-generator.tech).
 * Do not edit this file manually.
 *
 */

@file:Suppress(
    "ArrayInDataClass",
    "EnumEntryName",
    "RemoveRedundantQualifierName",
    "UnusedImport"
)

package org.openapitools.client.apis

import java.io.IOException
import okhttp3.Call
import okhttp3.HttpUrl

import org.openapitools.client.models.ApiPet
import org.openapitools.client.models.ApiTag

import com.squareup.moshi.Json

import org.openapitools.client.infrastructure.ApiClient
import org.openapitools.client.infrastructure.ApiResponse
import org.openapitools.client.infrastructure.ClientException
import org.openapitools.client.infrastructure.ClientError
import org.openapitools.client.infrastructure.ServerException
import org.openapitools.client.infrastructure.ServerError
import org.openapitools.client.infrastructure.MultiValueMap
import org.openapitools.client.infrastructure.PartConfig
import org.openapitools.client.infrastructure.RequestConfig
import org.openapitools.client.infrastructure.RequestMethod
import org.openapitools.client.infrastructure.ResponseType
import org.openapitools.client.infrastructure.Success
import org.openapitools.client.infrastructure.toMultiValue

class BodyApi(basePath: kotlin.String = defaultBasePath, client: Call.Factory = ApiClient.defaultClient) : ApiClient(basePath, client) {
    companion object {
        @JvmStatic
        val defaultBasePath: String by lazy {
            System.getProperties().getProperty(ApiClient.baseUrlKey, "http://localhost:3000")
        }
    }

    /**
     * POST /binary/gif
     * Test binary (gif) response body
     * Test binary (gif) response body
     * @return java.io.File
     * @throws IllegalStateException If the request is not correctly configured
     * @throws IOException Rethrows the OkHttp execute method exception
     * @throws UnsupportedOperationException If the API returns an informational or redirection response
     * @throws ClientException If the API returns a client error response
     * @throws ServerException If the API returns a server error response
     */
    @Suppress("UNCHECKED_CAST")
    @Throws(IllegalStateException::class, IOException::class, UnsupportedOperationException::class, ClientException::class, ServerException::class)
    fun testBinaryGif() : java.io.File {
        val localVarResponse = testBinaryGifWithHttpInfo()

        return when (localVarResponse.responseType) {
            ResponseType.Success -> (localVarResponse as Success<*>).data as java.io.File
            ResponseType.Informational -> throw UnsupportedOperationException("Client does not support Informational responses.")
            ResponseType.Redirection -> throw UnsupportedOperationException("Client does not support Redirection responses.")
            ResponseType.ClientError -> {
                val localVarError = localVarResponse as ClientError<*>
                throw ClientException("Client error : ${localVarError.statusCode} ${localVarError.message.orEmpty()}", localVarError.statusCode, localVarResponse)
            }
            ResponseType.ServerError -> {
                val localVarError = localVarResponse as ServerError<*>
                throw ServerException("Server error : ${localVarError.statusCode} ${localVarError.message.orEmpty()} ${localVarError.body}", localVarError.statusCode, localVarResponse)
            }
        }
    }

    /**
     * POST /binary/gif
     * Test binary (gif) response body
     * Test binary (gif) response body
     * @return ApiResponse<java.io.File?>
     * @throws IllegalStateException If the request is not correctly configured
     * @throws IOException Rethrows the OkHttp execute method exception
     */
    @Suppress("UNCHECKED_CAST")
    @Throws(IllegalStateException::class, IOException::class)
    fun testBinaryGifWithHttpInfo() : ApiResponse<java.io.File?> {
        val localVariableConfig = testBinaryGifRequestConfig()

        return request<Unit, java.io.File>(
            localVariableConfig
        )
    }

    /**
     * To obtain the request config of the operation testBinaryGif
     *
     * @return RequestConfig
     */
    fun testBinaryGifRequestConfig() : RequestConfig<Unit> {
        val localVariableBody = null
        val localVariableQuery: MultiValueMap = mutableMapOf()
        val localVariableHeaders: MutableMap<String, String> = mutableMapOf()
        
        return RequestConfig(
            method = RequestMethod.POST,
            path = "/binary/gif",
            query = localVariableQuery,
            headers = localVariableHeaders,
            requiresAuthentication = false,
            body = localVariableBody
        )
    }

    /**
     * POST /body/application/octetstream/binary
     * Test body parameter(s)
     * Test body parameter(s)
     * @param body  (optional)
     * @return kotlin.String
     * @throws IllegalStateException If the request is not correctly configured
     * @throws IOException Rethrows the OkHttp execute method exception
     * @throws UnsupportedOperationException If the API returns an informational or redirection response
     * @throws ClientException If the API returns a client error response
     * @throws ServerException If the API returns a server error response
     */
    @Suppress("UNCHECKED_CAST")
    @Throws(IllegalStateException::class, IOException::class, UnsupportedOperationException::class, ClientException::class, ServerException::class)
    fun testBodyApplicationOctetstreamBinary(body: java.io.File? = null) : kotlin.String {
        val localVarResponse = testBodyApplicationOctetstreamBinaryWithHttpInfo(body = body)

        return when (localVarResponse.responseType) {
            ResponseType.Success -> (localVarResponse as Success<*>).data as kotlin.String
            ResponseType.Informational -> throw UnsupportedOperationException("Client does not support Informational responses.")
            ResponseType.Redirection -> throw UnsupportedOperationException("Client does not support Redirection responses.")
            ResponseType.ClientError -> {
                val localVarError = localVarResponse as ClientError<*>
                throw ClientException("Client error : ${localVarError.statusCode} ${localVarError.message.orEmpty()}", localVarError.statusCode, localVarResponse)
            }
            ResponseType.ServerError -> {
                val localVarError = localVarResponse as ServerError<*>
                throw ServerException("Server error : ${localVarError.statusCode} ${localVarError.message.orEmpty()} ${localVarError.body}", localVarError.statusCode, localVarResponse)
            }
        }
    }

    /**
     * POST /body/application/octetstream/binary
     * Test body parameter(s)
     * Test body parameter(s)
     * @param body  (optional)
     * @return ApiResponse<kotlin.String?>
     * @throws IllegalStateException If the request is not correctly configured
     * @throws IOException Rethrows the OkHttp execute method exception
     */
    @Suppress("UNCHECKED_CAST")
    @Throws(IllegalStateException::class, IOException::class)
    fun testBodyApplicationOctetstreamBinaryWithHttpInfo(body: java.io.File?) : ApiResponse<kotlin.String?> {
        val localVariableConfig = testBodyApplicationOctetstreamBinaryRequestConfig(body = body)

        return request<java.io.File, kotlin.String>(
            localVariableConfig
        )
    }

    /**
     * To obtain the request config of the operation testBodyApplicationOctetstreamBinary
     *
     * @param body  (optional)
     * @return RequestConfig
     */
    fun testBodyApplicationOctetstreamBinaryRequestConfig(body: java.io.File?) : RequestConfig<java.io.File> {
        val localVariableBody = body
        val localVariableQuery: MultiValueMap = mutableMapOf()
        val localVariableHeaders: MutableMap<String, String> = mutableMapOf()
        localVariableHeaders["Content-Type"] = "application/octet-stream"
        localVariableHeaders["Accept"] = "text/plain"

        return RequestConfig(
            method = RequestMethod.POST,
            path = "/body/application/octetstream/binary",
            query = localVariableQuery,
            headers = localVariableHeaders,
            requiresAuthentication = false,
            body = localVariableBody
        )
    }

    /**
     * POST /body/application/octetstream/array_of_binary
     * Test array of binary in multipart mime
     * Test array of binary in multipart mime
     * @param files 
     * @return kotlin.String
     * @throws IllegalStateException If the request is not correctly configured
     * @throws IOException Rethrows the OkHttp execute method exception
     * @throws UnsupportedOperationException If the API returns an informational or redirection response
     * @throws ClientException If the API returns a client error response
     * @throws ServerException If the API returns a server error response
     */
    @Suppress("UNCHECKED_CAST")
    @Throws(IllegalStateException::class, IOException::class, UnsupportedOperationException::class, ClientException::class, ServerException::class)
    fun testBodyMultipartFormdataArrayOfBinary(files: kotlin.collections.List<java.io.File>) : kotlin.String {
        val localVarResponse = testBodyMultipartFormdataArrayOfBinaryWithHttpInfo(files = files)

        return when (localVarResponse.responseType) {
            ResponseType.Success -> (localVarResponse as Success<*>).data as kotlin.String
            ResponseType.Informational -> throw UnsupportedOperationException("Client does not support Informational responses.")
            ResponseType.Redirection -> throw UnsupportedOperationException("Client does not support Redirection responses.")
            ResponseType.ClientError -> {
                val localVarError = localVarResponse as ClientError<*>
                throw ClientException("Client error : ${localVarError.statusCode} ${localVarError.message.orEmpty()}", localVarError.statusCode, localVarResponse)
            }
            ResponseType.ServerError -> {
                val localVarError = localVarResponse as ServerError<*>
                throw ServerException("Server error : ${localVarError.statusCode} ${localVarError.message.orEmpty()} ${localVarError.body}", localVarError.statusCode, localVarResponse)
            }
        }
    }

    /**
     * POST /body/application/octetstream/array_of_binary
     * Test array of binary in multipart mime
     * Test array of binary in multipart mime
     * @param files 
     * @return ApiResponse<kotlin.String?>
     * @throws IllegalStateException If the request is not correctly configured
     * @throws IOException Rethrows the OkHttp execute method exception
     */
    @Suppress("UNCHECKED_CAST")
    @Throws(IllegalStateException::class, IOException::class)
    fun testBodyMultipartFormdataArrayOfBinaryWithHttpInfo(files: kotlin.collections.List<java.io.File>) : ApiResponse<kotlin.String?> {
        val localVariableConfig = testBodyMultipartFormdataArrayOfBinaryRequestConfig(files = files)

        return request<Map<String, PartConfig<*>>, kotlin.String>(
            localVariableConfig
        )
    }

    /**
     * To obtain the request config of the operation testBodyMultipartFormdataArrayOfBinary
     *
     * @param files 
     * @return RequestConfig
     */
    fun testBodyMultipartFormdataArrayOfBinaryRequestConfig(files: kotlin.collections.List<java.io.File>) : RequestConfig<Map<String, PartConfig<*>>> {
        val localVariableBody = mapOf(
            "files" to PartConfig(body = files, headers = mutableMapOf()),)
        val localVariableQuery: MultiValueMap = mutableMapOf()
        val localVariableHeaders: MutableMap<String, String> = mutableMapOf("Content-Type" to "multipart/form-data")
        localVariableHeaders["Accept"] = "text/plain"

        return RequestConfig(
            method = RequestMethod.POST,
            path = "/body/application/octetstream/array_of_binary",
            query = localVariableQuery,
            headers = localVariableHeaders,
            requiresAuthentication = false,
            body = localVariableBody
        )
    }

    /**
     * POST /body/application/octetstream/single_binary
     * Test single binary in multipart mime
     * Test single binary in multipart mime
     * @param myFile  (optional)
     * @return kotlin.String
     * @throws IllegalStateException If the request is not correctly configured
     * @throws IOException Rethrows the OkHttp execute method exception
     * @throws UnsupportedOperationException If the API returns an informational or redirection response
     * @throws ClientException If the API returns a client error response
     * @throws ServerException If the API returns a server error response
     */
    @Suppress("UNCHECKED_CAST")
    @Throws(IllegalStateException::class, IOException::class, UnsupportedOperationException::class, ClientException::class, ServerException::class)
    fun testBodyMultipartFormdataSingleBinary(myFile: java.io.File? = null) : kotlin.String {
        val localVarResponse = testBodyMultipartFormdataSingleBinaryWithHttpInfo(myFile = myFile)

        return when (localVarResponse.responseType) {
            ResponseType.Success -> (localVarResponse as Success<*>).data as kotlin.String
            ResponseType.Informational -> throw UnsupportedOperationException("Client does not support Informational responses.")
            ResponseType.Redirection -> throw UnsupportedOperationException("Client does not support Redirection responses.")
            ResponseType.ClientError -> {
                val localVarError = localVarResponse as ClientError<*>
                throw ClientException("Client error : ${localVarError.statusCode} ${localVarError.message.orEmpty()}", localVarError.statusCode, localVarResponse)
            }
            ResponseType.ServerError -> {
                val localVarError = localVarResponse as ServerError<*>
                throw ServerException("Server error : ${localVarError.statusCode} ${localVarError.message.orEmpty()} ${localVarError.body}", localVarError.statusCode, localVarResponse)
            }
        }
    }

    /**
     * POST /body/application/octetstream/single_binary
     * Test single binary in multipart mime
     * Test single binary in multipart mime
     * @param myFile  (optional)
     * @return ApiResponse<kotlin.String?>
     * @throws IllegalStateException If the request is not correctly configured
     * @throws IOException Rethrows the OkHttp execute method exception
     */
    @Suppress("UNCHECKED_CAST")
    @Throws(IllegalStateException::class, IOException::class)
    fun testBodyMultipartFormdataSingleBinaryWithHttpInfo(myFile: java.io.File?) : ApiResponse<kotlin.String?> {
        val localVariableConfig = testBodyMultipartFormdataSingleBinaryRequestConfig(myFile = myFile)

        return request<Map<String, PartConfig<*>>, kotlin.String>(
            localVariableConfig
        )
    }

    /**
     * To obtain the request config of the operation testBodyMultipartFormdataSingleBinary
     *
     * @param myFile  (optional)
     * @return RequestConfig
     */
    fun testBodyMultipartFormdataSingleBinaryRequestConfig(myFile: java.io.File?) : RequestConfig<Map<String, PartConfig<*>>> {
        val localVariableBody = mapOf(
            "my-file" to PartConfig(body = myFile, headers = mutableMapOf()),)
        val localVariableQuery: MultiValueMap = mutableMapOf()
        val localVariableHeaders: MutableMap<String, String> = mutableMapOf("Content-Type" to "multipart/form-data")
        localVariableHeaders["Accept"] = "text/plain"

        return RequestConfig(
            method = RequestMethod.POST,
            path = "/body/application/octetstream/single_binary",
            query = localVariableQuery,
            headers = localVariableHeaders,
            requiresAuthentication = false,
            body = localVariableBody
        )
    }

    /**
     * POST /echo/body/FreeFormObject/response_string
     * Test free form object
     * Test free form object
     * @param body Free form object (optional)
     * @return kotlin.String
     * @throws IllegalStateException If the request is not correctly configured
     * @throws IOException Rethrows the OkHttp execute method exception
     * @throws UnsupportedOperationException If the API returns an informational or redirection response
     * @throws ClientException If the API returns a client error response
     * @throws ServerException If the API returns a server error response
     */
    @Suppress("UNCHECKED_CAST")
    @Throws(IllegalStateException::class, IOException::class, UnsupportedOperationException::class, ClientException::class, ServerException::class)
    fun testEchoBodyFreeFormObjectResponseString(body: kotlin.Any? = null) : kotlin.String {
        val localVarResponse = testEchoBodyFreeFormObjectResponseStringWithHttpInfo(body = body)

        return when (localVarResponse.responseType) {
            ResponseType.Success -> (localVarResponse as Success<*>).data as kotlin.String
            ResponseType.Informational -> throw UnsupportedOperationException("Client does not support Informational responses.")
            ResponseType.Redirection -> throw UnsupportedOperationException("Client does not support Redirection responses.")
            ResponseType.ClientError -> {
                val localVarError = localVarResponse as ClientError<*>
                throw ClientException("Client error : ${localVarError.statusCode} ${localVarError.message.orEmpty()}", localVarError.statusCode, localVarResponse)
            }
            ResponseType.ServerError -> {
                val localVarError = localVarResponse as ServerError<*>
                throw ServerException("Server error : ${localVarError.statusCode} ${localVarError.message.orEmpty()} ${localVarError.body}", localVarError.statusCode, localVarResponse)
            }
        }
    }

    /**
     * POST /echo/body/FreeFormObject/response_string
     * Test free form object
     * Test free form object
     * @param body Free form object (optional)
     * @return ApiResponse<kotlin.String?>
     * @throws IllegalStateException If the request is not correctly configured
     * @throws IOException Rethrows the OkHttp execute method exception
     */
    @Suppress("UNCHECKED_CAST")
    @Throws(IllegalStateException::class, IOException::class)
    fun testEchoBodyFreeFormObjectResponseStringWithHttpInfo(body: kotlin.Any?) : ApiResponse<kotlin.String?> {
        val localVariableConfig = testEchoBodyFreeFormObjectResponseStringRequestConfig(body = body)

        return request<kotlin.Any, kotlin.String>(
            localVariableConfig
        )
    }

    /**
     * To obtain the request config of the operation testEchoBodyFreeFormObjectResponseString
     *
     * @param body Free form object (optional)
     * @return RequestConfig
     */
    fun testEchoBodyFreeFormObjectResponseStringRequestConfig(body: kotlin.Any?) : RequestConfig<kotlin.Any> {
        val localVariableBody = body
        val localVariableQuery: MultiValueMap = mutableMapOf()
        val localVariableHeaders: MutableMap<String, String> = mutableMapOf()
        localVariableHeaders["Content-Type"] = "application/json"
        localVariableHeaders["Accept"] = "text/plain"

        return RequestConfig(
            method = RequestMethod.POST,
            path = "/echo/body/FreeFormObject/response_string",
            query = localVariableQuery,
            headers = localVariableHeaders,
            requiresAuthentication = false,
            body = localVariableBody
        )
    }

    /**
     * POST /echo/body/Pet
     * Test body parameter(s)
     * Test body parameter(s)
     * @param apiPet Pet object that needs to be added to the store (optional)
     * @return ApiPet
     * @throws IllegalStateException If the request is not correctly configured
     * @throws IOException Rethrows the OkHttp execute method exception
     * @throws UnsupportedOperationException If the API returns an informational or redirection response
     * @throws ClientException If the API returns a client error response
     * @throws ServerException If the API returns a server error response
     */
    @Suppress("UNCHECKED_CAST")
    @Throws(IllegalStateException::class, IOException::class, UnsupportedOperationException::class, ClientException::class, ServerException::class)
    fun testEchoBodyPet(apiPet: ApiPet? = null) : ApiPet {
        val localVarResponse = testEchoBodyPetWithHttpInfo(apiPet = apiPet)

        return when (localVarResponse.responseType) {
            ResponseType.Success -> (localVarResponse as Success<*>).data as ApiPet
            ResponseType.Informational -> throw UnsupportedOperationException("Client does not support Informational responses.")
            ResponseType.Redirection -> throw UnsupportedOperationException("Client does not support Redirection responses.")
            ResponseType.ClientError -> {
                val localVarError = localVarResponse as ClientError<*>
                throw ClientException("Client error : ${localVarError.statusCode} ${localVarError.message.orEmpty()}", localVarError.statusCode, localVarResponse)
            }
            ResponseType.ServerError -> {
                val localVarError = localVarResponse as ServerError<*>
                throw ServerException("Server error : ${localVarError.statusCode} ${localVarError.message.orEmpty()} ${localVarError.body}", localVarError.statusCode, localVarResponse)
            }
        }
    }

    /**
     * POST /echo/body/Pet
     * Test body parameter(s)
     * Test body parameter(s)
     * @param apiPet Pet object that needs to be added to the store (optional)
     * @return ApiResponse<ApiPet?>
     * @throws IllegalStateException If the request is not correctly configured
     * @throws IOException Rethrows the OkHttp execute method exception
     */
    @Suppress("UNCHECKED_CAST")
    @Throws(IllegalStateException::class, IOException::class)
    fun testEchoBodyPetWithHttpInfo(apiPet: ApiPet?) : ApiResponse<ApiPet?> {
        val localVariableConfig = testEchoBodyPetRequestConfig(apiPet = apiPet)

        return request<ApiPet, ApiPet>(
            localVariableConfig
        )
    }

    /**
     * To obtain the request config of the operation testEchoBodyPet
     *
     * @param apiPet Pet object that needs to be added to the store (optional)
     * @return RequestConfig
     */
    fun testEchoBodyPetRequestConfig(apiPet: ApiPet?) : RequestConfig<ApiPet> {
        val localVariableBody = apiPet
        val localVariableQuery: MultiValueMap = mutableMapOf()
        val localVariableHeaders: MutableMap<String, String> = mutableMapOf()
        localVariableHeaders["Content-Type"] = "application/json"
        localVariableHeaders["Accept"] = "application/json"

        return RequestConfig(
            method = RequestMethod.POST,
            path = "/echo/body/Pet",
            query = localVariableQuery,
            headers = localVariableHeaders,
            requiresAuthentication = false,
            body = localVariableBody
        )
    }

    /**
     * POST /echo/body/Pet/response_string
     * Test empty response body
     * Test empty response body
     * @param apiPet Pet object that needs to be added to the store (optional)
     * @return kotlin.String
     * @throws IllegalStateException If the request is not correctly configured
     * @throws IOException Rethrows the OkHttp execute method exception
     * @throws UnsupportedOperationException If the API returns an informational or redirection response
     * @throws ClientException If the API returns a client error response
     * @throws ServerException If the API returns a server error response
     */
    @Suppress("UNCHECKED_CAST")
    @Throws(IllegalStateException::class, IOException::class, UnsupportedOperationException::class, ClientException::class, ServerException::class)
    fun testEchoBodyPetResponseString(apiPet: ApiPet? = null) : kotlin.String {
        val localVarResponse = testEchoBodyPetResponseStringWithHttpInfo(apiPet = apiPet)

        return when (localVarResponse.responseType) {
            ResponseType.Success -> (localVarResponse as Success<*>).data as kotlin.String
            ResponseType.Informational -> throw UnsupportedOperationException("Client does not support Informational responses.")
            ResponseType.Redirection -> throw UnsupportedOperationException("Client does not support Redirection responses.")
            ResponseType.ClientError -> {
                val localVarError = localVarResponse as ClientError<*>
                throw ClientException("Client error : ${localVarError.statusCode} ${localVarError.message.orEmpty()}", localVarError.statusCode, localVarResponse)
            }
            ResponseType.ServerError -> {
                val localVarError = localVarResponse as ServerError<*>
                throw ServerException("Server error : ${localVarError.statusCode} ${localVarError.message.orEmpty()} ${localVarError.body}", localVarError.statusCode, localVarResponse)
            }
        }
    }

    /**
     * POST /echo/body/Pet/response_string
     * Test empty response body
     * Test empty response body
     * @param apiPet Pet object that needs to be added to the store (optional)
     * @return ApiResponse<kotlin.String?>
     * @throws IllegalStateException If the request is not correctly configured
     * @throws IOException Rethrows the OkHttp execute method exception
     */
    @Suppress("UNCHECKED_CAST")
    @Throws(IllegalStateException::class, IOException::class)
    fun testEchoBodyPetResponseStringWithHttpInfo(apiPet: ApiPet?) : ApiResponse<kotlin.String?> {
        val localVariableConfig = testEchoBodyPetResponseStringRequestConfig(apiPet = apiPet)

        return request<ApiPet, kotlin.String>(
            localVariableConfig
        )
    }

    /**
     * To obtain the request config of the operation testEchoBodyPetResponseString
     *
     * @param apiPet Pet object that needs to be added to the store (optional)
     * @return RequestConfig
     */
    fun testEchoBodyPetResponseStringRequestConfig(apiPet: ApiPet?) : RequestConfig<ApiPet> {
        val localVariableBody = apiPet
        val localVariableQuery: MultiValueMap = mutableMapOf()
        val localVariableHeaders: MutableMap<String, String> = mutableMapOf()
        localVariableHeaders["Content-Type"] = "application/json"
        localVariableHeaders["Accept"] = "text/plain"

        return RequestConfig(
            method = RequestMethod.POST,
            path = "/echo/body/Pet/response_string",
            query = localVariableQuery,
            headers = localVariableHeaders,
            requiresAuthentication = false,
            body = localVariableBody
        )
    }

    /**
     * POST /echo/body/Tag/response_string
     * Test empty json (request body)
     * Test empty json (request body)
     * @param apiTag Tag object (optional)
     * @return kotlin.String
     * @throws IllegalStateException If the request is not correctly configured
     * @throws IOException Rethrows the OkHttp execute method exception
     * @throws UnsupportedOperationException If the API returns an informational or redirection response
     * @throws ClientException If the API returns a client error response
     * @throws ServerException If the API returns a server error response
     */
    @Suppress("UNCHECKED_CAST")
    @Throws(IllegalStateException::class, IOException::class, UnsupportedOperationException::class, ClientException::class, ServerException::class)
    fun testEchoBodyTagResponseString(apiTag: ApiTag? = null) : kotlin.String {
        val localVarResponse = testEchoBodyTagResponseStringWithHttpInfo(apiTag = apiTag)

        return when (localVarResponse.responseType) {
            ResponseType.Success -> (localVarResponse as Success<*>).data as kotlin.String
            ResponseType.Informational -> throw UnsupportedOperationException("Client does not support Informational responses.")
            ResponseType.Redirection -> throw UnsupportedOperationException("Client does not support Redirection responses.")
            ResponseType.ClientError -> {
                val localVarError = localVarResponse as ClientError<*>
                throw ClientException("Client error : ${localVarError.statusCode} ${localVarError.message.orEmpty()}", localVarError.statusCode, localVarResponse)
            }
            ResponseType.ServerError -> {
                val localVarError = localVarResponse as ServerError<*>
                throw ServerException("Server error : ${localVarError.statusCode} ${localVarError.message.orEmpty()} ${localVarError.body}", localVarError.statusCode, localVarResponse)
            }
        }
    }

    /**
     * POST /echo/body/Tag/response_string
     * Test empty json (request body)
     * Test empty json (request body)
     * @param apiTag Tag object (optional)
     * @return ApiResponse<kotlin.String?>
     * @throws IllegalStateException If the request is not correctly configured
     * @throws IOException Rethrows the OkHttp execute method exception
     */
    @Suppress("UNCHECKED_CAST")
    @Throws(IllegalStateException::class, IOException::class)
    fun testEchoBodyTagResponseStringWithHttpInfo(apiTag: ApiTag?) : ApiResponse<kotlin.String?> {
        val localVariableConfig = testEchoBodyTagResponseStringRequestConfig(apiTag = apiTag)

        return request<ApiTag, kotlin.String>(
            localVariableConfig
        )
    }

    /**
     * To obtain the request config of the operation testEchoBodyTagResponseString
     *
     * @param apiTag Tag object (optional)
     * @return RequestConfig
     */
    fun testEchoBodyTagResponseStringRequestConfig(apiTag: ApiTag?) : RequestConfig<ApiTag> {
        val localVariableBody = apiTag
        val localVariableQuery: MultiValueMap = mutableMapOf()
        val localVariableHeaders: MutableMap<String, String> = mutableMapOf()
        localVariableHeaders["Content-Type"] = "application/json"
        localVariableHeaders["Accept"] = "text/plain"

        return RequestConfig(
            method = RequestMethod.POST,
            path = "/echo/body/Tag/response_string",
            query = localVariableQuery,
            headers = localVariableHeaders,
            requiresAuthentication = false,
            body = localVariableBody
        )
    }


    private fun encodeURIComponent(uriComponent: kotlin.String): kotlin.String =
        HttpUrl.Builder().scheme("http").host("localhost").addPathSegment(uriComponent).build().encodedPathSegments[0]
}
