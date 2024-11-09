package org.openapitools.client.apis

import org.openapitools.client.infrastructure.CollectionFormats.*
import retrofit2.http.*
import retrofit2.Response
import okhttp3.RequestBody
import com.google.gson.annotations.SerializedName

import org.openapitools.client.models.ApiStringEnumRef

interface HeaderApi {

    /**
    * enum for parameter enumNonrefStringHeader
    */
    enum class EnumNonrefStringHeaderTestHeaderIntegerBooleanStringEnums(val value: String) {
        @SerializedName(value = "success") SUCCESS("success"),
        @SerializedName(value = "failure") FAILURE("failure"),
        @SerializedName(value = "unclassified") UNCLASSIFIED("unclassified")
    }

    /**
     * Test header parameter(s)
     * Test header parameter(s)
     * Responses:
     *  - 200: Successful operation
     *
     * @param integerHeader  (optional)
     * @param booleanHeader  (optional)
     * @param stringHeader  (optional)
     * @param enumNonrefStringHeader  (optional)
     * @param enumRefStringHeader  (optional)
     * @return [String]
     */
    @GET("header/integer/boolean/string/enums")
    suspend fun testHeaderIntegerBooleanStringEnums(@Header("integer_header") integerHeader: Int? = null, @Header("boolean_header") booleanHeader: Boolean? = null, @Header("string_header") stringHeader: String? = null, @Header("enum_nonref_string_header") enumNonrefStringHeader: EnumNonrefStringHeaderTestHeaderIntegerBooleanStringEnums? = null, @Header("enum_ref_string_header") enumRefStringHeader: ApiStringEnumRef? = null): Response<String>

}
