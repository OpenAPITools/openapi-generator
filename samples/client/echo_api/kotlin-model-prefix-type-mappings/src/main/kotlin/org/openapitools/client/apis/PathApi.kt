package org.openapitools.client.apis

import org.openapitools.client.infrastructure.CollectionFormats.*
import retrofit2.http.*
import retrofit2.Response
import okhttp3.RequestBody
import com.google.gson.annotations.SerializedName

import org.openapitools.client.models.ApiStringEnumRef

interface PathApi {

    /**
    * enum for parameter enumNonrefStringPath
    */
    enum class EnumNonrefStringPathTestsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPath(val value: kotlin.String) {
        @SerializedName(value = "success") SUCCESS("success"),
        @SerializedName(value = "failure") FAILURE("failure"),
        @SerializedName(value = "unclassified") UNCLASSIFIED("unclassified")
    }

    /**
     * Test path parameter(s)
     * Test path parameter(s)
     * Responses:
     *  - 200: Successful operation
     *
     * @param pathString 
     * @param pathInteger 
     * @param enumNonrefStringPath 
     * @param enumRefStringPath 
     * @return [kotlin.String]
     */
    @GET("path/string/{path_string}/integer/{path_integer}/{enum_nonref_string_path}/{enum_ref_string_path}")
    suspend fun testsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPath(@Path("path_string") pathString: kotlin.String, @Path("path_integer") pathInteger: kotlin.Int, @Path("enum_nonref_string_path") enumNonrefStringPath: kotlin.String, @Path("enum_ref_string_path") enumRefStringPath: ApiStringEnumRef): Response<kotlin.String>

}
