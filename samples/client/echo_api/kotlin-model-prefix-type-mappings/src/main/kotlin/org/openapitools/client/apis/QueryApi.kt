package org.openapitools.client.apis

import org.openapitools.client.infrastructure.CollectionFormats.*
import retrofit2.http.*
import retrofit2.Response
import okhttp3.RequestBody
import com.google.gson.annotations.SerializedName

import org.openapitools.client.models.ApiPet
import org.openapitools.client.models.ApiStringEnumRef
import org.openapitools.client.models.ApiTestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter
import java.time.LocalDate
import java.time.OffsetDateTime

import org.openapitools.client.models.*

interface QueryApi {

    /**
    * enum for parameter enumNonrefStringQuery
    */
    enum class EnumNonrefStringQueryTestEnumRefString(val value: String) {
        @SerializedName(value = "success") SUCCESS("success"),
        @SerializedName(value = "failure") FAILURE("failure"),
        @SerializedName(value = "unclassified") UNCLASSIFIED("unclassified")
    }

    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * Responses:
     *  - 200: Successful operation
     *
     * @param enumNonrefStringQuery  (optional)
     * @param enumRefStringQuery  (optional)
     * @return [String]
     */
    @GET("query/enum_ref_string")
    suspend fun testEnumRefString(@Query("enum_nonref_string_query") enumNonrefStringQuery: EnumNonrefStringQueryTestEnumRefString? = null, @Query("enum_ref_string_query") enumRefStringQuery: ApiStringEnumRef? = null): Response<String>

    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * Responses:
     *  - 200: Successful operation
     *
     * @param datetimeQuery  (optional)
     * @param dateQuery  (optional)
     * @param stringQuery  (optional)
     * @return [String]
     */
    @GET("query/datetime/date/string")
    suspend fun testQueryDatetimeDateString(@Query("datetime_query") datetimeQuery: OffsetDateTime? = null, @Query("date_query") dateQuery: LocalDate? = null, @Query("string_query") stringQuery: String? = null): Response<String>

    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * Responses:
     *  - 200: Successful operation
     *
     * @param integerQuery  (optional)
     * @param booleanQuery  (optional)
     * @param stringQuery  (optional)
     * @return [String]
     */
    @GET("query/integer/boolean/string")
    suspend fun testQueryIntegerBooleanString(@Query("integer_query") integerQuery: Int? = null, @Query("boolean_query") booleanQuery: Boolean? = null, @Query("string_query") stringQuery: String? = null): Response<String>

    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * Responses:
     *  - 200: Successful operation
     *
     * @param queryObject  (optional)
     * @return [String]
     */
    @GET("query/style_deepObject/explode_true/object")
    suspend fun testQueryStyleDeepObjectExplodeTrueObject(@Query("query_object") queryObject: ApiPet? = null): Response<String>

    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * Responses:
     *  - 200: Successful operation
     *
     * @param values  (optional)
     * @return [String]
     */
    @GET("query/style_form/explode_true/array_string")
    suspend fun testQueryStyleFormExplodeTrueArrayString(@Query("values") values: List<String>? = null): Response<String>

    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * Responses:
     *  - 200: Successful operation
     *
     * @param id  (optional)
     * @param name 
     * @param category  (optional)
     * @param photoUrls 
     * @param tags  (optional)
     * @param status pet status in the store (optional)
     * @return [String]
     */
    @GET("query/style_form/explode_true/object")
    suspend fun testQueryStyleFormExplodeTrueObject(@Query("id") id: Long? = null, @Query("name") name: String, @Query("category") category: ApiCategory? = null, @Query("photoUrls") photoUrls: List<String>, @Query("tags") tags: List<ApiTag>? = null, @Query("status") status: String? = null): Response<String>

}
