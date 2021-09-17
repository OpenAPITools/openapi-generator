package org.openapitools.client.apis

import org.openapitools.client.infrastructure.CollectionFormats.*
import retrofit2.http.*
import retrofit2.Response
import okhttp3.RequestBody

import org.openapitools.client.models.Client
import org.openapitools.client.models.FileSchemaTestClass
import org.openapitools.client.models.HealthCheckResult
import org.openapitools.client.models.OuterComposite
import org.openapitools.client.models.Pet
import org.openapitools.client.models.User

import okhttp3.MultipartBody

interface FakeApi {
    /**
     * Health check endpoint
     * 
     * Responses:
     *  - 200: The instance started successfully
     * 
     * @return [HealthCheckResult]
     */
    @GET("fake/health")
    suspend fun fakeHealthGet(): Response<HealthCheckResult>

    /**
     * test http signature authentication
     * 
     * Responses:
     *  - 200: The instance started successfully
     * 
     * @param pet Pet object that needs to be added to the store 
     * @param query1 query parameter (optional)
     * @param header1 header parameter (optional)
     * @return [Unit]
     */
    @GET("fake/http-signature-test")
    suspend fun fakeHttpSignatureTest(@Body pet: Pet, @Query("query_1") query1: kotlin.String? = null, @Header("header_1") header1: kotlin.String): Response<Unit>

    /**
     * 
     * Test serialization of outer boolean types
     * Responses:
     *  - 200: Output boolean
     * 
     * @param body Input boolean as post body (optional)
     * @return [kotlin.Boolean]
     */
    @POST("fake/outer/boolean")
    suspend fun fakeOuterBooleanSerialize(@Body body: kotlin.Boolean? = null): Response<kotlin.Boolean>

    /**
     * 
     * Test serialization of object with outer number type
     * Responses:
     *  - 200: Output composite
     * 
     * @param outerComposite Input composite as post body (optional)
     * @return [OuterComposite]
     */
    @POST("fake/outer/composite")
    suspend fun fakeOuterCompositeSerialize(@Body outerComposite: OuterComposite? = null): Response<OuterComposite>

    /**
     * 
     * Test serialization of outer number types
     * Responses:
     *  - 200: Output number
     * 
     * @param body Input number as post body (optional)
     * @return [java.math.BigDecimal]
     */
    @POST("fake/outer/number")
    suspend fun fakeOuterNumberSerialize(@Body body: java.math.BigDecimal? = null): Response<java.math.BigDecimal>

    /**
     * 
     * Test serialization of outer string types
     * Responses:
     *  - 200: Output string
     * 
     * @param body Input string as post body (optional)
     * @return [kotlin.String]
     */
    @POST("fake/outer/string")
    suspend fun fakeOuterStringSerialize(@Body body: kotlin.String? = null): Response<kotlin.String>

    /**
     * 
     * For this test, the body for this request much reference a schema named &#x60;File&#x60;.
     * Responses:
     *  - 200: Success
     * 
     * @param fileSchemaTestClass  
     * @return [Unit]
     */
    @PUT("fake/body-with-file-schema")
    suspend fun testBodyWithFileSchema(@Body fileSchemaTestClass: FileSchemaTestClass): Response<Unit>

    /**
     * 
     * 
     * Responses:
     *  - 200: Success
     * 
     * @param query  
     * @param user  
     * @return [Unit]
     */
    @PUT("fake/body-with-query-params")
    suspend fun testBodyWithQueryParams(@Query("query") query: kotlin.String, @Body user: User): Response<Unit>

    /**
     * To test \&quot;client\&quot; model
     * To test \&quot;client\&quot; model
     * Responses:
     *  - 200: successful operation
     * 
     * @param client client model 
     * @return [Client]
     */
    @PATCH("fake")
    suspend fun testClientModel(@Body client: Client): Response<Client>

    /**
     * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
     * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
     * Responses:
     *  - 400: Invalid username supplied
     *  - 404: User not found
     * 
     * @param number None 
     * @param double None 
     * @param patternWithoutDelimiter None 
     * @param byte None 
     * @param integer None (optional)
     * @param int32 None (optional)
     * @param int64 None (optional)
     * @param float None (optional)
     * @param string None (optional)
     * @param binary None (optional)
     * @param date None (optional)
     * @param dateTime None (optional)
     * @param password None (optional)
     * @param paramCallback None (optional)
     * @return [Unit]
     */
    @FormUrlEncoded
    @POST("fake")
    suspend fun testEndpointParameters(@Field("number") number: java.math.BigDecimal, @Field("double") double: kotlin.Double, @Field("pattern_without_delimiter") patternWithoutDelimiter: kotlin.String, @Field("byte") byte: kotlin.ByteArray, @Field("integer") integer: kotlin.Int, @Field("int32") int32: kotlin.Int, @Field("int64") int64: kotlin.Long, @Field("float") float: kotlin.Float, @Field("string") string: kotlin.String, @Field("binary") binary: MultipartBody.Part, @Field("date") date: java.time.LocalDate, @Field("dateTime") dateTime: java.time.OffsetDateTime, @Field("password") password: kotlin.String, @Field("callback") paramCallback: kotlin.String): Response<Unit>

    /**
     * To test enum parameters
     * To test enum parameters
     * Responses:
     *  - 400: Invalid request
     *  - 404: Not found
     * 
     * @param enumHeaderStringArray Header parameter enum test (string array) (optional)
     * @param enumHeaderString Header parameter enum test (string) (optional, default to -efg)
     * @param enumQueryStringArray Query parameter enum test (string array) (optional)
     * @param enumQueryString Query parameter enum test (string) (optional, default to -efg)
     * @param enumQueryInteger Query parameter enum test (double) (optional)
     * @param enumQueryDouble Query parameter enum test (double) (optional)
     * @param enumFormStringArray Form parameter enum test (string array) (optional, default to $)
     * @param enumFormString Form parameter enum test (string) (optional, default to -efg)
     * @return [Unit]
     */
    @FormUrlEncoded
    @GET("fake")
    suspend fun testEnumParameters(@Header("enum_header_string_array") enumHeaderStringArray: kotlin.collections.List<kotlin.String>, @Header("enum_header_string") enumHeaderString: kotlin.String, @Query("enum_query_string_array") enumQueryStringArray: kotlin.collections.List<kotlin.String>? = null, @Query("enum_query_string") enumQueryString: kotlin.String? = null, @Query("enum_query_integer") enumQueryInteger: kotlin.Int? = null, @Query("enum_query_double") enumQueryDouble: kotlin.Double? = null, @Field("enum_form_string_array") enumFormStringArray: kotlin.collections.List<kotlin.String>, @Field("enum_form_string") enumFormString: kotlin.String): Response<Unit>

    /**
     * Fake endpoint to test group parameters (optional)
     * Fake endpoint to test group parameters (optional)
     * Responses:
     *  - 400: Someting wrong
     * 
     * @param requiredStringGroup Required String in group parameters 
     * @param requiredBooleanGroup Required Boolean in group parameters 
     * @param requiredInt64Group Required Integer in group parameters 
     * @param stringGroup String in group parameters (optional)
     * @param booleanGroup Boolean in group parameters (optional)
     * @param int64Group Integer in group parameters (optional)
     * @return [Unit]
     */
    @DELETE("fake")
    suspend fun testGroupParameters(@Query("required_string_group") requiredStringGroup: kotlin.Int, @Header("required_boolean_group") requiredBooleanGroup: kotlin.Boolean, @Query("required_int64_group") requiredInt64Group: kotlin.Long, @Query("string_group") stringGroup: kotlin.Int? = null, @Header("boolean_group") booleanGroup: kotlin.Boolean, @Query("int64_group") int64Group: kotlin.Long? = null): Response<Unit>

    /**
     * test inline additionalProperties
     * 
     * Responses:
     *  - 200: successful operation
     * 
     * @param requestBody request body 
     * @return [Unit]
     */
    @POST("fake/inline-additionalProperties")
    suspend fun testInlineAdditionalProperties(@Body requestBody: kotlin.collections.Map<kotlin.String, kotlin.String>): Response<Unit>

    /**
     * test json serialization of form data
     * 
     * Responses:
     *  - 200: successful operation
     * 
     * @param param field1 
     * @param param2 field2 
     * @return [Unit]
     */
    @FormUrlEncoded
    @GET("fake/jsonFormData")
    suspend fun testJsonFormData(@Field("param") param: kotlin.String, @Field("param2") param2: kotlin.String): Response<Unit>

    /**
     * 
     * To test the collection format in query parameters
     * Responses:
     *  - 200: Success
     * 
     * @param pipe  
     * @param ioutil  
     * @param http  
     * @param url  
     * @param context  
     * @return [Unit]
     */
    @PUT("fake/test-query-parameters")
    suspend fun testQueryParameterCollectionFormat(@Query("pipe") pipe: kotlin.collections.List<kotlin.String>, @Query("ioutil") ioutil: CSVParams, @Query("http") http: SSVParams, @Query("url") url: CSVParams, @Query("context") context: kotlin.collections.List<kotlin.String>): Response<Unit>

}
