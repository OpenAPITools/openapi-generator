package org.openapitools.client.apis

import org.openapitools.client.infrastructure.CollectionFormats.*
import retrofit2.http.*
import okhttp3.RequestBody
import io.reactivex.Single
import io.reactivex.Completable

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
     * @return [Call]<[HealthCheckResult]>
     */
    @GET("fake/health")
    fun fakeHealthGet(): Single<HealthCheckResult>

    /**
     * test http signature authentication
     * 
     * Responses:
     *  - 200: The instance started successfully
     * 
     * @param pet Pet object that needs to be added to the store 
     * @param query1 query parameter (optional)
     * @param header1 header parameter (optional)
     * @return [Call]<[Unit]>
     */
    @GET("fake/http-signature-test")
    fun fakeHttpSignatureTest(@Body pet: Pet, @Query("query_1") query1: kotlin.String? = null, @Header("header_1") header1: kotlin.String): Completable

    /**
     * 
     * Test serialization of outer boolean types
     * Responses:
     *  - 200: Output boolean
     * 
     * @param body Input boolean as post body (optional)
     * @return [Call]<[kotlin.Boolean]>
     */
    @POST("fake/outer/boolean")
    fun fakeOuterBooleanSerialize(@Body body: kotlin.Boolean? = null): Single<kotlin.Boolean>

    /**
     * 
     * Test serialization of object with outer number type
     * Responses:
     *  - 200: Output composite
     * 
     * @param outerComposite Input composite as post body (optional)
     * @return [Call]<[OuterComposite]>
     */
    @POST("fake/outer/composite")
    fun fakeOuterCompositeSerialize(@Body outerComposite: OuterComposite? = null): Single<OuterComposite>

    /**
     * 
     * Test serialization of outer number types
     * Responses:
     *  - 200: Output number
     * 
     * @param body Input number as post body (optional)
     * @return [Call]<[java.math.BigDecimal]>
     */
    @POST("fake/outer/number")
    fun fakeOuterNumberSerialize(@Body body: java.math.BigDecimal? = null): Single<java.math.BigDecimal>

    /**
     * 
     * Test serialization of outer string types
     * Responses:
     *  - 200: Output string
     * 
     * @param body Input string as post body (optional)
     * @return [Call]<[kotlin.String]>
     */
    @POST("fake/outer/string")
    fun fakeOuterStringSerialize(@Body body: kotlin.String? = null): Single<kotlin.String>

    /**
     * 
     * For this test, the body for this request much reference a schema named &#x60;File&#x60;.
     * Responses:
     *  - 200: Success
     * 
     * @param fileSchemaTestClass  
     * @return [Call]<[Unit]>
     */
    @PUT("fake/body-with-file-schema")
    fun testBodyWithFileSchema(@Body fileSchemaTestClass: FileSchemaTestClass): Completable

    /**
     * 
     * 
     * Responses:
     *  - 200: Success
     * 
     * @param query  
     * @param user  
     * @return [Call]<[Unit]>
     */
    @PUT("fake/body-with-query-params")
    fun testBodyWithQueryParams(@Query("query") query: kotlin.String, @Body user: User): Completable

    /**
     * To test \&quot;client\&quot; model
     * To test \&quot;client\&quot; model
     * Responses:
     *  - 200: successful operation
     * 
     * @param client client model 
     * @return [Call]<[Client]>
     */
    @PATCH("fake")
    fun testClientModel(@Body client: Client): Single<Client>

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
     * @return [Call]<[Unit]>
     */
    @FormUrlEncoded
    @POST("fake")
    fun testEndpointParameters(@Field("number") number: java.math.BigDecimal, @Field("double") double: kotlin.Double, @Field("pattern_without_delimiter") patternWithoutDelimiter: kotlin.String, @Field("byte") byte: kotlin.ByteArray, @Field("integer") integer: kotlin.Int, @Field("int32") int32: kotlin.Int, @Field("int64") int64: kotlin.Long, @Field("float") float: kotlin.Float, @Field("string") string: kotlin.String, @Field("binary") binary: MultipartBody.Part, @Field("date") date: java.time.LocalDate, @Field("dateTime") dateTime: java.time.OffsetDateTime, @Field("password") password: kotlin.String, @Field("callback") paramCallback: kotlin.String): Completable

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
     * @return [Call]<[Unit]>
     */
    @FormUrlEncoded
    @GET("fake")
    fun testEnumParameters(@Header("enum_header_string_array") enumHeaderStringArray: kotlin.collections.List<kotlin.String>, @Header("enum_header_string") enumHeaderString: kotlin.String, @Query("enum_query_string_array") enumQueryStringArray: kotlin.collections.List<kotlin.String>? = null, @Query("enum_query_string") enumQueryString: kotlin.String? = null, @Query("enum_query_integer") enumQueryInteger: kotlin.Int? = null, @Query("enum_query_double") enumQueryDouble: kotlin.Double? = null, @Field("enum_form_string_array") enumFormStringArray: kotlin.collections.List<kotlin.String>, @Field("enum_form_string") enumFormString: kotlin.String): Completable

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
     * @return [Call]<[Unit]>
     */
    @DELETE("fake")
    fun testGroupParameters(@Query("required_string_group") requiredStringGroup: kotlin.Int, @Header("required_boolean_group") requiredBooleanGroup: kotlin.Boolean, @Query("required_int64_group") requiredInt64Group: kotlin.Long, @Query("string_group") stringGroup: kotlin.Int? = null, @Header("boolean_group") booleanGroup: kotlin.Boolean, @Query("int64_group") int64Group: kotlin.Long? = null): Completable

    /**
     * test inline additionalProperties
     * 
     * Responses:
     *  - 200: successful operation
     * 
     * @param requestBody request body 
     * @return [Call]<[Unit]>
     */
    @POST("fake/inline-additionalProperties")
    fun testInlineAdditionalProperties(@Body requestBody: kotlin.collections.Map<kotlin.String, kotlin.String>): Completable

    /**
     * test json serialization of form data
     * 
     * Responses:
     *  - 200: successful operation
     * 
     * @param param field1 
     * @param param2 field2 
     * @return [Call]<[Unit]>
     */
    @FormUrlEncoded
    @GET("fake/jsonFormData")
    fun testJsonFormData(@Field("param") param: kotlin.String, @Field("param2") param2: kotlin.String): Completable

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
     * @return [Call]<[Unit]>
     */
    @PUT("fake/test-query-parameters")
    fun testQueryParameterCollectionFormat(@Query("pipe") pipe: kotlin.collections.List<kotlin.String>, @Query("ioutil") ioutil: CSVParams, @Query("http") http: SSVParams, @Query("url") url: CSVParams, @Query("context") context: kotlin.collections.List<kotlin.String>): Completable

}
