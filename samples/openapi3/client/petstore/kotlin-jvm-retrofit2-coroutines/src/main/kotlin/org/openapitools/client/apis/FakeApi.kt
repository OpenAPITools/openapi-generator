package org.openapitools.client.apis

import org.openapitools.client.infrastructure.CollectionFormats.*
import retrofit2.http.*
import okhttp3.RequestBody
import okhttp3.ResponseBody
import okhttp3.MultipartBody

import org.openapitools.client.models.Client
import org.openapitools.client.models.FileSchemaTestClass
import org.openapitools.client.models.HealthCheckResult
import org.openapitools.client.models.OuterComposite
import org.openapitools.client.models.Pet
import org.openapitools.client.models.User

interface FakeApi {

    @GET("fake/health")
    suspend fun fakeHealthGet(): HealthCheckResult

    @GET("fake/http-signature-test")
    suspend fun fakeHttpSignatureTest(@Body pet: Pet, @Query("query_1") query1: kotlin.String? = null, @Header("header_1") header1: kotlin.String? = null): Unit

    @POST("fake/outer/boolean")
    suspend fun fakeOuterBooleanSerialize(@Body body: kotlin.Boolean? = null): kotlin.Boolean

    @POST("fake/outer/composite")
    suspend fun fakeOuterCompositeSerialize(@Body outerComposite: OuterComposite? = null): OuterComposite

    @POST("fake/outer/number")
    suspend fun fakeOuterNumberSerialize(@Body body: java.math.BigDecimal? = null): java.math.BigDecimal

    @POST("fake/outer/string")
    suspend fun fakeOuterStringSerialize(@Body body: kotlin.String? = null): kotlin.String

    @PUT("fake/body-with-file-schema")
    suspend fun testBodyWithFileSchema(@Body fileSchemaTestClass: FileSchemaTestClass): Unit

    @PUT("fake/body-with-query-params")
    suspend fun testBodyWithQueryParams(@Query("query") query: kotlin.String, @Body user: User): Unit

    @PATCH("fake")
    suspend fun testClientModel(@Body client: Client): Client

    @FormUrlEncoded
    @POST("fake")
    suspend fun testEndpointParameters(@Field("number") number: java.math.BigDecimal, @Field("double") double: kotlin.Double, @Field("pattern_without_delimiter") patternWithoutDelimiter: kotlin.String, @Field("byte") byte: kotlin.ByteArray, @Field("integer") integer: kotlin.Int? = null, @Field("int32") int32: kotlin.Int? = null, @Field("int64") int64: kotlin.Long? = null, @Field("float") float: kotlin.Float? = null, @Field("string") string: kotlin.String? = null, @Field("binary") binary: MultipartBody.Part? = null, @Field("date") date: java.time.LocalDate? = null, @Field("dateTime") dateTime: java.time.OffsetDateTime? = null, @Field("password") password: kotlin.String? = null, @Field("callback") paramCallback: kotlin.String? = null): Unit

    @FormUrlEncoded
    @GET("fake")
    suspend fun testEnumParameters(@Header("enum_header_string_array") enumHeaderStringArray: kotlin.collections.List<kotlin.String>? = null, @Header("enum_header_string") enumHeaderString: kotlin.String? = null, @Query("enum_query_string_array") enumQueryStringArray: kotlin.collections.List<kotlin.String>? = null, @Query("enum_query_string") enumQueryString: kotlin.String? = null, @Query("enum_query_integer") enumQueryInteger: kotlin.Int? = null, @Query("enum_query_double") enumQueryDouble: kotlin.Double? = null, @Field("enum_form_string_array") enumFormStringArray: kotlin.collections.List<kotlin.String>? = null, @Field("enum_form_string") enumFormString: kotlin.String? = null): Unit

    @DELETE("fake")
    suspend fun testGroupParameters(@Query("required_string_group") requiredStringGroup: kotlin.Int, @Header("required_boolean_group") requiredBooleanGroup: kotlin.Boolean, @Query("required_int64_group") requiredInt64Group: kotlin.Long, @Query("string_group") stringGroup: kotlin.Int? = null, @Header("boolean_group") booleanGroup: kotlin.Boolean? = null, @Query("int64_group") int64Group: kotlin.Long? = null): Unit

    @POST("fake/inline-additionalProperties")
    suspend fun testInlineAdditionalProperties(@Body requestBody: kotlin.collections.Map<kotlin.String, kotlin.String>): Unit

    @FormUrlEncoded
    @GET("fake/jsonFormData")
    suspend fun testJsonFormData(@Field("param") param: kotlin.String, @Field("param2") param2: kotlin.String): Unit

    @PUT("fake/test-query-paramters")
    suspend fun testQueryParameterCollectionFormat(@Query("pipe") pipe: kotlin.collections.List<kotlin.String>, @Query("ioutil") ioutil: CSVParams, @Query("http") http: SPACEParams, @Query("url") url: CSVParams, @Query("context") context: kotlin.collections.List<kotlin.String>): Unit

}
