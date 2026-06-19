package org.openapitools.client.apis

import org.openapitools.client.infrastructure.CollectionFormats.*
import retrofit2.http.*
import retrofit2.Response
import okhttp3.RequestBody
import com.google.gson.annotations.SerializedName


interface FormApi {
    /**
     * POST form/integer/boolean/string
     * Test form parameter(s)
     * Test form parameter(s)
     * Responses:
     *  - 200: Successful operation
     *
     * @param integerForm  (optional)
     * @param booleanForm  (optional)
     * @param stringForm  (optional)
     * @return [kotlin.String]
     */
    @FormUrlEncoded
    @POST("form/integer/boolean/string")
    suspend fun testFormIntegerBooleanString(@Field("integer_form") integerForm: kotlin.Int? = null, @Field("boolean_form") booleanForm: kotlin.Boolean? = null, @Field("string_form") stringForm: kotlin.String? = null): Response<kotlin.String>

    /**
     * POST form/oneof
     * Test form parameter(s) for oneOf schema
     * Test form parameter(s) for oneOf schema
     * Responses:
     *  - 200: Successful operation
     *
     * @param form1  (optional)
     * @param form2  (optional)
     * @param form3  (optional)
     * @param form4  (optional)
     * @param id  (optional)
     * @param name  (optional)
     * @return [kotlin.String]
     */
    @FormUrlEncoded
    @POST("form/oneof")
    suspend fun testFormOneof(@Field("form1") form1: kotlin.String? = null, @Field("form2") form2: kotlin.Int? = null, @Field("form3") form3: kotlin.String? = null, @Field("form4") form4: kotlin.Boolean? = null, @Field("id") id: kotlin.Long? = null, @Field("name") name: kotlin.String? = null): Response<kotlin.String>

}
