package org.openapitools.client.apis

import org.openapitools.client.infrastructure.CollectionFormats.*
import retrofit2.http.*
import retrofit2.Call
import okhttp3.RequestBody
import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable

import org.openapitools.client.models.AnyOfUserOrPet
import org.openapitools.client.models.AnyOfUserOrPetOrArrayString
import org.openapitools.client.models.BooleanOrLong
import org.openapitools.client.models.StringOrLong
import org.openapitools.client.models.UserOrPet
import org.openapitools.client.models.UserOrPetOrArrayString

interface TestApi {
    /**
     * GET v1/test/anyOf
     * 
     * 
     * Responses:
     *  - 200: OK
     *
     * @return [Call]<[AnyOfUserOrPet]>
     */
    @GET("v1/test/anyOf")
    fun getAnyOf(): Call<AnyOfUserOrPet>

    /**
     * GET v1/test/anyOfArray
     * 
     * 
     * Responses:
     *  - 200: OK
     *
     * @return [Call]<[AnyOfUserOrPetOrArrayString]>
     */
    @GET("v1/test/anyOfArray")
    fun getAnyOfArray(): Call<AnyOfUserOrPetOrArrayString>

    /**
     * GET v1/test/oneOf
     * 
     * 
     * Responses:
     *  - 200: OK
     *
     * @return [Call]<[UserOrPet]>
     */
    @GET("v1/test/oneOf")
    fun getOneOf(): Call<UserOrPet>

    /**
     * GET v1/test/oneOfArray
     * 
     * 
     * Responses:
     *  - 200: OK
     *
     * @return [Call]<[UserOrPetOrArrayString]>
     */
    @GET("v1/test/oneOfArray")
    fun getOneOfArray(): Call<UserOrPetOrArrayString>

    /**
     * GET v1/test/oneOfBooleanPrimitive
     * 
     * 
     * Responses:
     *  - 200: OK
     *
     * @return [Call]<[BooleanOrLong]>
     */
    @GET("v1/test/oneOfBooleanPrimitive")
    fun getOneOfBooleanPrimitive(): Call<BooleanOrLong>

    /**
     * GET v1/test/oneOfPrimitive
     * 
     * 
     * Responses:
     *  - 200: OK
     *
     * @return [Call]<[StringOrLong]>
     */
    @GET("v1/test/oneOfPrimitive")
    fun getOneOfPrimitive(): Call<StringOrLong>

}
