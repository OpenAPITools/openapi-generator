package org.openapitools.client.apis

import org.openapitools.client.infrastructure.CollectionFormats.*
import retrofit2.http.*
import retrofit2.Call
import okhttp3.RequestBody
import com.squareup.moshi.Json

import java.math.BigDecimal

interface DefaultApi {
    /**
     * Tests default values
     * Tests default values of different parameters
     * Responses:
     *  - 200: Success
     *
     * @param pi0  (default to 10)
     * @param pi1 
     * @param pn0  (default to 10.0)
     * @param pn1 
     * @param qi0  (optional, default to 10)
     * @param qi1  (default to 71)
     * @param qi2  (optional)
     * @param qi3 
     * @param qn0  (optional, default to 10.0)
     * @param qn1  (default to 71.0)
     * @param qn2  (optional)
     * @param qn3 
     * @param hi0  (optional, default to 10)
     * @param hi1  (default to 71)
     * @param hi2  (optional)
     * @param hi3 
     * @param hn0  (optional, default to 10.0)
     * @param hn1  (default to 71.0)
     * @param hn2  (optional)
     * @param hn3 
     * @param fi0  (optional, default to 10)
     * @param fi1  (default to 71)
     * @param fi2  (optional)
     * @param fi3 
     * @param fn0  (optional, default to 10.0)
     * @param fn1  (default to 71.0)
     * @param fn2  (optional)
     * @param fn3 
     * @param fn4 
     * @return [Call]<[Unit]>
     */
    @Multipart
    @POST("test")
    fun test(@Path("pi0") pi0: Int = 10, @Path("pi1") pi1: Int, @Path("pn0") pn0: BigDecimal = BigDecimal("10.0"), @Path("pn1") pn1: BigDecimal, @Query("qi0") qi0: Int? = 10, @Query("qi1") qi1: Int = 71, @Query("qi2") qi2: Int? = null, @Query("qi3") qi3: Int, @Query("qn0") qn0: BigDecimal? = BigDecimal("10.0"), @Query("qn1") qn1: BigDecimal = BigDecimal("71.0"), @Query("qn2") qn2: BigDecimal? = null, @Query("qn3") qn3: BigDecimal, @Header("hi0") hi0: Int? = 10, @Header("hi1") hi1: Int = 71, @Header("hi2") hi2: Int? = null, @Header("hi3") hi3: Int, @Header("hn0") hn0: BigDecimal? = BigDecimal("10.0"), @Header("hn1") hn1: BigDecimal = BigDecimal("71.0"), @Header("hn2") hn2: BigDecimal? = null, @Header("hn3") hn3: BigDecimal, @Part("fi0") fi0: Int? = 10, @Part("fi1") fi1: Int = 71, @Part("fi2") fi2: Int? = null, @Part("fi3") fi3: Int, @Part("fn0") fn0: BigDecimal? = BigDecimal("10.0"), @Part("fn1") fn1: BigDecimal = BigDecimal("71.0"), @Part("fn2") fn2: BigDecimal? = null, @Part("fn3") fn3: BigDecimal, @Part("fn4") fn4: List<String>): Call<Unit>

}
