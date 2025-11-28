package org.openapitools.client.apis

import org.openapitools.client.infrastructure.CollectionFormats.*
import retrofit2.http.*
import retrofit2.Call
import okhttp3.RequestBody
import com.squareup.moshi.Json

import org.openapitools.client.models.ItemWithDollarAttributesAndExamples
import org.openapitools.client.models.ItemsItemIdSomethingItemSubIdGet200Response

interface DefaultApi {
    /**
     * GET items/{item$Id}/something/{item$SubId}
     * SQ &#x3D; \&quot;; SBS &#x3D; \\; DBS &#x3D; \\\\; SD &#x3D; $some
     * SQ &#x3D; \&quot;; SBS &#x3D; \\; DBS &#x3D; \\\\; SD &#x3D; $some
     * Responses:
     *  - 200: SQ = \"; SBS = \\; DBS = \\\\; SD = $some
     *
     * @param itemDollarId SQ &#x3D; \&quot;; SBS &#x3D; \\; DBS &#x3D; \\\\; SD &#x3D; $some
     * @param itemDollarSubId SQ &#x3D; \&quot;; SBS &#x3D; \\; DBS &#x3D; \\\\; SD &#x3D; $some
     * @param filterDollarType SQ &#x3D; \&quot;; SBS &#x3D; \\; DBS &#x3D; \\\\; SD &#x3D; $some (optional, default to "SQ = \"; SBS = \\; DBS = \\\\; SD = $some")
     * @param filterDollarSubType SQ &#x3D; \&quot;; SBS &#x3D; \\; DBS &#x3D; \\\\; SD &#x3D; $some (optional, default to "SQ = \"; SBS = \\; DBS = \\\\; SD = $some")
     * @param xCustomHeader SQ &#x3D; \&quot;; SBS &#x3D; \\; DBS &#x3D; \\\\; SD &#x3D; $some (optional)
     * @param xCustomHeaderTwo SQ &#x3D; \&quot;; SBS &#x3D; \\; DBS &#x3D; \\\\; SD &#x3D; $some (optional)
     * @return [Call]<[ItemsItemIdSomethingItemSubIdGet200Response]>
     */
    @GET("items/{item$Id}/something/{item$SubId}")
    fun itemsItemIdSomethingItemSubIdGet(@Path("item$Id") itemDollarId: kotlin.String, @Path("item$SubId") itemDollarSubId: kotlin.String, @Query("filter$Type") filterDollarType: kotlin.String? = "SQ = \"; SBS = \\; DBS = \\\\; SD = $some", @Query("filter$SubType") filterDollarSubType: kotlin.String? = "SQ = \"; SBS = \\; DBS = \\\\; SD = $some", @Header("X-Custom_Header") xCustomHeader: kotlin.String? = null, @Header("X-Custom_Header_two") xCustomHeaderTwo: kotlin.String? = null): Call<ItemsItemIdSomethingItemSubIdGet200Response>

    /**
     * POST items
     * SQ &#x3D; \&quot;; SBS &#x3D; \\; DBS &#x3D; \\\\; SD &#x3D; $some
     * SQ &#x3D; \&quot;; SBS &#x3D; \\; DBS &#x3D; \\\\; SD &#x3D; $some
     * Responses:
     *  - 201: SQ = \"; SBS = \\; DBS = \\\\; SD = $some
     *
     * @param xPostHeader SQ &#x3D; \&quot;; SBS &#x3D; \\; DBS &#x3D; \\\\; SD &#x3D; $some (optional)
     * @param formDollarName SQ &#x3D; \\\&quot;; SBS &#x3D; \\\\; DBS &#x3D; \\\\\\\\; SD &#x3D; $some (optional)
     * @param formDollarValue SQ &#x3D; \\\&quot;; SBS &#x3D; \\\\; DBS &#x3D; \\\\\\\\; SD &#x3D; $some (optional, default to "SQ = \"; SBS = \\; DBS = \\\\; SD = $some")
     * @return [Call]<[ItemWithDollarAttributesAndExamples]>
     */
    @FormUrlEncoded
    @POST("items")
    fun itemsPost(@Header("X-Post_Header") xPostHeader: kotlin.String? = null, @Field("form$Name") formDollarName: kotlin.String? = null, @Field("form$Value") formDollarValue: kotlin.String? = "SQ = \"; SBS = \\; DBS = \\\\; SD = $some"): Call<ItemWithDollarAttributesAndExamples>

}
