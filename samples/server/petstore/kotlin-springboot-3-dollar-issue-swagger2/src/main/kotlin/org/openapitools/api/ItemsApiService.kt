package org.openapitools.api

import org.openapitools.model.ItemWithDollarAttributesAndExamples
import org.openapitools.model.ItemsItemIdSomethingItemSubIdGet200Response

interface ItemsApiService {

    /**
     * GET /items/{item$Id}/something/{item$SubId} : SQ &#x3D; \&quot;; SBS &#x3D; \\; DBS &#x3D; \\\\; SD &#x3D; $some
     * SQ &#x3D; \&quot;; SBS &#x3D; \\; DBS &#x3D; \\\\; SD &#x3D; $some
     *
     * @param itemDollarId SQ &#x3D; \&quot;; SBS &#x3D; \\; DBS &#x3D; \\\\; SD &#x3D; $some (required)
     * @param itemDollarSubId SQ &#x3D; \&quot;; SBS &#x3D; \\; DBS &#x3D; \\\\; SD &#x3D; $some (required)
     * @param filterDollarType SQ &#x3D; \&quot;; SBS &#x3D; \\; DBS &#x3D; \\\\; SD &#x3D; $some (optional, default to &quot;SQ &#x3D; \&quot;; SBS &#x3D; \\; DBS &#x3D; \\\\; SD &#x3D; $some&quot;)
     * @param filterDollarSubType SQ &#x3D; \&quot;; SBS &#x3D; \\; DBS &#x3D; \\\\; SD &#x3D; $some (optional, default to &quot;SQ &#x3D; \&quot;; SBS &#x3D; \\; DBS &#x3D; \\\\; SD &#x3D; $some&quot;)
     * @param xCustomHeader SQ &#x3D; \&quot;; SBS &#x3D; \\; DBS &#x3D; \\\\; SD &#x3D; $some (optional)
     * @param xCustomHeaderTwo SQ &#x3D; \&quot;; SBS &#x3D; \\; DBS &#x3D; \\\\; SD &#x3D; $some (optional)
     * @param sessionDollarToken SQ &#x3D; \&quot;; SBS &#x3D; \\; DBS &#x3D; \\\\; SD &#x3D; $some (optional)
     * @param sessionDollarTokenTwo SQ &#x3D; \&quot;; SBS &#x3D; \\; DBS &#x3D; \\\\; SD &#x3D; $some (optional)
     * @return SQ &#x3D; \&quot;; SBS &#x3D; \\; DBS &#x3D; \\\\; SD &#x3D; $some (status code 200)
     * @see ItemsApi#itemsItemIdSomethingItemSubIdGet
     */
    fun itemsItemIdSomethingItemSubIdGet(itemDollarId: kotlin.String, itemDollarSubId: kotlin.String, filterDollarType: kotlin.String, filterDollarSubType: kotlin.String, xCustomHeader: kotlin.String?, xCustomHeaderTwo: kotlin.String?, sessionDollarToken: kotlin.String?, sessionDollarTokenTwo: kotlin.String?): ItemsItemIdSomethingItemSubIdGet200Response

    /**
     * POST /items : SQ &#x3D; \&quot;; SBS &#x3D; \\; DBS &#x3D; \\\\; SD &#x3D; $some
     * SQ &#x3D; \&quot;; SBS &#x3D; \\; DBS &#x3D; \\\\; SD &#x3D; $some
     *
     * @param xPostHeader SQ &#x3D; \&quot;; SBS &#x3D; \\; DBS &#x3D; \\\\; SD &#x3D; $some (optional)
     * @param formDollarName SQ &#x3D; \\\&quot;; SBS &#x3D; \\\\; DBS &#x3D; \\\\\\\\; SD &#x3D; $some (optional)
     * @param formDollarValue SQ &#x3D; \\\&quot;; SBS &#x3D; \\\\; DBS &#x3D; \\\\\\\\; SD &#x3D; $some (optional, default to &quot;SQ &#x3D; \&quot;; SBS &#x3D; \\; DBS &#x3D; \\\\; SD &#x3D; $some&quot;)
     * @return SQ &#x3D; \&quot;; SBS &#x3D; \\; DBS &#x3D; \\\\; SD &#x3D; $some (status code 201)
     * @see ItemsApi#itemsPost
     */
    fun itemsPost(xPostHeader: kotlin.String?, formDollarName: kotlin.String?, formDollarValue: kotlin.String): ItemWithDollarAttributesAndExamples
}
