package org.openapitools.api

import org.openapitools.model.ItemWithDollarAttributesAndExamples
import org.openapitools.model.ItemsItemIdSomethingItemSubIdGet200Response

interface ItemsApiService {

    /**
     * GET /items/{item$Id}/something/{item$SubId} : SQ = "; SBS = \; DBS = \\; SD = $some
     * SQ = "; SBS = \; DBS = \\; SD = $some
     *
     * @param itemDollarId SQ = "; SBS = \; DBS = \\; SD = $some (required)
     * @param itemDollarSubId SQ = "; SBS = \; DBS = \\; SD = $some (required)
     * @param filterDollarType SQ = "; SBS = \; DBS = \\; SD = $some (optional, default to "SQ = \"; SBS = \\; DBS = \\\\; SD = \$some")
     * @param filterDollarSubType SQ = "; SBS = \; DBS = \\; SD = $some (optional, default to "SQ = \"; SBS = \\; DBS = \\\\; SD = \$some")
     * @param xCustomHeader SQ = "; SBS = \; DBS = \\; SD = $some (optional)
     * @param xCustomHeaderTwo SQ = "; SBS = \; DBS = \\; SD = $some (optional)
     * @param sessionDollarToken SQ = "; SBS = \; DBS = \\; SD = $some (optional)
     * @param sessionDollarTokenTwo SQ = "; SBS = \; DBS = \\; SD = $some (optional)
     * @return SQ = "; SBS = \; DBS = \\; SD = $some (status code 200)
     * @see ItemsApi#itemsItemIdSomethingItemSubIdGet
     */
    fun itemsItemIdSomethingItemSubIdGet(itemDollarId: kotlin.String, itemDollarSubId: kotlin.String, filterDollarType: kotlin.String, filterDollarSubType: kotlin.String, xCustomHeader: kotlin.String?, xCustomHeaderTwo: kotlin.String?, sessionDollarToken: kotlin.String?, sessionDollarTokenTwo: kotlin.String?): ItemsItemIdSomethingItemSubIdGet200Response

    /**
     * POST /items : SQ = "; SBS = \; DBS = \\; SD = $some
     * SQ = "; SBS = \; DBS = \\; SD = $some
     *
     * @param xPostHeader SQ = "; SBS = \; DBS = \\; SD = $some (optional)
     * @param formDollarName SQ = \"; SBS = \\; DBS = \\\\; SD = $some (optional)
     * @param formDollarValue SQ = \"; SBS = \\; DBS = \\\\; SD = $some (optional, default to "SQ = \"; SBS = \\; DBS = \\\\; SD = \$some")
     * @return SQ = "; SBS = \; DBS = \\; SD = $some (status code 201)
     * @see ItemsApi#itemsPost
     */
    fun itemsPost(xPostHeader: kotlin.String?, formDollarName: kotlin.String?, formDollarValue: kotlin.String): ItemWithDollarAttributesAndExamples
}
