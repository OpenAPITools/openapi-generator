package org.openapitools.api

import org.openapitools.model.ItemsItemIdGet200Response
import org.springframework.http.HttpStatus
import org.springframework.http.MediaType
import org.springframework.http.ResponseEntity
import org.springframework.web.context.request.NativeWebRequest
import kotlinx.coroutines.flow.Flow

import java.util.Optional

/**
 * A delegate to be called by the {@link ItemsApiController}}.
 * Implement this interface with a {@link org.springframework.stereotype.Service} annotated class.
 */
@javax.annotation.Generated(value = ["org.openapitools.codegen.languages.KotlinSpringServerCodegen"], comments = "Generator version: 7.18.0-SNAPSHOT")
interface ItemsApiDelegate {

    fun getRequest(): Optional<NativeWebRequest> = Optional.empty()

    /**
     * @see ItemsApi#itemsItemIdGet
     */
    suspend fun itemsItemIdGet(itemDollarId: kotlin.String,
        filterDollarType: kotlin.String?,
        exchange: org.springframework.web.server.ServerWebExchange): ResponseEntity<ItemsItemIdGet200Response>

}
