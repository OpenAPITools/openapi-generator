package org.openapitools.client.models.responses

import okhttp3.Response
import org.openapitools.client.infrastructure.ApiClient
import org.openapitools.lib.datatypes.ReturnErrorObject
import org.openapitools.client.models.FirstModel
import org.openapitools.client.models.FourthModel
import org.openapitools.lib.datatypes.NoContentObject
import org.openapitools.client.models.SecondModel
import org.openapitools.client.models.ThirdModel
import org.openapitools.lib.datatypes.ReturnObject

class MultiStatusReturnGetResponse private constructor(
        val status200: ReturnObject<FirstModel>? = null,
        val status204: NoContentObject? = null,
        val status242: ReturnObject<SecondModel>? = null,
        val status400: ReturnObject<ThirdModel>? = null,
        val status500: ReturnObject<FourthModel>? = null,
        val error: ReturnErrorObject? = null
) {
    companion object {
        private fun createStatus200(response: Response): MultiStatusReturnGetResponse {
            return MultiStatusReturnGetResponse(status200 = ReturnObject(response, ApiClient.parseResponse(response)!!))
        }
        private fun createStatus204(response: Response): MultiStatusReturnGetResponse {
            return MultiStatusReturnGetResponse(status204 = NoContentObject(response))
        }
        private fun createStatus242(response: Response): MultiStatusReturnGetResponse {
            return MultiStatusReturnGetResponse(status242 = ReturnObject(response, ApiClient.parseResponse(response)!!))
        }
        private fun createStatus400(response: Response): MultiStatusReturnGetResponse {
            return MultiStatusReturnGetResponse(status400 = ReturnObject(response, ApiClient.parseResponse(response)!!))
        }
        private fun createStatus500(response: Response): MultiStatusReturnGetResponse {
            return MultiStatusReturnGetResponse(status500 = ReturnObject(response, ApiClient.parseResponse(response)!!))
        }
        private fun createError(response: Response): MultiStatusReturnGetResponse {
            return MultiStatusReturnGetResponse(error = ReturnErrorObject(response))
        }
        fun createFromResponse(response: Response): MultiStatusReturnGetResponse {
            val body = response.body
            if ((body == null) || (body.contentLength() <= 0)) {
                return createError(response)
            }
            return when (response.code) {
                200 -> createStatus200(response = response)
                204 -> createStatus204(response = response)
                242 -> createStatus242(response = response)
                400 -> createStatus400(response = response)
                500 -> createStatus500(response = response)
                else -> createError(response = response)
            }
        }
    }
    fun handle(
            handle200: (model: ReturnObject<FirstModel>) -> Any,
            handle204: (model: NoContentObject) -> Any,
            handle242: (model: ReturnObject<SecondModel>) -> Any,
            handle400: (model: ReturnObject<ThirdModel>) -> Any,
            handle500: (model: ReturnObject<FourthModel>) -> Any,
            handleError: (model: ReturnErrorObject) -> Any
    ) {
        when {
            this.status200 != null -> handle200(this.status200)
            this.status204 != null -> handle204(this.status204)
            this.status242 != null -> handle242(this.status242)
            this.status400 != null -> handle400(this.status400)
            this.status500 != null -> handle500(this.status500)
            this.error != null -> handleError(this.error)
        }
    }
}
