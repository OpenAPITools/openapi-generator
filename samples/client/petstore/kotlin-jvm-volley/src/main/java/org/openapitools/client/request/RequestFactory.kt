// Knowing the details of an operation it will produce a call to a Volley Request constructor
package org.openapitools.client.request


import com.android.volley.Request
import com.android.volley.Response
import android.util.Base64
import android.util.Base64
import org.openapitools.client.request.IRequestFactory.Companion.escapeString
import java.lang.reflect.Type
import java.util.Locale
import java.util.UUID


class RequestFactory(vararg headerFactory : () -> Map<String, String>): IRequestFactory {
   companion object Authentication{
  // Where a header factory requires parameters a client will need to bind these
  // TODO Generate appropriate header factories based on settings
      // Api Key auth supports query header and cookie.
// Query is supported in the path generation only with a hardcoded value.
// TODO: Not sure about cookie auth

val apiKeyAuthHeaderFactoryBuilder = { paramName: String, apiKeyPrefix: String?, apiKey: String? ->
{ mapOf("$paramName" to if (apiKeyPrefix != null) {
"$apiKeyPrefix $apiKey"
} else {
apiKey!!
})}
}

      // If implementing api key in query parameter use the ^isKeyInHeader property

}

   companion object Authentication{
  // Where a header factory requires parameters a client will need to bind these
  // TODO Generate appropriate header factories based on settings
      // TODO: Oauth not implemented yet - comment out below as OAuth does not exist


}


private val headerFactories : List<() -> Map<String, String>> = listOf(*headerFactory)

    /**
    * {@inheritDoc}
    */
    @Suppress("UNCHECKED_CAST")
    override fun <T> build(
        method: Int,
        url: String,
        body: Any?,
        headers: Map<String, String>?,
        params: Map<String, String>?,
        contentTypeForBody: String?,
        type: Type,
        responseListener: Response.Listener<T>,
        errorListener: Response.ErrorListener
    ): Request<T> {
            val afterMarketHeaders = (headers?.toMutableMap() ?: mutableMapOf())
            // Factory built and aftermarket
            // Merge the after market headers on top of the base ones in case we are overriding per call auth
            val allHeaders = headerFactories.fold(afterMarketHeaders, {acc, factory -> (acc + factory.invoke()).toMutableMap() }   )

                // If we decide to support auth parameters in the url, then you will reference them by supplying a url string
                // with known variable name refernces in the string. We will then apply
                val updatedUrl = if (!params.isNullOrEmpty()) {
              params.asSequence().fold("$url?") {acc, param ->
                "$acc${escapeString(param.key)}=${escapeString(param.value)}&"
              }.trimEnd('&')
            } else {
              url
            }

            return GsonRequest(
                method,
                updatedUrl,
                body,
                allHeaders,
                params?.toMutableMap(),
                contentTypeForBody,
                null,
                type,
                responseListener,
                errorListener)
    }
}
