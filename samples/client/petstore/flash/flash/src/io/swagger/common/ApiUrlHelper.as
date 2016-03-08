package io.swagger.common {
import io.swagger.common.ApiUserCredentials;

/**
 * @private
 * Internal class for the Rest client
 */
internal class ApiUrlHelper {

    private static const API_URL_KEY:String = "api_key";
    private static const AUTH_TOKEN_URL_KEY:String = "auth_token";

    private static const HTTP_URL_PREFIX:String = "http://";

    internal static function appendTokenInfo(restUrl:String, requestHeader: Object, credentials: ApiUserCredentials): String {
        //checks for the presence api credentials on client initialization and not repeated here
        if(restUrl.indexOf("?") == -1){
            restUrl += ( "?" + API_URL_KEY + "=" + credentials.apiToken );
        }
        else{
            restUrl += ( "&" + API_URL_KEY + "=" + credentials.apiToken );
        }
        requestHeader.api_key = credentials.apiToken;

        if(credentials.authToken != null && credentials.authToken != ""){
            restUrl += ( "&" + AUTH_TOKEN_URL_KEY + "=" + credentials.authToken );
            requestHeader.auth_token = credentials.authToken;
        }

        return restUrl;
    }

    internal static function getProxyUrl(hostName: String, proxyPath: String): String{
        if (hostName..charAt(hostName.length) == "/") //remove trailing slash
        {
            hostName = hostName.substring(0, hostName.length - 1);
        }
        return HTTP_URL_PREFIX + hostName + proxyPath;
    }
}
}
