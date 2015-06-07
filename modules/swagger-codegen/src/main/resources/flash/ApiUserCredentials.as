package io.swagger.common {

/**
 * Api account credentials.
 *
 */
public class ApiUserCredentials {
    /**
     * An apitoken that is passed along with the requests
     */
    public var apiToken:String;
    /**
     * A valid auth_token which could be necessary for certain operations
     */
    public var authToken:String;
    /**
     * The userId which could be required for certain operations
     */
    public var userId:Number;
    /**
     * The host name for the Rest API eg. api.companyName.com
     */
    public var hostName:String;
	
	/**
	 * The base path to the api resources - used along with the hostname 
	 * eg. /v4  
	 */ 
	public var apiPath: String;
	
	/**
	 * The base path to the blazeds proxy
	 * eg. /v4/messagebroker/restproxy
	 */
	public var proxyPath: String;

	/**
	 * If a proxy server has been set up for the services specify the URL here. This value is used when the Api is invoked with 
	 * the value useProxy as true
	 */ 
	public var apiProxyServerUrl: String;

    /**
     * Constructor of ApiUserCredentials
     * @param apiToken An apitoken that is passed along with the requests
     * @param authToken A valid auth_token which could necessary for certain operations
     * @param hostName The host name for the Rest API eg. api.companyName.com
     * @param userId The userId which is required for certain operations - currently, get user lists
     */
    public function ApiUserCredentials(hostName: String, apiPath: String, apiToken: String,
                                       authToken: String = null, userId: Number = -1, apiProxyServerUrl: String="",
                                       proxyPath: String = null) {
        this.hostName = hostName;
        this.apiToken = apiToken;
        this.authToken = authToken;
        this.userId = userId;
		this.apiPath = apiPath;
		this.apiProxyServerUrl = apiProxyServerUrl;
		this.proxyPath = proxyPath;
    }

}
}