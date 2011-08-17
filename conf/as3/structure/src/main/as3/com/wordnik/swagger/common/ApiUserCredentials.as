package com.wordnik.swagger.common {

/**
 * Wordnik Api account credentials. The info is used to authenticate with the Wordnik API and perform
 * account-specific user actions
 */
public class ApiUserCredentials {
    /**
     * All requests must be signed with your Wordnik API key
     */
    public var apiToken:String;
    /**
     * A valid auth_token which is necessary for certain operations - currently, user accounts and list-related CRUD operations
     */
    public var authToken:String;
    /**
     * The userId which is required for certain operations - currently, get user lists
     */
    public var userId:Number;
    /**
     * The host name for the Wordnik Rest API eg. api.wordnik.com
     */
    public var hostName:String;
	
	/**
	 * The base path to the api resources - used along with the hostname 
	 * eg. /v4  
	 */ 
	public var apiPath: String;
	
	/**
	 * If a proxy server has been set up for the services specify the URL here. This value is used when the Api is invoked with 
	 * the value useProxy as true
	 */ 
	public var apiProxyServerUrl: String;

    /**
     * Constructor of ApiUserCredentials
     * @param apiToken All requests must be signed with your Wordnik API key
     * @param authToken A valid auth_token which is necessary for certain operations - currently, user accounts and list-related CRUD operations
     * @param hostName The host name for the Wordnik Rest API eg. api.wordnik.com
     * @param userId The userId which is required for certain operations - currently, get user lists
     */
    public function ApiUserCredentials(apiToken: String, authToken: String = null, hostName: String = null, userId: Number = -1, 
									   apiPath: String = "", apiProxyServerUrl: String="") {
        this.hostName = hostName;
        this.apiToken = apiToken;
        this.authToken = authToken;
        this.userId = userId;
		this.apiPath = apiPath;
		this.apiProxyServerUrl = apiProxyServerUrl;
    }

}
}