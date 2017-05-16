package io.swagger.client;

import java.util.LinkedHashMap;
import java.util.Map;

import org.apache.oltu.oauth2.client.request.OAuthClientRequest.AuthenticationRequestBuilder;
import org.apache.oltu.oauth2.client.request.OAuthClientRequest.TokenRequestBuilder;

import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.datatype.joda.JodaModule;

import feign.Feign;
import feign.RequestInterceptor;
import feign.form.FormEncoder;
import feign.jackson.JacksonDecoder;
import feign.jackson.JacksonEncoder;
import feign.slf4j.Slf4jLogger;
import io.swagger.client.auth.*;
import io.swagger.client.auth.OAuth.AccessTokenListener;


public class ApiClient {
  public interface Api {}

  protected ObjectMapper objectMapper;
  private String basePath = "http://petstore.swagger.io:80/v2";
  private Map<String, RequestInterceptor> apiAuthorizations;
  private Feign.Builder feignBuilder;

  public ApiClient() {
    objectMapper = createObjectMapper();
    apiAuthorizations = new LinkedHashMap<String, RequestInterceptor>();
    feignBuilder = Feign.builder()
                .encoder(new FormEncoder(new JacksonEncoder(objectMapper)))
                .decoder(new JacksonDecoder(objectMapper))
                .logger(new Slf4jLogger());
  }

  public ApiClient(String[] authNames) {
    this();
    for(String authName : authNames) {
      RequestInterceptor auth;
      if ("api_key".equals(authName)) {
        auth = new ApiKeyAuth("header", "api_key");
      } else if ("http_basic_test".equals(authName)) {
        auth = new HttpBasicAuth();
      } else if ("petstore_auth".equals(authName)) {
        auth = new OAuth(OAuthFlow.implicit, "http://petstore.swagger.io/api/oauth/dialog", "", "write:pets, read:pets");
      } else {
        throw new RuntimeException("auth name \"" + authName + "\" not found in available auth names");
      }
      addAuthorization(authName, auth);
    }
  }

  /**
   * Basic constructor for single auth name
   * @param authName
   */
  public ApiClient(String authName) {
    this(new String[]{authName});
  }

  /**
   * Helper constructor for single api key
   * @param authName
   * @param apiKey
   */
  public ApiClient(String authName, String apiKey) {
    this(authName);
    this.setApiKey(apiKey);
  }

  /**
   * Helper constructor for single basic auth or password oauth2
   * @param authName
   * @param username
   * @param password
   */
  public ApiClient(String authName, String username, String password) {
    this(authName);
    this.setCredentials(username,  password);
  }

  /**
   * Helper constructor for single password oauth2
   * @param authName
   * @param clientId
   * @param secret
   * @param username
   * @param password
   */
   public ApiClient(String authName, String clientId, String secret, String username, String password) {
     this(authName);
     this.getTokenEndPoint()
            .setClientId(clientId)
            .setClientSecret(secret)
            .setUsername(username)
            .setPassword(password);
   }

  public String getBasePath() {
    return basePath;
  }

  public ApiClient setBasePath(String basePath) {
    this.basePath = basePath;
    return this;
  }

  public Map<String, RequestInterceptor> getApiAuthorizations() {
    return apiAuthorizations;
  }

  public void setApiAuthorizations(Map<String, RequestInterceptor> apiAuthorizations) {
    this.apiAuthorizations = apiAuthorizations;
  }

  public Feign.Builder getFeignBuilder() {
    return feignBuilder;
  }

  public ApiClient setFeignBuilder(Feign.Builder feignBuilder) {
    this.feignBuilder = feignBuilder;
    return this;
  }

  private ObjectMapper createObjectMapper() {
    ObjectMapper objectMapper = new ObjectMapper();
    objectMapper.enable(SerializationFeature.WRITE_ENUMS_USING_TO_STRING);
    objectMapper.enable(DeserializationFeature.READ_ENUMS_USING_TO_STRING);
    objectMapper.disable(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES);
    objectMapper.disable(DeserializationFeature.FAIL_ON_INVALID_SUBTYPE);
    objectMapper.disable(SerializationFeature.WRITE_DATES_AS_TIMESTAMPS);
    objectMapper.setDateFormat(new RFC3339DateFormat());
    objectMapper.registerModule(new JodaModule());
    return objectMapper;
  }

  public ObjectMapper getObjectMapper(){
    return objectMapper;
  }

  /**
   * Creates a feign client for given API interface.
   *
   * Usage:
   *    ApiClient apiClient = new ApiClient();
   *    apiClient.setBasePath("http://localhost:8080");
   *    XYZApi api = apiClient.buildClient(XYZApi.class);
   *    XYZResponse response = api.someMethod(...);
   * @param <T> Type
   * @param clientClass Client class
   * @return The Client
   */
  public <T extends Api> T buildClient(Class<T> clientClass) {
    return feignBuilder.target(clientClass, basePath);
  }

  /**
   * Select the Accept header's value from the given accepts array:
   *   if JSON exists in the given array, use it;
   *   otherwise use all of them (joining into a string)
   *
   * @param accepts The accepts array to select from
   * @return The Accept header to use. If the given array is empty,
   *   null will be returned (not to set the Accept header explicitly).
   */
  public String selectHeaderAccept(String[] accepts) {
    if (accepts.length == 0) return null;
    if (StringUtil.containsIgnoreCase(accepts, "application/json")) return "application/json";
    return StringUtil.join(accepts, ",");
  }

  /**
   * Select the Content-Type header's value from the given array:
   *   if JSON exists in the given array, use it;
   *   otherwise use the first one of the array.
   *
   * @param contentTypes The Content-Type array to select from
   * @return The Content-Type header to use. If the given array is empty,
   *   JSON will be used.
   */
  public String selectHeaderContentType(String[] contentTypes) {
    if (contentTypes.length == 0) return "application/json";
    if (StringUtil.containsIgnoreCase(contentTypes, "application/json")) return "application/json";
    return contentTypes[0];
  }

  /**
   * Helper method to configure the first api key found
   * @param apiKey API key
   */
  public void setApiKey(String apiKey) {
    for(RequestInterceptor apiAuthorization : apiAuthorizations.values()) {
      if (apiAuthorization instanceof ApiKeyAuth) {
        ApiKeyAuth keyAuth = (ApiKeyAuth) apiAuthorization;
        keyAuth.setApiKey(apiKey);
        return ;
      }
    }
    throw new RuntimeException("No API key authentication configured!");
  }

  /**
   * Helper method to configure the username/password for basic auth or password OAuth
   * @param username Username
   * @param password Password
   */
  public void setCredentials(String username, String password) {
    for(RequestInterceptor apiAuthorization : apiAuthorizations.values()) {
      if (apiAuthorization instanceof HttpBasicAuth) {
        HttpBasicAuth basicAuth = (HttpBasicAuth) apiAuthorization;
        basicAuth.setCredentials(username, password);
        return;
      }
      if (apiAuthorization instanceof OAuth) {
        OAuth oauth = (OAuth) apiAuthorization;
        oauth.getTokenRequestBuilder().setUsername(username).setPassword(password);
        return;
      }
    }
    throw new RuntimeException("No Basic authentication or OAuth configured!");
  }

  /**
   * Helper method to configure the token endpoint of the first oauth found in the apiAuthorizations (there should be only one)
   * @return Token request builder
   */
  public TokenRequestBuilder getTokenEndPoint() {
    for(RequestInterceptor apiAuthorization : apiAuthorizations.values()) {
      if (apiAuthorization instanceof OAuth) {
        OAuth oauth = (OAuth) apiAuthorization;
        return oauth.getTokenRequestBuilder();
      }
    }
    return null;
  }

  /**
   * Helper method to configure authorization endpoint of the first oauth found in the apiAuthorizations (there should be only one)
   * @return Authentication request builder
   */
  public AuthenticationRequestBuilder getAuthorizationEndPoint() {
    for(RequestInterceptor apiAuthorization : apiAuthorizations.values()) {
      if (apiAuthorization instanceof OAuth) {
        OAuth oauth = (OAuth) apiAuthorization;
        return oauth.getAuthenticationRequestBuilder();
      }
    }
    return null;
  }

  /**
   * Helper method to pre-set the oauth access token of the first oauth found in the apiAuthorizations (there should be only one)
   * @param accessToken Access Token
   * @param expiresIn Validity period in seconds
   */
  public void setAccessToken(String accessToken, Long expiresIn) {
    for(RequestInterceptor apiAuthorization : apiAuthorizations.values()) {
      if (apiAuthorization instanceof OAuth) {
        OAuth oauth = (OAuth) apiAuthorization;
        oauth.setAccessToken(accessToken, expiresIn);
        return;
      }
    }
  }

  /**
   * Helper method to configure the oauth accessCode/implicit flow parameters
   * @param clientId Client ID
   * @param clientSecret Client secret
   * @param redirectURI Redirect URI
   */
  public void configureAuthorizationFlow(String clientId, String clientSecret, String redirectURI) {
    for(RequestInterceptor apiAuthorization : apiAuthorizations.values()) {
      if (apiAuthorization instanceof OAuth) {
        OAuth oauth = (OAuth) apiAuthorization;
        oauth.getTokenRequestBuilder()
                .setClientId(clientId)
                .setClientSecret(clientSecret)
                .setRedirectURI(redirectURI);
        oauth.getAuthenticationRequestBuilder()
                .setClientId(clientId)
                .setRedirectURI(redirectURI);
        return;
      }
    }
  }

  /**
   * Configures a listener which is notified when a new access token is received.
   * @param accessTokenListener Acesss token listener
   */
  public void registerAccessTokenListener(AccessTokenListener accessTokenListener) {
    for(RequestInterceptor apiAuthorization : apiAuthorizations.values()) {
      if (apiAuthorization instanceof OAuth) {
        OAuth oauth = (OAuth) apiAuthorization;
        oauth.registerAccessTokenListener(accessTokenListener);
        return;
      }
    }
  }

  /**
   * Gets request interceptor based on authentication name
   * @param authName Authentiation name
   * @return Request Interceptor
   */
  public RequestInterceptor getAuthorization(String authName) {
    return apiAuthorizations.get(authName);
  }

  /**
   * Adds an authorization to be used by the client
   * @param authName Authentication name
   * @param authorization Request interceptor
   */
  public void addAuthorization(String authName, RequestInterceptor authorization) {
    if (apiAuthorizations.containsKey(authName)) {
      throw new RuntimeException("auth name \"" + authName + "\" already in api authorizations");
    }
    apiAuthorizations.put(authName, authorization);
    feignBuilder.requestInterceptor(authorization);
  }

}
