package io.swagger.client;

import java.io.IOException;
import java.lang.annotation.Annotation;
import java.lang.reflect.Type;
import java.util.Date;
import java.util.LinkedHashMap;
import java.util.Map;

import org.apache.oltu.oauth2.client.request.OAuthClientRequest.AuthenticationRequestBuilder;
import org.apache.oltu.oauth2.client.request.OAuthClientRequest.TokenRequestBuilder;

import retrofit.Converter;
import retrofit.Retrofit;
import retrofit.GsonConverterFactory;
import retrofit.RxJavaCallAdapterFactory;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonParseException;
import com.squareup.okhttp.Interceptor;
import com.squareup.okhttp.OkHttpClient;
import com.squareup.okhttp.RequestBody;
import com.squareup.okhttp.ResponseBody;


import io.swagger.client.auth.HttpBasicAuth;
import io.swagger.client.auth.ApiKeyAuth;
import io.swagger.client.auth.OAuth;
import io.swagger.client.auth.OAuth.AccessTokenListener;
import io.swagger.client.auth.OAuthFlow;


public class ApiClient {

    private Map<String, Interceptor> apiAuthorizations;
    private OkHttpClient okClient;
    private Retrofit.Builder adapterBuilder;

    public ApiClient() {
        apiAuthorizations = new LinkedHashMap<String, Interceptor>();
        createDefaultAdapter();
    }

    public ApiClient(String[] authNames) {
        this();
        for(String authName : authNames) { 
            Interceptor auth;
            if (authName == "petstore_auth") { 
                auth = new OAuth(OAuthFlow.implicit, "http://petstore.swagger.io/api/oauth/dialog", "", "write:pets, read:pets");
            } else if (authName == "api_key") { 
                auth = new ApiKeyAuth("header", "api_key");
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
    
   public void createDefaultAdapter() {
        Gson gson = new GsonBuilder()
                .setDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSSZ")
                .create();

        okClient = new OkHttpClient();
        
        String baseUrl = "http://petstore.swagger.io/v2";
        if(!baseUrl.endsWith("/"))
        	baseUrl = baseUrl + "/";

        adapterBuilder = new Retrofit
                .Builder()
                .baseUrl(baseUrl)
                .client(okClient)
                .addCallAdapterFactory(RxJavaCallAdapterFactory.create())
                .addConverterFactory(GsonCustomConverterFactory.create(gson));
    }

    public <S> S createService(Class<S> serviceClass) {
        return adapterBuilder.build().create(serviceClass);
        
    }

    /**
     * Helper method to configure the first api key found
     * @param apiKey
     */
    private void setApiKey(String apiKey) {
        for(Interceptor apiAuthorization : apiAuthorizations.values()) {
            if (apiAuthorization instanceof ApiKeyAuth) {
                ApiKeyAuth keyAuth = (ApiKeyAuth) apiAuthorization;
                keyAuth.setApiKey(apiKey);
                return;
            }
        }
    }

    /**
     * Helper method to configure the username/password for basic auth or password oauth
     * @param username
     * @param password
     */
    private void setCredentials(String username, String password) {
        for(Interceptor apiAuthorization : apiAuthorizations.values()) {
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
    }

    /**
     * Helper method to configure the token endpoint of the first oauth found in the apiAuthorizations (there should be only one)
     * @return
     */
    public TokenRequestBuilder getTokenEndPoint() {
        for(Interceptor apiAuthorization : apiAuthorizations.values()) {
            if (apiAuthorization instanceof OAuth) {
                OAuth oauth = (OAuth) apiAuthorization;
                return oauth.getTokenRequestBuilder();
            }
        }
        return null;
    }

    /**
     * Helper method to configure authorization endpoint of the first oauth found in the apiAuthorizations (there should be only one)
     * @return
     */
    public AuthenticationRequestBuilder getAuthorizationEndPoint() {
        for(Interceptor apiAuthorization : apiAuthorizations.values()) {
            if (apiAuthorization instanceof OAuth) {
                OAuth oauth = (OAuth) apiAuthorization;
                return oauth.getAuthenticationRequestBuilder();
            }
        }
        return null;
    }

    /**
     * Helper method to pre-set the oauth access token of the first oauth found in the apiAuthorizations (there should be only one)
     * @param accessToken
     */
    public void setAccessToken(String accessToken) {
        for(Interceptor apiAuthorization : apiAuthorizations.values()) {
            if (apiAuthorization instanceof OAuth) {
                OAuth oauth = (OAuth) apiAuthorization;
                oauth.setAccessToken(accessToken);
                return;
            }
        }
    }
    
    /**
     * Helper method to configure the oauth accessCode/implicit flow parameters
     * @param clientId
     * @param clientSecret
     * @param redirectURI
     */
    public void configureAuthorizationFlow(String clientId, String clientSecret, String redirectURI) {
        for(Interceptor apiAuthorization : apiAuthorizations.values()) {
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
     * @param accessTokenListener
     */
    public void registerAccessTokenListener(AccessTokenListener accessTokenListener) {
        for(Interceptor apiAuthorization : apiAuthorizations.values()) {
            if (apiAuthorization instanceof OAuth) {
                OAuth oauth = (OAuth) apiAuthorization;
                oauth.registerAccessTokenListener(accessTokenListener);
                return;
            }
        }
    }

    /**
     * Adds an authorization to be used by the client
     * @param authName
     * @param authorization
     */
    public void addAuthorization(String authName, Interceptor authorization) {
        if (apiAuthorizations.containsKey(authName)) {
            throw new RuntimeException("auth name \"" + authName + "\" already in api authorizations");
        }
        apiAuthorizations.put(authName, authorization);
        okClient.interceptors().add(authorization);
    }

    public Map<String, Interceptor> getApiAuthorizations() {
        return apiAuthorizations;
    }

    public void setApiAuthorizations(Map<String, Interceptor> apiAuthorizations) {
        this.apiAuthorizations = apiAuthorizations;
    }

    public Retrofit.Builder getAdapterBuilder() {
        return adapterBuilder;
    }

    public void setAdapterBuilder(Retrofit.Builder adapterBuilder) {
        this.adapterBuilder = adapterBuilder;
    }

    public OkHttpClient getOkClient() {
        return okClient;
    }
    
    public void addAuthsToOkClient(OkHttpClient okClient) {
        for(Interceptor apiAuthorization : apiAuthorizations.values()) {
            okClient.interceptors().add(apiAuthorization);
        }
    }

    /**
     * Clones the okClient given in parameter, adds the auth interceptors and uses it to configure the Retrofit
     * @param okClient
     */
    public void configureFromOkclient(OkHttpClient okClient) {
        OkHttpClient clone = okClient.clone();
        addAuthsToOkClient(clone);
        adapterBuilder.client(clone);
    }
}

/**
 * This wrapper is to take care of this case:
 * when the deserialization fails due to JsonParseException and the
 * expected type is String, then just return the body string.
 */
class GsonResponseBodyConverterToString<T> implements Converter<ResponseBody, T> {
	  private final Gson gson;
	  private final Type type;

	  GsonResponseBodyConverterToString(Gson gson, Type type) {
	    this.gson = gson;
	    this.type = type;
	  }

	  @Override public T convert(ResponseBody value) throws IOException {
	    String returned = value.string();
	    try {
	      return gson.fromJson(returned, type);
	    } 
	    catch (JsonParseException e) {
                return (T) returned;
        } 
	 }
}

class GsonCustomConverterFactory extends Converter.Factory 
{
	public static GsonCustomConverterFactory create(Gson gson) {
	    return new GsonCustomConverterFactory(gson);
	  }

	  private final Gson gson;
	  private final GsonConverterFactory gsonConverterFactory;

	  private GsonCustomConverterFactory(Gson gson) {
	    if (gson == null) throw new NullPointerException("gson == null");
	    this.gson = gson;
	    this.gsonConverterFactory = GsonConverterFactory.create(gson);
	  }

	  @Override
	  public Converter<ResponseBody, ?> fromResponseBody(Type type, Annotation[] annotations) {
		  if(type.equals(String.class))
			  return new GsonResponseBodyConverterToString<Object>(gson, type);
		  else
			  return gsonConverterFactory.fromResponseBody(type, annotations);
	  }

	  @Override
	  public Converter<?, RequestBody> toRequestBody(Type type, Annotation[] annotations) {
	    return gsonConverterFactory.toRequestBody(type, annotations);
	  }
}

