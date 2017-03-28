package io.swagger.client;

import java.io.IOException;
import java.lang.annotation.Annotation;
import java.lang.reflect.Type;
import java.util.Date;
import java.util.LinkedHashMap;
import java.util.Map;

import org.apache.oltu.oauth2.client.request.OAuthClientRequest.AuthenticationRequestBuilder;
import org.apache.oltu.oauth2.client.request.OAuthClientRequest.TokenRequestBuilder;
import org.joda.time.DateTime;
import org.joda.time.LocalDate;
import org.joda.time.format.DateTimeFormatter;
import org.joda.time.format.ISODateTimeFormat;
import retrofit2.Converter;
import retrofit2.Retrofit;
import retrofit2.adapter.rxjava.RxJavaCallAdapterFactory;
import retrofit2.converter.gson.GsonConverterFactory;
import retrofit2.converter.scalars.ScalarsConverterFactory;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonParseException;
import com.google.gson.TypeAdapter;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonWriter;
import okhttp3.Interceptor;
import okhttp3.OkHttpClient;
import okhttp3.RequestBody;
import okhttp3.ResponseBody;

import io.swagger.client.auth.HttpBasicAuth;
import io.swagger.client.auth.ApiKeyAuth;
import io.swagger.client.auth.OAuth;
import io.swagger.client.auth.OAuth.AccessTokenListener;
import io.swagger.client.auth.OAuthFlow;

public class ApiClient {

  private Map<String, Interceptor> apiAuthorizations;
  private OkHttpClient.Builder okBuilder;
  private Retrofit.Builder adapterBuilder;

  public ApiClient() {
    apiAuthorizations = new LinkedHashMap<String, Interceptor>();
    createDefaultAdapter();
  }

  public ApiClient(String[] authNames) {
    this();
    for(String authName : authNames) {
      Interceptor auth;
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
   * @param authName Authentication name
   */
  public ApiClient(String authName) {
    this(new String[]{authName});
  }

  /**
   * Helper constructor for single api key
   * @param authName Authentication name
   * @param apiKey API key
   */
  public ApiClient(String authName, String apiKey) {
    this(authName);
    this.setApiKey(apiKey);
  }

  /**
   * Helper constructor for single basic auth or password oauth2
   * @param authName Authentication name
   * @param username Username
   * @param password Password
   */
  public ApiClient(String authName, String username, String password) {
    this(authName);
    this.setCredentials(username,  password);
  }

  /**
   * Helper constructor for single password oauth2
   * @param authName Authentication name
   * @param clientId Client ID
   * @param secret Client Secret
   * @param username Username
   * @param password Password
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
      .registerTypeAdapter(DateTime.class, new DateTimeTypeAdapter())
      .registerTypeAdapter(LocalDate.class, new LocalDateTypeAdapter())
      .create();

    okBuilder = new OkHttpClient.Builder();

    String baseUrl = "http://petstore.swagger.io:80/v2";
    if(!baseUrl.endsWith("/"))
      baseUrl = baseUrl + "/";

    adapterBuilder = new Retrofit
      .Builder()
      .baseUrl(baseUrl)
      .addCallAdapterFactory(RxJavaCallAdapterFactory.create())
      .addConverterFactory(ScalarsConverterFactory.create())
      .addConverterFactory(GsonCustomConverterFactory.create(gson));
  }

  public <S> S createService(Class<S> serviceClass) {
    return adapterBuilder
      .client(okBuilder.build())
      .build()
      .create(serviceClass);

  }

  /**
   * Helper method to configure the first api key found
   * @param apiKey API key
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
   * @param username Username
   * @param password Password
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
   * @return Token request builder
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
   * @return Authentication request builder
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
   * @param accessToken Access token
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
   * @param clientId Client ID
   * @param clientSecret Client secret
   * @param redirectURI Redirect URI
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
   * @param accessTokenListener Access token listener
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
   * @param authName Authentication name
   * @param authorization Authorization interceptor
   */
  public void addAuthorization(String authName, Interceptor authorization) {
    if (apiAuthorizations.containsKey(authName)) {
      throw new RuntimeException("auth name \"" + authName + "\" already in api authorizations");
    }
    apiAuthorizations.put(authName, authorization);
    okBuilder.addInterceptor(authorization);
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

  public OkHttpClient.Builder getOkBuilder() {
    return okBuilder;
  }

  public void addAuthsToOkBuilder(OkHttpClient.Builder okBuilder) {
    for(Interceptor apiAuthorization : apiAuthorizations.values()) {
      okBuilder.addInterceptor(apiAuthorization);
    }
  }

  /**
   * Clones the okBuilder given in parameter, adds the auth interceptors and uses it to configure the Retrofit
   * @param okClient An instance of OK HTTP client
   */
  public void configureFromOkclient(OkHttpClient okClient) {
    this.okBuilder = okClient.newBuilder();
    addAuthsToOkBuilder(this.okBuilder);
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

class GsonCustomConverterFactory extends Converter.Factory {

  private final Gson gson;
  private final GsonConverterFactory gsonConverterFactory;

  public static GsonCustomConverterFactory create(Gson gson) {
    return new GsonCustomConverterFactory(gson);
  }

  private GsonCustomConverterFactory(Gson gson) {
    if (gson == null)
      throw new NullPointerException("gson == null");
    this.gson = gson;
    this.gsonConverterFactory = GsonConverterFactory.create(gson);
  }

  @Override
  public Converter<ResponseBody, ?> responseBodyConverter(Type type, Annotation[] annotations, Retrofit retrofit) {
    if (type.equals(String.class))
      return new GsonResponseBodyConverterToString<Object>(gson, type);
    else
      return gsonConverterFactory.responseBodyConverter(type, annotations, retrofit);
  }

  @Override
  public Converter<?, RequestBody> requestBodyConverter(Type type, Annotation[] parameterAnnotations, Annotation[] methodAnnotations, Retrofit retrofit) {
    return gsonConverterFactory.requestBodyConverter(type, parameterAnnotations, methodAnnotations, retrofit);
  }
}

/**
 * Gson TypeAdapter for Joda DateTime type
 */
class DateTimeTypeAdapter extends TypeAdapter<DateTime> {

  private final DateTimeFormatter parseFormatter = ISODateTimeFormat.dateOptionalTimeParser();
  private final DateTimeFormatter printFormatter = ISODateTimeFormat.dateTime();

  @Override
  public void write(JsonWriter out, DateTime date) throws IOException {
    if (date == null) {
        out.nullValue();
    } else {
        out.value(printFormatter.print(date));
    }
  }

  @Override
  public DateTime read(JsonReader in) throws IOException {
    switch (in.peek()) {
      case NULL:
        in.nextNull();
        return null;
      default:
        String date = in.nextString();
        return parseFormatter.parseDateTime(date);
    }
  }
}

/**
 * Gson TypeAdapter for Joda LocalDate type
 */
class LocalDateTypeAdapter extends TypeAdapter<LocalDate> {

  private final DateTimeFormatter formatter = ISODateTimeFormat.date();

  @Override
  public void write(JsonWriter out, LocalDate date) throws IOException {
    if (date == null) {
      out.nullValue();
    } else {
      out.value(formatter.print(date));
    }
  }

  @Override
  public LocalDate read(JsonReader in) throws IOException {
    switch (in.peek()) {
      case NULL:
        in.nextNull();
        return null;
      default:
        String date = in.nextString();
        return formatter.parseLocalDate(date);
    }
  }
}
