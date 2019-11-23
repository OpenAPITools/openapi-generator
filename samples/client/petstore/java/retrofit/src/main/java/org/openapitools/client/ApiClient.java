package org.openapitools.client;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.Type;
import java.util.LinkedHashMap;
import java.util.Map;

import org.apache.oltu.oauth2.client.request.OAuthClientRequest.AuthenticationRequestBuilder;
import org.apache.oltu.oauth2.client.request.OAuthClientRequest.TokenRequestBuilder;

import org.joda.time.DateTime;
import org.joda.time.LocalDate;
import org.joda.time.format.DateTimeFormatter;
import org.joda.time.format.ISODateTimeFormat;

import retrofit.RestAdapter;
import retrofit.client.OkClient;
import retrofit.converter.ConversionException;
import retrofit.converter.Converter;
import retrofit.converter.GsonConverter;
import retrofit.mime.TypedByteArray;
import retrofit.mime.TypedInput;
import retrofit.mime.TypedOutput;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonParseException;
import com.google.gson.TypeAdapter;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonWriter;
import com.squareup.okhttp.Interceptor;
import com.squareup.okhttp.OkHttpClient;

import org.openapitools.client.auth.HttpBasicAuth;
import org.openapitools.client.auth.HttpBearerAuth;
import org.openapitools.client.auth.ApiKeyAuth;
import org.openapitools.client.auth.OAuth;
import org.openapitools.client.auth.OAuth.AccessTokenListener;
import org.openapitools.client.auth.OAuthFlow;

public class ApiClient {

    private Map<String, Interceptor> apiAuthorizations;
    private OkHttpClient okClient;
    private RestAdapter.Builder adapterBuilder;

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
            } else if ("api_key_query".equals(authName)) {
                auth = new ApiKeyAuth("query", "api_key_query");
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
     * @param secret Client secret
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

         okClient = new OkHttpClient();

         adapterBuilder = new RestAdapter
                 .Builder()
                 .setEndpoint("http://petstore.swagger.io:80/v2")
                 .setClient(new OkClient(okClient))
                 .setConverter(new GsonConverterWrapper(gson));
    }

    public <S> S createService(Class<S> serviceClass) {
        return adapterBuilder.build().create(serviceClass);

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
     * Helper method to set token for the first Http Bearer authentication found.
     * @param bearerToken Bearer token
     */
    public void setBearerToken(String bearerToken) {
        for (Interceptor apiAuthorization : apiAuthorizations.values()) {
            if (apiAuthorization instanceof HttpBearerAuth) {
                ((HttpBearerAuth) apiAuthorization).setBearerToken(bearerToken);
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
     * @param authorization Authorization
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

    public RestAdapter.Builder getAdapterBuilder() {
        return adapterBuilder;
    }

    public void setAdapterBuilder(RestAdapter.Builder adapterBuilder) {
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
     * Clones the okClient given in parameter, adds the auth interceptors and uses it to configure the RestAdapter
     * @param okClient OkHttp client
     */
    public void configureFromOkclient(OkHttpClient okClient) {
        OkHttpClient clone = okClient.clone();
        addAuthsToOkClient(clone);
        adapterBuilder.setClient(new OkClient(clone));
    }
}

/**
 * This wrapper is to take care of this case:
 * when the deserialization fails due to JsonParseException and the
 * expected type is String, then just return the body string.
 */
class GsonConverterWrapper implements Converter {
    private GsonConverter converter;

    public GsonConverterWrapper(Gson gson) {
        converter = new GsonConverter(gson);
    }

    @Override public Object fromBody(TypedInput body, Type type) throws ConversionException {
        byte[] bodyBytes = readInBytes(body);
        TypedByteArray newBody = new TypedByteArray(body.mimeType(), bodyBytes);
        try {
            return converter.fromBody(newBody, type);
        } catch (ConversionException e) {
            if (e.getCause() instanceof JsonParseException && type.equals(String.class)) {
                return new String(bodyBytes);
            } else {
                throw e;
            }
        }
    }

    @Override public TypedOutput toBody(Object object) {
        return converter.toBody(object);
    }

    private byte[] readInBytes(TypedInput body) throws ConversionException {
        InputStream in = null;
        try {
            in = body.in();
            ByteArrayOutputStream os = new ByteArrayOutputStream();
            byte[] buffer = new byte[0xFFFF];
            for (int len; (len = in.read(buffer)) != -1;)
                os.write(buffer, 0, len);
            os.flush();
            return os.toByteArray();
        } catch (IOException e) {
            throw new ConversionException(e);
        } finally {
            if (in != null) {
                try {
                    in.close();
                } catch (IOException ignored) {
                }
            }
        }

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
 * Gson TypeAdapter for Joda DateTime type
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
