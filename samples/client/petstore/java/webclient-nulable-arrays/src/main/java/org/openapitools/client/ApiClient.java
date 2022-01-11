package org.openapitools.client;

import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import org.openapitools.jackson.nullable.JsonNullableModule;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpRequest;
import org.springframework.http.HttpStatus;
import org.springframework.http.InvalidMediaTypeException;
import org.springframework.http.MediaType;
import org.springframework.http.RequestEntity;
import org.springframework.http.RequestEntity.BodyBuilder;
import org.springframework.http.ResponseEntity;
import org.springframework.http.client.BufferingClientHttpRequestFactory;
import org.springframework.http.client.ClientHttpRequestExecution;
import org.springframework.http.client.ClientHttpRequestInterceptor;
import org.springframework.http.client.ClientHttpResponse;
import org.springframework.http.codec.json.Jackson2JsonDecoder;
import org.springframework.http.codec.json.Jackson2JsonEncoder;
import org.springframework.util.CollectionUtils;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;
import org.springframework.util.StringUtils;
import org.springframework.http.client.reactive.ClientHttpRequest;
import org.springframework.web.client.RestClientException;
import org.springframework.web.util.UriComponentsBuilder;
import org.springframework.web.reactive.function.client.WebClient;
import org.springframework.web.reactive.function.client.WebClient.ResponseSpec;
import org.springframework.web.reactive.function.client.ClientResponse;
import org.springframework.web.reactive.function.BodyInserter;
import org.springframework.web.reactive.function.BodyInserters;
import org.springframework.web.reactive.function.client.ExchangeStrategies;
import reactor.core.publisher.Mono;
import reactor.core.publisher.Flux;
import java.util.Optional;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.text.DateFormat;
import java.text.ParseException;
import java.util.Arrays;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.TimeZone;

import javax.annotation.Nullable;

import java.time.OffsetDateTime;

import org.openapitools.client.auth.Authentication;
import org.openapitools.client.auth.HttpBasicAuth;
import org.openapitools.client.auth.HttpBearerAuth;
import org.openapitools.client.auth.ApiKeyAuth;

@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaClientCodegen")
public class ApiClient extends JavaTimeFormatter {
    public enum CollectionFormat {
        CSV(","), TSV("\t"), SSV(" "), PIPES("|"), MULTI(null);

        private final String separator;
        private CollectionFormat(String separator) {
            this.separator = separator;
        }

        private String collectionToString(Collection<?> collection) {
            return StringUtils.collectionToDelimitedString(collection, separator);
        }
    }

    private static final String URI_TEMPLATE_ATTRIBUTE = WebClient.class.getName() + ".uriTemplate";

    private HttpHeaders defaultHeaders = new HttpHeaders();
    private MultiValueMap<String, String> defaultCookies = new LinkedMultiValueMap<String, String>();

    private String basePath = "http://localhost";

    private final WebClient webClient;
    private final DateFormat dateFormat;
    private final ObjectMapper objectMapper;

    private Map<String, Authentication> authentications;


    public ApiClient() {
        this.dateFormat = createDefaultDateFormat();
        this.objectMapper = createDefaultObjectMapper(this.dateFormat);
        this.webClient = buildWebClient(this.objectMapper);
        this.init();
    }

    public ApiClient(WebClient webClient) {
        this(Optional.ofNullable(webClient).orElseGet(() -> buildWebClient()), createDefaultDateFormat());
    }

    public ApiClient(ObjectMapper mapper, DateFormat format) {
        this(buildWebClient(mapper.copy()), format);
    }

    public ApiClient(WebClient webClient, ObjectMapper mapper, DateFormat format) {
        this(Optional.ofNullable(webClient).orElseGet(() -> buildWebClient(mapper.copy())), format);
    }

    private ApiClient(WebClient webClient, DateFormat format) {
        this.webClient = webClient;
        this.dateFormat = format;
        this.objectMapper = createDefaultObjectMapper(format);
        this.init();
    }

    public static DateFormat createDefaultDateFormat() {
        DateFormat dateFormat = new RFC3339DateFormat();
        dateFormat.setTimeZone(TimeZone.getTimeZone("UTC"));
        return dateFormat;
    }

    public static ObjectMapper createDefaultObjectMapper(@Nullable DateFormat dateFormat) {
        if (null == dateFormat) {
            dateFormat = createDefaultDateFormat();
        }
        ObjectMapper mapper = new ObjectMapper();
        mapper.setDateFormat(dateFormat);
        mapper.registerModule(new JavaTimeModule());
        mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
        JsonNullableModule jnm = new JsonNullableModule();
        mapper.registerModule(jnm);
        return mapper;
    }

    protected void init() {
        // Setup authentications (key: authentication name, value: authentication).
        authentications = new HashMap<String, Authentication>();
        // Prevent the authentications from being modified.
        authentications = Collections.unmodifiableMap(authentications);
    }

    /**
    * Build the WebClientBuilder used to make WebClient.
    * @param mapper ObjectMapper used for serialize/deserialize
    * @return WebClient
    */
    public static WebClient.Builder buildWebClientBuilder(ObjectMapper mapper) {
        ExchangeStrategies strategies = ExchangeStrategies
            .builder()
            .codecs(clientDefaultCodecsConfigurer -> {
                clientDefaultCodecsConfigurer.defaultCodecs().jackson2JsonEncoder(new Jackson2JsonEncoder(mapper, MediaType.APPLICATION_JSON));
                clientDefaultCodecsConfigurer.defaultCodecs().jackson2JsonDecoder(new Jackson2JsonDecoder(mapper, MediaType.APPLICATION_JSON));
            }).build();
        WebClient.Builder webClientBuilder = WebClient.builder().exchangeStrategies(strategies);
        return webClientBuilder;
    }

    /**
     * Build the WebClientBuilder used to make WebClient.
     * @return WebClient
     */
    public static WebClient.Builder buildWebClientBuilder() {
        return buildWebClientBuilder(createDefaultObjectMapper(null));
    }

    /**
     * Build the WebClient used to make HTTP requests.
     * @param mapper ObjectMapper used for serialize/deserialize
     * @return WebClient
     */
    public static WebClient buildWebClient(ObjectMapper mapper) {
        return buildWebClientBuilder(mapper).build();
    }

    /**
     * Build the WebClient used to make HTTP requests.
     * @return WebClient
     */
    public static WebClient buildWebClient() {
        return buildWebClientBuilder(createDefaultObjectMapper(null)).build();
    }

    /**
     * Get the current base path
     * @return String the base path
     */
    public String getBasePath() {
        return basePath;
    }

    /**
     * Set the base path, which should include the host
     * @param basePath the base path
     * @return ApiClient this client
     */
    public ApiClient setBasePath(String basePath) {
        this.basePath = basePath;
        return this;
    }

    /**
     * Get authentications (key: authentication name, value: authentication).
     * @return Map the currently configured authentication types
     */
    public Map<String, Authentication> getAuthentications() {
        return authentications;
    }

    /**
     * Get authentication for the given name.
     *
     * @param authName The authentication name
     * @return The authentication, null if not found
     */
    public Authentication getAuthentication(String authName) {
        return authentications.get(authName);
    }

    /**
     * Helper method to set access token for the first Bearer authentication.
     * @param bearerToken Bearer token
     */
    public void setBearerToken(String bearerToken) {
        for (Authentication auth : authentications.values()) {
            if (auth instanceof HttpBearerAuth) {
                ((HttpBearerAuth) auth).setBearerToken(bearerToken);
                return;
            }
        }
        throw new RuntimeException("No Bearer authentication configured!");
    }

    /**
     * Helper method to set username for the first HTTP basic authentication.
     * @param username the username
     */
    public void setUsername(String username) {
        for (Authentication auth : authentications.values()) {
            if (auth instanceof HttpBasicAuth) {
                ((HttpBasicAuth) auth).setUsername(username);
                return;
            }
        }
        throw new RuntimeException("No HTTP basic authentication configured!");
    }

    /**
     * Helper method to set password for the first HTTP basic authentication.
     * @param password the password
     */
    public void setPassword(String password) {
        for (Authentication auth : authentications.values()) {
            if (auth instanceof HttpBasicAuth) {
                ((HttpBasicAuth) auth).setPassword(password);
                return;
            }
        }
        throw new RuntimeException("No HTTP basic authentication configured!");
    }

    /**
     * Helper method to set API key value for the first API key authentication.
     * @param apiKey the API key
     */
    public void setApiKey(String apiKey) {
        for (Authentication auth : authentications.values()) {
            if (auth instanceof ApiKeyAuth) {
                ((ApiKeyAuth) auth).setApiKey(apiKey);
                return;
            }
        }
        throw new RuntimeException("No API key authentication configured!");
    }

    /**
     * Helper method to set API key prefix for the first API key authentication.
     * @param apiKeyPrefix the API key prefix
     */
    public void setApiKeyPrefix(String apiKeyPrefix) {
        for (Authentication auth : authentications.values()) {
            if (auth instanceof ApiKeyAuth) {
                ((ApiKeyAuth) auth).setApiKeyPrefix(apiKeyPrefix);
                return;
            }
        }
        throw new RuntimeException("No API key authentication configured!");
    }

    /**
     * Set the User-Agent header's value (by adding to the default header map).
     * @param userAgent the user agent string
     * @return ApiClient this client
     */
    public ApiClient setUserAgent(String userAgent) {
        addDefaultHeader("User-Agent", userAgent);
        return this;
    }

    /**
     * Add a default header.
     *
     * @param name The header's name
     * @param value The header's value
     * @return ApiClient this client
     */
    public ApiClient addDefaultHeader(String name, String value) {
        if (defaultHeaders.containsKey(name)) {
            defaultHeaders.remove(name);
        }
        defaultHeaders.add(name, value);
        return this;
    }

    /**
     * Add a default cookie.
     *
     * @param name The cookie's name
     * @param value The cookie's value
     * @return ApiClient this client
     */
    public ApiClient addDefaultCookie(String name, String value) {
        if (defaultCookies.containsKey(name)) {
            defaultCookies.remove(name);
        }
        defaultCookies.add(name, value);
        return this;
    }

    /**
     * Get the date format used to parse/format date parameters.
     * @return DateFormat format
     */
    public DateFormat getDateFormat() {
        return dateFormat;
    }

    /**
     * Parse the given string into Date object.
     */
    public Date parseDate(String str) {
        try {
            return dateFormat.parse(str);
        } catch (ParseException e) {
            throw new RuntimeException(e);
        }
    }

    /**
     * Format the given Date object into string.
     */
    public String formatDate(Date date) {
        return dateFormat.format(date);
    }

    /**
     * Get the ObjectMapper used to make HTTP requests.
     * @return ObjectMapper objectMapper
     */
    public ObjectMapper getObjectMapper() {
        return objectMapper;
    }

    /**
     * Get the WebClient used to make HTTP requests.
     * @return WebClient webClient
     */
    public WebClient getWebClient() {
        return webClient;
    }

    /**
     * Format the given parameter object into string.
     * @param param the object to convert
     * @return String the parameter represented as a String
     */
    public String parameterToString(Object param) {
        if (param == null) {
            return "";
        } else if (param instanceof Date) {
            return formatDate( (Date) param);
        } else if (param instanceof OffsetDateTime) {
            return formatOffsetDateTime((OffsetDateTime) param);
        } else if (param instanceof Collection) {
            StringBuilder b = new StringBuilder();
            for(Object o : (Collection<?>) param) {
                if(b.length() > 0) {
                    b.append(",");
                }
                b.append(String.valueOf(o));
            }
            return b.toString();
        } else {
            return String.valueOf(param);
        }
    }

    /**
     * Converts a parameter to a {@link MultiValueMap} for use in REST requests
     * @param collectionFormat The format to convert to
     * @param name The name of the parameter
     * @param value The parameter's value
     * @return a Map containing the String value(s) of the input parameter
     */
    public MultiValueMap<String, String> parameterToMultiValueMap(CollectionFormat collectionFormat, String name, Object value) {
        final MultiValueMap<String, String> params = new LinkedMultiValueMap<String, String>();

        if (name == null || name.isEmpty() || value == null) {
            return params;
        }

        if(collectionFormat == null) {
            collectionFormat = CollectionFormat.CSV;
        }

        if (value instanceof Map) {
            @SuppressWarnings("unchecked")
            final Map<String, Object> valuesMap = (Map<String, Object>) value;
            for (final Entry<String, Object> entry : valuesMap.entrySet()) {
                params.add(entry.getKey(), parameterToString(entry.getValue()));
            }
            return params;
        }

        Collection<?> valueCollection = null;
        if (value instanceof Collection) {
            valueCollection = (Collection<?>) value;
        } else {
            params.add(name, parameterToString(value));
            return params;
        }

        if (valueCollection.isEmpty()){
            return params;
        }

        if (collectionFormat.equals(CollectionFormat.MULTI)) {
            for (Object item : valueCollection) {
                params.add(name, parameterToString(item));
            }
            return params;
        }

        List<String> values = new ArrayList<String>();
        for(Object o : valueCollection) {
            values.add(parameterToString(o));
        }
        params.add(name, collectionFormat.collectionToString(values));

        return params;
    }

    /**
    * Check if the given {@code String} is a JSON MIME.
    * @param mediaType the input MediaType
    * @return boolean true if the MediaType represents JSON, false otherwise
    */
    public boolean isJsonMime(String mediaType) {
        // "* / *" is default to JSON
        if ("*/*".equals(mediaType)) {
            return true;
        }

        try {
            return isJsonMime(MediaType.parseMediaType(mediaType));
        } catch (InvalidMediaTypeException e) {
        }
        return false;
    }

    /**
     * Check if the given MIME is a JSON MIME.
     * JSON MIME examples:
     *     application/json
     *     application/json; charset=UTF8
     *     APPLICATION/JSON
     * @param mediaType the input MediaType
     * @return boolean true if the MediaType represents JSON, false otherwise
     */
    public boolean isJsonMime(MediaType mediaType) {
        return mediaType != null && (MediaType.APPLICATION_JSON.isCompatibleWith(mediaType) || mediaType.getSubtype().matches("^.*\\+json[;]?\\s*$"));
    }

    /**
    * Check if the given {@code String} is a Problem JSON MIME (RFC-7807).
    * @param mediaType the input MediaType
    * @return boolean true if the MediaType represents Problem JSON, false otherwise
    */
    public boolean isProblemJsonMime(String mediaType) {
        return "application/problem+json".equalsIgnoreCase(mediaType);
    }

    /**
     * Select the Accept header's value from the given accepts array:
     *     if JSON exists in the given array, use it;
     *     otherwise use all of them (joining into a string)
     *
     * @param accepts The accepts array to select from
     * @return List The list of MediaTypes to use for the Accept header
     */
    public List<MediaType> selectHeaderAccept(String[] accepts) {
        if (accepts.length == 0) {
            return null;
        }
        for (String accept : accepts) {
            MediaType mediaType = MediaType.parseMediaType(accept);
            if (isJsonMime(mediaType) && !isProblemJsonMime(accept)) {
                return Collections.singletonList(mediaType);
            }
        }
        return MediaType.parseMediaTypes(StringUtils.arrayToCommaDelimitedString(accepts));
    }

    /**
     * Select the Content-Type header's value from the given array:
     *     if JSON exists in the given array, use it;
     *     otherwise use the first one of the array.
     *
     * @param contentTypes The Content-Type array to select from
     * @return MediaType The Content-Type header to use. If the given array is empty, null will be returned.
     */
    public MediaType selectHeaderContentType(String[] contentTypes) {
        if (contentTypes.length == 0) {
            return null;
        }
        for (String contentType : contentTypes) {
            MediaType mediaType = MediaType.parseMediaType(contentType);
            if (isJsonMime(mediaType)) {
                return mediaType;
            }
        }
        return MediaType.parseMediaType(contentTypes[0]);
    }

    /**
     * Select the body to use for the request
     * @param obj the body object
     * @param formParams the form parameters
     * @param contentType the content type of the request
     * @return Object the selected body
     */
    protected BodyInserter<?, ? super ClientHttpRequest> selectBody(Object obj, MultiValueMap<String, Object> formParams, MediaType contentType) {
        if(MediaType.APPLICATION_FORM_URLENCODED.equals(contentType)) {
            MultiValueMap<String, String> map = new LinkedMultiValueMap<>();

            formParams
                    .toSingleValueMap()
                    .entrySet()
                    .forEach(es -> map.add(es.getKey(), String.valueOf(es.getValue())));

            return BodyInserters.fromFormData(map);
        } else if(MediaType.MULTIPART_FORM_DATA.equals(contentType)) {
            return BodyInserters.fromMultipartData(formParams);
        } else {
            return obj != null ? BodyInserters.fromValue(obj) : null;
        }
    }

    /**
     * Invoke API by sending HTTP request with the given options.
     *
     * @param <T> the return type to use
     * @param path The sub-path of the HTTP URL
     * @param method The request method
     * @param pathParams The path parameters
     * @param queryParams The query parameters
     * @param body The request body object
     * @param headerParams The header parameters
     * @param formParams The form parameters
     * @param accept The request's Accept header
     * @param contentType The request's Content-Type header
     * @param authNames The authentications to apply
     * @param returnType The return type into which to deserialize the response
     * @return The response body in chosen type
     */
    public <T> ResponseSpec invokeAPI(String path, HttpMethod method, Map<String, Object> pathParams, MultiValueMap<String, String> queryParams, Object body, HttpHeaders headerParams, MultiValueMap<String, String> cookieParams, MultiValueMap<String, Object> formParams, List<MediaType> accept, MediaType contentType, String[] authNames, ParameterizedTypeReference<T> returnType) throws RestClientException {
        final WebClient.RequestBodySpec requestBuilder = prepareRequest(path, method, pathParams, queryParams, body, headerParams, cookieParams, formParams, accept, contentType, authNames);
        return requestBuilder.retrieve();
    }

    /**
     * Include queryParams in uriParams taking into account the paramName
     * @param queryParams The query parameters
     * @param uriParams The path parameters
     * return templatized query string
     */
    private String generateQueryUri(MultiValueMap<String, String> queryParams, Map<String, Object> uriParams) {
        StringBuilder queryBuilder = new StringBuilder();
        queryParams.forEach((name, values) -> {
            if (CollectionUtils.isEmpty(values)) {
                if (queryBuilder.length() != 0) {
                    queryBuilder.append('&');
                }
                queryBuilder.append(name);
            } else {
                int valueItemCounter = 0;
                for (Object value : values) {
                    if (queryBuilder.length() != 0) {
                        queryBuilder.append('&');
                    }
                    queryBuilder.append(name);
                    if (value != null) {
                        String templatizedKey = name + valueItemCounter++;
                        uriParams.put(templatizedKey, value.toString());
                        queryBuilder.append('=').append("{").append(templatizedKey).append("}");
                    }
                }
            }
        });
        return queryBuilder.toString();
    }

    private WebClient.RequestBodySpec prepareRequest(String path, HttpMethod method, Map<String, Object> pathParams,
        MultiValueMap<String, String> queryParams, Object body, HttpHeaders headerParams,
        MultiValueMap<String, String> cookieParams, MultiValueMap<String, Object> formParams, List<MediaType> accept,
        MediaType contentType, String[] authNames) {
        updateParamsForAuth(authNames, queryParams, headerParams, cookieParams);

        final UriComponentsBuilder builder = UriComponentsBuilder.fromHttpUrl(basePath).path(path);

        String finalUri = builder.build(false).toUriString();
        Map<String, Object> uriParams = new HashMap<>();
        uriParams.putAll(pathParams);

        if (queryParams != null && !queryParams.isEmpty()) {
            //Include queryParams in uriParams taking into account the paramName
            String queryUri = generateQueryUri(queryParams, uriParams);
            //Append to finalUri the templatized query string like "?param1={param1Value}&.......
            finalUri += "?" + queryUri;
        }

        final WebClient.RequestBodySpec requestBuilder = webClient.method(method).uri(finalUri, uriParams);

        if (accept != null) {
            requestBuilder.accept(accept.toArray(new MediaType[accept.size()]));
        }
        if(contentType != null) {
            requestBuilder.contentType(contentType);
        }

        addHeadersToRequest(headerParams, requestBuilder);
        addHeadersToRequest(defaultHeaders, requestBuilder);
        addCookiesToRequest(cookieParams, requestBuilder);
        addCookiesToRequest(defaultCookies, requestBuilder);

        requestBuilder.attribute(URI_TEMPLATE_ATTRIBUTE, path);

        requestBuilder.body(selectBody(body, formParams, contentType));
        return requestBuilder;
    }

    /**
     * Add headers to the request that is being built
     * @param headers The headers to add
     * @param requestBuilder The current request
     */
    protected void addHeadersToRequest(HttpHeaders headers, WebClient.RequestBodySpec requestBuilder) {
        for (Entry<String, List<String>> entry : headers.entrySet()) {
            List<String> values = entry.getValue();
            for(String value : values) {
                if (value != null) {
                    requestBuilder.header(entry.getKey(), value);
                }
            }
        }
    }

    /**
     * Add cookies to the request that is being built
     * @param cookies The cookies to add
     * @param requestBuilder The current request
     */
    protected void addCookiesToRequest(MultiValueMap<String, String> cookies, WebClient.RequestBodySpec requestBuilder) {
        for (Entry<String, List<String>> entry : cookies.entrySet()) {
            List<String> values = entry.getValue();
            for(String value : values) {
                if (value != null) {
                    requestBuilder.cookie(entry.getKey(), value);
                }
            }
        }
    }

    /**
     * Update query and header parameters based on authentication settings.
     *
     * @param authNames The authentications to apply
     * @param queryParams The query parameters
     * @param headerParams The header parameters
     * @param cookieParams the cookie parameters
     */
    protected void updateParamsForAuth(String[] authNames, MultiValueMap<String, String> queryParams, HttpHeaders headerParams, MultiValueMap<String, String> cookieParams) {
        for (String authName : authNames) {
            Authentication auth = authentications.get(authName);
            if (auth == null) {
                throw new RestClientException("Authentication undefined: " + authName);
            }
            auth.applyToParams(queryParams, headerParams, cookieParams);
        }
    }

    /**
    * Formats the specified collection path parameter to a string value.
    *
    * @param collectionFormat The collection format of the parameter.
    * @param values The values of the parameter.
    * @return String representation of the parameter
    */
    public String collectionPathParameterToString(CollectionFormat collectionFormat, Collection<?> values) {
        // create the value based on the collection format
        if (CollectionFormat.MULTI.equals(collectionFormat)) {
            // not valid for path params
            return parameterToString(values);
        }

         // collectionFormat is assumed to be "csv" by default
        if(collectionFormat == null) {
            collectionFormat = CollectionFormat.CSV;
        }

        return collectionFormat.collectionToString(values);
    }
}
