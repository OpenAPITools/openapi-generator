package org.openapitools.client;

import okhttp3.*;
import okio.AsyncTimeout;
import okio.Buffer;
import okio.BufferedSource;
import okio.Timeout;
import play.libs.ws.WSClient;
import play.libs.ws.WSRequest;
import play.libs.ws.WSResponse;
import play.libs.ws.WSRequestFilter;
import play.libs.ws.WSCookie;
import play.libs.ws.WSCookieBuilder;

import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletionStage;
import java.util.concurrent.Executor;

/**
 * Creates {@link Call} instances that invoke underlying {@link WSClient}
 */
public class Play26CallFactory implements okhttp3.Call.Factory {

    /** PlayWS http client */
    private final WSClient wsClient;

    /** Extra headers to add to request */
    private Map<String, String> extraHeaders = new HashMap<>();

    /** Extra cookies to add to request */
    private Map<String, String> extraCookies = new HashMap<>();

    /** Extra query parameters to add to request */
    private List<Pair> extraQueryParams = new ArrayList<>();

    /** Filters (interceptors) */
    private List<WSRequestFilter> filters = new ArrayList<>();

    /** Executor for WSClient */
    private Executor executor;

    public Play26CallFactory(WSClient wsClient) {
        this.wsClient = wsClient;
    }

    public Play26CallFactory(WSClient wsClient, List<WSRequestFilter> filters) {
        this.wsClient = wsClient;
        this.filters.addAll(filters);
    }

    public Play26CallFactory(WSClient wsClient, Map<String, String> extraHeaders,
        Map<String, String> extraCookies,
        List<Pair> extraQueryParams) {
        this.wsClient = wsClient;

        this.extraHeaders.putAll(extraHeaders);
        this.extraCookies.putAll(extraCookies);
        this.extraQueryParams.addAll(extraQueryParams);
    }

    public Play26CallFactory withExecutor(Executor executor) {
        this.executor = executor;
        return this;
    }

    @Override
    public Call newCall(Request request) {
        // add extra headers
        Request.Builder rb = request.newBuilder();
        for (Map.Entry<String, String> header : this.extraHeaders.entrySet()) {
            rb.addHeader(header.getKey(), header.getValue());
        }
        for (Map.Entry<String, String> cookie : this.extraCookies.entrySet()) {
            rb.addHeader("Cookie", String.format("%s=%s", cookie.getKey(), cookie.getValue()));
        }

        // add extra query params
        if (!this.extraQueryParams.isEmpty()) {
            String newQuery = request.url().uri().getQuery();
            for (Pair queryParam : this.extraQueryParams) {
                String param = String.format("%s=%s", queryParam.getName(), queryParam.getValue());
                if (newQuery == null) {
                    newQuery = param;
                } else {
                    newQuery += "&" + param;
                }
            }

            URI newUri;
            try {
                newUri = new URI(request.url().uri().getScheme(), request.url().uri().getAuthority(),
                        request.url().uri().getPath(), newQuery, request.url().uri().getFragment());
                rb.url(newUri.toURL());
            } catch (MalformedURLException | URISyntaxException e) {
                throw new RuntimeException("Error while updating an url", e);
            }
        }

        return new PlayWSCall(wsClient, this.executor, this.filters, rb.build());
    }

    /**
     * Call implementation that delegates to Play WS Client
     */
    static class PlayWSCall implements Call {

        private final WSClient wsClient;
        private WSRequest wsRequest;
        private List<WSRequestFilter> filters;
        private Executor executor = java.util.concurrent.ForkJoinPool.commonPool();

        private final Request request;
        private final AsyncTimeout timeout;

        public PlayWSCall(WSClient wsClient, Executor executor, List<WSRequestFilter> filters, Request request) {
            this.wsClient = wsClient;
            this.request = request;
            this.filters = filters;
            this.timeout = new AsyncTimeout();

            if (executor != null) {
                this.executor = executor;
            }
        }

        @Override
        public Request request() {
            return request;
        }

        @Override
        public Timeout timeout() {
            return timeout;
        }

        @Override
        public void enqueue(final okhttp3.Callback responseCallback) {
            final Call call = this;
            final CompletionStage<WSResponse> promise = executeAsync();

            promise.whenCompleteAsync((v, t) -> {
                if (t != null) {
                    if (t instanceof IOException) {
                        responseCallback.onFailure(call, (IOException) t);
                    } else {
                        responseCallback.onFailure(call, new IOException(t));
                    }
                } else {
                    try {
                        responseCallback.onResponse(call, PlayWSCall.this.toWSResponse(v));
                    } catch (Exception e) {
                        responseCallback.onFailure(call, new IOException(e));
                    }
                }
            }, this.executor);
        }

        CompletionStage<WSResponse> executeAsync() {
            try {
                HttpUrl url = request.url();
                wsRequest = wsClient.url(url.scheme()+ "://" + url.host() + ":" + url.port() + url.encodedPath());
                url.queryParameterNames().forEach(queryParam -> {
                    wsRequest.addQueryParameter(queryParam, url.queryParameter(queryParam));
                });
                addHeaders(wsRequest);
                addCookies(wsRequest);
                if (request.body() != null) {
                    addBody(wsRequest);
                }
                filters.stream().forEach(f -> wsRequest.setRequestFilter(f));

                return wsRequest.execute(request.method());
            } catch (Exception e) {
                throw new RuntimeException(e.getMessage(), e);
            }
        }

        private void addHeaders(WSRequest wsRequest) {
            for(Map.Entry<String, List<String>> entry : request.headers().toMultimap().entrySet()) {
                List<String> values = entry.getValue();
                for (String value : values) {
                    wsRequest.setHeader(entry.getKey(), value);
                }
            }
        }

        private void addCookies(WSRequest wsRequest) {
            for (final WSCookie cookie : getCookies()) {
                wsRequest.addCookie(cookie);
            }
        }

        List<WSCookie> getCookies() {
            final List<WSCookie> cookies = new ArrayList<>();
            for (final String cookieString : request.headers("Cookie")) {
                for (String cookie : cookieString.split(";")) {
                    cookie = cookie.trim();
                    final String[] nameAndValue = cookie.split("=");
                    if (nameAndValue.length != 2) {
                        continue;
                    }
                    cookies.add(
                        new WSCookieBuilder()
                            .setName(nameAndValue[0])
                            .setValue(nameAndValue[1])
                            .build()
                    );
                }
            }
            return cookies;
        }

        private void addBody(WSRequest wsRequest) throws IOException {
            MediaType mediaType = request.body().contentType();
            if (mediaType != null) {
                wsRequest.setContentType(mediaType.toString());
            }

            Buffer buffer = new Buffer();
            request.body().writeTo(buffer);
            wsRequest.setBody(buffer.inputStream());
        }

        private Response toWSResponse(final WSResponse r) {
            final Response.Builder builder = new Response.Builder();
            builder.request(request)
                   .code(r.getStatus())
                   .body(new ResponseBody() {

                       @Override
                       public MediaType contentType() {
                          return r.getSingleHeader("Content-Type")
                                  .map(MediaType::parse)
                                  .orElse(null);
                       }

                       @Override
                       public long contentLength() {
                           return r.asByteArray().length;
                       }

                       @Override
                       public BufferedSource source() {
                           return new Buffer().write(r.asByteArray());
                       }
                   });

            for (Map.Entry<String, List<String>> entry : r.getAllHeaders().entrySet()) {
                for (String value : entry.getValue()) {
                    builder.addHeader(entry.getKey(), value);
                }
            }
            for (final WSCookie cookie : r.getCookies()) {
                builder.addHeader("Cookie", String.format("%s=%s", cookie.getName(), cookie.getValue()));
            }

            builder.message(r.getStatusText());
            builder.protocol(Protocol.HTTP_1_1);
            return builder.build();
        }

        @Override
        public Response execute() throws IOException {
            throw new UnsupportedOperationException("Not supported");
        }

        @Override
        public void cancel() {
            throw new UnsupportedOperationException("Not supported");
        }

        @Override
        public PlayWSCall clone() {
            throw new UnsupportedOperationException("Not supported");
        }

        @Override
        public boolean isExecuted() {
            return false;
        }

        @Override
        public boolean isCanceled() {
            return false;
        }
    }
}
