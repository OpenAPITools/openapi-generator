/*
 * Copyright 2026 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package xyz.abcdef.runtime;

import com.fasterxml.jackson.databind.ObjectMapper;
import xyz.abcdef.ApiClient;
import xyz.abcdef.ApiResponse;
import xyz.abcdef.api.DefaultApi;

import javax.net.ssl.SSLContext;
import javax.net.ssl.SSLParameters;
import javax.net.ssl.SSLSession;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.net.Authenticator;
import java.net.CookieHandler;
import java.net.ProxySelector;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpHeaders;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.util.Arrays;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.Executor;
import java.util.concurrent.Flow;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.zip.GZIPInputStream;

public final class NativeInputStreamRuntimeHarness {
  private static final Duration TIMEOUT = Duration.ofSeconds(5);
  private static final byte[] PAYLOAD = "issue-24057-payload".getBytes(StandardCharsets.UTF_8);

  private NativeInputStreamRuntimeHarness() {}

  public static void run(String scenario) throws Exception {
    switch (scenario) {
      case "async-ownership":
        asyncConvenienceCancellationBeforeCompletionClosesStream();
        asyncWithHttpInfoCancellationDuringProcessingClosesStream();
        asyncSuccessLeavesStreamsCallerOwned();
        return;
      case "plain-replay":
        requestPublisherIsOneShot(false);
        return;
      case "gzip-replay":
        requestPublisherIsOneShot(true);
        return;
      default:
        throw new AssertionError("Unknown scenario: " + scenario);
    }
  }

  private static void asyncConvenienceCancellationBeforeCompletionClosesStream() throws Exception {
    TestContext context = new TestContext(null);
    TrackingInputStream responseBody = new TrackingInputStream(PAYLOAD);

    CompletableFuture<InputStream> returned = context.api.download();
    check(returned.cancel(true), "convenience future did not accept cancellation");
    await(context.httpClient.transport.cancelRequested, "convenience cancellation did not reach transport");

    TrackingInputStream outerUndeliverableBody = new TrackingInputStream(PAYLOAD);
    check(!completeConvenienceResponse(returned, outerUndeliverableBody),
        "cancelled convenience future accepted a late response stream");
    await(outerUndeliverableBody.closed, "convenience delivery bridge did not close its late response stream");
    check(outerUndeliverableBody.closeCount.get() == 1,
        "convenience delivery bridge closed its late response stream more than once");

    check(context.httpClient.transport.complete(response(responseBody)), "transport did not complete");
    await(responseBody.closed, "convenience cancellation leaked the undeliverable response stream");
    check(responseBody.closeCount.get() == 1, "convenience cancellation closed the stream more than once");
  }

  private static boolean completeConvenienceResponse(
      CompletableFuture<InputStream> returned, InputStream responseBody) throws Exception {
    Method completeResponse = returned.getClass().getDeclaredMethod("completeResponse", InputStream.class);
    completeResponse.setAccessible(true);
    try {
      return (Boolean) completeResponse.invoke(returned, responseBody);
    } catch (InvocationTargetException e) {
      Throwable cause = e.getCause();
      if (cause instanceof Exception) {
        throw (Exception) cause;
      }
      throw new AssertionError("convenience delivery bridge failed", cause);
    }
  }

  private static void asyncWithHttpInfoCancellationDuringProcessingClosesStream() throws Exception {
    CountDownLatch processingStarted = new CountDownLatch(1);
    CountDownLatch continueProcessing = new CountDownLatch(1);
    TestContext context = new TestContext(ignored -> {
      processingStarted.countDown();
      awaitUnchecked(continueProcessing, "processing was not released");
    });
    TrackingInputStream responseBody = new TrackingInputStream(PAYLOAD);

    CompletableFuture<ApiResponse<InputStream>> returned = context.api.downloadWithHttpInfo();
    check(context.httpClient.transport.complete(response(responseBody)), "transport did not complete");
    await(processingStarted, "async response processing did not start");

    check(returned.cancel(true), "WithHttpInfo future did not accept cancellation");
    await(context.httpClient.transport.cancelRequested, "WithHttpInfo cancellation did not reach transport");
    continueProcessing.countDown();

    await(responseBody.closed, "WithHttpInfo cancellation leaked the processed response stream");
    check(responseBody.closeCount.get() == 1, "WithHttpInfo cancellation closed the stream more than once");
  }

  private static void asyncSuccessLeavesStreamsCallerOwned() throws Exception {
    TestContext convenienceContext = new TestContext(null);
    TrackingInputStream convenienceBody = new TrackingInputStream(PAYLOAD);
    CompletableFuture<InputStream> convenienceResult = convenienceContext.api.download();
    convenienceContext.httpClient.transport.complete(response(convenienceBody));
    InputStream deliveredBody = convenienceResult.get(TIMEOUT.toMillis(), TimeUnit.MILLISECONDS);
    check(convenienceBody.closeCount.get() == 0, "convenience success closed the caller-owned stream");
    check(Arrays.equals(PAYLOAD, deliveredBody.readAllBytes()), "convenience success returned the wrong bytes");
    deliveredBody.close();
    check(convenienceBody.closeCount.get() == 1, "caller close was not observed for convenience result");

    TestContext infoContext = new TestContext(null);
    TrackingInputStream infoBody = new TrackingInputStream(PAYLOAD);
    CompletableFuture<ApiResponse<InputStream>> infoResult = infoContext.api.downloadWithHttpInfo();
    infoContext.httpClient.transport.complete(response(infoBody));
    InputStream deliveredInfoBody = infoResult.get(TIMEOUT.toMillis(), TimeUnit.MILLISECONDS).getData();
    check(infoBody.closeCount.get() == 0, "WithHttpInfo success closed the caller-owned stream");
    check(Arrays.equals(PAYLOAD, deliveredInfoBody.readAllBytes()), "WithHttpInfo success returned the wrong bytes");
    deliveredInfoBody.close();
    check(infoBody.closeCount.get() == 1, "caller close was not observed for WithHttpInfo result");
  }

  private static void requestPublisherIsOneShot(boolean gzip) throws Exception {
    TestContext context = new TestContext(null);
    TrackingInputStream requestBody = new TrackingInputStream(PAYLOAD);
    context.api.upload(requestBody);

    HttpRequest request = context.httpClient.request;
    check(request != null, "upload request was not captured");
    HttpRequest.BodyPublisher publisher = request.bodyPublisher()
        .orElseThrow(() -> new AssertionError("upload request has no body publisher"));

    CollectingSubscriber first = new CollectingSubscriber();
    publisher.subscribe(first);
    byte[] published = first.success();
    byte[] decoded = gzip ? gunzip(published) : published;
    check(Arrays.equals(PAYLOAD, decoded), "first subscription published the wrong payload");
    await(requestBody.closed, "first subscription did not close the source");
    int closesAfterFirstSubscription = requestBody.closeCount.get();
    check(closesAfterFirstSubscription > 0, "first subscription did not close the source");
    if (gzip) {
      check(closesAfterFirstSubscription == 1, "gzip first subscription did not close the source exactly once");
    }

    CollectingSubscriber second = new CollectingSubscriber();
    publisher.subscribe(second);
    Throwable repeatFailure = second.failure();
    check(repeatFailure != null, "second subscription reused a one-shot InputStream");
    check(requestBody.closeCount.get() == closesAfterFirstSubscription,
        "second subscription touched the consumed source stream");

    if (gzip) {
      check("gzip".equals(request.headers().firstValue("Content-Encoding").orElse(null)),
          "gzip request did not set Content-Encoding");
    }
  }

  private static byte[] gunzip(byte[] compressed) throws IOException {
    try (GZIPInputStream gzip = new GZIPInputStream(new ByteArrayInputStream(compressed))) {
      return gzip.readAllBytes();
    }
  }

  private static HttpResponse<InputStream> response(InputStream body) {
    return new TestResponse(body);
  }

  private static void await(CountDownLatch latch, String message) throws InterruptedException {
    check(latch.await(TIMEOUT.toMillis(), TimeUnit.MILLISECONDS), message);
  }

  private static void awaitUnchecked(CountDownLatch latch, String message) {
    try {
      await(latch, message);
    } catch (InterruptedException e) {
      Thread.currentThread().interrupt();
      throw new AssertionError(message, e);
    }
  }

  private static void check(boolean condition, String message) {
    if (!condition) {
      throw new AssertionError(message);
    }
  }

  private static final class TestContext {
    private final ControlledHttpClient httpClient = new ControlledHttpClient();
    private final DefaultApi api;

    private TestContext(java.util.function.Consumer<HttpResponse<InputStream>> asyncInterceptor) {
      ApiClient client = new ApiClient(new ControlledBuilder(httpClient), new ObjectMapper(), "http://localhost");
      client.setAsyncResponseInterceptor(asyncInterceptor);
      api = new DefaultApi(client);
    }
  }

  private static final class TrackingInputStream extends ByteArrayInputStream {
    private final AtomicInteger closeCount = new AtomicInteger();
    private final CountDownLatch closed = new CountDownLatch(1);

    private TrackingInputStream(byte[] bytes) {
      super(bytes);
    }

    @Override
    public void close() throws IOException {
      closeCount.incrementAndGet();
      closed.countDown();
      super.close();
    }
  }

  private static final class NonCancellingFuture<T> extends CompletableFuture<T> {
    private final CountDownLatch cancelRequested = new CountDownLatch(1);

    @Override
    public boolean cancel(boolean mayInterruptIfRunning) {
      cancelRequested.countDown();
      return false;
    }
  }

  private static final class ControlledHttpClient extends HttpClient {
    private final NonCancellingFuture<HttpResponse<InputStream>> transport = new NonCancellingFuture<>();
    private volatile HttpRequest request;

    @Override
    public Optional<CookieHandler> cookieHandler() {
      return Optional.empty();
    }

    @Override
    public Optional<Duration> connectTimeout() {
      return Optional.empty();
    }

    @Override
    public Redirect followRedirects() {
      return Redirect.NEVER;
    }

    @Override
    public Optional<ProxySelector> proxy() {
      return Optional.empty();
    }

    @Override
    public SSLContext sslContext() {
      return null;
    }

    @Override
    public SSLParameters sslParameters() {
      return new SSLParameters();
    }

    @Override
    public Optional<Authenticator> authenticator() {
      return Optional.empty();
    }

    @Override
    public Version version() {
      return Version.HTTP_1_1;
    }

    @Override
    public Optional<Executor> executor() {
      return Optional.empty();
    }

    @Override
    public <T> HttpResponse<T> send(HttpRequest request, HttpResponse.BodyHandler<T> handler) {
      throw new UnsupportedOperationException();
    }

    @SuppressWarnings("unchecked")
    @Override
    public <T> CompletableFuture<HttpResponse<T>> sendAsync(
        HttpRequest request, HttpResponse.BodyHandler<T> handler) {
      this.request = request;
      return (CompletableFuture<HttpResponse<T>>) (CompletableFuture<?>) transport;
    }

    @Override
    public <T> CompletableFuture<HttpResponse<T>> sendAsync(
        HttpRequest request,
        HttpResponse.BodyHandler<T> handler,
        HttpResponse.PushPromiseHandler<T> pushPromiseHandler) {
      return sendAsync(request, handler);
    }
  }

  private static final class ControlledBuilder implements HttpClient.Builder {
    private final HttpClient client;

    private ControlledBuilder(HttpClient client) {
      this.client = client;
    }

    @Override
    public HttpClient.Builder cookieHandler(CookieHandler cookieHandler) { return this; }

    @Override
    public HttpClient.Builder connectTimeout(Duration duration) { return this; }

    @Override
    public HttpClient.Builder sslContext(SSLContext sslContext) { return this; }

    @Override
    public HttpClient.Builder sslParameters(SSLParameters sslParameters) { return this; }

    @Override
    public HttpClient.Builder executor(Executor executor) { return this; }

    @Override
    public HttpClient.Builder followRedirects(HttpClient.Redirect policy) { return this; }

    @Override
    public HttpClient.Builder version(HttpClient.Version version) { return this; }

    @Override
    public HttpClient.Builder priority(int priority) { return this; }

    @Override
    public HttpClient.Builder proxy(ProxySelector proxySelector) { return this; }

    @Override
    public HttpClient.Builder authenticator(Authenticator authenticator) { return this; }

    @Override
    public HttpClient build() { return client; }
  }

  private static final class TestResponse implements HttpResponse<InputStream> {
    private final InputStream body;

    private TestResponse(InputStream body) {
      this.body = body;
    }

    @Override
    public int statusCode() { return 200; }

    @Override
    public HttpRequest request() { return HttpRequest.newBuilder(URI.create("http://localhost/download")).build(); }

    @Override
    public Optional<HttpResponse<InputStream>> previousResponse() { return Optional.empty(); }

    @Override
    public HttpHeaders headers() { return HttpHeaders.of(java.util.Map.of(), (name, value) -> true); }

    @Override
    public InputStream body() { return body; }

    @Override
    public Optional<SSLSession> sslSession() { return Optional.empty(); }

    @Override
    public URI uri() { return URI.create("http://localhost/download"); }

    @Override
    public HttpClient.Version version() { return HttpClient.Version.HTTP_1_1; }
  }

  private static final class CollectingSubscriber implements Flow.Subscriber<ByteBuffer> {
    private final ByteArrayOutputStream bytes = new ByteArrayOutputStream();
    private final CompletableFuture<byte[]> result = new CompletableFuture<>();

    @Override
    public void onSubscribe(Flow.Subscription subscription) {
      subscription.request(Long.MAX_VALUE);
    }

    @Override
    public void onNext(ByteBuffer item) {
      byte[] chunk = new byte[item.remaining()];
      item.get(chunk);
      bytes.write(chunk, 0, chunk.length);
    }

    @Override
    public void onError(Throwable throwable) {
      result.completeExceptionally(throwable);
    }

    @Override
    public void onComplete() {
      result.complete(bytes.toByteArray());
    }

    private byte[] success() throws Exception {
      return result.get(TIMEOUT.toMillis(), TimeUnit.MILLISECONDS);
    }

    private Throwable failure() throws Exception {
      try {
        result.get(TIMEOUT.toMillis(), TimeUnit.MILLISECONDS);
        return null;
      } catch (java.util.concurrent.ExecutionException e) {
        return e.getCause();
      } catch (CompletionException e) {
        return e.getCause();
      }
    }
  }
}
