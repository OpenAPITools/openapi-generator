# OpenAI C++ SDK Implementation Plan

## Objective

Turn this generated OpenAPI client snapshot into a supported, secure, ergonomic public binary C++ SDK for the OpenAI API. The SDK must support every non-legacy, non-beta OpenAPI operation, plus synchronous requests, streaming, file and binary transfers, Realtime WebSockets, webhook verification, typed errors, and a stable public C++ interface.

The initial production milestone is a configured, HTTPS-by-default Responses client with typed errors, request IDs, Server-Sent Events (SSE) streaming, and reliable tests. Full API coverage follows in deliberate, tested tiers.

## Current State

The directory currently contains an OpenAPI document and generated code only.

- `openapi.yaml` is OpenAPI 3.1, API definition version 2.3.0, and declares bearer authentication for `https://api.openai.com/v1`.
- `generated/CMakeLists.txt` now builds `api/HttpClientImpl.cpp`, uses Boost 1.75, links OpenSSL and Threads, and installs generated headers. It still sets C++11, does not export a relocatable CMake package, and does not define a public SDK target.
- `generated/api/HttpClientImpl.cpp` now supports explicit HTTP and HTTPS transports, TLS 1.2 or newer, SNI, peer and hostname verification, operation timeouts, response body limits, and serialized execution. The legacy constructor still defaults to plaintext HTTP, opens a new connection per request, has no proxy or cancellation API, and returns only a status/body pair.
- Generated operations now use Boost.JSON, RFC 3986 path/query encoding, `Content-Type`, multipart formatting, and explicit unexpected-status exceptions. They still do not apply client-level bearer authentication, retain response headers or request IDs, expose structured OpenAI error bodies, stream SSE responses, or preserve binary/file streams without buffering.
- The generated output has no public `openai` namespace, client configuration type, SDK facade, documentation, test suite, packaging, CI, release policy, or reproducible generation command.

## Product Boundaries

### Public SDK promises

The public SDK must provide:

- A stable `openai` namespace and an RAII `openai::Client` facade.
- Configurable API keys, base URL, project and organization headers, beta headers, timeouts, proxies, user agent, retry policy, and custom transports.
- A C++17 synchronous and callback-based asynchronous core. An optional C++20 coroutine adapter may wrap that core without changing the core binary ABI.
- Typed request and response models, typed API errors, raw response access, request IDs, and pagination helpers.
- SSE streaming for Responses and streaming media endpoints.
- Multipart uploads, binary downloads, and stream-to-file or stream-to-callback interfaces.
- WebSocket support for Realtime and Responses WebSocket mode.
- Standards-compliant webhook verification over the unmodified raw body.
- A documented compatibility policy for supported OpenAI API groups and beta features.

### Non-goals for the first production release

- Supporting every beta endpoint before the core API is reliable.
- Automatically retrying every non-idempotent request.
- Exposing generator-internal classes as the long-term public API.
- Shipping API keys, webhook secrets, or live credentials in source, examples, tests, or CI logs.

## Target Layout

Keep generated code private and wrap it with hand-written, stable interfaces.

```text
openai-cpp-sdk/
  CMakeLists.txt
  CMakePresets.json
  cmake/
  include/openai/
    client.hpp
    client_options.hpp
    error.hpp
    response.hpp
    transport.hpp
    streaming.hpp
    webhook.hpp
    realtime.hpp
    responses.hpp
    files.hpp
    embeddings.hpp
  src/
    client.cpp
    core/
    transport/
    streaming/
    realtime/
    webhook/
    services/
  generated/
    # Reproducible, private OpenAPI output only.
  tests/
    unit/
    integration/
    contract/
    fixtures/
  examples/
  docs/
  scripts/
  abi/
    public-headers.txt
    baseline/
  openapi/
    openapi.yaml
    generator-config.yaml
    generator/
      templates/
      patches/
    coverage.yaml
```

The public facade should expose `openai::*` types. The generated `org::openapitools::*` namespace must remain private behind adapters until the generator is customized to emit a private, SDK-owned namespace. Generated files are never edited by hand: they are output from the pinned generator, SDK-owned templates, and an ordered patch series under `openapi/generator/`.

## Architecture Decisions

### Language and dependencies

- Require C++17 for the ABI-stable core and supported toolchains. Ship an optional C++20 coroutine adapter as a separately versioned target that is not part of the core ABI contract.
- Use Boost.Beast and Boost.Asio for HTTP/1.1, TLS, WebSockets, and async I/O.
- Use OpenSSL for TLS and cryptographic primitives such as webhook HMAC verification.
- Use Boost.JSON for JSON parsing and serialization. Do not use `boost::property_tree` at the API boundary.
- Require CMake 3.20 or newer, Boost 1.75 or newer, and OpenSSL 3.0 or newer. Do not expose Boost, Asio, or OpenSSL types in public headers or ABI signatures.
- Use exported CMake targets, relocatable `find_package(openai_sdk CONFIG REQUIRED)` configuration, CMake presets, and platform CI builds.
- Keep an internal transport seam for deterministic mocks and integration fixtures. Any public custom transport or executor hook lives in `openai::experimental`, is source-compatible only, and is explicitly excluded from the stable binary ABI until independently versioned.

### Binary ABI contract

- Publish a shared `openai_sdk` binary with an explicit export header generated by CMake `GenerateExportHeader`; hide all other symbols with `CXX_VISIBILITY_PRESET=hidden` and `VISIBILITY_INLINES_HIDDEN=TRUE`.
- Export only documented `openai::*` symbols. Generated classes, Boost/Asio/OpenSSL types, implementation helpers, and transport internals are not public ABI.
- Use PImpl for exported classes with non-inline destructors and avoid virtual public interfaces unless their ABI is explicitly versioned. Do not expose data-member layouts, STL containers, or template-heavy generated models in exported class layouts.
- Keep request/response protocol structs, generated models, virtual transport interfaces, and custom executor hooks behind the library boundary. Public services return exported PImpl-backed handles or value types whose ABI is separately tested; they do not expose generated protocol types as durable ABI.
- Preserve ABI compatibility within a major SDK version for each supported operating system, architecture, compiler family, standard library ABI, build mode, and runtime configuration. Do not promise binary compatibility across different C++ standard libraries, compiler ABIs, or MSVC runtime modes.
- Store approved ABI baselines under `abi/baseline/`; compare release candidates against them in CI with an ABI compatibility tool. Breaking ABI changes require a major version bump.
- Ship the core binary and optional C++20 adapter as separate targets. The adapter may depend on the core but cannot alter or expose the core's private layout.
- Publish a binary artifact matrix per supported tuple, including the SDK build mode, compiler/runtime requirements, dependency linkage strategy, license notices, checksums, and signatures. A downstream binary built against an earlier compatible minor release must link and run against the candidate release in ABI CI.

### Generator ownership

- Pin the exact OpenAPI Generator version, template revision, and generation container or toolchain digest in `openapi/generator-config.yaml`.
- Keep SDK-specific templates and patches in `openapi/generator/`; every patch records its upstream template source and applies cleanly during `scripts/regenerate.sh`.
- Generate into a clean temporary directory, run formatting and generated-source compilation, then compare the result with `generated/`. A patch conflict, unformatted output, missing generated file, or compilation failure fails CI.
- Make generator changes upstream where practical, but do not depend on an unreleased upstream fix for reproducibility. Record the upstream issue or commit in the local patch metadata.

### Client model

```cpp
openai::ClientOptions options;
options.api_key = api_key;
options.base_url = "https://api.openai.com/v1";
options.project = project_id;
options.organization = organization_id;

openai::Client client(options);

auto response = client.responses().create(request);
for (const auto& event : client.responses().create_stream(request)) {
    // Handle typed SSE events.
}
```

`Client` owns immutable shared configuration and a transport pool. Service objects such as `responses()`, `files()`, and `embeddings()` are lightweight views over that shared state.

`ClientOptions` contains separate `api_key` and `admin_api_key` credential providers. Route metadata selects the minimum appropriate credential type. Admin credentials are rejected for model/data routes, normal project credentials are rejected for administration routes, and callers may provide a rotating credential provider without exposing secret values to logs.

### Transport model

Use an internal transport result that retains protocol information instead of returning only a status and string body. The public facade translates it to exported PImpl-backed response/error handles and does not expose this structure as a stable ABI type.

```cpp
struct HttpResponse {
    int status;
    HeaderMap headers;
    std::string request_id;
    std::vector<std::byte> body;
};
```

The core transport must support request cancellation, connect/read/write deadlines, TLS verification, SNI, HTTP keep-alive, configurable proxy behavior, and bounded body sizes. It must never log secrets by default.

## Workstreams and Milestones

### Phase 0: Foundation and reproducibility

Deliverables:

- Create the root CMake project, exported `openai::sdk` target, CMake presets, install rules, package configuration, license files, and a real README.
- Define the installed target set: stable `openai::sdk`, optional C++20 `openai::sdk_coro`, and explicit opt-in `openai::sdk_beta`. Generated and internal targets are never installed as public dependencies.
- Move the canonical API definition to `openapi/openapi.yaml`; record its source URL, document version, SHA-256 digest, refresh date, and deprecation snapshot.
- Add `openapi/generator-config.yaml`, SDK-owned templates/patches, and a `scripts/regenerate.sh` command that regenerates to a temporary directory, applies patches, compiles generated source, normalizes formatting, and fails if committed output differs.
- Generate `openapi/coverage.yaml` from every OpenAPI operation ID. Each operation must be classified as `stable`, `admin`, `beta`, `legacy`, or `unsupported` with a rationale, owner, replacement, and target release where applicable.
- Treat OpenAPI `deprecated` markers and documented deprecations as `legacy` by default. Treat vendor-marked beta operations as `beta` by default. Any override requires a reviewed explicit manifest entry.
- Define a written compatibility policy: SemVer, ABI policy, supported compiler/standard-library/runtime tuples, C++17 core and C++20 adapter policy, supported dependency versions, beta policy, and deprecation policy.
- Add CI for Linux, macOS, and Windows with GCC, Clang, and MSVC; cache dependencies without caching generated source changes.

Acceptance criteria:

- A fresh clone configures, builds, installs, and consumes the package from a small downstream CMake project.
- `scripts/regenerate.sh --check` is deterministic and leaves no untracked generated artifacts.
- The generated target compiles from a clean directory with the pinned Boost/OpenSSL toolchain, and generation CI fails before a source-list, transport, or generated-model regression can be committed.
- Installed package tests validate exported symbols, relocatable CMake package discovery, and the supported compiler/runtime matrix.
- ABI CI builds a downstream fixture against the previous compatible minor binary, replaces only the SDK binary with the candidate build, and executes the fixture for every supported release tuple.
- The coverage manifest has no unclassified operations.

### Phase 1: Secure HTTP foundation

Deliverables:

- Harden the existing TLS-capable Beast transport rather than replacing it: retain `boost::beast::ssl_stream`, TLS 1.2 minimum, SNI, peer verification, hostname verification, operation timeouts, and response-size limits.
- Make HTTPS the only public default. Deprecate the plaintext-HTTP constructor and allow HTTP only through a test-only or explicitly unsafe development option.
- Add caller cancellation, proxy support, connection reuse/pooling, configurable CA bundles, per-stage timeout configuration, safe socket shutdown, and a concurrency model that does not serialize all SDK calls behind one mutex.
- Implement a `ClientOptions` type for separate normal and administration credential providers, base URL, optional organization/project identifiers, extra headers, beta headers, proxy settings, user agent, timeouts, and retry configuration. Expose custom transports and executors only from an explicitly unstable `openai::experimental` extension API.
- Apply the credential selected by route metadata. Apply project, organization, beta, idempotency, and safety identifier headers only when configured and applicable; reject credentials that are invalid for a route class before transmitting a request.
- Return status, response headers, parsed request ID, and raw bytes. Preserve bodies for both successful and failed requests.

Acceptance criteria:

- A mocked TLS server confirms SNI, certificate validation, authorization, header propagation, timeouts, and cancellation.
- A request to `https://api.openai.com/v1` cannot accidentally use plaintext TCP.
- Mocked administration and model endpoints confirm that admin credentials never cross onto model/data routes and normal credentials never cross onto administration routes.
- Secrets never appear in exception strings, logs, test snapshots, or tracing attributes by default.

### Phase 2: Error handling, JSON, and generated REST correctness

Deliverables:

- Validate and complete the current Boost.JSON generated serialization with exhaustive tests for booleans, integers, doubles, null, arrays, objects, free-form properties, `oneOf`, `anyOf`, discriminators, and unknown event fields.
- Keep the current path/query encoding and `Content-Type` generator fixes under regression tests, then complete generator support for repeated parameters, optional parameters, required headers, JSON bodies, multipart bodies, binary bodies, and response content negotiation.
- Replace the current string-buffered multipart implementation with a streaming form writer that preserves actual filenames and content types, and ensure no request body can be overwritten by unrelated header or query parameters.
- Map non-2xx responses to `openai::ApiError` with HTTP status, parsed OpenAI error fields, request ID, and raw body.
- Add explicit error subclasses or categories for authentication, permission, validation, rate limiting, conflict, timeout, transport, parsing, and server failures.
- Parse and expose `x-ratelimit-*`, `retry-after`, and request ID response headers. Add conservative jittered exponential retry/backoff behavior for retryable transport failures and selected HTTP failures. Never automatically repeat non-idempotent calls unless the operation is explicitly safe or carries an idempotency key.

Acceptance criteria:

- Golden request tests cover every parameter serialization mode in the OpenAPI document.
- An injected 400, 401, 403, 404, 409, 422, 429, and 5xx response produces a typed error with the original request ID and body available.
- Rate-limit fixtures verify parsed limits, reset times, retry timing, bounded retry attempts, and no automatic retries for unsafe operations.
- The core endpoint test suite finds no silent default-model returns for unhandled status codes.

### Phase 3: Core API vertical slice

Implement a small, high-value set completely before broad endpoint generation, then complete the entire non-legacy, non-beta surface tracked by `openapi/coverage.yaml`.

Priority services:

1. Responses: create, retrieve, delete, cancel background responses, list input items, and structured tool-call output handling.
2. Embeddings: create embeddings with typed vectors and usage metadata.
3. Models: list and retrieve models.
4. Files: upload, retrieve metadata, retrieve content, list, and delete.
5. Moderations: create moderation requests.
6. Images and Audio: initial create operations plus robust non-JSON result handling.

Deliverables:

- A hand-written ergonomic facade over generated request and response types.
- Convenience accessors for response output text, tool calls, structured output, pagination, and raw response details.
- Cursor/page helpers that avoid implicit unbounded fetching.
- Examples for a text response, structured output, embedding, upload/download, and error handling.

Acceptance criteria:

- Each priority service has mock-backed tests and opt-in live contract tests.
- A developer can complete common Responses, Embeddings, and Files workflows without constructing generated API classes directly.
- Every non-legacy, non-beta operation has an assigned subsequent implementation milestone in the coverage manifest before this phase closes.

### Phase 4: Streaming, multipart, and binary transfers

Deliverables:

- Implement SSE parsing with typed events, a callback interface, an iterator/range interface, cancellation, reconnect policy where supported, and bounded buffering/backpressure behavior.
- Support Responses streaming plus streaming Audio and transcription events where the endpoint returns `text/event-stream`.
- Implement a multipart writer that can stream files from memory, `std::istream`, or a filesystem path without loading arbitrary files into RAM.
- Implement binary response sinks for file content, audio, image, and video downloads; avoid coercing binary payloads into JSON strings.
- Add content-type validation and user-selectable raw-body handling for future media types.

Acceptance criteria:

- Streaming tests exercise fragmented chunks, comments, multi-line SSE data, malformed events, cancellation, and mid-stream errors.
- Multipart tests assert boundaries, filenames, content type, and large-file streaming behavior.
- Binary round-trip tests verify no byte corruption.

### Phase 5: Realtime and Responses WebSockets

Deliverables:

- Add a TLS WebSocket session built on Beast with authorization, `OpenAI-Safety-Identifier` support, ping/pong, cancellation, bounded event queues, and typed client/server events.
- Implement Responses WebSocket mode for multi-turn `response.create` workflows.
- Implement Realtime sessions, client event sending, server event dispatch, audio buffer transfer, tool-call event handling, and session updates.
- Keep WebRTC-specific setup calls separate from WebSocket event transport; document supported connection modes and platform constraints.
- Model Responses WebSocket mode as a state machine: one in-flight response per connection, a maximum connection lifetime, explicit close/reconnect events, and caller-controlled recovery. Reconnection must not transparently replay tool calls; it must continue from a durable `previous_response_id` or caller-supplied full context.

Acceptance criteria:

- A local WebSocket fixture verifies connection setup, fragmented messages, event ordering, close/error handling, and backpressure.
- WebSocket tests enforce one in-flight Responses request, connection-expiry handling, and caller-directed recovery without duplicate tool execution.
- An opt-in live smoke test can create and close a Responses WebSocket session without exposing an API key in logs.

### Phase 6: Webhook verification and platform services

Deliverables:

- Implement webhook verification over the exact raw request body using the OpenAI webhook secret and Standard Webhooks-compatible validation.
- Require and expose `webhook-id`, `webhook-timestamp`, and `webhook-signature`; enforce a configurable timestamp tolerance and provide idempotency/replay hooks.
- Parse verified webhook events into typed event envelopes while preserving unknown event data for forward compatibility.
- Add the remaining stable service surface in manifest order: Chat/Completions where not classified legacy, Uploads, Batches, Fine-tuning, Vector Stores, Conversations, Evals, Containers, hosted tools, and organization/project administration.
- Keep admin and beta APIs in explicitly marked modules so their permissions and stability are clear.

Acceptance criteria:

- Valid signatures verify, altered raw bodies fail, expired timestamps fail, and duplicate webhook IDs can be rejected by an application-provided store.
- Every added service has request, response, error, and pagination coverage before being marked stable.
- No non-legacy, non-beta operation remains unimplemented or untested at the end of this phase.

### Phase 7: Release hardening

Deliverables:

- API coverage matrix generated from `openapi/coverage.yaml`, showing stable, admin, beta, legacy, and unsupported operations with rationale and replacements.
- Doxygen or Markdown API reference, migration guide, troubleshooting guide, security policy, and changelog.
- SemVer release automation, source archives, release artifacts, SBOM, dependency vulnerability scanning, and reproducible build documentation.
- Fuzz/property tests for JSON parsing, SSE parsing, multipart construction, URL/query encoding, and webhook verification.
- Performance tests for connection reuse, large downloads, high-volume streaming, and Realtime event throughput.

Acceptance criteria:

- CI runs formatting, linting, unit tests, mock integration tests, generated-code freshness checks, package-consumer tests, and security scans.
- CI runs ABI compatibility checks against the latest release baseline for each supported binary tuple.
- Live tests remain opt-in and use short-lived environment-provided credentials.
- The release checklist signs off supported platforms, dependency versions, API coverage, and known limitations.

## API Coverage Policy

| Tier | Scope | Release requirement |
| --- | --- | --- |
| Stable | Every OpenAPI operation not classified as legacy or beta | Full request/response/error coverage, ergonomic facade, package-consumer coverage |
| Admin | Stable administration operations | Separate module, route-level admin credentials, audit-safe logging |
| Beta | Vendor-marked or documented beta operations | Explicit opt-in module, API-specific integration coverage, no stable compatibility promise |
| Legacy | Deprecated or documented legacy operations | Not supported by default; manifest includes replacement and removal timeline |
| Unsupported | Explicitly excluded operations | Manifest rationale, replacement where available, and reviewed owner |

No endpoint is labeled supported solely because an OpenAPI class exists. It must pass its relevant transport, serialization, error, integration, and package-consumer tests. CI fails if a new OpenAPI operation has no coverage classification.

The initial coverage manifest must classify the Assistants API as legacy because OpenAI has announced its deprecation and migration to Responses. It must classify the retired Realtime beta protocol as legacy while treating the released Realtime API as a separate, independently tested surface.

## Test Strategy

### Unit tests

- URL path and query encoding, headers, auth, client configuration, retry classification, idempotency, and error parsing.
- Boost.JSON model serialization and deserialization for scalar, null, union, arbitrary object, and unknown-field cases.
- TLS configuration, certificate verification, timeout/cancellation behavior, and connection reuse using injectable transports.
- SSE, WebSocket, multipart, binary, and webhook verification parsers.
- Public-header compile tests, exported-symbol tests, and ABI comparison tests for each supported binary tuple.

### Mocked integration tests

- Local HTTP/TLS server fixtures for complete request/response exchanges.
- SSE and WebSocket fixture servers for fragmented and malformed protocol data.
- Golden tests generated from curated OpenAPI examples, with sensitive values replaced by deterministic placeholders.

### Live contract tests

- Disabled by default and gated by an explicit environment variable.
- Read credentials only from the environment or a secret manager integration.
- Use a dedicated test project, low-cost models, narrow quotas, strict cleanup, and no snapshotting of bodies containing user data.

## Security Requirements

- TLS certificate and hostname verification are enabled by default and cannot be disabled accidentally in production builds.
- API keys and webhook secrets are redacted from exceptions, logs, traces, crash reports, and debug output.
- Administration credentials are isolated from normal API credentials and never transmitted to model or data routes.
- Webhooks are verified against the raw request body before JSON parsing; reject invalid signatures and stale timestamps.
- The public API supports a privacy-preserving `OpenAI-Safety-Identifier` but never derives one from raw user data.
- Retries do not replay non-idempotent uploads or response creation requests unless explicitly configured with an idempotency strategy.
- Dependency updates and OpenSSL/Boost vulnerability alerts are tracked in CI.

## Open Decisions

Resolve these before Phase 1 implementation:

1. Must the callback-based C++17 core support custom Asio executors supplied by host applications, or only SDK-owned execution contexts?
2. Which binary tuples are first-class release artifacts: operating system, architecture, compiler family/version, standard library ABI, and MSVC runtime mode?
3. Which distribution channels are first-class: vcpkg, Conan, Homebrew, system packages, NuGet, GitHub Releases, or source-only CMake?
4. Should generated types remain internal permanently, or should the generator eventually emit public `openai` types while preserving ABI guarantees?
5. Which beta APIs, if any, are required as explicit opt-in modules for the first release?
6. What observability hooks are needed for the consuming application: logging, OpenTelemetry, custom headers, request hooks, or metrics?

## Authoritative References

- OpenAI API definition: `https://platform.openai.com/docs/static/api-definition.yaml`
- OpenAI Responses and WebSocket mode guides: `https://developers.openai.com/api/docs/guides/websocket-mode`
- OpenAI Realtime WebSocket guide: `https://developers.openai.com/api/docs/guides/realtime-websocket`
- OpenAI webhook guide: `https://developers.openai.com/api/docs/guides/webhooks`
- OpenAI rate-limit and retry guidance: `https://developers.openai.com/api/docs/guides/rate-limits`
- OpenAI deprecations: `https://developers.openai.com/api/docs/deprecations`
- Boost.JSON documentation: `https://www.boost.org/doc/libs/1_75_0/libs/json/doc/html/json/index.html`
- CMake export-header documentation: `https://cmake.org/cmake/help/latest/module/GenerateExportHeader.html`
