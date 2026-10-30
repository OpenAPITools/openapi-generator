package org.openapitools.codegen.config;

import ch.qos.logback.classic.spi.ILoggingEvent;
import ch.qos.logback.core.read.ListAppender;
import com.fasterxml.jackson.databind.node.ObjectNode;
import io.swagger.v3.core.util.Json31;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.PathItem;
import io.swagger.v3.oas.models.callbacks.Callback;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.parameters.Parameter;
import io.swagger.v3.oas.models.parameters.RequestBody;
import io.swagger.v3.oas.models.responses.ApiResponse;
import io.swagger.v3.oas.models.responses.ApiResponses;
import io.swagger.v3.parser.core.models.ParseOptions;
import io.swagger.v3.parser.core.models.SwaggerParseResult;
import org.openapitools.codegen.ClientOptInput;
import org.openapitools.codegen.CodegenCallback;
import org.openapitools.codegen.CodegenOperation;
import org.openapitools.codegen.CodegenParameter;
import org.openapitools.codegen.DefaultCodegen;
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.languages.JavaClientCodegen;
import org.openapitools.codegen.utils.ModelUtils;
import org.openapitools.codegen.utils.Oas32Extensions;
import org.slf4j.LoggerFactory;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.io.Closeable;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.nio.file.Paths;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * Tests for reading OpenAPI 3.2 documents through {@link Oas32CompatSpecLoader}
 * (issue #25477): the document is rewritten to 3.1 with 3.2-only members kept
 * in {@code x-oas32-*} extensions, then parsed with the released
 * swagger-parser.
 */
public class Oas32CompatSpecLoaderTest {

    private static final String SPEC = "src/test/resources/3_2/oas32-compat.yaml";

    @SuppressWarnings("unchecked")
    private Context<OpenAPI> contextFor(String inputSpec) {
        return (Context<OpenAPI>) new CodegenConfigurator()
                .setInputSpec(inputSpec)
                .setGeneratorName("java")
                .toContext();
    }

    @Test
    public void forInputSpecNeverPreReadsTheDocument() {
        // The loader is chosen inside load(): first the plain parser read,
        // then - only when that read reports an unreadable document whose
        // "openapi" version is unknown to the released schema - the raw
        // re-read with authorization values. forInputSpec itself reads
        // nothing, so it answers even for locations that cannot be read.
        for (String spec : new String[]{
                SPEC,
                "src/test/resources/3_0/petstore.yaml",
                "src/test/resources/3_1/petstore.yaml",
                "src/test/resources/3_2/does-not-exist.yaml"}) {
            Assert.assertTrue(SpecLoader.forInputSpec(spec) instanceof DefaultSpecLoader, spec);
        }
    }

    @Test
    public void fetchesA31UrlSpecOnceLikeThePlainParser() throws Exception {
        // A 3.1 document must cost exactly the one fetch the plain parser
        // makes: no anonymous, timeout-less pre-read for version sniffing.
        String body = "openapi: 3.1.0\n"
                + "info: {title: t, version: '1'}\n"
                + "paths:\n"
                + "  /x:\n"
                + "    get:\n"
                + "      operationId: g\n"
                + "      responses: {'200': {description: ok}}\n";
        AtomicInteger getCount = new AtomicInteger();
        SpecServer server = serving(body, getCount);
        try {
            String url = urlOf(server);
            ParseOptions options = new ParseOptions();
            options.setResolve(true);
            options.setResolveResponses(true);
            SwaggerParseResult result =
                    SpecLoader.forInputSpec(url).load(url, null, options);
            Assert.assertNotNull(result.getOpenAPI());
            Assert.assertEquals(getCount.get(), 1,
                    "a 3.1 document is fetched once, exactly like master");
        } finally {
            server.close();
        }
    }

    @Test
    public void fetchesAnOas32UrlSpecThroughTheAuthenticatedReread() throws Exception {
        // A 3.2 document costs the parser's read (which reports the unknown
        // "openapi" version) plus the raw re-read that carries the caller's
        // authorization values through the same RemoteUrl path the parser uses.
        String body = "openapi: 3.2.0\n"
                + "info: {title: t, version: '1'}\n"
                + "paths:\n"
                + "  /x:\n"
                + "    query:\n"
                + "      operationId: q\n"
                + "      responses: {'200': {description: ok}}\n";
        AtomicInteger getCount = new AtomicInteger();
        SpecServer server = serving(body, getCount);
        try {
            String url = urlOf(server);
            ParseOptions options = new ParseOptions();
            options.setResolve(true);
            options.setResolveResponses(true);
            SwaggerParseResult master =
                    new io.swagger.parser.OpenAPIParser().readLocation(url, null, options);
            Assert.assertNull(master.getOpenAPI());
            int masterGets = getCount.get();
            SwaggerParseResult result =
                    SpecLoader.forInputSpec(url).load(url, null, options);
            Assert.assertNotNull(result.getOpenAPI());
            Assert.assertEquals(
                    result.getOpenAPI().getExtensions().get(Oas32Extensions.OPENAPI_VERSION_EXT),
                    "3.2.0");
            Assert.assertTrue(result.getOpenAPI().getPaths().get("/x").getExtensions()
                    .containsKey(Oas32Extensions.QUERY_OPERATION_EXT));
            // the parser itself reads a failing URL document several times
            // (the v3 read plus the v2-converter attempt); the adapter adds
            // exactly one fetch on top — the authorized raw re-read
            Assert.assertEquals(getCount.get() - masterGets, masterGets + 1,
                    "the loader's reads must equal the parser's own reads "
                            + "plus the single authorized raw re-read");
        } finally {
            server.close();
        }
    }

    private SpecServer serving(String body, AtomicInteger getCount) throws Exception {
        return new SpecServer(body, getCount);
    }

    private String urlOf(SpecServer server) {
        return "http://127.0.0.1:" + server.getPort() + "/spec.yaml";
    }

    /**
     * The smallest HTTP/1.1 server {@code java.net} can express:
     * {@code com.sun.net.httpserver.HttpServer} is a non-portable JDK API
     * forbidden-apis rejects, so a {@link ServerSocket} accept loop answers
     * every GET with the same fixed body and counts the requests. Each
     * connection handles exactly one request ({@code Connection: close}),
     * which keeps one accepted socket equal to one fetch.
     */
    private static final class SpecServer implements Closeable {
        private final ServerSocket socket;
        private final Thread acceptLoop;
        private final AtomicInteger getCount;

        private SpecServer(String body, AtomicInteger getCount) throws IOException {
            this.getCount = getCount;
            socket = new ServerSocket();
            socket.bind(new InetSocketAddress(InetAddress.getLoopbackAddress(), 0));
            byte[] responseBody = body.getBytes(StandardCharsets.UTF_8);
            byte[] head = ("HTTP/1.1 200 OK\r\n"
                    + "Content-Type: application/yaml\r\n"
                    + "Content-Length: " + responseBody.length + "\r\n"
                    + "Connection: close\r\n\r\n")
                    .getBytes(StandardCharsets.US_ASCII);
            acceptLoop = new Thread(() -> serve(head, responseBody), "oas32-test-server");
            acceptLoop.setDaemon(true);
            acceptLoop.start();
        }

        private void serve(byte[] head, byte[] responseBody) {
            while (!socket.isClosed()) {
                try (Socket connection = socket.accept()) {
                    connection.setSoTimeout(10_000);
                    if (readRequestHead(connection.getInputStream())) {
                        getCount.incrementAndGet();
                    }
                    OutputStream out = connection.getOutputStream();
                    out.write(head);
                    out.write(responseBody);
                    out.flush();
                    connection.shutdownOutput();
                } catch (IOException e) {
                    if (socket.isClosed()) {
                        return;
                    }
                    // a dropped or stalled connection must not stop serving
                }
            }
        }

        /**
         * Consumes one request head (terminated by a blank line) so the
         * response is not lost to a connection reset; returns whether the
         * request line was a GET.
         */
        private boolean readRequestHead(InputStream in) throws IOException {
            boolean isGet = readLine(in).startsWith("GET ");
            while (readLine(in).length() > 0) {
                // drain the remaining headers
            }
            return isGet;
        }

        private String readLine(InputStream in) throws IOException {
            StringBuilder line = new StringBuilder();
            int current;
            while ((current = in.read()) != -1) {
                if (current == '\n') {
                    break;
                }
                if (current != '\r') {
                    line.append((char) current);
                }
            }
            return line.toString();
        }

        private int getPort() {
            return socket.getLocalPort();
        }

        @Override
        public void close() throws IOException {
            socket.close();
        }
    }

    @Test
    public void readsOas32DocumentWithValidateSpec() {
        // default validateSpec=true must not throw SpecValidationException
        OpenAPI openAPI = contextFor(SPEC).getSpecDocument();
        Assert.assertNotNull(openAPI);

        // the original version is kept as an extension
        Assert.assertEquals(openAPI.getExtensions().get(Oas32Extensions.OPENAPI_VERSION_EXT), "3.2.0");

        PathItem things = openAPI.getPaths().get("/things");
        Assert.assertNotNull(things);
        Assert.assertTrue(things.getExtensions().containsKey(Oas32Extensions.QUERY_OPERATION_EXT));
        Assert.assertTrue(things.getExtensions().containsKey(Oas32Extensions.ADDITIONAL_OPERATIONS_EXT));

        // in: "querystring" lowered to query, original kept in x-oas32-in
        Parameter raw = things.getParameters().get(1);
        Assert.assertEquals(raw.getIn(), "query");
        Assert.assertEquals(raw.getExtensions().get(Oas32Extensions.PARAMETER_IN_EXT), "querystring");
        Assert.assertTrue(Oas32Extensions.isQueryStringParam(openAPI, raw));

        // 3.2-only keys elsewhere are kept under x-oas32-*
        Assert.assertEquals(openAPI.getServers().get(0).getExtensions().get("x-oas32-name"), "main");
        Assert.assertEquals(openAPI.getTags().get(0).getExtensions().get("x-oas32-kind"), "nav");
        Assert.assertEquals(
                openAPI.getPaths().get("/things").getGet().getResponses().get("200")
                        .getExtensions().get("x-oas32-summary"), "things list");
        Assert.assertNotNull(openAPI.getComponents().getExtensions().get("x-oas32-mediaTypes"));

        // 3.2 members inside a $ref'd schema also survive: the dereferencer
        // resolves "#/components/..." against the rewritten tree (location=null)
        // rather than re-reading the original file, which would lose the
        // rewrite and report the keys as unexpected.
        Schema thing = openAPI.getComponents().getSchemas().get("Thing");
        Assert.assertEquals(thing.getXml().getExtensions().get("x-oas32-nodeType"), "element");
        Assert.assertEquals(
                thing.getDiscriminator().getExtensions().get("x-oas32-defaultMapping"), "Thing");
    }

    @Test
    public void restoredOperationsReachProcessPaths() throws Exception {
        OpenAPI openAPI = contextFor(SPEC).getSpecDocument();
        Map<String, List<CodegenOperation>> ops = processPaths(openAPI);

        CodegenOperation query = findOperation(ops, "/things", "QUERY");
        Assert.assertNotNull(query, "QUERY operation must reach CodegenOperation");
        Assert.assertEquals(query.operationId, "queryThings");
        Assert.assertEquals(query.httpMethod, "QUERY");

        CodegenOperation purge = findOperation(ops, "/things", "PURGE");
        Assert.assertNotNull(purge, "additionalOperations.PURGE must reach CodegenOperation");
        Assert.assertEquals(purge.operationId, "purgeThings");
        Assert.assertEquals(purge.httpMethod, "PURGE");

        // the querystring parameter of the QUERY operation carries the marker
        CodegenParameter filter = query.allParams.stream()
                .filter(p -> "filter".equals(p.baseName))
                .findFirst().orElse(null);
        Assert.assertNotNull(filter, "querystring parameter 'filter' must be present");
        Assert.assertEquals(filter.vendorExtensions.get(Oas32Extensions.QUERYSTRING_PARAMETER_EXT), true);
        Assert.assertTrue(filter.isQueryParam);
    }

    @Test
    public void restoredOperationsReachProcessWebhooks() throws Exception {
        OpenAPI openAPI = contextFor(SPEC).getSpecDocument();
        Assert.assertNotNull(openAPI.getWebhooks());
        Map<String, List<CodegenOperation>> ops = processWebhooks(openAPI);
        // strict-spec behaviour prefixes the webhook name with '/'
        CodegenOperation query = findOperation(ops, "/thingChanged", "QUERY");
        Assert.assertNotNull(query, "webhook QUERY operation must reach CodegenOperation");
        Assert.assertEquals(query.operationId, "thingChangedQuery");
    }

    @Test
    public void resolvesRelativeFileRefsFromAnyWorkingDirectory() {
        // Absolute inputSpec paths make the resolved $ref base independent of
        // the test runner's working directory: the loader absolutizes relative
        // refs against the spec's location — as plain filesystem paths the
        // parser's readFile accepts — instead of passing a location that
        // would be re-read unrewritten.
        String spec = Paths.get("src/test/resources/3_2/oas32-external-ref.yaml")
                .toAbsolutePath().toString();
        OpenAPI openAPI = contextFor(spec).getSpecDocument();
        Assert.assertNotNull(openAPI);
        // with resolve=true the external schema is inlined at the ref site
        Schema schema = openAPI.getPaths().get("/things").getGet().getResponses().get("200")
                .getContent().get("application/json").getSchema();
        Assert.assertNotNull(schema,
                "relative $ref './oas32-external-components.yaml' must resolve");
        Assert.assertTrue(schema.getProperties().containsKey("id"));

        // ".." relative refs into a parent directory resolve the same way
        String subSpec = Paths.get("src/test/resources/3_2/sub/oas32-parent-ref.yaml")
                .toAbsolutePath().toString();
        OpenAPI sub = contextFor(subSpec).getSpecDocument();
        Assert.assertNotNull(sub);
        Schema subSchema = sub.getPaths().get("/things").getGet().getResponses().get("200")
                .getContent().get("application/json").getSchema();
        Assert.assertNotNull(subSchema,
                "relative $ref '../oas32-external-components.yaml#/components/schemas/Thing' must resolve");
        Assert.assertTrue(subSchema.getProperties().containsKey("id"));
    }

    @Test
    public void absolutizesRefsForUrlAndWindowsInputSpecs() {
        // URL inputSpec: relative refs resolve against the URL location
        URI urlBase = Oas32CompatSpecLoader.refBaseUri("https://example.com/api/spec.yaml");
        Assert.assertEquals(
                Oas32CompatSpecLoader.absolutizeRef("./schemas/Ext.yaml#/components/schemas/Ext", urlBase),
                "https://example.com/api/schemas/Ext.yaml#/components/schemas/Ext");
        Assert.assertEquals(
                Oas32CompatSpecLoader.absolutizeRef("../up.yaml", urlBase),
                "https://example.com/up.yaml");

        // Windows drive-path inputSpec produces a bare "C:/..." reference on
        // any OS — the form Visitor.readURI hands verbatim to readFile, where
        // "file:///C:/..." would lose the drive letter on Windows
        URI winBase = Oas32CompatSpecLoader.refBaseUri("C:\\specs\\api\\spec.yaml");
        Assert.assertEquals(
                Oas32CompatSpecLoader.absolutizeRef("schemas/Ext.yaml", winBase),
                "C:/specs/api/schemas/Ext.yaml");
        Assert.assertEquals(
                Oas32CompatSpecLoader.absolutizeRef("schemas/Ext.yaml#/components/schemas/Ext", winBase),
                "C:/specs/api/schemas/Ext.yaml#/components/schemas/Ext");

        // POSIX local paths become plain absolute paths too
        URI posixBase = Oas32CompatSpecLoader.refBaseUri("/specs/api/spec.yaml");
        Assert.assertEquals(
                Oas32CompatSpecLoader.absolutizeRef("./schemas/Ext.yaml", posixBase),
                "/specs/api/schemas/Ext.yaml");
        Assert.assertEquals(
                Oas32CompatSpecLoader.absolutizeRef("./schemas/Ext.yaml#/components/schemas/Ext", posixBase),
                "/specs/api/schemas/Ext.yaml#/components/schemas/Ext");

        // names needing percent-escapes keep the file: URI form (readURI
        // decodes it through URI.getPath()), since plain text would either
        // break new URI() or reach the filesystem undecoded
        URI escapedBase = URI.create("file:///specs/a%20b/spec.yaml");
        Assert.assertEquals(
                Oas32CompatSpecLoader.absolutizeRef("./schemas/Ext.yaml", escapedBase),
                "file:///specs/a%20b/schemas/Ext.yaml");

        // same-document refs and already-absolute refs are left untouched
        Assert.assertNull(Oas32CompatSpecLoader.absolutizeRef("#/components/schemas/X", urlBase));
        Assert.assertNull(Oas32CompatSpecLoader.absolutizeRef("https://a.example/x.yaml", urlBase));
        Assert.assertNull(Oas32CompatSpecLoader.absolutizeRef("file:///mnt/x/y.yaml", urlBase));
    }

    @Test
    public void warnsOncePerReferencedDocument() {
        ObjectNode doc = Json31.mapper().createObjectNode();
        URI base = URI.create("file:///specs/api/spec.yaml");
        ObjectNode properties = doc.putObject("components").putObject("schemas")
                .putObject("Pair").putObject("properties");
        // two refs into the same document (different fragments) + one elsewhere
        properties.putObject("a")
                .put("$ref", "./ext/Thing.yaml#/components/schemas/Thing");
        properties.putObject("b")
                .put("$ref", "./ext/Thing.yaml#/components/schemas/Other");
        properties.putObject("c")
                .put("$ref", "./ext/Other.yaml#/components/schemas/X");

        ch.qos.logback.classic.Logger logger = (ch.qos.logback.classic.Logger)
                LoggerFactory.getLogger(Oas32CompatSpecLoader.class);
        ListAppender<ILoggingEvent> appender = new ListAppender<>();
        appender.start();
        logger.addAppender(appender);
        try {
            Oas32CompatSpecLoader.absolutizeRefs(doc, base);
        } finally {
            logger.detachAppender(appender);
        }

        long warnings = appender.list.stream()
                .filter(e -> e.getFormattedMessage()
                        .contains("parsed without the OpenAPI 3.2 compatibility rewrite"))
                .count();
        Assert.assertEquals(warnings, 2,
                "one warning per referenced document, fragments deduplicated");
        Assert.assertEquals(doc.at("/components/schemas/Pair/properties/a/$ref").asText(),
                "/specs/api/ext/Thing.yaml#/components/schemas/Thing");
    }

    @Test
    public void absolutizeRefsSkipsPayloadDataButRewritesNameMapMembers() {
        ObjectNode doc = Json31.mapper().createObjectNode();
        URI base = URI.create("file:///specs/api/spec.yaml");

        // payload data: a "$ref" string inside example/default/enum values is
        // document content, not a reference, and must stay untouched
        ObjectNode components = doc.putObject("components");
        ObjectNode schema = components.putObject("schemas").putObject("S");
        schema.putObject("example").putObject("top").put("$ref", "literal.yaml");
        schema.putObject("default").put("$ref", "literal.yaml");
        schema.putArray("enum").addObject().put("$ref", "literal.yaml");
        components.putObject("examples").putObject("ex")
                .putObject("value").putObject("p").put("$ref", "literal.yaml");

        // name -> object maps: member names may collide with payload keys —
        // a property called "example", a "default" response or a "$defs"
        // member called "example" still holds a real $ref that must be
        // rewritten
        schema.putObject("properties").putObject("example")
                .put("$ref", "./schemas/Ext.yaml");
        schema.putObject("$defs").putObject("example")
                .put("$ref", "./schemas/Def.yaml");
        doc.putObject("paths").putObject("/x").putObject("get")
                .putObject("responses").putObject("default")
                .put("$ref", "./responses/err.yaml");

        Oas32CompatSpecLoader.absolutizeRefs(doc, base);

        Assert.assertEquals(doc.at("/components/schemas/S/example/top/$ref").asText(),
                "literal.yaml");
        Assert.assertEquals(doc.at("/components/schemas/S/default/$ref").asText(),
                "literal.yaml");
        Assert.assertEquals(doc.at("/components/schemas/S/enum/0/$ref").asText(),
                "literal.yaml");
        Assert.assertEquals(doc.at("/components/examples/ex/value/p/$ref").asText(),
                "literal.yaml");
        Assert.assertEquals(doc.at("/components/schemas/S/properties/example/$ref").asText(),
                "/specs/api/schemas/Ext.yaml");
        Assert.assertEquals(doc.at("/components/schemas/S/$defs/example/$ref").asText(),
                "/specs/api/schemas/Def.yaml");
        Assert.assertEquals(doc.at("/paths/~1x/get/responses/default/$ref").asText(),
                "/specs/api/responses/err.yaml");
    }

    @Test
    public void responsesWithoutDescriptionPassValidation() {
        // C1: a Response Object without a description is legal in 3.2, so
        // the loader supplies an empty description (recorded by the
        // x-oas32-description-absent marker) and the document validates.
        OpenAPI openAPI = contextFor("src/test/resources/3_2/oas32-nodesc.yaml")
                .getSpecDocument();
        Assert.assertNotNull(openAPI);
        ApiResponse noContent = openAPI.getPaths().get("/things").getGet()
                .getResponses().get("204");
        Assert.assertEquals(noContent.getDescription(), "");
        Assert.assertEquals(noContent.getExtensions()
                .get("x-oas32-description-absent"), true);

        // a "$ref" member makes the value a Reference Object, which gets no
        // description injected even when it has none
        ObjectNode doc = Json31.mapper().createObjectNode();
        doc.put("openapi", "3.2.0");
        doc.putObject("paths").putObject("/x").putObject("get")
                .putObject("responses").putObject("default")
                .put("$ref", "#/components/responses/Err");
        Oas32CompatSpecLoader.rewriteForCompatibility(doc);
        Assert.assertFalse(
                doc.at("/paths/~1x/get/responses/default").has("description"),
                "a Reference Object must not gain a description");
    }

    @Test
    public void responsesObjectExtensionsAreNotRewrittenAsResponses() {
        // An Operation's "responses" member is a Responses Object (spec
        // 3.2.1), so "x-note" is a specification extension — opaque data
        // that must not be treated as a Response, must not gain the
        // injected "description": "" and must not carry the
        // x-oas32-description-absent marker. components.responses on the
        // other hand is a plain name -> Response map with no extension
        // slots, so a member named "x-component-response" is a real
        // response and still gets the empty description the released
        // parser's 3.1 schema requires.
        OpenAPI openAPI = contextFor("src/test/resources/3_2/oas32-resp-ext.yaml")
                .getSpecDocument();
        Assert.assertNotNull(openAPI);

        ApiResponses responses =
                openAPI.getPaths().get("/t").getGet().getResponses();
        Assert.assertEquals(responses.get("200").getDescription(), "ok");
        Assert.assertNull(responses.get("x-note"),
                "responses.x-note is an extension of the Responses Object, "
                        + "not a response member");
        Object xNote = responses.getExtensions() == null ? null
                : responses.getExtensions().get("x-note");
        Assert.assertEquals(xNote, Map.of("hello", "world"),
                "x-note must pass through untouched: no injected "
                        + "description, no compatibility marker");

        // components.responses is a plain map: an x- name there is a real
        // response the rewrite must still walk — checked at the JSON
        // level, since the released parser drops the member anyway
        ObjectNode doc = Json31.mapper().createObjectNode();
        doc.put("openapi", "3.2.0");
        doc.putObject("paths");
        doc.putObject("components").putObject("responses")
                .putObject("x-component-response")
                .putObject("content").putObject("text/plain")
                .putObject("schema").put("type", "string");
        Oas32CompatSpecLoader.rewriteForCompatibility(doc);
        Assert.assertEquals(
                doc.at("/components/responses/x-component-response/description")
                        .asText(), "",
                "components.responses members named x-... are real "
                        + "responses and still get the injected "
                        + "empty description");
    }

    @Test
    public void cookieStyleIsLoweredToFormKeepingTheOriginal() {
        // C2: style: "cookie" (3.2, in:"cookie" only) fails the released
        // parser's StyleEnum; the loader lowers it to "form" — the 3.1
        // default for cookie parameters — keeping the original value in
        // x-oas32-style.
        OpenAPI openAPI = contextFor("src/test/resources/3_2/oas32-cookie-style.yaml")
                .getSpecDocument();
        Assert.assertNotNull(openAPI);
        Parameter session = openAPI.getPaths().get("/c").getGet()
                .getParameters().get(0);
        Assert.assertEquals(session.getStyle(), Parameter.StyleEnum.FORM);
        Assert.assertEquals(session.getExtensions().get("x-oas32-style"), "cookie");
    }

    @Test
    public void absolutizeRefsLeavesPayloadAndExtensionRefsUntouched() {
        // C3: $ref-looking strings inside payload data are document
        // content, not references. The walk runs after the compatibility
        // rewrite, so moved example values already sit under
        // x-oas32-dataValue; link parameters/requestBody and user x-*
        // extensions are likewise opaque. Loader-created extensions that
        // hold structures keep their contents absolutized.
        ObjectNode doc = Json31.mapper().createObjectNode();
        URI base = URI.create("file:///specs/api/spec.yaml");
        ObjectNode components = doc.putObject("components");

        ObjectNode examples = components.putObject("examples");
        examples.putObject("ex")
                .putObject("x-oas32-dataValue").put("$ref", "./payload.yaml");
        examples.putObject("ex2")
                .putObject("dataValue").put("$ref", "./payload2.yaml");
        ObjectNode links = components.putObject("links");
        ObjectNode link = links.putObject("getThing");
        link.putObject("requestBody").put("$ref", "./body.yaml");
        link.putObject("parameters").putObject("p").put("$ref", "./p.yaml");
        links.putObject("byRef").put("$ref", "./links/L.yaml");
        doc.putObject("info").putObject("x-payload").put("$ref", "./ext.yaml");
        doc.putObject("x-oas32-itemSchema").put("$ref", "./schemas/S.yaml");

        Oas32CompatSpecLoader.absolutizeRefs(doc, base);

        Assert.assertEquals(
                doc.at("/components/examples/ex/x-oas32-dataValue/$ref").asText(),
                "./payload.yaml");
        Assert.assertEquals(
                doc.at("/components/examples/ex2/dataValue/$ref").asText(),
                "./payload2.yaml");
        Assert.assertEquals(
                doc.at("/components/links/getThing/requestBody/$ref").asText(),
                "./body.yaml");
        Assert.assertEquals(
                doc.at("/components/links/getThing/parameters/p/$ref").asText(),
                "./p.yaml");
        Assert.assertEquals(
                doc.at("/info/x-payload/$ref").asText(), "./ext.yaml");
        // positive controls: a Reference Object where a link is expected,
        // and a structural x-oas32-* extension, still absolutize
        Assert.assertEquals(doc.at("/components/links/byRef/$ref").asText(),
                "/specs/api/links/L.yaml");
        Assert.assertEquals(doc.at("/x-oas32-itemSchema/$ref").asText(),
                "/specs/api/schemas/S.yaml");
    }

    @Test
    public void refsInsideSchemaIdScopeResolveAgainstDocumentLocation() {
        // E1: the released parser does not use a schema $id as the ref
        // base for 3.1 documents — it tries the $id base and falls back
        // to the document location — so the loader resolves relative
        // $refs inside an $id scope against the document location like
        // any other ref, and warns once per load that the $id is unused.
        ObjectNode doc = Json31.mapper().createObjectNode();
        URI base = URI.create("file:///specs/api/spec.yaml");
        ObjectNode schemas = doc.putObject("components").putObject("schemas");
        ObjectNode scoped = schemas.putObject("Scoped");
        scoped.put("$id", "https://schemas.example.com/Scoped");
        scoped.putObject("properties").putObject("a").put("$ref", "./part.yaml");
        schemas.putObject("Plain").put("$ref", "./t.yaml");

        ListAppender<ILoggingEvent> appender = new ListAppender<>();
        appender.start();
        ch.qos.logback.classic.Logger logger = (ch.qos.logback.classic.Logger)
                LoggerFactory.getLogger(Oas32CompatSpecLoader.class);
        logger.addAppender(appender);
        try {
            Oas32CompatSpecLoader.absolutizeRefs(doc, base);
        } finally {
            logger.detachAppender(appender);
        }

        Assert.assertEquals(
                doc.at("/components/schemas/Scoped/properties/a/$ref").asText(),
                "/specs/api/part.yaml",
                "a relative $ref under a schema $id resolves against the "
                        + "document location, matching the 3.1 parser");
        Assert.assertEquals(doc.at("/components/schemas/Plain/$ref").asText(),
                "/specs/api/t.yaml",
                "a $ref outside the $id scope still absolutizes");
        long idWarnings = appender.list.stream()
                .filter(e -> e.getFormattedMessage().contains("$id"))
                .count();
        Assert.assertEquals(idWarnings, 1,
                "the unused $id base is warned about once per load");
    }

    @Test
    public void refsInsideSchemaIdScopeResolveFromTheSpecDirectory() {
        // End to end: the spec lives in 3_2/ and references ext-a.yaml
        // next to it; the test runner's working directory is the module
        // directory, so a ref left relative would resolve against the
        // wrong base (master's 3.1 read of the same document succeeds).
        String spec = Paths.get("src/test/resources/3_2/oas32-idscope.yaml")
                .toAbsolutePath().toString();
        OpenAPI openAPI = contextFor(spec).getSpecDocument();
        Assert.assertNotNull(openAPI);
        Schema schema = openAPI.getPaths().get("/t").getGet().getResponses().get("200")
                .getContent().get("application/json").getSchema();
        Assert.assertNotNull(schema);
        Schema withId = (Schema) schema.getProperties().get("withid");
        Assert.assertNotNull(withId, "the $id-scoped property must be present");
        Schema child = (Schema) withId.getProperties().get("child");
        Assert.assertNotNull(child,
                "the relative $ref inside the $id scope must resolve "
                        + "against the spec's directory, not the working "
                        + "directory");
        Assert.assertTrue(child.getProperties().containsKey("a"),
                "the child schema must resolve to ext-a.yaml's schema A");
    }

    @Test
    public void selfIsPreservedButNotUsedAsRefBase() {
        // E1: the root $self is the document's self-declared base URI, but
        // this loader never uses it — it is kept as x-oas32-$self and
        // relative $refs still resolve against the document location.
        ObjectNode doc = Json31.mapper().createObjectNode();
        doc.put("openapi", "3.2.0");
        doc.put("$self", "https://self.example.com/api/spec.yaml");
        doc.putObject("paths").putObject("/x").putObject("get")
                .putObject("responses").putObject("200").putObject("content")
                .putObject("application/json").putObject("schema")
                .put("$ref", "./schemas/T.yaml");

        ListAppender<ILoggingEvent> appender = new ListAppender<>();
        appender.start();
        ch.qos.logback.classic.Logger logger = (ch.qos.logback.classic.Logger)
                LoggerFactory.getLogger(Oas32CompatSpecLoader.class);
        logger.addAppender(appender);
        try {
            Oas32CompatSpecLoader.rewriteForCompatibility(doc);
            Oas32CompatSpecLoader.absolutizeRefs(doc,
                    URI.create("file:///specs/api/spec.yaml"));
        } finally {
            logger.detachAppender(appender);
        }

        Assert.assertEquals(doc.get("x-oas32-$self").asText(),
                "https://self.example.com/api/spec.yaml");
        Assert.assertEquals(
                doc.at("/paths/~1x/get/responses/200/content/application~1json/schema/$ref")
                        .asText(),
                "/specs/api/schemas/T.yaml",
                "the $self host must not become the ref base");
        long selfWarnings = appender.list.stream()
                .filter(e -> e.getFormattedMessage().contains("$self"))
                .count();
        Assert.assertEquals(selfWarnings, 1,
                "the unused $self is warned about once");
    }

    @Test
    public void refsIntoMovedMembersWarnOnceEach() {
        // E2: a JSON Pointer naming a member the rewrite moved to an
        // x-oas32-* extension can no longer resolve; each distinct such ref
        // is warned about once. Pointers that still resolve — including
        // ones through members merely sharing a moved name — are quiet.
        ObjectNode doc = Json31.mapper().createObjectNode();
        doc.put("openapi", "3.2.0");
        ObjectNode components = doc.putObject("components");
        components.putObject("mediaTypes").putObject("application/mt");
        ObjectNode paths = doc.putObject("paths");
        paths.putObject("/x").putObject("query")
                .putObject("responses").putObject("200").putObject("content")
                .putObject("application/json").putObject("schema")
                .put("$ref", "#/components/schemas/T");
        ObjectNode yResponses = paths.putObject("/y").putObject("get")
                .putObject("responses");
        yResponses.putObject("200").putObject("content")
                .putObject("application/json").putObject("schema")
                .put("$ref", "#/components/mediaTypes/application~1mt");
        yResponses.putObject("404").putObject("content")
                .putObject("application/json").putObject("schema")
                .put("$ref", "#/paths/~1x/query");
        components.putObject("schemas").putObject("T")
                .putObject("properties").putObject("query").put("type", "string");
        paths.putObject("/z").putObject("get").putObject("responses")
                .putObject("200").putObject("content")
                .putObject("application/json").putObject("schema")
                .put("$ref", "#/components/schemas/T/properties/query");

        Oas32CompatSpecLoader.rewriteForCompatibility(doc);
        ListAppender<ILoggingEvent> appender = new ListAppender<>();
        appender.start();
        ch.qos.logback.classic.Logger logger = (ch.qos.logback.classic.Logger)
                LoggerFactory.getLogger(Oas32CompatSpecLoader.class);
        logger.addAppender(appender);
        try {
            Oas32CompatSpecLoader.absolutizeRefs(doc,
                    URI.create("file:///specs/api/spec.yaml"));
        } finally {
            logger.detachAppender(appender);
        }

        long movedWarnings = appender.list.stream()
                .filter(e -> e.getFormattedMessage()
                        .contains("can no longer be resolved"))
                .count();
        Assert.assertEquals(movedWarnings, 2,
                "one warning per ref into moved members "
                        + "(mediaTypes + the moved query operation)");
    }

    @Test
    public void oas30And31DocumentsUnaffected() {
        // 3.0 and 3.1 documents do not gain the compat extensions
        for (String spec : new String[]{
                "src/test/resources/3_0/petstore.yaml",
                "src/test/resources/3_1/petstore.yaml"}) {
            OpenAPI openAPI = contextFor(spec).getSpecDocument();
            Assert.assertNotNull(openAPI, spec);
            Assert.assertTrue(openAPI.getExtensions() == null
                    || !openAPI.getExtensions().containsKey(Oas32Extensions.OPENAPI_VERSION_EXT), spec);
            for (PathItem pathItem : openAPI.getPaths().values()) {
                Assert.assertTrue(Oas32Extensions.operations(openAPI, pathItem).isEmpty(), spec);
            }
        }
    }

    @Test
    public void sameNamedExtensionsOnNonCompatDocumentsPassThroughUntouched() throws Exception {
        // A2: extensions happen to share the x-oas32-* names; without the
        // loader's root marker they are passed through verbatim instead of
        // being interpreted (x-oas32-query: memo previously crashed
        // generation trying to read it as an Operation).
        OpenAPI openAPI = contextFor("src/test/resources/3_1/x-oas32-extensions.yaml")
                .getSpecDocument();
        Assert.assertNotNull(openAPI);

        PathItem things = openAPI.getPaths().get("/things");
        Assert.assertNotNull(things);
        Assert.assertEquals(
                things.getExtensions().get(Oas32Extensions.QUERY_OPERATION_EXT), "memo");
        Assert.assertTrue(Oas32Extensions.operations(openAPI, things).isEmpty());

        Map<String, List<CodegenOperation>> ops = processPaths(openAPI);
        Assert.assertNotNull(findOperation(ops, "/things", "GET"));
        Assert.assertNull(findOperation(ops, "/things", "QUERY"));
        Assert.assertNull(findOperation(ops, "/things", "PURGE"));
    }

    @Test
    public void compatLoaderMarksTheInstanceNotTheDocument() {
        // The compatibility mark lives on the OpenAPI instance the loader
        // returned, never inside the document: an extension marker would
        // serialize into generated output (differing per JVM) and any
        // authored document could declare its name.
        OpenAPI openAPI = contextFor(SPEC).getSpecDocument();
        Assert.assertNotNull(openAPI);
        Map<String, Object> rootExtensions = openAPI.getExtensions();
        Assert.assertNotNull(rootExtensions,
                "x-oas32-openapi keeps the original version readable");
        Assert.assertNull(rootExtensions.get("x-oas32-loader"),
                "no document member may carry the compatibility mark");

        // the mark still gates the restored operations on this instance
        PathItem things = openAPI.getPaths().get("/things");
        Assert.assertFalse(Oas32Extensions.operations(openAPI, things).isEmpty(),
                "the loader-returned instance exposes restored operations");

        // a different instance carrying the same PathItem is not marked —
        // identity, not document content, is what is remembered
        OpenAPI lookalike = new OpenAPI();
        lookalike.setPaths(openAPI.getPaths());
        Assert.assertTrue(Oas32Extensions.operations(lookalike, things).isEmpty(),
                "the mark is instance identity, not document content");
    }

    @Test
    public void restoredOperationsAreParsedLikeFixedOperations() {
        // B2/B3: the moved operations are staged under synthetic paths so the
        // single parser read applies parameter defaults and $ref resolution
        // to them exactly as it does for fixed-method operations.
        OpenAPI openAPI = contextFor("src/test/resources/3_2/oas32-cb-query.yaml")
                .getSpecDocument();
        Assert.assertNotNull(openAPI);

        // the synthetic staging never leaks into the parsed document
        for (String path : openAPI.getPaths().keySet()) {
            Assert.assertFalse(path.startsWith("/__oas32__"),
                    "synthetic path must be removed: " + path);
        }

        PathItem things = openAPI.getPaths().get("/things");
        Operation get = things.getGet();
        Operation query = Oas32Extensions.operations(openAPI, things).get("QUERY");
        Operation purge = Oas32Extensions.operations(openAPI, things).get("PURGE");
        Assert.assertNotNull(query, "restored QUERY must be a parsed Operation");
        Assert.assertNotNull(purge, "restored PURGE must be a parsed Operation");
        Assert.assertTrue(query.getExtensions() == null
                        || !query.getExtensions().containsKey("x-oas32-op-id"),
                "the staging marker must not leak into the restored operation");

        // same parameter, same parser defaults: 'filter' (defaults) and 'ids'
        // (explicit form+explode:false) serialize identically on GET and QUERY
        Parameter getFilter = get.getParameters().get(0);
        Parameter queryFilter = query.getParameters().get(0);
        Assert.assertEquals(queryFilter.getStyle(), getFilter.getStyle());
        Assert.assertEquals(queryFilter.getExplode(), getFilter.getExplode());
        Assert.assertEquals(queryFilter.getRequired(), getFilter.getRequired());
        Parameter getIds = get.getParameters().get(1);
        Parameter queryIds = query.getParameters().get(1);
        Assert.assertEquals(queryIds.getStyle(), getIds.getStyle());
        Assert.assertEquals(queryIds.getExplode(), getIds.getExplode());

        // a $ref'd request body inside additionalOperations resolves to the
        // same component the parser resolves for fixed operations
        RequestBody purgeBody = ModelUtils.getReferencedRequestBody(openAPI, purge.getRequestBody());
        Assert.assertNotNull(purgeBody);
        Assert.assertTrue(purgeBody.getContent().containsKey("application/json"));

        // the external $ref inside the QUERY operation resolves like the GET
        // operation's reference to the same schema (inlined with resolve=true)
        Schema getExtSchema = openAPI.getPaths().get("/ext").getGet().getResponses().get("200")
                .getContent().get("application/json").getSchema();
        Assert.assertTrue(getExtSchema.getProperties().containsKey("id"),
                "sanity check: the external schema resolves on fixed operations");
        Schema queryExtSchema = Oas32Extensions
                .operations(openAPI, openAPI.getPaths().get("/ext")).get("QUERY")
                .getResponses().get("200").getContent().get("application/json").getSchema();
        Assert.assertTrue(queryExtSchema.getProperties().containsKey("id"),
                "external $ref inside a restored operation must resolve");
        Assert.assertNull(queryExtSchema.get$ref(),
                "the resolved schema is inlined, not left as a file path $ref");
    }

    @Test
    public void queryParametersSerializeLikeGetParameters() throws Exception {
        // B2 end to end: the codegen parameter derived from the restored
        // QUERY operation carries the same style/explode serialization as the
        // identical GET parameter.
        OpenAPI openAPI = contextFor("src/test/resources/3_2/oas32-cb-query.yaml")
                .getSpecDocument();
        Assert.assertNotNull(openAPI);
        Map<String, List<CodegenOperation>> ops = processPaths(openAPI);

        CodegenOperation getOp = findOperation(ops, "/things", "GET");
        CodegenOperation queryOp = findOperation(ops, "/things", "QUERY");
        Assert.assertNotNull(getOp);
        Assert.assertNotNull(queryOp);
        for (String name : new String[]{"filter", "ids"}) {
            CodegenParameter getParam = parameterNamed(getOp, name);
            CodegenParameter queryParam = parameterNamed(queryOp, name);
            Assert.assertNotNull(getParam, name);
            Assert.assertNotNull(queryParam, name);
            Assert.assertEquals(queryParam.collectionFormat, getParam.collectionFormat,
                    name + " must serialize the same on QUERY as on GET");
            Assert.assertEquals(queryParam.isCollectionFormatMulti, getParam.isCollectionFormatMulti, name);
        }
    }

    @Test
    public void javaPreprocessStampsContentTypeExtensionsOnRestoredOperations() {
        // The Java preprocess stamps x-content-type / x-accepts on every
        // operation (Feign's @Headers reads them); the stamp loop must
        // reach the operations the compat loader restored — a plain
        // readOperations() traversal leaves QUERY unstamped.
        OpenAPI openAPI = contextFor("src/test/resources/3_2/oas32-cb-query.yaml")
                .getSpecDocument();
        JavaClientCodegen codegen = new JavaClientCodegen();
        codegen.setOpenAPI(openAPI);
        codegen.preprocessOpenAPI(openAPI);

        Operation query = Oas32Extensions
                .operations(openAPI, openAPI.getPaths().get("/things")).get("QUERY");
        Assert.assertNotNull(query);
        Assert.assertEquals(query.getExtensions().get("x-content-type"),
                "application/json");
        Assert.assertEquals((String[]) query.getExtensions().get("x-accepts"),
                new String[]{"application/json"});

        // a document that never went through the loader is stamped exactly
        // as before (allOperationsList degrades to readOperations())
        OpenAPI petstore = contextFor("src/test/resources/3_0/petstore.yaml")
                .getSpecDocument();
        JavaClientCodegen petCodegen = new JavaClientCodegen();
        petCodegen.setOpenAPI(petstore);
        petCodegen.preprocessOpenAPI(petstore);
        Assert.assertNotNull(
                petstore.getPaths().get("/pet").getPost().getExtensions()
                        .get("x-content-type"),
                "fixed operations on a non-compat document are stamped as before");
    }

    @Test
    public void callbackQueryOperationReachesFromCallback() {
        // B4: a query operation inside a callback path item is restored and
        // reaches fromCallback instead of being dropped.
        OpenAPI openAPI = contextFor("src/test/resources/3_2/oas32-cb-query.yaml")
                .getSpecDocument();
        Assert.assertNotNull(openAPI);

        Callback onEvent = openAPI.getPaths().get("/subscribe").getPost()
                .getCallbacks().get("onEvent");
        PathItem callbackPathItem = onEvent.values().iterator().next();
        Operation restored = Oas32Extensions.operations(openAPI, callbackPathItem).get("QUERY");
        Assert.assertNotNull(restored, "callback path item's QUERY must be a parsed Operation");
        Assert.assertEquals(restored.getOperationId(), "onEventQuery");

        DefaultCodegen config = new DefaultCodegen();
        config.setOpenAPI(openAPI);
        CodegenCallback codegenCallback = config.fromCallback("onEvent", onEvent, null);
        Assert.assertFalse(codegenCallback.urls.isEmpty());
        Assert.assertTrue(codegenCallback.urls.get(0).requests.stream()
                        .anyMatch(r -> "QUERY".equals(r.httpMethod)
                                && "onEventQuery".equals(r.operationId)),
                "the callback's query operation must reach fromCallback");
    }

    @Test
    public void fileUrlInputSpecReadsThroughOpenStream() {
        // A file: URI must not reach RemoteUrl.urlToString: it casts the
        // connection to HttpURLConnection, which FileURLConnection is not.
        // Non-HTTP schemes take the URL.openStream() read instead.
        String fileUrl = Paths.get("src/test/resources/3_2/selfname.yaml")
                .toAbsolutePath().toUri().toString();
        Assert.assertTrue(fileUrl.startsWith("file:"), fileUrl);
        OpenAPI openAPI = contextFor(fileUrl).getSpecDocument();
        Assert.assertNotNull(openAPI, "file: URL input must load");
        Assert.assertEquals(
                openAPI.getExtensions().get(Oas32Extensions.OPENAPI_VERSION_EXT), "3.2.0");
    }

    @Test
    public void extensionMembersOfPathMapsAreNotStagedAsOperations() {
        // x- members of the paths map and of a callback's expression ->
        // Path Item map are specification extensions: their "query"
        // payloads are opaque data, never operations to stage. webhooks
        // is different — a plain name -> Path Item map with no extension
        // slots (spec 3.2.1) — so "x-meta" is a real webhook name whose
        // query member is staged and restored like ping's.
        OpenAPI openAPI = SpecLoader
                .forInputSpec("src/test/resources/3_2/oas32-ext-keys.yaml")
                .load("src/test/resources/3_2/oas32-ext-keys.yaml", null,
                        new ParseOptions())
                .getOpenAPI();
        Assert.assertNotNull(openAPI);

        // every parsed PathItem must be free of the staging marker
        assertNoStagingMarker(openAPI, openAPI.getPaths());
        assertNoStagingMarker(openAPI, openAPI.getWebhooks());

        // real moved operations are still restored — including the
        // operations of the webhook literally named "x-meta"
        PathItem t = openAPI.getPaths().get("/t");
        Assert.assertNotNull(
                Oas32Extensions.operations(openAPI, t).get("QUERY"));
        PathItem ping = openAPI.getWebhooks().get("ping");
        Assert.assertNotNull(
                Oas32Extensions.operations(openAPI, ping).get("QUERY"));
        PathItem xMeta = openAPI.getWebhooks().get("x-meta");
        Assert.assertNotNull(xMeta,
                "the webhooks map has no extension slots: 'x-meta' is a webhook name");
        Operation metaQuery = Oas32Extensions.operations(openAPI, xMeta).get("QUERY");
        Assert.assertNotNull(metaQuery,
                "the query operation of webhook 'x-meta' must be restored");
        Assert.assertEquals(metaQuery.getOperationId(), "metaQuery");

        // the callback's x- member is untouched while its real expression
        // path item's query operation is restored
        Operation queryT = Oas32Extensions.operations(openAPI, t).get("QUERY");
        Callback onEvent = queryT == null || queryT.getCallbacks() == null ? null
                : queryT.getCallbacks().get("onEvent");
        Assert.assertNotNull(onEvent);
        PathItem expressionItem = onEvent.get("{$request.body#/callbackUrl}");
        Assert.assertNotNull(
                Oas32Extensions.operations(openAPI, expressionItem).get("QUERY"));
    }

    private void assertNoStagingMarker(OpenAPI openAPI, Map<String, PathItem> pathItems) {
        for (PathItem pathItem : pathItems.values()) {
            for (Operation operation : Oas32Extensions.allOperationsList(openAPI, pathItem)) {
                Assert.assertTrue(operation.getExtensions() == null
                                || !operation.getExtensions().containsKey("x-oas32-op-id"),
                        "staging marker must not leak");
            }
        }
    }

    @Test
    public void stringValuedMapsAreNotAbsolutized() {
        // Members literally named "$ref" inside name -> string maps are
        // data (a scope name, a schema name), never references.
        ObjectNode doc = Json31.mapper().createObjectNode();
        URI base = URI.create("file:///specs/api/spec.yaml");
        ObjectNode components = doc.putObject("components");
        components.putObject("securitySchemes").putObject("oauth")
                .putObject("flows").putObject("implicit")
                .putObject("scopes").put("$ref", "permission");
        components.putObject("schemas").putObject("Pet")
                .putObject("discriminator").putObject("mapping").put("$ref", "Dog");

        Oas32CompatSpecLoader.absolutizeRefs(doc, base);

        Assert.assertEquals(doc.at("/components/securitySchemes/oauth/flows"
                + "/implicit/scopes/$ref").asText(), "permission");
        Assert.assertEquals(doc.at("/components/schemas/Pet/discriminator"
                + "/mapping/$ref").asText(), "Dog");
    }

    @Test
    public void refsResolvingToTheInputDocumentCollapseToFragments() {
        // "selfname.yaml#/components/schemas/X" inside selfname.yaml names
        // the input itself: it collapses to a same-document ref, so it is
        // served from the rewritten tree and needs no external re-read.
        ObjectNode doc = Json31.mapper().createObjectNode();
        URI base = URI.create("file:///specs/api/selfname.yaml");
        doc.putObject("paths").putObject("/x").putObject("get")
                .putObject("responses").putObject("200").putObject("content")
                .putObject("application/json").putObject("schema")
                .put("$ref", "selfname.yaml#/components/schemas/X");
        Oas32CompatSpecLoader.absolutizeRefs(doc, base);
        Assert.assertEquals(
                doc.at("/paths/~1x/get/responses/200/content/application~1json/schema/$ref")
                        .asText(),
                "#/components/schemas/X");

        // end to end: the ref stays a same-document pointer (the parser
        // resolves internal refs lazily, so $ref is kept) instead of an
        // external path that would re-read the unrewritten document
        String spec = Paths.get("src/test/resources/3_2/selfname.yaml")
                .toAbsolutePath().toString();
        OpenAPI openAPI = contextFor(spec).getSpecDocument();
        Assert.assertNotNull(openAPI);
        Schema schema = openAPI.getPaths().get("/t").getGet().getResponses().get("200")
                .getContent().get("application/json").getSchema();
        Assert.assertEquals(schema.get$ref(), "#/components/schemas/WithXml",
                "self-named $ref must collapse to a same-document fragment");
        Schema withXml = openAPI.getComponents().getSchemas().get("WithXml");
        Assert.assertTrue(withXml.getProperties().containsKey("id"));
        Assert.assertEquals(withXml.getXml().getExtensions().get("x-oas32-nodeType"),
                "element");
    }

    @Test
    public void parserMessagesNameOriginalLocations() {
        // Validation messages that report the operation staged under a
        // synthetic path must name the member it was moved from.
        String spec = Paths.get("src/test/resources/3_2/badquery.yaml")
                .toAbsolutePath().toString();
        SwaggerParseResult result = SpecLoader.forInputSpec(spec)
                .load(spec, null, new ParseOptions());
        Assert.assertNotNull(result.getOpenAPI());
        Assert.assertNotNull(result.getMessages());
        for (String message : result.getMessages()) {
            Assert.assertFalse(message != null && message.contains("__oas32__"),
                    "synthetic path leaked into a parser message: " + message);
        }
        Assert.assertTrue(result.getMessages().stream().anyMatch(
                        message -> message != null && message.contains("paths.'/t'(query)")),
                "a message must name the query operation's original location; got: "
                        + result.getMessages());
    }

    @Test
    public void logsInfoThatSwagger2ConversionErrorsAreExpected() {
        // A successful 3.2 read through the compatibility loader logs one
        // INFO line explaining the ERROR stacks the parser's Swagger 2
        // converter fallback printed for the same document.
        String spec = Paths.get("src/test/resources/3_2/oas32-cb-query.yaml")
                .toAbsolutePath().toString();
        ListAppender<ILoggingEvent> appender = new ListAppender<>();
        appender.start();
        ch.qos.logback.classic.Logger logger = (ch.qos.logback.classic.Logger)
                LoggerFactory.getLogger(DefaultSpecLoader.class);
        logger.addAppender(appender);
        try {
            SwaggerParseResult result = SpecLoader.forInputSpec(spec)
                    .load(spec, null, new ParseOptions());
            Assert.assertNotNull(result.getOpenAPI());
        } finally {
            logger.detachAppender(appender);
        }
        Assert.assertTrue(appender.list.stream().anyMatch(e -> e.getFormattedMessage()
                        .contains("Swagger 2 converter")),
                "expected the INFO line about the Swagger 2 converter errors; got: "
                        + appender.list);
    }

    @Test
    public void realSyntheticPrefixedPathSurvivesStaging() {
        // A document path genuinely named "/__oas32__/0" is not a staging
        // target: staging picks free names, and grafting removes exactly
        // the paths it created. (The fixture exercises payload $refs the
        // 3.1 schema cannot express, so the parse — not validateSpec —
        // carries it.)
        String spec = Paths.get("src/test/resources/3_2/dataref.yaml")
                .toAbsolutePath().toString();
        OpenAPI openAPI = SpecLoader.forInputSpec(spec)
                .load(spec, null, new ParseOptions()).getOpenAPI();
        Assert.assertNotNull(openAPI);
        PathItem real = openAPI.getPaths().get("/__oas32__/0");
        Assert.assertNotNull(real, "the document's own /__oas32__/0 must survive");
        Assert.assertEquals(real.getGet().getOperationId(), "realSynthetic");
        Assert.assertEquals(
                Oas32Extensions.operations(openAPI, openAPI.getPaths().get("/t"))
                        .get("QUERY").getOperationId(),
                "queryT");
    }

    @Test
    public void documentWithoutPathsRestoresWebhookOperations() {
        // No "paths" member at all: staging still mounts the moved webhook
        // operations and the (loader-created) paths map is removed again.
        OpenAPI openAPI = contextFor("src/test/resources/3_2/webhooks-only.yaml")
                .getSpecDocument();
        Assert.assertNotNull(openAPI);
        Assert.assertTrue(openAPI.getPaths() == null || openAPI.getPaths().isEmpty(),
                "no paths may remain: " + openAPI.getPaths());
        Assert.assertNotNull(openAPI.getWebhooks());
        Operation query = Oas32Extensions
                .operations(openAPI, openAPI.getWebhooks().get("ping")).get("QUERY");
        Assert.assertNotNull(query, "the webhook query operation must be restored");
        Assert.assertEquals(query.getOperationId(), "pingQuery");
    }

    private CodegenParameter parameterNamed(CodegenOperation operation, String name) {
        return operation.allParams.stream()
                .filter(p -> name.equals(p.baseName))
                .findFirst()
                .orElse(null);
    }

    private Map<String, List<CodegenOperation>> processPaths(OpenAPI openAPI) throws Exception {
        ClientOptInput opts = new ClientOptInput();
        opts.openAPI(openAPI);
        DefaultCodegen config = new DefaultCodegen();
        config.setOpenAPI(openAPI);
        opts.config(config);
        DefaultGenerator generator = new DefaultGenerator();
        generator.opts(opts);
        return generator.processPaths(openAPI.getPaths());
    }

    private Map<String, List<CodegenOperation>> processWebhooks(OpenAPI openAPI) throws Exception {
        ClientOptInput opts = new ClientOptInput();
        opts.openAPI(openAPI);
        DefaultCodegen config = new DefaultCodegen();
        config.setOpenAPI(openAPI);
        opts.config(config);
        DefaultGenerator generator = new DefaultGenerator();
        generator.opts(opts);
        return generator.processWebhooks(openAPI.getWebhooks());
    }

    private CodegenOperation findOperation(Map<String, List<CodegenOperation>> ops,
                                           String path, String httpMethod) {
        return ops.values().stream()
                .flatMap(List::stream)
                .filter(op -> path.equals(op.path) && httpMethod.equals(op.httpMethod))
                .findFirst()
                .orElse(null);
    }
}
