package org.openapitools.codegen.utils;

import io.swagger.v3.core.util.Json31;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.PathItem;
import io.swagger.v3.oas.models.parameters.Parameter;

import java.lang.ref.Reference;
import java.lang.ref.ReferenceQueue;
import java.lang.ref.WeakReference;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Accessor for the OpenAPI 3.2 members that
 * {@link org.openapitools.codegen.config.Oas32CompatSpecLoader} moves to
 * {@code x-oas32-*} specification extensions while reading a 3.2 document with
 * the released (3.1) swagger-parser. When swagger-core/swagger-parser ship
 * native 3.2 support, this accessor and the rewrite are the two places that
 * change (issue #25477).
 *
 * <p>Every read is gated on the document being an instance the
 * compatibility loader returned this JVM (see
 * {@link #markCompatDocument(OpenAPI)}): only those documents have their
 * {@code x-oas32-*} members interpreted. The mark is kept on the instance
 * — by identity, in a weak set — rather than in the document: a marker
 * extension would appear in generated output and differ per JVM, and any
 * marker a document could carry is forgeable — a plain 3.0/3.1 document
 * may legitimately use the same extension names (e.g.
 * {@code x-oas32-query: memo}), and its extensions must be passed through
 * untouched.
 */
public final class Oas32Extensions {

    /** Root extension keeping the document's original {@code openapi} value. */
    public static final String OPENAPI_VERSION_EXT = "x-oas32-openapi";
    /** Path-item extension holding the 3.2 {@code query} operation. */
    public static final String QUERY_OPERATION_EXT = "x-oas32-query";
    /** Path-item extension holding the 3.2 {@code additionalOperations} map. */
    public static final String ADDITIONAL_OPERATIONS_EXT = "x-oas32-additionalOperations";
    /** Parameter extension keeping the original {@code in} value. */
    public static final String PARAMETER_IN_EXT = "x-oas32-in";
    /**
     * Marker placed on a {@link org.openapitools.codegen.CodegenParameter}'s
     * vendor extensions when the source parameter was {@code in: "querystring"}.
     */
    public static final String QUERYSTRING_PARAMETER_EXT = "x-oas32-querystring";

    /**
     * The {@link OpenAPI} instances {@link #markCompatDocument} registered,
     * compared by identity and held weakly so a collected document releases
     * its slot. A plain {@link java.util.WeakHashMap} would compare keys by
     * {@code equals} (which the model implements), and an
     * {@link java.util.IdentityHashMap} would grow without bound.
     */
    private static final ReferenceQueue<OpenAPI> COMPAT_DOCS_QUEUE = new ReferenceQueue<>();
    private static final Set<CompatRef> COMPAT_DOCS =
            Collections.synchronizedSet(new HashSet<>());

    private Oas32Extensions() {
    }

    /**
     * A weak reference that compares by referent identity: two distinct
     * {@link OpenAPI} instances that happen to be {@code equals} are still
     * two different members of {@link #COMPAT_DOCS}, and a collected
     * document releases its slot through {@link #COMPAT_DOCS_QUEUE}.
     */
    private static final class CompatRef extends WeakReference<OpenAPI> {
        private final int hash;

        private CompatRef(OpenAPI referent) {
            super(referent, COMPAT_DOCS_QUEUE);
            hash = System.identityHashCode(referent);
        }

        @Override
        public int hashCode() {
            return hash;
        }

        @Override
        public boolean equals(Object other) {
            return other instanceof CompatRef
                    && get() != null && get() == ((CompatRef) other).get();
        }
    }

    private static void expungeCompatDocs() {
        for (Reference<? extends OpenAPI> ref = COMPAT_DOCS_QUEUE.poll();
                ref != null; ref = COMPAT_DOCS_QUEUE.poll()) {
            COMPAT_DOCS.remove(ref);
        }
    }

    /**
     * Registers {@code openAPI} as a document the 3.2 compatibility loader
     * produced: every {@code x-oas32-*} read in this class is gated on that
     * mark. Called by {@code Oas32CompatSpecLoader} once it has the parsed
     * instance; the mark lives on the instance, never in the document, so
     * nothing can leak into serialized output or be declared by an
     * authored file.
     */
    public static void markCompatDocument(OpenAPI openAPI) {
        if (openAPI != null) {
            expungeCompatDocs();
            COMPAT_DOCS.add(new CompatRef(openAPI));
        }
    }

    /**
     * Returns the operations a 3.2 path item carries in its
     * {@code x-oas32-query} / {@code x-oas32-additionalOperations} extensions,
     * keyed by HTTP method name ({@code QUERY} for the former, the map key
     * verbatim for the latter). Empty unless {@code openAPI} is a document
     * read through the 3.2 compatibility loader (see class javadoc).
     */
    public static Map<String, Operation> operations(OpenAPI openAPI, PathItem pathItem) {
        Map<String, Operation> operations = new LinkedHashMap<>();
        if (!isCompatDocument(openAPI)) {
            return operations;
        }
        Map<String, Object> extensions = pathItem.getExtensions();
        if (extensions == null) {
            return operations;
        }
        Operation query = toOperation(extensions.get(QUERY_OPERATION_EXT));
        if (query != null) {
            operations.put("QUERY", query);
        }
        Object additional = extensions.get(ADDITIONAL_OPERATIONS_EXT);
        if (additional instanceof Map) {
            ((Map<?, ?>) additional).forEach((method, operation) -> {
                Operation parsed = toOperation(operation);
                // putIfAbsent: a contradictory additionalOperations entry
                // keyed "QUERY" must not replace the real query operation
                if (method instanceof String && parsed != null) {
                    operations.putIfAbsent((String) method, parsed);
                }
            });
        }
        return operations;
    }

    /**
     * Returns every operation a path item exposes, keyed by method name:
     * the fixed 3.1 HTTP methods first (in {@link PathItem#readOperationsMap()}
     * order, keyed by the enum name such as {@code "GET"}), then the 3.2
     * operations restored from the {@code x-oas32-*} extensions
     * ({@code "QUERY"}, then the {@code additionalOperations} keys verbatim).
     * For a document that never went through the compatibility loader the
     * result is exactly the {@code readOperationsMap()} enumeration under
     * string keys — nothing is added or reordered.
     */
    public static Map<String, Operation> allOperations(OpenAPI openAPI, PathItem pathItem) {
        Map<String, Operation> operations = new LinkedHashMap<>();
        if (pathItem == null) {
            return operations;
        }
        Map<PathItem.HttpMethod, Operation> fixed = pathItem.readOperationsMap();
        if (fixed != null) {
            fixed.forEach((method, operation) -> operations.put(method.name(), operation));
        }
        // putIfAbsent: the fixed methods win over a contradictory
        // additionalOperations key like "GET"
        operations(openAPI, pathItem).forEach(operations::putIfAbsent);
        return operations;
    }

    /**
     * Returns every operation a path item exposes as a list: the fixed 3.1
     * HTTP methods first in {@link PathItem#readOperations()} order, then
     * the 3.2 operations restored from the {@code x-oas32-*} extensions
     * ({@code QUERY}, then the {@code additionalOperations} values in
     * declaration order). For a document that never went through the
     * compatibility loader the result is the {@code readOperations()}
     * enumeration — nothing is added or reordered. Use this variant wherever
     * the replaced code iterated {@code readOperations()}, whose order
     * differs from {@link PathItem#readOperationsMap}.
     */
    public static List<Operation> allOperationsList(OpenAPI openAPI, PathItem pathItem) {
        List<Operation> operations = new ArrayList<>();
        if (pathItem == null) {
            return operations;
        }
        List<Operation> fixed = pathItem.readOperations();
        if (fixed != null) {
            operations.addAll(fixed);
        }
        operations.addAll(operations(openAPI, pathItem).values());
        return operations;
    }

    /**
     * Whether the parameter was declared {@code in: "querystring"} in the
     * source document (the loader keeps the original value in
     * {@link #PARAMETER_IN_EXT} after lowering {@code in} to {@code query}).
     * False unless {@code openAPI} is a document read through the 3.2
     * compatibility loader.
     */
    public static boolean isQueryStringParam(OpenAPI openAPI, Parameter parameter) {
        if (!isCompatDocument(openAPI)) {
            return false;
        }
        Map<String, Object> extensions = parameter.getExtensions();
        return extensions != null && "querystring".equals(extensions.get(PARAMETER_IN_EXT));
    }

    /**
     * Whether {@code openAPI} is an instance the 3.2 compatibility loader
     * returned (see {@link #markCompatDocument}). Checking instance
     * identity rather than a marker inside the document is what a plain
     * 3.0/3.1 document cannot forge — it may carry
     * {@code x-oas32-query: memo} but it is never the instance the loader
     * produced. ({@link #OPENAPI_VERSION_EXT} stays on the document as the
     * readable record of the original version, but gates nothing.)
     */
    private static boolean isCompatDocument(OpenAPI openAPI) {
        if (openAPI == null) {
            return false;
        }
        expungeCompatDocs();
        return COMPAT_DOCS.contains(new CompatRef(openAPI));
    }

    private static Operation toOperation(Object value) {
        if (value == null) {
            return null;
        }
        if (value instanceof Operation) {
            return (Operation) value;
        }
        return Json31.mapper().convertValue(value, Operation.class);
    }
}
