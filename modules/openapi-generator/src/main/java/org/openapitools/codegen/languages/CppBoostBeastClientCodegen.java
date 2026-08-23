package org.openapitools.codegen.languages;

import com.google.common.collect.ImmutableMap;

import com.samskivert.mustache.Mustache.Lambda;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.PathItem;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.media.MediaType;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.parameters.Parameter;
import io.swagger.v3.oas.models.responses.ApiResponse;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.text.StringEscapeUtils;
import org.openapitools.codegen.*;
import org.openapitools.codegen.languages.Oas31CompositionLowering.AllOfIntersection;
import org.openapitools.codegen.languages.Oas31CompositionLowering.CompositionBranchDescriptor;
import org.openapitools.codegen.languages.Oas31CompositionLowering.CompositionDescriptor;
import org.openapitools.codegen.languages.Oas31CompositionLowering.DiscriminatorDescriptor;

import java.io.File;
import java.util.*;
import java.util.Map;
import java.util.HashMap;
import java.util.stream.Collectors;

import org.openapitools.codegen.meta.features.*;
import org.openapitools.codegen.model.ModelMap;
import org.openapitools.codegen.model.ModelsMap;
import org.openapitools.codegen.model.OperationsMap;
import org.openapitools.codegen.utils.ModelUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import static org.openapitools.codegen.utils.StringUtils.camelize;

public class CppBoostBeastClientCodegen extends AbstractCppCodegen {

    public static final String DEFAULT_PACKAGE_NAME = "CppBoostBeastOpenAPIClient";

    /** Policy for format metadata in composition branch matching.
     *  Formats remain annotations and never affect branch match counts. */
    private String formatAssertionPolicy = "annotation";

    /** Value type for the formatAssertion option. */
    private static final String FORMAT_ASSERTION_POLICY_ANNOTATION = "annotation";

    /** SSE schema interpretation mode. */
    private String sseSchemaMode = "representation";
    private static final String SSE_SCHEMA_MODE_REPRESENTATION = "representation";
    private static final String SSE_SCHEMA_MODE_JSON_EVENT_DATA = "jsonEventData";
    /** Controls composition-branch validation during model decoding. */
    private boolean validateOnDecode = true;
    /** Compatibility mode for server responses that send undeclared nulls. */
    private boolean tolerateNonNullableNulls = true;

    private static final String X_CODEGEN_IS_RAW_BODY = "x-codegen-is-raw-body";
    private static final String X_CODEGEN_IS_OPTIONAL_QUERY_PARAMETER =
            "x-codegen-is-optional-query-parameter";
    // Authoritative parameter serialization facts stamped by codegenParameterStyled().
    private static final String X_CODEGEN_PARAM_STYLE = "x-codegen-param-style";
    private static final String X_CODEGEN_PARAM_EXPLODE = "x-codegen-param-explode";
    private static final String X_CODEGEN_PARAM_ALLOW_RESERVED =
            "x-codegen-param-allow-reserved";
    private static final String X_CODEGEN_PARAM_ALLOW_EMPTY_VALUE =
            "x-codegen-param-allow-empty-value";
    private static final String X_CPP_TOLERATE_NONNULLABLE_NULL =
            "x-cpp-tolerate-nonnullable-null";
    private final Logger LOGGER = LoggerFactory.getLogger(CppBoostBeastClientCodegen.class);
    /** Tracks model names resolved as oneOf/anyOf variant types for shared_ptr exclusion. */
    private Set<String> variantModels = new HashSet<>();
    /** Caches resolved C++ types for composed models so postProcessModels can
     *  transitively resolve $ref chains through model aliases (for example,
     *  ModelIds referencing ModelIdsShared, both ultimately std::string). */
    private Map<String, String> resolvedAliasTypes = new HashMap<>();
    /** Retains composition semantics after named schemas are lowered to C++ aliases. */
    private Map<String, String> composedKeywordsByModel = new HashMap<>();
    /** Descriptor index mapping schema name to composition descriptor, populated
     *  in preprocessOpenAPI after inline model flattening. Replaces raw schema
     *  inspection as the semantic source for branch lowering. */
    private Map<String, CompositionDescriptor> compositionDescriptors = new LinkedHashMap<>();
    /** OpenAPI document retained for operation and model post-processing. */
    private OpenAPI sourceOpenApi;
    /** Preserved inbound-only webhook metadata. Webhooks are removed from
     *  outbound API generation so upstream folding cannot replace path APIs. */
    private List<String> webhookPreservation = new ArrayList<>();

    public List<String> getWebhookPreservation() {
        return new ArrayList<>(webhookPreservation);
    }

    private static String idOf(io.swagger.v3.oas.models.Operation op) {
        return op.getOperationId() == null ? "(no operationId)" : op.getOperationId();
    }

    /** Callback and response-link names keyed by path and method. */
    private Map<String, List<String>> operationCallbacks = new HashMap<>();
    private Map<String, List<String>> operationLinks = new HashMap<>();

    private void captureOperationMetadata(OpenAPI openAPI) {
        operationCallbacks.clear();
        operationLinks.clear();
        if (openAPI == null || openAPI.getPaths() == null) {
            return;
        }
        for (Map.Entry<String, PathItem> pathEntry : openAPI.getPaths().entrySet()) {
            PathItem pathItem = pathEntry.getValue();
            if (pathItem == null || pathItem.readOperationsMap() == null) {
                continue;
            }
            for (Map.Entry<PathItem.HttpMethod, Operation> operationEntry
                    : pathItem.readOperationsMap().entrySet()) {
                Operation operation = operationEntry.getValue();
                if (operation == null) {
                    continue;
                }
                String key = pathEntry.getKey() + '\0' + operationEntry.getKey().name();
                List<String> callbackNames = operation.getCallbacks() == null
                        ? Collections.emptyList()
                        : new ArrayList<>(operation.getCallbacks().keySet());
                operationCallbacks.put(key, callbackNames);

                Set<String> linkNames = new LinkedHashSet<>();
                if (operation.getResponses() != null) {
                    for (ApiResponse candidate : operation.getResponses().values()) {
                        if (candidate == null) {
                            continue;
                        }
                        if (candidate.getLinks() != null) {
                            linkNames.addAll(candidate.getLinks().keySet());
                        }
                        ApiResponse resolved = ModelUtils.getReferencedApiResponse(
                                openAPI, candidate);
                        if (resolved != null && resolved.getLinks() != null) {
                            linkNames.addAll(resolved.getLinks().keySet());
                        }
                    }
                }
                operationLinks.put(key, new ArrayList<>(linkNames));
            }
        }
    }

    /** Cached allOf intersections keyed by model name. Populated during
     *  preprocessOpenAPI and consumed by fromModel to build synthetic schemas. */
    private Map<String, AllOfIntersection> allOfIntersections = new LinkedHashMap<>();

    /** Starts an isolated state set for one generator invocation. */
    private void beginGeneration(OpenAPI openApi) {
        sourceOpenApi = openApi;
        variantModels = new HashSet<>();
        resolvedAliasTypes = new HashMap<>();
        composedKeywordsByModel = new HashMap<>();
        compositionDescriptors = new LinkedHashMap<>();
        webhookPreservation = new ArrayList<>();
        operationCallbacks = new HashMap<>();
        operationLinks = new HashMap<>();
        allOfIntersections = new LinkedHashMap<>();
    }

    /**
     * Returns the composition descriptor for the given schema name, or null
     * if the schema is not composed or was not indexed.
     */
    public CompositionDescriptor getCompositionDescriptor(String schemaName) {
        return compositionDescriptors.get(schemaName);
    }

    /**
     * Returns an unmodifiable view of the full composition descriptor index.
     */
    public Map<String, CompositionDescriptor> getCompositionDescriptors() {
        return Collections.unmodifiableMap(compositionDescriptors);
    }
    protected String packageName = DEFAULT_PACKAGE_NAME;

    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    public String getName() {
        return "cpp-boost-beast-client";
    }

    public String getHelp() {
        return "Generates a cpp-boost-beast client.";
    }

    @Override
    public void preprocessOpenAPI(OpenAPI openAPI) {
        beginGeneration(openAPI);
        List<String> policyDiagnostics = validateDialectPolicy(openAPI);
        if (!policyDiagnostics.isEmpty()) {
            throw new IllegalArgumentException(String.join("; ", policyDiagnostics));
        }
        super.preprocessOpenAPI(openAPI);
        // Webhooks are inbound-only metadata for a client generator. Upstream
        // folds them into the API map under the same fallback classname as path
        // operations, which can replace the path API. Preserve their metadata,
        // then remove them so outbound paths still generate; no listener is emitted.
        if (openAPI.getWebhooks() != null && !openAPI.getWebhooks().isEmpty()) {
            for (Map.Entry<String, PathItem> e : openAPI.getWebhooks().entrySet()) {
                PathItem item = e.getValue();
                List<String> methods = new ArrayList<>();
                if (item.getGet() != null) methods.add("GET " + idOf(item.getGet()));
                if (item.getPut() != null) methods.add("PUT " + idOf(item.getPut()));
                if (item.getPost() != null) methods.add("POST " + idOf(item.getPost()));
                if (item.getDelete() != null) methods.add("DELETE " + idOf(item.getDelete()));
                if (item.getPatch() != null) methods.add("PATCH " + idOf(item.getPatch()));
                if (item.getHead() != null) methods.add("HEAD " + idOf(item.getHead()));
                if (item.getOptions() != null) methods.add("OPTIONS " + idOf(item.getOptions()));
                if (item.getTrace() != null) methods.add("TRACE " + idOf(item.getTrace()));
                webhookPreservation.add(e.getKey()
                        + "[" + String.join(", ", methods) + "]");
            }
            openAPI.setWebhooks(null);
        }
        // Capture callback and response-link names for generated API comments.
        captureOperationMetadata(openAPI);
        // Recover prefixItems dropped when the shared OAS 3.1 normalizer
        // converts a type-array JsonSchema to ArraySchema. This must precede
        // descriptor scanning so child schemas retain the pristine value.
        Oas31RawSpecRecovery.restoreNormalizerDroppedPrefixItems(openAPI, getInputSpec());
        Oas31RawSpecRecovery.recoverPristineLiterals(openAPI, getInputSpec());
        // Populate variantModels and build composition descriptors before
        // model processing begins so that getTypeDeclaration can resolve $ref
        // to composed models as value types and branch semantics are captured
        // before fromModel consumes composed schemas.
        Map<String, Schema> schemas = openAPI.getComponents() != null
                ? openAPI.getComponents().getSchemas() : null;
        if (schemas != null) {
            // Build descriptor index: must happen after inline model resolver
            // flattening so all inline schemas have been extracted to component
            // references with stable $ref targets.
            for (Map.Entry<String, Schema> entry : schemas.entrySet()) {
                String schemaName = entry.getKey();
                Schema schema = entry.getValue();
                CompositionDescriptor descriptor =
                        Oas31CompositionLowering.buildCompositionDescriptor(
                                schemaName, schema, openAPI, schemas);
                if (descriptor != null) {
                    // Index by toModelName so lookups via cm.classname match.
                    compositionDescriptors.put(toModelName(schemaName), descriptor);
                    // Every detectable branch assertion that can affect
                    // membership must have an implementation.
                    Oas31CompositionLowering.validateDescriptorAssertions(descriptor);

                    // Precompute recursive allOf intersections for storage modeling.
                    // fromModel consumes the synthetic schema instead of a shallow
                    // property-conflict scan.
                    if ("allOf".equals(descriptor.getKeyword())) {
                        AllOfIntersection intersection =
                                Oas31CompositionLowering.computeAllOfIntersection(
                                        schemaName, schema, openAPI, schemas, new HashSet<>());
                        if (intersection != null) {
                            allOfIntersections.put(toModelName(schemaName), intersection);
                        }
                    }
                }
                if ((schema.getOneOf() != null && !schema.getOneOf().isEmpty())
                        || (schema.getAnyOf() != null && !schema.getAnyOf().isEmpty())) {
                    variantModels.add(schemaName);
                }
            }
        }
}

    // ========================================================================
    // OAS 3.1 dialect and schema policy
    // ========================================================================

    /** Pinned OAS 3.1 Schema dialect (spec.openapis.org/oas/3.1/dialect/2024-11-10). */
    public static final String OAS_31_DIALECT =
            "https://spec.openapis.org/oas/3.1/dialect/2024-11-10";

    /** OAS alias accepted only as the identifier for the same pinned revision. */
    public static final String OAS_31_DIALECT_BASE_ALIAS =
            "https://spec.openapis.org/oas/3.1/dialect/base";

    /** Plain JSON Schema Draft 2020-12 core identifier (non-OAS dialect). */
    public static final String DRAFT_2020_12 =
            "https://json-schema.org/draft/2020-12/schema";

    /** Classified effective schema dialect for an OpenAPI document. */
    public enum OasDialect {
        /** OAS 3.1 pinned dialect (or its base alias). */
        OAS_31,
        /** Plain JSON Schema Draft 2020-12 (not OAS-wrapped). */
        DRAFT_2020_12_REC,
        /** A dialect identifier not recognized by this program. */
        UNRECOGNIZED,
        /** No dialect declared (OAS 3.1 default applies for OAS 3.1 documents). */
        UNSPECIFIED
    }

    /**
     * Dialect resolution, normative-structure checks, and the exhaustive
     * keyword-occurrence scanner live in {@link Oas31KeywordScanner};
     * the delegates below keep this generator's public API stable for
     * tests and templates.
     */
    public static OasDialect resolveEffectiveDialect(String jsonSchemaDialect, String rootSchema) {
        return Oas31KeywordScanner.resolveEffectiveDialect(jsonSchemaDialect, rootSchema);
    }

    /** Resolve the effective dialect of an OpenAPI document from its declared knobs. */
    public static OasDialect resolveDocumentDialect(OpenAPI openAPI) {
        return Oas31KeywordScanner.resolveDocumentDialect(openAPI);
    }

    /** OAS 3 structural normative checks (see {@link Oas31KeywordScanner}). */
    public List<String> validateNormativeOas3Structure(OpenAPI openAPI) {
        return Oas31KeywordScanner.validateNormativeOas3Structure(openAPI);
    }

    /** Dialect/metaschema policy gate (see {@link Oas31KeywordScanner}). */
    public List<String> validateDialectPolicy(OpenAPI openAPI) {
        return Oas31KeywordScanner.validateDialectPolicy(openAPI);
    }


    /**
     * Exhaustive schema-valued-position scanner (see {@link Oas31KeywordScanner}).
     */
    public Oas31KeywordScanner.KeywordOccurrenceLedger scanSchemaKeywordOccurrences(
            OpenAPI openAPI) {
        return Oas31KeywordScanner.scanSchemaKeywordOccurrences(openAPI);
    }


    /** Set of fail-closed required-vocabulary keywords for this document. */
    public Set<String> failClosedKeywords(OpenAPI openAPI) {
        return Oas31KeywordScanner.failClosedKeywords(openAPI);
    }


    public CppBoostBeastClientCodegen() {
        super();
        openapiNormalizer.put("NORMALIZER_CLASS", CppBoostBeastOpenAPINormalizer.class.getName());
        modifyFeatureSet(features -> features
                .includeDocumentationFeatures(DocumentationFeature.Readme)
                .securityFeatures(EnumSet.noneOf(SecurityFeature.class))
                .includeGlobalFeatures(
                        GlobalFeature.ParameterStyling,
                        GlobalFeature.MultiServer,
                        // Preserve callback, webhook, and link metadata visibly;
                        // an outbound client does not generate inbound listeners.
                        GlobalFeature.Callbacks,
                        GlobalFeature.LinkObjects
                )
                .excludeGlobalFeatures(
                        GlobalFeature.XMLStructureDefinitions
                )
                .includeSchemaSupportFeatures(
                        SchemaSupportFeature.Polymorphism,
                        SchemaSupportFeature.Composite,
                        SchemaSupportFeature.oneOf,
                        SchemaSupportFeature.anyOf,
                        SchemaSupportFeature.allOf,
                        SchemaSupportFeature.not,
                        SchemaSupportFeature.Union
                )
                .includeDataTypeFeatures(
                        // Destination numeric domains validated by the corpus.
                        // Floating-point destinations narrow after exact validation;
                        // non-finite destinations produce representation diagnostics.
                        DataTypeFeature.Int32,
                        DataTypeFeature.Int64,
                        DataTypeFeature.Float,
                        DataTypeFeature.Double,
                        DataTypeFeature.String,
                        DataTypeFeature.Boolean,
                        DataTypeFeature.Enum,
                        DataTypeFeature.Array,
                        DataTypeFeature.Maps,
                        DataTypeFeature.Object,
                        DataTypeFeature.Null,
                        DataTypeFeature.AnyType
                )
                .excludeDataTypeFeatures(
                        // No decimal destination domain exists (format:
                        // decimal maps to double; exact decimals are not
                        // a declared C++ type) — the base set lists it.
                        DataTypeFeature.Decimal,
                        // Formats are annotations by default. String-domain
                        // formats map to std::string, so the generator does not
                        // advertise format-specific destination types.
                        DataTypeFeature.Date,
                        DataTypeFeature.DateTime,
                        DataTypeFeature.Uuid,
                        DataTypeFeature.Byte,
                        DataTypeFeature.Binary,
                        DataTypeFeature.Password
                )
                // Form-style cookie parameters are joined into the Cookie header.
                .includeParameterFeatures(
                        ParameterFeature.Cookie
                )
        );

        outputFolder = "generated-code" + File.separator + "cpp-boost-beast";
        modelTemplateFiles.put("model-header.mustache", ".h");
        modelTemplateFiles.put("model-source.mustache", ".cpp");
        apiTemplateFiles.put("api-header.mustache", ".h");
        apiTemplateFiles.put("api-source.mustache", ".cpp");

        embeddedTemplateDir = templateDir = "cpp-boost-beast-client";

        modelPackage = "org.openapitools.client.model";
        apiPackage = "org.openapitools.client.api";

        cliOptions.clear();

        // CLI options
        addOption(CodegenConstants.PACKAGE_NAME, "C++ package and library name.", DEFAULT_PACKAGE_NAME);
        addOption(CodegenConstants.MODEL_PACKAGE, "C++ namespace for models (convention: name.space.model).",
                this.modelPackage);
        addOption(CodegenConstants.API_PACKAGE, "C++ namespace for apis (convention: name.space.api).",
                this.apiPackage);
        CliOption formatAssertionOption = new CliOption("formatAssertionPolicy",
                "Format handling in composition branch matching. Only 'annotation'"
                + " is supported: format metadata never affects match counts.");
        formatAssertionOption.defaultValue(FORMAT_ASSERTION_POLICY_ANNOTATION);
        formatAssertionOption.addEnum(FORMAT_ASSERTION_POLICY_ANNOTATION,
                "Formats are annotations and do not affect validation");
        cliOptions.add(formatAssertionOption);

        CliOption sseSchemaModeOption = new CliOption("sseSchemaMode",
                "SSE schema interpretation mode for text/event-stream responses."
                + " 'representation' (default): the response schema describes the"
                + " text/event-stream media representation; return one raw data"
                + " string per framed event. Non-data fields such as event, id,"
                + " and retry are not surfaced. 'jsonEventData': the response schema"
                + " describes each JSON data field; decode each event's data payload"
                + " against the schema. Use the x-sse-event-data-schema vendor"
                + " extension for per-operation opt-in to typed event-data decoding.");
        sseSchemaModeOption.defaultValue(SSE_SCHEMA_MODE_REPRESENTATION);
        sseSchemaModeOption.addEnum(SSE_SCHEMA_MODE_REPRESENTATION,
                "Strict mode — schema describes media representation");
        sseSchemaModeOption.addEnum(SSE_SCHEMA_MODE_JSON_EVENT_DATA,
                "Schema describes each JSON event data payload");
        cliOptions.add(sseSchemaModeOption);

        CliOption compileWithValidationOption = new CliOption("compileWithValidation",
                "Emit schema-validation IR, per-model validate_* branch functions,"
                + " and kValidateOnDecode=true in generated ValidationTypes.h (default)."
                + " Set to false to omit the IR and validate_* functions for"
                + " high-throughput clients. Representation diagnostics (non-finite"
                + " destinations, integer range, required properties) remain active.");

        compileWithValidationOption.defaultValue(Boolean.TRUE.toString());
        cliOptions.add(compileWithValidationOption);
        CliOption tolerateNonNullableNullsOption = new CliOption(
                "tolerateNonNullableNulls",
                "Treat explicit JSON null values as absent for generated model properties"
                + " whose schemas do not allow null. Enabled by default to tolerate"
                + " non-conforming server responses while preserving required-key"
                + " presence checks; set to false for strict schema decoding."
                + " Non-null values remain fully validated.");
        tolerateNonNullableNullsOption.defaultValue(Boolean.TRUE.toString());
        cliOptions.add(tolerateNonNullableNullsOption);


        supportingFiles.add(new SupportingFile("validation-types.mustache", "model", "ValidationTypes.h"));
        supportingFiles.add(new SupportingFile("NullableField.h.mustache", "model", "NullableField.h"));
        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("CMakeLists.txt.mustache", "", "CMakeLists.txt"));
        supportingFiles.add(new SupportingFile("http-client-header.mustache", "api", "HttpClient.h"));
        supportingFiles.add(new SupportingFile("http-client-impl-header.mustache", "api", "HttpClientImpl.h"));
        supportingFiles.add(new SupportingFile("http-client-impl-source.mustache", "api", "HttpClientImpl.cpp"));
        supportingFiles.add(new SupportingFile("anytype-header.mustache", "model", "AnyType.h"));
        supportingFiles.add(new SupportingFile("MultipartWireTest.cpp.mustache", "test", "MultipartWireTest.cpp"));

        // Header-only schema-validation support. The templates place their
        // implementation types under the configured model namespace.
        supportingFiles.add(new SupportingFile("oas31_exact_number.mustache", "model", "Oas31ExactNumber.h"));
        supportingFiles.add(new SupportingFile("oas31_schema_ir.mustache", "model", "Oas31SchemaIr.h"));
        supportingFiles.add(new SupportingFile("oas31_deep_equal.mustache", "model", "Oas31DeepEqual.h"));
        supportingFiles.add(new SupportingFile("oas31_exact_json.mustache", "model", "Oas31ExactJson.h"));
        supportingFiles.add(new SupportingFile("oas31_validator.mustache", "model", "Oas31Validator.h"));
        // Generation-time IR tables, optional bounded source chunks, and thin
        // validate_<id> dispatch. Content is rendered from supporting-file data.
        supportingFiles.add(new SupportingFile("oas31_schema_ir_header.mustache", "model", "Oas31SchemaRegistry.h"));
        supportingFiles.add(new SupportingFile("oas31_schema_ir_source.mustache", "model", "schema_ir.generated.cpp"));
        supportingFiles.add(new SupportingFile("oas31_schema_validate.mustache", "model", "schema_validate.generated.cpp"));

        languageSpecificPrimitives = new HashSet<String>(
                Arrays.asList("int", "char", "bool", "long", "float", "double", "std::int32_t", "std::int64_t"));

        super.typeMapping = new HashMap<String, String>();
        typeMapping.put("date", "std::string");
        typeMapping.put("DateTime", "std::string");
        typeMapping.put("string", "std::string");
        typeMapping.put("integer", "std::int32_t");
        typeMapping.put("long", "std::int64_t");
        typeMapping.put("boolean", "bool");
        typeMapping.put("array", "std::vector");
        // uniqueItems constrains JSON arrays; it does not change their ordered wire representation.
        typeMapping.put("set", "std::vector");
        typeMapping.put("map", "std::map");
        typeMapping.put("file", "std::string");
        typeMapping.put("object", "boost::json::value");
        typeMapping.put("number", "double");
        typeMapping.put("UUID", "std::string");
        typeMapping.put("URI", "std::string");
        typeMapping.put("ByteArray", "std::string");

        super.importMapping = new HashMap<String, String>();
        importMapping.put("std::vector", "#include <vector>");
        importMapping.put("std::map", "#include <map>");
        importMapping.put("std::string", "#include <string>");
        importMapping.put("int32_t", "#include <cstdint>");
        importMapping.put("int64_t", "#include <cstdint>");
        importMapping.put("boost::json::value", "#include <boost/json.hpp>");
        importMapping.put("std::nullptr_t", "#include <cstddef>");
        importMapping.put("Null", "#include <cstddef>");
        importMapping.put("std::optional", "#include <optional>");
        importMapping.put("std::variant", "#include <variant>");
        importMapping.put("std::monostate", "#include <variant>");
        importMapping.put("std::shared_ptr", "#include <memory>");
        importMapping.put("AnyType", "#include \"AnyType.h\"");
    }

    @Override
    protected ImmutableMap.Builder<String, Lambda> addMustacheLambdas() {
        return super.addMustacheLambdas()
                .put("cppStringLiteral", (fragment, writer) -> writer.write(
                        escapeCppStringContent(
                                StringEscapeUtils.unescapeHtml4(fragment.execute()))));
    }

    @Override
    public String escapeText(String input) {
        return input == null ? null : escapeCppStringContent(input);
    }

    /**
     * Generator-specific normalizer that preserves composition structure
     * (branch cardinality, null multiplicity, original keyword) for all
     * oneOf/anyOf/anyOf-string-enum schemas. Set-equivalent simplification
     * happens later in the generator's semantic analyzer (processComposedModel),
     * never in the pre-descriptor normalizer.
     */
    public static class CppBoostBeastOpenAPINormalizer extends OpenAPINormalizer {
        public CppBoostBeastOpenAPINormalizer(OpenAPI openAPI, Map<String, String> inputRules) {
            super(openAPI, inputRules);
        }

        @Override
        protected Schema processSimplifyAnyOf(Schema schema) {
            if (schema.getAnyOf() != null && !schema.getAnyOf().isEmpty()) {
                return schema;
            }
            return super.processSimplifyAnyOf(schema);
        }

        @Override
        protected Schema processSimplifyOneOf(Schema schema) {
            if (schema.getOneOf() != null && !schema.getOneOf().isEmpty()) {
                return schema;
            }
            return super.processSimplifyOneOf(schema);
        }

        @Override
        protected Schema processSimplifyAnyOfStringAndEnumString(Schema schema) {
            if (schema.getAnyOf() != null && !schema.getAnyOf().isEmpty()) {
                return schema;
            }
            return super.processSimplifyAnyOfStringAndEnumString(schema);
        }

        @Override
        protected Schema processSimplifyOneOfEnum(Schema schema) {
            if (schema.getOneOf() != null && !schema.getOneOf().isEmpty()) {
                return schema;
            }
            return super.processSimplifyOneOfEnum(schema);
        }

        @Override
        protected Schema processSimplifyAnyOfEnum(Schema schema) {
            if (schema.getAnyOf() != null && !schema.getAnyOf().isEmpty()) {
                return schema;
            }
            return super.processSimplifyAnyOfEnum(schema);
        }
    }


    @Override
    public Map<String, ModelsMap> updateAllModels(Map<String, ModelsMap> objs)  {
        // Index all CodegenModels by model name.
        Map<String, CodegenModel> allModels = getAllModels(objs);

        // Clean interfaces of ambiguity
        for (Map.Entry<String, CodegenModel> cm : allModels.entrySet()) {
            if (cm.getValue().interfaces != null && !cm.getValue().interfaces.isEmpty()) {
                List<String> newIntf = new ArrayList<>(cm.getValue().interfaces);

                for (String intf : allModels.get(cm.getKey()).interfaces) {
                    if (allModels.get(intf).interfaces != null && !allModels.get(intf).interfaces.isEmpty()) {
                        for (String intfInner : allModels.get(intf).interfaces) {
                            newIntf.remove(intfInner);
                        }
                    }
                }
                cm.getValue().interfaces = newIntf;
            }
        }

        // --- Critical: Normalize shared_ptr types for cycle detection ---
        // DefaultCodegen.setCircularReferences compares property dataType strings
        // to model names literally. Since getTypeDeclaration wraps refs in
        // "std::shared_ptr<X>", the comparison "std::shared_ptr<Node>" != "Node"
        // never matches, so cycle edges would lose shared_ptr wrappers and emit
        // invalid value self-references.
        //
        // Fix: Temporarily strip std::shared_ptr<> wrappers from all property
        // dataTypes BEFORE super.updateAllModels runs (which calls setCircularReferences),
        // then restore them after. This ensures setCircularReferences sees bare model
        // names and correctly identifies cycles.
        Map<String, Map<String, String>> savedSharedPtr = new HashMap<>();
        for (CodegenModel cm : allModels.values()) {
            Map<String, String> modelSaves = new HashMap<>();
            for (CodegenProperty var : allVarsOf(cm)) {
                if (var == null) continue;
                checkAndSaveSharedPtr(var, cm.classname, modelSaves);
                if (var.isContainer && var.items != null) {
                    checkAndSaveSharedPtr(var.items, cm.classname, modelSaves);
                }
            }
            if (!modelSaves.isEmpty()) {
                savedSharedPtr.put(cm.classname, modelSaves);
            }
        }

        objs = super.updateAllModels(objs);

        // Restore shared_ptr wrappers stripped above.
        // isCircularReference flags are now correctly set by setCircularReferences
        // because it compared bare model names.
        for (CodegenModel cm : allModels.values()) {
            Map<String, String> modelSaves = savedSharedPtr.get(cm.classname);
            if (modelSaves == null) continue;
            for (CodegenProperty var : allVarsOf(cm)) {
                if (var == null) continue;
                restoreSavedSharedPtr(var, cm.classname, modelSaves);
                if (var.isContainer && var.items != null) {
                    restoreSavedSharedPtr(var.items, cm.classname + ".items", modelSaves);
                }
            }
        }

        // Phase: Strip std::shared_ptr<X> from non-cyclic object refs.
        // super.updateAllModels → DefaultCodegen.updateAllModels → setCircularReferences
        // has now run, setting isCircularReference flags on properties correctly.
        // Non-cyclic edges should use value semantics (plain X) rather than
        // std::shared_ptr<X> to avoid unnecessary heap allocation.
        for (CodegenModel cm : allModels.values()) {
            for (CodegenProperty var : allVarsOf(cm)) {
                if (var == null) continue;
                stripNonCyclicSharedPtr(var);
                if (var.isContainer && var.items != null) {
                    stripNonCyclicSharedPtr(var.items);
                }
            }
        }

        return objs;
    }

    /**
     * Returns all property lists of a model for iteration.
     */
    private static List<CodegenProperty> allVarsOf(CodegenModel cm) {
        List<CodegenProperty> combined = new ArrayList<>();
        if (cm.vars != null) combined.addAll(cm.vars);
        if (cm.allVars != null) combined.addAll(cm.allVars);
        if (cm.requiredVars != null) combined.addAll(cm.requiredVars);
        if (cm.optionalVars != null) combined.addAll(cm.optionalVars);
        if (cm.readOnlyVars != null) combined.addAll(cm.readOnlyVars);
        if (cm.readWriteVars != null) combined.addAll(cm.readWriteVars);
        if (cm.parentVars != null) combined.addAll(cm.parentVars);
        return combined;
    }

    /**
     * If a property has a dataType wrapped in std::shared_ptr<>, strips the
     * wrapper and saves the original under a compound key (modelName.baseName)
     * so it can be restored after setCircularReferences runs.
     */
    private static void checkAndSaveSharedPtr(CodegenProperty var, String modelName,
                                               Map<String, String> saves) {
        if (var.dataType != null && var.dataType.startsWith("std::shared_ptr<")) {
            String key = modelName + "." + var.baseName;
            if (!saves.containsKey(key)) {
                saves.put(key, var.dataType);
            }
            var.dataType = var.dataType.substring(16, var.dataType.length() - 1);
        }
    }

    /**
     * Restores a previously saved shared_ptr-wrapped dataType onto a property.
     */
    private static void restoreSavedSharedPtr(CodegenProperty var, String modelName,
                                               Map<String, String> saves) {
        String key = modelName + "." + var.baseName;
        String saved = saves.get(key);
        if (saved != null) {
            var.dataType = saved;
        }
    }

    /**
     * Strips std::shared_ptr<X> from a non-cyclic property, replacing it with
     * bare value type X. Cyclic properties retain the shared_ptr wrapper.
     */
    private static void stripNonCyclicSharedPtr(CodegenProperty var) {
        if (var.dataType != null && var.dataType.startsWith("std::shared_ptr<")
                && !var.isCircularReference) {
            String innerType = var.dataType.substring(16, var.dataType.length() - 1);
            var.dataType = innerType;
            var.defaultValue = null;
        }
    }

    @Override
    public ModelsMap postProcessModels(ModelsMap objs) {
        // Clear parent for non-inheriting array/map models (inherited from AbstractCppCodegen)
        for (ModelMap mo : objs.getModels()) {
            CodegenModel cm = mo.getModel();
            if ((cm.isArray || cm.isMap) && (cm.parentModel == null)) {
                cm.parent = null;
            }
        }

        ModelsMap result = postProcessModelsEnum(objs);

        // Lower oneOf/anyOf models before template-dispatch metadata is derived.
        for (ModelMap mo : result.getModels()) {
            processComposedModel(mo.getModel());
        }

        // Tag models with alias/variant flags for template dispatch.
        // Mustache templates use these flags to choose between emitting a using
        // alias (with to_json/from_json overloads for variants) vs. the existing
        // object model class template (with properties).
        for (ModelMap mo : result.getModels()) {
            CodegenModel cm = mo.getModel();
            if (cm.vendorExtensions.containsKey("x-cpp-type")) {
                cm.vendorExtensions.put("x-cpp-is-alias", true);
                String resolvedType = (String) cm.vendorExtensions.get("x-cpp-type");
                // Resolve non-std:: types through the alias chain to detect
                // models that alias to a variant (e.g., ParentServerEvent →
                // StreamEventUnion → std::variant<...>).
                String ultimateType = resolveThroughAliases(resolvedType);
                if (ultimateType != null && ultimateType.startsWith("std::variant<")) {
                    cm.vendorExtensions.put("x-cpp-is-variant", true);
                    cm.vendorExtensions.putIfAbsent("x-cpp-composed-keyword", "oneOf");
                }
            } else if (cm.parent != null && !cm.parent.isEmpty()
                    && resolvedAliasTypes.containsKey(cm.parent)) {
                // (e.g., ParentServerEvent : public StreamEventUnion) but where
                // the parent is a resolved variant/alias. Since inheritance from a
                // variant alias is invalid C++, treat this model as an alias too.
                // Example: ParentServerEvent has anyOf: [StreamEventUnion] where
                // StreamEventUnion = std::variant<...>.
                String parentAlias = cm.parent;
                cm.vendorExtensions.put("x-cpp-type", parentAlias);
                cm.vendorExtensions.put("x-cpp-is-alias", true);
                cm.dataType = parentAlias;
                resolvedAliasTypes.put(cm.classname, parentAlias);
                String parentResolvedType = resolvedAliasTypes.get(parentAlias);
                if (parentResolvedType != null && parentResolvedType.startsWith("std::variant<")) {
                    cm.vendorExtensions.put("x-cpp-is-variant", true);
                    // Non-variant alias source template (Path B) only generates
                    // stubs. For variant aliases (Path A), we need the composed
                    // keyword to generate fromJsonValue_/toJsonValue_ functions.
                    // Default to oneOf (conservative: exactly-one enforcement).
                    cm.vendorExtensions.putIfAbsent("x-cpp-composed-keyword", "oneOf");
                }
            }
        }

        // Fallback: Detect models whose composedSchemas were consumed by fromModel
        // before processComposedModel had a chance to run. This happens when the
        // default codegen pipeline collapses a bare oneOf/anyOf (without type:object)
        // into a flat dataType. These models have no vars and a dataType that differs
        // from their classname (e.g., SingleBranchTest → std::string).
        // A descriptor, when present, is the semantic source rather than dataType.
        for (ModelMap mo : result.getModels()) {
            CodegenModel cm = mo.getModel();
            if (cm.vendorExtensions.containsKey("x-cpp-is-alias")) {
                continue;
            }
            if (compositionDescriptors.containsKey(cm.classname)) {
                continue; // descriptor provides semantics; skip dataType heuristic
            }
            if (cm.vars != null && !cm.vars.isEmpty()) {
                continue;
            }
            if (cm.isArray || cm.isMap) {
                continue;
            }
            if (cm.dataType != null
                    && !cm.dataType.equals(cm.classname)
                    && (cm.dataType.startsWith("std::") || "boost::json::value".equals(cm.dataType)
                            || resolvedAliasTypes.containsKey(cm.dataType))) {
                cm.vendorExtensions.put("x-cpp-type", cm.dataType);
                cm.vendorExtensions.put("x-cpp-is-alias", true);
                resolvedAliasTypes.put(cm.classname, cm.dataType);
                if (cm.dataType.startsWith("std::variant<")) {
                    cm.vendorExtensions.put("x-cpp-is-variant", true);
                }
                // Determine composed keyword from the CodegenModel's anyOf/oneOf sets
                // for fallback paths that bypassed processComposedModel. For variant
                // types, oneOf is the conservative default (enables exactly-one checking
                // in fromJsonValue).
                String fallbackKeyword = null;
                if (cm.oneOf != null && !cm.oneOf.isEmpty()) {
                    fallbackKeyword = "oneOf";
                } else if (cm.anyOf != null && !cm.anyOf.isEmpty()) {
                    fallbackKeyword = "anyOf";
                }
                if (fallbackKeyword == null) {
                    fallbackKeyword = "oneOf";
                }
                cm.vendorExtensions.put("x-cpp-composed-keyword", fallbackKeyword);
                composedKeywordsByModel.put(cm.classname, fallbackKeyword);
            }
        }

        // Degenerate fallback: Models like AllNullTest whose composed schemas
        // (anyOf [null, null]) were entirely consumed by the default codegen
        // without leaving usable branches or dataType. These models have no vars,
        // are not arrays/maps, and have `isAnyType = true` (no explicit `type` field
        // on the OpenAPI schema). Treat as boost::json::value alias.
        // Apply only when a composition descriptor establishes the schema semantics.
        for (ModelMap mo : result.getModels()) {
            CodegenModel cm = mo.getModel();
            if (cm.vendorExtensions.containsKey("x-cpp-is-alias")) {
                continue;
            }
            if (compositionDescriptors.containsKey(cm.classname)) {
                continue; // descriptor provides semantics; skip dataType heuristic
            }
            if (cm.vars != null && !cm.vars.isEmpty()) {
                continue;
            }
            if (cm.isArray || cm.isMap) {
                continue;
            }
            if (cm.getIsAnyType()) {
                cm.vendorExtensions.put("x-cpp-type", "boost::json::value");
                resolvedAliasTypes.put(cm.classname, "boost::json::value");
                cm.vendorExtensions.put("x-cpp-is-alias", true);
                // Even for boost::json::value fallbacks, set the keyword so
                // template code referencing vendorExtensions.x-cpp-composed-keyword
                // does not encounter an undefined variable.
                cm.vendorExtensions.put("x-cpp-composed-keyword", "oneOf");
                composedKeywordsByModel.put(cm.classname, "oneOf");
            }
        }

        // Recover all-null oneOf/anyOf models that still reach model processing
        // as one std::nullptr_t branch. Preserve the authored cardinality with
        // tagged alternatives and add matching descriptors so the schema IR
        // owns every generated branch validator.
        for (ModelMap mo : result.getModels()) {
            CodegenModel cm = mo.getModel();
            String checkType = (String) cm.vendorExtensions.get("x-cpp-type");
            if (checkType == null && cm.isAlias) {
                checkType = cm.dataType;
            }
            if ("std::nullptr_t".equals(checkType)
                    && !cm.vendorExtensions.containsKey("x-cpp-is-variant")) {
                CompositionDescriptor descriptor = compositionDescriptors.get(cm.classname);
                int branchCount = descriptor != null && descriptor.getBranches().size() > 1
                        ? descriptor.getBranches().size() : 2;
                String keyword = descriptor == null ? "oneOf" : descriptor.getKeyword();
                boolean isNullComposition = branchCount > 1
                        && ("oneOf".equals(keyword) || "anyOf".equals(keyword));
                if (isNullComposition) {
                    String rawSchemaName = cm.schemaName != null && !cm.schemaName.isEmpty()
                            ? cm.schemaName : cm.classname;
                    if (descriptor == null || descriptor.getBranches().size() != branchCount) {
                        List<CompositionBranchDescriptor> nullBranches = new ArrayList<>();
                        String validatorPrefix = toValidIdentifier(rawSchemaName);
                        for (int bi = 0; bi < branchCount; bi++) {
                            String storageType =
                                    "CompositionBranchValue<" + bi + ", std::nullptr_t>";
                            Map<String, Object> validateParams = new LinkedHashMap<>();
                            validateParams.put("validation-type", "null");
                            nullBranches.add(new CompositionBranchDescriptor(
                                    bi, null, "null", storageType,
                                    validatorPrefix + "_branch_" + bi,
                                    CompositionBranchDescriptor.NullCapability.ALWAYS,
                                    List.of("type"), Collections.emptyList(), validateParams));
                        }
                        String schemaLocation = descriptor != null
                                ? descriptor.getSchemaLocation()
                                : "#/components/schemas/"
                                        + rawSchemaName.replace("~", "~0")
                                                .replace("/", "~1");
                        descriptor = new CompositionDescriptor(
                                rawSchemaName, schemaLocation, keyword, nullBranches, null);
                        compositionDescriptors.put(cm.classname, descriptor);
                    }

                    List<String> tagged = new ArrayList<>();
                    for (int bi = 0; bi < branchCount; bi++) {
                        tagged.add("CompositionBranchValue<" + bi + ", std::nullptr_t>");
                    }
                    String variantType = "std::variant<" + String.join(", ", tagged) + ">";
                    cm.vendorExtensions.put("x-cpp-type", variantType);
                    cm.dataType = variantType;
                    resolvedAliasTypes.put(cm.classname, variantType);
                    variantModels.add(cm.classname);
                    cm.vendorExtensions.put("x-cpp-is-variant", true);
                    cm.vendorExtensions.put("x-cpp-is-alias", true);
                    cm.vendorExtensions.put("x-cpp-has-duplicate-types", true);
                    cm.vendorExtensions.put("x-cpp-composed-keyword", keyword);
                    composedKeywordsByModel.put(cm.classname, keyword);
                    cm.vendorExtensions.put("x-cpp-branches",
                            new ArrayList<>(Collections.nCopies(
                                    branchCount, "std::nullptr_t")));

                    Map<String, Object> templateMap = descriptor.toTemplateMap();
                    templateMap.put("has-duplicate-types", true);
                    @SuppressWarnings("unchecked")
                    List<Map<String, Object>> branchMaps =
                            (List<Map<String, Object>>) templateMap.get("branches");
                    for (int bi = 0; bi < branchMaps.size(); bi++) {
                        branchMaps.get(bi).put("storage-cpp-type",
                                "CompositionBranchValue<" + bi + ", std::nullptr_t>");
                        branchMaps.get(bi).put("inner-cpp-type", "std::nullptr_t");
                    }
                    cm.vendorExtensions.put("x-cpp-composition-branches", templateMap);
                }
            }
        }

        // Tag properties whose types already embed optional semantics so the
        // template skips redundant IsSet state.
        for (ModelMap mo : result.getModels()) {
            CodegenModel cm = mo.getModel();
            for (CodegenProperty var : allVarsOf(cm)) {
                if (var.dataType != null && var.dataType.startsWith("std::optional<")) {
                    var.vendorExtensions.put("x-cpp-no-is-set", true);
                }
            }
        }


        // Cross-model property tagging runs in postProcessAllModels, where the
        // complete model index is available.

        // Tag optional-impossible properties from allOf intersections.
        // These properties have an empty intersection (e.g., string ∩ integer).
        // The generated decode validation rejects the property when present
        // in JSON but accepts the object when the property is absent. The
        // getter/setter and member are still emitted (non-empty shell).
        for (ModelMap mo : result.getModels()) {
            CodegenModel cm = mo.getModel();
            @SuppressWarnings("unchecked")
            List<String> optImpProps = (List<String>) cm.vendorExtensions
                    .remove("x-cpp-optional-impossible-properties");
            if (optImpProps == null || optImpProps.isEmpty()) continue;
            for (CodegenProperty var : allVarsOf(cm)) {
                if (optImpProps.contains(var.baseName)) {
                    var.vendorExtensions.put("x-cpp-optional-impossible", true);
                    var.vendorExtensions.put("x-cpp-reject-if-present", true);
                }
            }
        }

        // Emit complete includes for resolved alias and variant types.
        for (ModelMap mo : result.getModels()) {
            CodegenModel cm = mo.getModel();
            if (!cm.vendorExtensions.containsKey("x-cpp-is-alias")) {
                continue;
            }
            String resolvedType = (String) cm.vendorExtensions.get("x-cpp-type");
            List<String> branchTypes = (List<String>) cm.vendorExtensions.get("x-cpp-branches");
            collectImportsForType(resolvedType, cm);
            if (branchTypes != null) {
                for (String branchType : branchTypes) {
                    collectImportsForType(branchType, cm);
                }
            }
            // Remove self-includes that were added by the branch/type scan.
            // A variant like std::variant<std::string, TracingConfiguration> referencing
            // itself as a branch causes the model to include its own header.
            cm.imports.removeIf(imp -> imp.equals("#include \"" + cm.classname + ".h\""));
        }

        // Phase: Emit x-cpp-composition-branches for allOf models that were
        // processed by fromModel (not by processComposedModel). These models
        // have descriptors but were bypassed by the oneOf/anyOf lowering loop.
        for (ModelMap mo : result.getModels()) {
            CodegenModel cm = mo.getModel();
            if (cm.vendorExtensions.containsKey("x-cpp-composition-branches")) {
                continue;
            }
            CompositionDescriptor desc = compositionDescriptors.get(cm.classname);
            if (desc != null && "allOf".equals(desc.getKeyword())) {
                cm.vendorExtensions.put("x-cpp-composition-branches", desc.toTemplateMap());
            }
        }

        // Phase: Convert allOf models with scalar-type intersection (e.g.,
        // allOf of two string enums, allOf of a scalar type and an object)
        // to type aliases when the merged properties are empty. These models
        // have an AllOfIntersection with a rootScalarType but no object
        // properties, so they should emit "using Name = std::string;" rather
        // than an empty class shell.
        for (ModelMap mo : result.getModels()) {
            CodegenModel cm = mo.getModel();
            if (cm.vendorExtensions.containsKey("x-cpp-is-alias")) {
                continue;
            }
            AllOfIntersection intersection = allOfIntersections.get(cm.classname);
            if (intersection == null) {
                continue;
            }
            if (intersection.getRootScalarType() == null) {
                continue;
            }
            // Only convert to alias when the merged properties are empty
            // (no object properties from allOf contributors). Models with
            // both a root scalar and properties need a class.
            if (!intersection.getProperties().isEmpty()) {
                continue;
            }
            if (!intersection.isSatisfiable()) {
                continue;
            }
            // Resolve the root scalar type to its C++ type
            String resolvedType = resolveOpenApiTypeName(intersection.getRootScalarType());
            // Apply intersected root-level enum values: if the allOf produces
            // an enum intersection (e.g., [a,b] ∩ [b,c] = [b]), keep the type
            // as std::string (not an enum class), since the intersection may
            // be narrower than the full enum set.
            cm.vendorExtensions.put("x-cpp-type", resolvedType);
            cm.vendorExtensions.put("x-cpp-is-alias", true);
            cm.dataType = resolvedType;
            resolvedAliasTypes.put(cm.classname, resolvedType);
            cm.vendorExtensions.put("x-cpp-composed-keyword", "allOf");
            composedKeywordsByModel.put(cm.classname, "allOf");
            // Propagate intersected enum values to vendor extensions so the
            // alias fromJsonValue template can generate enum validation.
            // Enum values are stored as List<String> for Mustache iteration.
            if (intersection.getRootEnumValues() != null
                    && !intersection.getRootEnumValues().isEmpty()) {
                List<String> intersectedEnum = new ArrayList<>();
                for (Object ev : intersection.getRootEnumValues()) {
                    if (ev != null) {
                        intersectedEnum.add(escapeCppStringContent(ev.toString()));
                    }
                }
                cm.vendorExtensions.put("x-cpp-allof-intersected-enum-values",
                        intersectedEnum);
                cm.vendorExtensions.put("x-cpp-allof-intersected-enum", true);
            }
        }

        return result;
    }

    private static boolean hasTaggedCompositionBranches(String resolvedType) {
        // Duplicate lowering wraps every outer alternative. A nested variant may
        // contain the same tag text without changing the outer storage contract.
        return resolvedType != null
                && resolvedType.startsWith("std::variant<CompositionBranchValue<");
    }

    @SuppressWarnings("unchecked")
    private void refreshCompositionStorageMetadata(
            CodegenModel model, List<ComposedBranch> branches, String resolvedType) {
        Object metadataObject = model.vendorExtensions.get("x-cpp-composition-branches");
        if (!(metadataObject instanceof Map)) {
            return;
        }
        Map<String, Object> metadata = (Map<String, Object>) metadataObject;
        Object branchMapsObject = metadata.get("branches");
        if (!(branchMapsObject instanceof List)) {
            return;
        }
        List<Map<String, Object>> branchMaps = (List<Map<String, Object>>) branchMapsObject;
        boolean wrapped = hasTaggedCompositionBranches(resolvedType);

        for (ComposedBranch branch : branches) {
            int index = branch.originalBranchIndex;
            if (index < 0 || index >= branchMaps.size()) {
                continue;
            }
            Map<String, Object> branchMap = branchMaps.get(index);
            if (wrapped) {
                branchMap.put("storage-cpp-type",
                        "CompositionBranchValue<" + index + ", " + branch.cppType + ">");
                branchMap.put("inner-cpp-type", branch.cppType);
            } else {
                branchMap.put("storage-cpp-type", branch.cppType);
                branchMap.remove("inner-cpp-type");
            }
        }

        metadata.put("has-duplicate-types", wrapped);
        if (wrapped) {
            model.vendorExtensions.put("x-cpp-has-duplicate-types", true);
        } else {
            model.vendorExtensions.remove("x-cpp-has-duplicate-types");
        }
    }


    @Override
    public Map<String, ModelsMap> postProcessAllModels(Map<String, ModelsMap> objs) {
        Map<String, ModelsMap> processed = super.postProcessAllModels(objs);
        Map<String, CodegenModel> allModels = getAllModels(processed);
        // Resolve composed aliases to a fixed point. An acyclic dependency graph
        // must converge within one pass per model.
        int maxAliasResolutionPasses = processed.values().stream()
                .mapToInt(models -> models.getModels().size())
                .sum() + 1;
        boolean typeChanged = true;
        int aliasResolutionPass = 0;
        while (typeChanged && aliasResolutionPass < maxAliasResolutionPasses) {
            typeChanged = false;
            aliasResolutionPass++;
            for (Map.Entry<String, ModelsMap> entry : processed.entrySet()) {
                for (ModelMap mo : entry.getValue().getModels()) {
                    CodegenModel cm = mo.getModel();
                    if (!cm.vendorExtensions.containsKey("x-cpp-type")) {
                        continue;
                    }
                    String composedKeyword = (String) cm.vendorExtensions.get("x-cpp-composed-keyword");
                    if (composedKeyword == null) {
                        continue;
                    }
                    List<String> branchTypes = (List<String>) cm.vendorExtensions.get("x-cpp-branches");
                    if (branchTypes == null) {
                        continue;
                    }
                    List<String> resolved = branchTypes.stream()
                            .map(this::resolveThroughAliases)
                            .collect(Collectors.toList());
                    if (resolved.equals(branchTypes)) {
                        continue;
                    }
                    String currentType = (String) cm.vendorExtensions.get("x-cpp-type");
                    String newType;
                    List<ComposedBranch> branchesWithMeta = new ArrayList<>();
                    try {
                        // Reconstruct ComposedBranch objects using resolved C++ type
                        // strings and per-branch isEnum metadata.  Without isEnum, a
                        // oneOf [open-string, string-enum] whose branches resolve to
                        // ["std::string", "std::string"] through the alias chain would
                        // collapse to plain std::string and lose the oneOf overlap
                        // detection that correctly type-erases to boost::json::value.
                        //
                        // Branch isEnum comes from two sources:
                        //   1. For branches whose original type is a model name (not a C++
                        //      type string), look up the CodegenModel to check isEnum.
                        //   2. Fall back to stored x-cpp-branch-is-enum metadata from the
                        //      first lowering pass (handles inline enum schemas where the
                        //      CodegenProperty.isEnum flag was set directly).
                        //
                        // Preserve the original descriptor index after
                        // self-referencing branches are filtered.
                        @SuppressWarnings("unchecked")
                        List<Boolean> storedIsEnum = (List<Boolean>) cm.vendorExtensions.get("x-cpp-branch-is-enum");
                        @SuppressWarnings("unchecked")
                        List<Integer> storedOriginalIndices = (List<Integer>) cm.vendorExtensions
                                .get("x-cpp-branch-original-index");
                        for (int i = 0; i < resolved.size(); i++) {
                            int descIndex = (storedOriginalIndices != null && i < storedOriginalIndices.size())
                                    ? storedOriginalIndices.get(i) : i;
                            boolean isEnum = false;
                            if ("std::string".equals(resolved.get(i))) {
                                // Source 1: Look up the original branch model for enum status.
                                String originalType = branchTypes.get(i);
                                CodegenModel branchModel = allModels.get(originalType);
                                isEnum = branchModel != null && branchModel.isEnum;
                                // Source 2: Fall back to stored metadata from first pass.
                                if (!isEnum && storedIsEnum != null && i < storedIsEnum.size()) {
                                    isEnum = storedIsEnum.get(i);
                                }
                            }
                            boolean isStringLike = "std::string".equals(resolved.get(i));
                            branchesWithMeta.add(new ComposedBranch(resolved.get(i), isEnum, isStringLike, descIndex));
                        }
                        CompositionDescriptor descriptor =
                                compositionDescriptors.get(cm.classname);
                        newType = Oas31CompositionLowering.lowerComposedTypes(
                                branchesWithMeta, composedKeyword, descriptor, LOGGER::warn);
                    } catch (RuntimeException e) {
                        throw new IllegalStateException(
                                "Failed to resolve composed aliases for '" + cm.classname + "'", e);
                    }
                    if (!newType.equals(currentType)) {
                        cm.vendorExtensions.put("x-cpp-type", newType);
                        // Keep original x-cpp-branches for import resolution.
                        cm.dataType = newType;
                        resolvedAliasTypes.put(cm.classname, newType);
                        refreshCompositionStorageMetadata(cm, branchesWithMeta, newType);
                        // Self-reference filtering needs the final post-collapse type,
                        // not the value cached during the first lowering pass.
                        if (cm.discriminator != null) {
                            cm.vendorExtensions.put("x-discriminator-resolved-type", newType);
                        }
                        typeChanged = true;
                    }
                }
            }
        }
        if (typeChanged) {
            throw new IllegalStateException("Composed alias resolution did not converge");
        }

        // Recompute alias and variant flags after transitive type resolution so
        // aliases of variant types inherit variant serialization behavior.
        for (Map.Entry<String, ModelsMap> entry : processed.entrySet()) {
            for (ModelMap mo : entry.getValue().getModels()) {
                CodegenModel cm = mo.getModel();
                if (cm.vendorExtensions.containsKey("x-cpp-is-alias")) {
                    String resolvedType = (String) cm.vendorExtensions.get("x-cpp-type");
                    String ultimateType = resolveThroughAliases(resolvedType);
                    if (ultimateType != null && ultimateType.startsWith("std::variant<")) {
                        cm.vendorExtensions.put("x-cpp-is-variant", true);
                        cm.vendorExtensions.putIfAbsent("x-cpp-composed-keyword", "oneOf");
                    } else {
                        cm.vendorExtensions.remove("x-cpp-is-variant");
                    }
                }
            }
        }

        // Type-erased oneOf aliases still need to validate the original branch
        // constraints before accepting the JSON value.
        for (Map.Entry<String, ModelsMap> entry : processed.entrySet()) {
            for (ModelMap modelMap : entry.getValue().getModels()) {
                CodegenModel codegenModel = modelMap.getModel();
                if ("oneOf".equals(codegenModel.vendorExtensions.get("x-cpp-composed-keyword"))
                        && "boost::json::value".equals(codegenModel.vendorExtensions.get("x-cpp-type"))
                        && codegenModel.getComposedSchemas() != null
                        && codegenModel.getComposedSchemas().getOneOf() != null
                        && !codegenModel.getComposedSchemas().getOneOf().isEmpty()) {
                    codegenModel.vendorExtensions.put(
                            "x-cpp-type-erased-oneof-branches",
                            buildTypeErasedOneOfBranches(codegenModel, allModels));
                    codegenModel.vendorExtensions.put("x-cpp-type-erased-oneof", true);
                }
            }
        }

        // Remove discriminator mappings that resolve to the current model type;
        // retaining one would recurse indefinitely and try to construct a variant
        // from itself. Update the CodegenDiscriminator consumed by templates.
        for (Map.Entry<String, ModelsMap> entry : processed.entrySet()) {
            for (ModelMap mo : entry.getValue().getModels()) {
                CodegenModel cm = mo.getModel();
                if (cm.discriminator == null) continue;
                String resolvedType = (String) cm.vendorExtensions.get("x-discriminator-resolved-type");
                if (resolvedType == null) continue;
                Set<CodegenDiscriminator.MappedModel> mappedModels = cm.discriminator.getMappedModels();
                if (mappedModels == null || mappedModels.isEmpty()) continue;
                Set<CodegenDiscriminator.MappedModel> filtered = new TreeSet<>();
                for (CodegenDiscriminator.MappedModel mm : mappedModels) {
                    if (mm.getModelName() != null) {
                        String resolvedTarget = resolveThroughAliases(mm.getModelName());
                        if (resolvedTarget.equals(resolvedType)) {
                            continue; // skip self-referential mapping
                        }
                    }
                    CodegenDiscriminator.MappedModel escapedMapping =
                            new CodegenDiscriminator.MappedModel(
                                    escapeCppStringContent(mm.getMappingName()),
                                    mm.getModelName(),
                                    mm.getSchemaName(),
                                    mm.isExplicitMapping());
                    escapedMapping.setModel(mm.getModel());
                    filtered.add(escapedMapping);
                }
                cm.discriminator.setMappedModels(filtered);
            }
        }

        // Finalize nullable storage only after updateAllModels has identified
        // cycles and removed shared_ptr from non-cyclic model references.
        for (ModelsMap modelsMap : processed.values()) {
            for (ModelMap modelMap : modelsMap.getModels()) {
                CodegenModel cm = modelMap.getModel();
                boolean needsNullableFieldInclude = false;
                for (CodegenProperty var : allVarsOf(cm)) {
                    if (tolerateNonNullableNulls && !var.isNullable) {
                        var.vendorExtensions.put(X_CPP_TOLERATE_NONNULLABLE_NULL, true);
                    }
                    if (!var.isNullable || var.dataType == null
                            || Boolean.TRUE.equals(var.vendorExtensions.get("x-cpp-nullable-field"))) {
                        continue;
                    }
                    String innerType = extractOptionalInnerType(var.dataType);
                    if (innerType == null && !Boolean.TRUE.equals(var.vendorExtensions
                            .get(Oas31RawSpecRecovery.LEGACY_NULLABLE_EXT))) {
                        continue;
                    }
                    if (var.isEnum) {
                        var.vendorExtensions.put(
                                "x-cpp-enum-value-type",
                                innerType == null ? var.dataType : innerType);
                    }
                    if (var.required) {
                        if (innerType == null) {
                            var.dataType = "std::optional<" + var.dataType + ">";
                            cm.imports.add("#include <optional>");
                            var.vendorExtensions.put("x-cpp-no-is-set", true);
                        }
                        continue;
                    }
                    if (innerType == null) {
                        innerType = var.dataType;
                        var.vendorExtensions.put("x-cpp-no-is-set", true);
                    }
                    var.dataType = "NullableField<" + innerType + ">";
                    var.vendorExtensions.put("x-cpp-nullable-field", true);
                    var.vendorExtensions.put("x-cpp-nullable-field-inner-type", innerType);
                    needsNullableFieldInclude = true;
                }
                if (needsNullableFieldInclude) {
                    cm.imports.add("#include \"NullableField.h\"");
                }
            }
        }

        // Tag properties that refer to variant aliases so templates use the
        // keyword-aware free conversion functions rather than the generic variant
        // converter, which always enforces oneOf semantics. This global pass can
        // inspect every model and unwrap NullableField before alias lookup.
        for (Map.Entry<String, ModelsMap> entry : processed.entrySet()) {
            for (ModelMap mo : entry.getValue().getModels()) {
                CodegenModel cm = mo.getModel();
                for (CodegenProperty var : allVarsOf(cm)) {
                    if (var.dataType != null) {
                        // Strip NullableField wrapper when present: use inner type
                        // for alias lookup.
                        String lookupType;
                        if (Boolean.TRUE.equals(var.vendorExtensions.get("x-cpp-nullable-field"))) {
                            lookupType = (String) var.vendorExtensions.get("x-cpp-nullable-field-inner-type");
                        } else {
                            lookupType = var.dataType;
                        }
                        if (lookupType == null) {
                            continue;
                        }
                        ModelsMap targetEntry = processed.get(lookupType);
                        if (targetEntry != null) {
                            for (ModelMap targetMo : targetEntry.getModels()) {
                                CodegenModel targetModel = targetMo.getModel();
                                if (Boolean.TRUE.equals(targetModel.vendorExtensions.get("x-cpp-is-variant"))) {
                                    var.vendorExtensions.put("x-cpp-variant-alias", true);
                                    var.vendorExtensions.put("x-cpp-variant-alias-name", lookupType);
                                }
                            }
                        }
                    }
                }
            }
        }

        // Include discriminator-mapped models used by generated variant dispatch.
        // Without these includes, the conversion functions are undeclared.
        for (Map.Entry<String, ModelsMap> entry : processed.entrySet()) {
            for (ModelMap mo : entry.getValue().getModels()) {
                CodegenModel cm = mo.getModel();
                @SuppressWarnings("unchecked")
                Map<String, String> mapping = (Map<String, String>)
                        cm.vendorExtensions.get("x-discriminator-mapping");
                if (mapping == null) continue;
                for (String modelName : mapping.values()) {
                    if (modelName != null) {
                        collectImportsForType(modelName, cm);
                    }
                }
            }
        }

        return processed;
    }

    /**
     * Scans a type string for known standard types and adds corresponding
     * #include directives to the model's import set. Types that look like
     * model names (start with an uppercase letter and are not otherwise
     * mapped) are resolved via toModelImport.
     */
    private void collectImportsForType(String type, CodegenModel cm) {
        if (type == null) {
            return;
        }
        boolean matchedImportMapping = false;
        for (Map.Entry<String, String> entry : importMapping.entrySet()) {
            String mappedKey = entry.getKey();
            String mappedInclude = entry.getValue();
            if (type.contains(mappedKey)) {
                cm.imports.add(mappedInclude);
                if (type.equals(mappedKey) || type.startsWith(mappedKey + "<")) {
                    matchedImportMapping = true;
                }
            }
        }
        // If the type was not matched by importMapping and looks like a model
        // name (starts with uppercase), treat it as a model include.
            if (!matchedImportMapping && !type.isEmpty() && Character.isUpperCase(type.charAt(0))) {
            String modelInclude = toModelImport(type);
            if (modelInclude != null && !modelInclude.isEmpty()) {
                cm.imports.add(modelInclude);
            }
        }
    }

    /**
     * Maps OpenAPI type names (from composed branch properties) to C++ types.
     * Composed properties created by DefaultCodegen.fromProperty use OpenAPI
     * type names (e.g., "null", "integer", "string") rather than mapped C++ types.
     */

    private String resolveOpenApiTypeName(String type) {
        if (type == null) {
            return null;
        }
        // Check typeMapping first for known OpenAPI type names
        if ("null".equals(type)) {
            return "std::nullptr_t";
        }
        // Check if it's already a C++ type (starts with std:: or boost:: or is a model name)
        if (type.startsWith("std::") || type.startsWith("boost::") || type.contains("<")) {
            return type;
        }
        // Map through typeMapping for OpenAPI primitive type names
        String mapped = typeMapping.get(type);
        if (mapped != null) {
            return mapped;
        }
        // If it has underscores or uppercase letters, assume it's already a model name
        return type;
    }

    /**
     * Applies the ordered type lowering rules to a composed (oneOf/anyOf) model.
     * Sets vendor extensions consumed by templates and records the model as a variant type.
     *
     * NOTE: When a schema uses <b>both</b> allOf and oneOf/anyOf at the same root level,
     * the allOf branches are merged into properties while the oneOf/anyOf branches are
     * lowered to variant types. This can produce a model with both concrete properties
     * AND a variant type, which may generate conflicting C++ declarations. Avoid such
     * mixed-schema patterns; prefer separate allOf-only or oneOf-only schemas.
     */
    private void processComposedModel(CodegenModel cm) {
        if (cm.getComposedSchemas() == null) {
            // Descriptor-complete path: when composedSchemas were consumed by
            // fromModel before we could access them, use the CompositionDescriptor
            // built in preprocessOpenAPI to reconstruct branch metadata and
            // perform lowering.
            CompositionDescriptor desc = compositionDescriptors.get(cm.classname);
            if (desc == null || "allOf".equals(desc.getKeyword())) {
                return; // allOf models handled separately in postProcessModels
            }
            processComposedModelFromDescriptor(cm, desc);
            return;
        }

        List<CodegenProperty> branches = null;
        String composedKeyword = null;

        if (cm.getComposedSchemas().getOneOf() != null && !cm.getComposedSchemas().getOneOf().isEmpty()) {
            branches = cm.getComposedSchemas().getOneOf();
            composedKeyword = "oneOf";
        } else if (cm.getComposedSchemas().getAnyOf() != null && !cm.getComposedSchemas().getAnyOf().isEmpty()) {
            branches = cm.getComposedSchemas().getAnyOf();
            composedKeyword = "anyOf";
        }

        if (branches == null) {
            // Fall through to descriptor path when oneOf/anyOf branches were
            // consumed by the default pipeline but a composition descriptor
            // still exists (e.g., all branches were self-references or the
            // schema uses composedSchemas for allOf only).
            CompositionDescriptor desc = compositionDescriptors.get(cm.classname);
            if (desc != null && !"allOf".equals(desc.getKeyword())) {
                processComposedModelFromDescriptor(cm, desc);
            }
            return;
        }

        // Look up the composition descriptor as the semantic source for lowering.
        // When available, descriptor metadata (null capability, assertions, keyword)
        // is used by lowerComposedTypes instead of inferring semantics from C++ type
        // strings alone.
        CompositionDescriptor descriptor = compositionDescriptors.get(cm.classname);

        // Collect C++ branch types (strip shared_ptr wrappers for variant members).
        // Map OpenAPI type names (e.g., "null", "integer", "string") to C++ types
        // because composed properties from fromProperty use OpenAPI type names as-is.
        // Self-referencing branches (a variant containing itself) are excluded
        // because they would create an illegal recursive type alias in C++.
        // Binary branches (format: binary) are mapped to std::vector<std::uint8_t>
        // so the multipart addVariantFormParameter helper can dispatch them as
        // file parts via compile-time type checking.
        // Deduplicate in lowerComposedTypes so oneOf retains branch identity when
        // identical C++ types represent distinct schemas.
        //
        // Track originalBranchIndex (bi) for descriptor alignment after
        // self-referencing branches are filtered out.
        List<ComposedBranch> composedBranches = new ArrayList<>();
        for (int bi = 0; bi < branches.size(); bi++) {
            CodegenProperty b = branches.get(bi);
            String cppType;
            if (b.isBinary || b.isFile) {
                cppType = "std::vector<std::uint8_t>";
            } else {
                String rawType = stripSharedPtr(b.dataType);
                if (rawType == null || "null".equals(rawType)) {
                    cppType = "std::nullptr_t";
                } else {
                    cppType = resolveOpenApiTypeName(rawType);
                }
            }
            if (cppType != null && cppType.equals(cm.classname)) {
                continue;
            }
            boolean isStringLike = b.isString || "std::string".equals(cppType)
                    || "string".equals(b.dataType);
            composedBranches.add(new ComposedBranch(cppType, b.isEnum, isStringLike, bi));
        }
        List<String> branchTypes = composedBranches.stream()
                .map(cb -> cb.cppType)
                .collect(Collectors.toList());

        String resolvedType;
        try {
            resolvedType = Oas31CompositionLowering.lowerComposedTypes(
                    composedBranches, composedKeyword, descriptor, LOGGER::warn);
        } catch (RuntimeException e) {
            throw new IllegalStateException(
                    "Failed to lower composed model '" + cm.classname + "'", e);
        }

        // Cache the resolved type for transitive alias resolution.
        resolvedAliasTypes.put(cm.classname, resolvedType);

        // Record as variant model for getTypeDeclaration shared_ptr exclusion
        variantModels.add(cm.classname);

        // Emit vendor extensions consumed by Mustache templates
        cm.vendorExtensions.put("x-cpp-type", resolvedType);
        cm.vendorExtensions.put("x-cpp-branches", branchTypes);
        cm.vendorExtensions.put("x-cpp-composed-keyword", composedKeyword);
        composedKeywordsByModel.put(cm.classname, composedKeyword);

        // Populate each descriptor branch's storage type and expose duplicate
        // alternatives so templates generate CompositionBranchValue accessors.
        boolean hasDuplicateTypes = hasTaggedCompositionBranches(resolvedType);
        if (descriptor != null) {
            Map<String, Object> templateMap = descriptor.toTemplateMap();
            @SuppressWarnings("unchecked")
            var templateBranches = (List<Map<String, Object>>) templateMap.get("branches");
            for (int bi = 0; bi < composedBranches.size(); bi++) {
                ComposedBranch cb = composedBranches.get(bi);
                int descIdx = cb.originalBranchIndex;
                if (descIdx >= 0 && descIdx < templateBranches.size()) {
                    Map<String, Object> tBranch = templateBranches.get(descIdx);
                    String storageType;
                    if (hasDuplicateTypes) {
                        storageType = "CompositionBranchValue<" + descIdx
                                + ", " + cb.cppType + ">";
                        tBranch.put("inner-cpp-type", cb.cppType);
                    } else {
                        storageType = cb.cppType;
                    }
                    tBranch.put("storage-cpp-type", storageType);
                }
            }
            templateMap.put("has-duplicate-types", hasDuplicateTypes);
            cm.vendorExtensions.put("x-cpp-composition-branches", templateMap);
            if (hasDuplicateTypes) {
                cm.vendorExtensions.put("x-cpp-has-duplicate-types", true);
            }
        } else {
            // Fallback: build branch maps from the composed branches when no
            // precomputed descriptor exists (e.g., inline schemas not in the
            // component schema index).
            List<Map<String, Object>> fallbackBranches = new ArrayList<>();
            for (int bi = 0; bi < composedBranches.size(); bi++) {
                ComposedBranch cb = composedBranches.get(bi);
                Map<String, Object> branchMap = new LinkedHashMap<>();
                branchMap.put("branch-index", bi);
                branchMap.put("source-schema-ref", null);
                branchMap.put("resolved-schema-name", cb.cppType);
                String storageType = hasDuplicateTypes
                        ? "CompositionBranchValue<" + bi + ", " + cb.cppType + ">"
                        : cb.cppType;
                branchMap.put("storage-cpp-type", storageType);
                if (hasDuplicateTypes) {
                    branchMap.put("inner-cpp-type", cb.cppType);
                }
                branchMap.put("validator-id", null);
                branchMap.put("null-capability",
                        "std::nullptr_t".equals(cb.cppType) ? "always" : "never");
                fallbackBranches.add(branchMap);
            }
            Map<String, Object> fallbackMap = new LinkedHashMap<>();
            fallbackMap.put("schema-name", cm.classname);
            fallbackMap.put("schema-location", null);
            fallbackMap.put("keyword", composedKeyword);
            fallbackMap.put("branches", fallbackBranches);
            fallbackMap.put("has-duplicate-types", hasDuplicateTypes);
            cm.vendorExtensions.put("x-cpp-composition-branches", fallbackMap);
            if (hasDuplicateTypes) {
                cm.vendorExtensions.put("x-cpp-has-duplicate-types", true);
            }
        }

        // Preserve enum identity for the later alias-resolution pass: open strings
        // and string enums both lower to std::string, so the C++ type alone cannot
        // detect overlap.
        List<Boolean> branchIsEnumFlags = composedBranches.stream()
                .map(cb -> cb.isEnum)
                .collect(Collectors.toList());
        cm.vendorExtensions.put("x-cpp-branch-is-enum", branchIsEnumFlags);
        // Preserve descriptor indices when self-referential branches are filtered.
        List<Integer> branchOriginalIndices = composedBranches.stream()
                .map(cb -> cb.originalBranchIndex)
                .collect(Collectors.toList());
        cm.vendorExtensions.put("x-cpp-branch-original-index", branchOriginalIndices);

        if (cm.discriminator != null) {
            cm.vendorExtensions.put("x-has-discriminator", true);
            cm.vendorExtensions.put("x-discriminator-property", cm.discriminator.getPropertyBaseName());
            cm.vendorExtensions.put("x-discriminator-mapping", cm.discriminator.getMapping());
            // Store the resolved type until all aliases are available for
            // discriminator self-reference filtering.
            cm.vendorExtensions.put("x-discriminator-resolved-type", resolvedType);

            // Build discriminator-value to branch-index metadata for diagnostic
            // ordering. Self-referential mappings are omitted.
            if (cm.discriminator != null && cm.discriminator.getMappedModels() != null
                    && !cm.discriminator.getMappedModels().isEmpty()
                    && descriptor != null) {
                // Filter out self-referential MappedModel entries
                Set<CodegenDiscriminator.MappedModel> filtered = new LinkedHashSet<>();
                for (CodegenDiscriminator.MappedModel mm : cm.discriminator.getMappedModels()) {
                    if (mm.getModelName() == null || !mm.getModelName().equals(cm.classname)) {
                        filtered.add(mm);
                    }
                }
                if (!filtered.isEmpty()) {
                    List<Map<String, Object>> discBranchIndex =
                            Oas31CompositionLowering.buildDiscriminatorBranchIndex(
                            filtered, descriptor.getBranches());
                    if (!discBranchIndex.isEmpty()) {
                        cm.vendorExtensions.put("x-discriminator-branch-index", discBranchIndex);
                        cm.vendorExtensions.put("x-has-discriminator-branch-index", true);
                    }
                }
            } else if (descriptor != null && descriptor.hasDiscriminator()) {
                // Fallback: use explicit descriptor mapping when MappedModel unavailable
                List<Map<String, Object>> discBranchIndex =
                        Oas31CompositionLowering.buildDiscriminatorBranchIndex(
                        descriptor.getDiscriminator().getMapping(),
                        descriptor.getBranches());
                if (!discBranchIndex.isEmpty()) {
                    cm.vendorExtensions.put("x-discriminator-branch-index", discBranchIndex);
                    cm.vendorExtensions.put("x-has-discriminator-branch-index", true);
                }
            }
        }

        // Update data type so templates and references use the resolved type
        cm.dataType = resolvedType;
    }

    /**
     * Descriptor-complete path: process a composed model whose composedSchemas
     * were consumed by fromModel, using only the descriptor metadata.
     * Reconstructs ComposedBranch entries from descriptor branch schema names,
     * resolves C++ types, then runs the same lowering/emission pipeline as
     * the normal composedSchemas path.
     */
    private void processComposedModelFromDescriptor(CodegenModel cm,
                                                     CompositionDescriptor desc) {
        List<ComposedBranch> composedBranches = new ArrayList<>();
        List<CompositionBranchDescriptor> descBranches = desc.getBranches();

        for (int bi = 0; bi < descBranches.size(); bi++) {
            CompositionBranchDescriptor db = descBranches.get(bi);
            String resolvedSchemaName = db.getResolvedSchemaName();
            String cppType = resolveOpenApiTypeName(resolvedSchemaName);

            // Skip self-referencing branches
            if (cppType != null && cppType.equals(cm.classname)) {
                continue;
            }
            if (cppType == null) {
                cppType = resolvedSchemaName;
            }
            // Skip self-referencing after fallback
            if (cppType.equals(cm.classname)) {
                continue;
            }

            // Determine isEnum from descriptor assertion metadata
            boolean isEnum = db.getSupportedAssertions().contains("enum");
            boolean isStringLike = "std::string".equals(cppType);
            composedBranches.add(new ComposedBranch(cppType, isEnum, isStringLike, bi));
        }

        List<String> branchTypes = composedBranches.stream()
                .map(cb -> cb.cppType)
                .collect(Collectors.toList());

        String resolvedType;
        try {
            resolvedType = Oas31CompositionLowering.lowerComposedTypes(
                    composedBranches, desc.getKeyword(), desc, LOGGER::warn);
        } catch (RuntimeException e) {
            throw new IllegalStateException(
                    "Failed to lower descriptor-backed model '" + cm.classname + "'", e);
        }

        // Cache the resolved type
        resolvedAliasTypes.put(cm.classname, resolvedType);
        variantModels.add(cm.classname);

        // Populate descriptor storage types, including duplicate-type wrappers.
        boolean hasDuplicateTypes = hasTaggedCompositionBranches(resolvedType);
        Map<String, Object> descTemplateMap = desc.toTemplateMap();
        {
            @SuppressWarnings("unchecked")
            var templateBranches = (List<Map<String, Object>>) descTemplateMap.get("branches");
            // When hasDuplicateTypes, all branches (including null) get
            // CompositionBranchValue wrapping — match shortcut behavior.
            for (int bi = 0; bi < composedBranches.size(); bi++) {
                ComposedBranch cb = composedBranches.get(bi);
                int descIdx = cb.originalBranchIndex;
                if (descIdx >= 0 && descIdx < templateBranches.size()) {
                    Map<String, Object> tBranch = templateBranches.get(descIdx);
                    String storageType;
                    if (hasDuplicateTypes) {
                        storageType = "CompositionBranchValue<" + descIdx
                                + ", " + cb.cppType + ">";
                        tBranch.put("inner-cpp-type", cb.cppType);
                    } else {
                        storageType = cb.cppType;
                    }
                    tBranch.put("storage-cpp-type", storageType);
                }
            }
        }
        descTemplateMap.put("has-duplicate-types", hasDuplicateTypes);

        // Emit vendor extensions
        cm.vendorExtensions.put("x-cpp-type", resolvedType);
        cm.vendorExtensions.put("x-cpp-branches", branchTypes);
        cm.vendorExtensions.put("x-cpp-composed-keyword", desc.getKeyword());
        composedKeywordsByModel.put(cm.classname, desc.getKeyword());
        cm.vendorExtensions.put("x-cpp-composition-branches", descTemplateMap);
        if (hasDuplicateTypes) {
            cm.vendorExtensions.put("x-cpp-has-duplicate-types", true);
        }

        // Preserve branch metadata for transitive alias resolution.
        List<Boolean> branchIsEnumFlags = composedBranches.stream()
                .map(cb -> cb.isEnum)
                .collect(Collectors.toList());
        cm.vendorExtensions.put("x-cpp-branch-is-enum", branchIsEnumFlags);
        List<Integer> branchOriginalIndices = composedBranches.stream()
                .map(cb -> cb.originalBranchIndex)
                .collect(Collectors.toList());
        cm.vendorExtensions.put("x-cpp-branch-original-index", branchOriginalIndices);

        if (desc.hasDiscriminator()) {
            cm.vendorExtensions.put("x-has-discriminator", true);
            cm.vendorExtensions.put("x-discriminator-property",
                    desc.getDiscriminator().getPropertyName());
            cm.vendorExtensions.put("x-discriminator-mapping",
                    desc.getDiscriminator().getMapping());
            cm.vendorExtensions.put("x-discriminator-resolved-type", resolvedType);

            // Prefer complete mapped-model metadata for discriminator ordering and
            // fall back to explicit descriptor mappings. Omit self-references.
            if (cm.discriminator != null && cm.discriminator.getMappedModels() != null
                    && !cm.discriminator.getMappedModels().isEmpty()) {
                // Filter out self-referential MappedModel entries
                Set<CodegenDiscriminator.MappedModel> filtered = new LinkedHashSet<>();
                for (CodegenDiscriminator.MappedModel mm : cm.discriminator.getMappedModels()) {
                    if (mm.getModelName() == null || !mm.getModelName().equals(cm.classname)) {
                        filtered.add(mm);
                    }
                }
                if (!filtered.isEmpty()) {
                    List<Map<String, Object>> discBranchIndex =
                            Oas31CompositionLowering.buildDiscriminatorBranchIndex(
                            filtered, descBranches);
                    if (!discBranchIndex.isEmpty()) {
                        cm.vendorExtensions.put("x-discriminator-branch-index", discBranchIndex);
                        cm.vendorExtensions.put("x-has-discriminator-branch-index", true);
                    }
                }
            } else if (desc.hasDiscriminator()) {
                // Fallback: use explicit descriptor mapping when MappedModel unavailable
                List<Map<String, Object>> discBranchIndex =
                        Oas31CompositionLowering.buildDiscriminatorBranchIndex(
                        desc.getDiscriminator().getMapping(),
                        descBranches);
                if (!discBranchIndex.isEmpty()) {
                    cm.vendorExtensions.put("x-discriminator-branch-index", discBranchIndex);
                    cm.vendorExtensions.put("x-has-discriminator-branch-index", true);
                }
            }
        }

        cm.dataType = resolvedType;
    }

    /** Branch metadata used by ordered composition lowering. */
    static final class ComposedBranch {
        final String cppType;
        final boolean isEnum;
        final boolean isStringLike;
        /** Index into the CompositionDescriptor branch list.
         *  -1 means no descriptor alignment (fallback path). */
        final int originalBranchIndex;

        ComposedBranch(String cppType, boolean isEnum, boolean isStringLike,
                       int originalBranchIndex) {
            this.cppType = cppType;
            this.isEnum = isEnum;
            this.isStringLike = isStringLike;
            this.originalBranchIndex = originalBranchIndex;
        }
    }

    private List<Map<String, Object>> buildTypeErasedOneOfBranches(
            CodegenModel codegenModel, Map<String, CodegenModel> allModels) {
        List<Map<String, Object>> validationBranches = new ArrayList<>();
        for (CodegenProperty branch : codegenModel.getComposedSchemas().getOneOf()) {
            String originalType = stripSharedPtr(branch.dataType);
            CodegenModel referencedModel = allModels.get(originalType);
            String resolvedType = resolveThroughAliases(originalType);
            if (referencedModel != null && referencedModel.dataType != null) {
                resolvedType = resolveThroughAliases(stripSharedPtr(referencedModel.dataType));
            }
            resolvedType = resolveOpenApiTypeName(resolvedType);

            Map<String, Object> validationBranch = new LinkedHashMap<>();
            if ("std::string".equals(resolvedType)) {
                validationBranch.put("is-string", true);
                List<Object> enumValues = getEnumValues(branch, referencedModel);
                if (!enumValues.isEmpty()) {
                    validationBranch.put("has-enum-values", true);
                    List<Map<String, String>> escapedValues = new ArrayList<>();
                    for (Object enumValue : enumValues) {
                        escapedValues.add(Collections.singletonMap(
                                "literal", escapeCppStringContent(String.valueOf(enumValue))));
                    }
                    validationBranch.put("enum-values", escapedValues);
                }
            } else if ("bool".equals(resolvedType)) {
                validationBranch.put("is-boolean", true);
            } else if ("std::int32_t".equals(resolvedType) || "int32_t".equals(resolvedType)) {
                validationBranch.put("is-int32", true);
            } else if ("std::int64_t".equals(resolvedType) || "int64_t".equals(resolvedType)) {
                validationBranch.put("is-integer", true);
            } else if ("double".equals(resolvedType) || "float".equals(resolvedType)) {
                validationBranch.put("is-number", true);
            } else if ("std::nullptr_t".equals(resolvedType)) {
                validationBranch.put("is-null", true);
            } else if (resolvedType != null && resolvedType.startsWith("std::vector<")) {
                validationBranch.put("is-array", true);
            } else if (resolvedType != null
                    && (resolvedType.startsWith("std::map<")
                    || (!resolvedType.startsWith("std::")
                    && !resolvedType.startsWith("boost::")))) {
                validationBranch.put("is-object", true);
            } else {
                validationBranch.put("is-any", true);
            }
            validationBranches.add(validationBranch);
        }
        return validationBranches;
    }

    @SuppressWarnings("unchecked")
    private static List<Object> getEnumValues(
            CodegenProperty branch, CodegenModel referencedModel) {
        Map<String, Object> allowableValues = branch.allowableValues;
        if ((allowableValues == null || allowableValues.get("values") == null)
                && referencedModel != null) {
            allowableValues = referencedModel.allowableValues;
        }
        if (allowableValues == null || !(allowableValues.get("values") instanceof List)) {
            return Collections.emptyList();
        }
        return (List<Object>) allowableValues.get("values");
    }

    static String escapeCppStringContent(String value) {
        if (value == null) {
            return "";
        }
        for (int index = 0; index < value.length(); ++index) {
            char constUnit = value.charAt(index);
            if (Character.isHighSurrogate(constUnit)) {
                if (index + 1 >= value.length()
                        || !Character.isLowSurrogate(value.charAt(index + 1))) {
                    throw new IllegalArgumentException(
                            "Cannot emit an unpaired UTF-16 high surrogate");
                }
                ++index;
            } else if (Character.isLowSurrogate(constUnit)) {
                throw new IllegalArgumentException(
                        "Cannot emit an unpaired UTF-16 low surrogate");
            }
        }

        byte[] utf8 = value.getBytes(java.nio.charset.StandardCharsets.UTF_8);
        StringBuilder escaped = new StringBuilder(utf8.length);
        for (byte encoded : utf8) {
            int character = Byte.toUnsignedInt(encoded);
            switch (character) {
                case '\\':
                    escaped.append("\\\\");
                    break;
                case '"':
                    escaped.append("\\\"");
                    break;
                case '\n':
                    escaped.append("\\n");
                    break;
                case '\r':
                    escaped.append("\\r");
                    break;
                case '\t':
                    escaped.append("\\t");
                    break;
                case '\b':
                    escaped.append("\\b");
                    break;
                case '\f':
                    escaped.append("\\f");
                    break;
                default:
                    if (character >= 0x20 && character <= 0x7e) {
                        escaped.append((char) character);
                    } else {
                        // Three-digit octal escapes cannot absorb following
                        // hexadecimal characters and preserve exact UTF-8 bytes.
                        escaped.append('\\')
                                .append((char) ('0' + ((character >>> 6) & 7)))
                                .append((char) ('0' + ((character >>> 3) & 7)))
                                .append((char) ('0' + (character & 7)));
                    }
                    break;
            }
        }
        return escaped.toString();
    }

    /**
     * Converts an arbitrary schema name into a valid C++ identifier for use
     * in generated validator function names. Replaces non-alphanumeric
     * characters with underscores and ensures the result starts with a letter.
     */
    static String toValidIdentifier(String name) {
        if (name == null || name.isEmpty()) {
            return "schema";
        }
        StringBuilder sb = new StringBuilder(name.length());
        for (int i = 0; i < name.length(); i++) {
            char c = name.charAt(i);
            if (Character.isLetterOrDigit(c) || c == '_') {
                sb.append(c);
            } else {
                sb.append('_');
            }
        }
        String result = sb.toString();
        if (!result.isEmpty() && !Character.isLetter(result.charAt(0))
                && result.charAt(0) != '_') {
            result = "_" + result;
        }
        return result.isEmpty() ? "schema" : result;
    }

    /** Returns the schema IR id for a raw component schema name. */
    static String componentSchemaId(String schemaName) {
        return toValidIdentifier(schemaName) + "_component";
    }

    /**
     * Thrown during generation when a schema branch has assertion keywords that
     * can affect composition membership but no generated validator exists.
     * Carries the schema location, keyword, and remediation guidance.
     */
    public static final class UnsupportedSchemaAssertionException
            extends RuntimeException {
        private final String schemaLocation;
        private final String assertionKeyword;

        public UnsupportedSchemaAssertionException(
                String schemaLocation, String assertionKeyword) {
            super(buildMessage(schemaLocation, assertionKeyword));
            this.schemaLocation = schemaLocation;
            this.assertionKeyword = assertionKeyword;
        }

        public String getSchemaLocation() { return schemaLocation; }
        public String getAssertionKeyword() { return assertionKeyword; }

        private static String buildMessage(
                String schemaLocation, String assertionKeyword) {
            return "Unsupported schema assertion '" + assertionKeyword
                    + "' at " + schemaLocation
                    + ". This keyword can affect composition membership but "
                    + "no generated validator exists. Add support in a later generator "
                    + "version, or restructure the schema to avoid this keyword.";
        }
    }

    /**
     * Exception thrown when an allOf intersection produces an unsatisfiable
     * result on a required property, preventing model generation.
     */
    public static final class AllOfRequiredUnsatisfiableException
            extends RuntimeException {
        private final String schemaName;
        private final String reason;

        public AllOfRequiredUnsatisfiableException(
                String schemaName, String reason) {
            super(buildMessage(schemaName, reason));
            this.schemaName = schemaName;
            this.reason = reason;
        }

        public String getSchemaName() { return schemaName; }
        public String getReason() { return reason; }

        private static String buildMessage(
                String schemaName, String reason) {
            return "Unsatisfiable allOf intersection for schema '"
                    + schemaName + "': " + reason;
        }
    }

    /**
     * Resolves a type name transitively through the resolvedAliasTypes map.
     * For example, if ModelIdsResponses → std::string and ModelIdsShared → std::string,
     * then resolveThroughAliases("ModelIdsResponses") returns "std::string".
     * <p>
     * Cyclic alias maps fail generation; an unmapped type is returned unchanged.
     */
    private String resolveThroughAliases(String typeName) {
        if (typeName == null) {
            return null;
        }
        Set<String> visited = new HashSet<>();
        String current = typeName;
        while (true) {
            String resolved = resolvedAliasTypes.get(current);
            if (resolved == null || resolved.equals(current)) {
                return current;
            }
            if (!visited.add(current)) {
                throw new IllegalStateException(
                        "Cyclic resolved alias chain starting at '" + typeName + "'");
            }
            current = resolved;
        }
    }

    /**
     * Detects whether a schema is a null union (anyOf/oneOf with [T, null] or [null, T])
     * that should lower to std::optional&lt;T&gt;. Returns the lowered type string,
     * or null if the schema is not a simple null union.
     */
    private String detectNullUnion(Schema schema, String className) {
        // Use raw List and cast explicitly because Schema is unparameterized.
        List anyOfRaw = schema.getAnyOf();
        List oneOfRaw = schema.getOneOf();
        List<Schema> branches = null;
        if (anyOfRaw != null && !anyOfRaw.isEmpty()) {
            branches = anyOfRaw;
        } else if (oneOfRaw != null && !oneOfRaw.isEmpty()) {
            branches = oneOfRaw;
        }
        if (branches == null) {
            return null;
        }
        if (branches.size() != 2) {
            return null;
        }

        // Find the non-null branch using ModelUtils for correct null-type detection
        // (handles both OAS 3.0 nullable and OAS 3.1 type: "null")
        Schema nonNullBranch = null;
        for (Object brObj : branches) {
            Schema branch = (Schema) brObj;
            if (!ModelUtils.isNullType(branch)) {
                nonNullBranch = branch;
            }
        }
        if (nonNullBranch == null) {
            return null; // Both branches are null
        }
        // Verify exactly one null branch exists
        long nullBranchCount = 0;
        for (Object brObj : branches) {
            if (ModelUtils.isNullType((Schema) brObj)) nullBranchCount++;
        }
        if (nullBranchCount != 1) {
            return null;
        }

        // Resolve the non-null branch type. For $ref schemas, resolve to model name.
        String nonNullType;
        if (nonNullBranch.get$ref() != null) {
            nonNullType = ModelUtils.getSimpleRef(nonNullBranch.get$ref());
        } else {
            nonNullType = getTypeDeclaration(nonNullBranch);
        }

        // Avoid self-referencing optional (optional of the model itself)
        if (nonNullType.equals(className)) {
            return "boost::json::value";
        }

        return "std::optional<" + nonNullType + ">";
    }

    /**
     * Recursively strips {@code std::shared_ptr<X>} wrappers from a type string.
     * <ul>
     *   <li>{@code std::shared_ptr<Foo>} → {@code Foo}</li>
     *   <li>{@code std::vector<std::shared_ptr<Foo>>} → {@code std::vector<Foo>}</li>
     *   <li>{@code std::map<std::string, std::shared_ptr<Foo>>} → {@code std::map<std::string, Foo>}</li>
     *   <li>{@code std::string} → {@code std::string} (unchanged)</li>
     * </ul>
     */
    private static String stripSharedPtr(String type) {
        if (type == null) {
            return null;
        }
        // Direct std::shared_ptr<X> wrapper — extract inner type and recurse.
        if (type.startsWith("std::shared_ptr<") && type.endsWith(">")) {
            return stripSharedPtr(type.substring(16, type.length() - 1));
        }
        // Check for template arguments (contains '<' and '>').
        int firstLt = type.indexOf('<');
        int lastGt = type.lastIndexOf('>');
        if (firstLt > 0 && lastGt > firstLt) {
            // Split arguments at commas at depth 0 (not inside nested angle brackets).
            String prefix = type.substring(0, firstLt);
            String argsSection = type.substring(firstLt + 1, lastGt);
            List<String> args = splitTemplateArgs(argsSection);
            for (int i = 0; i < args.size(); i++) {
                args.set(i, stripSharedPtr(args.get(i).trim()));
            }
            return prefix + "<" + String.join(", ", args) + ">";
        }
        return type;
    }

    /**
     * Splits a comma-separated template argument list, respecting nested angle brackets.
     * {@code "std::string, std::shared_ptr<Foo>"} → {@code ["std::string", "std::shared_ptr<Foo>"]}
     */
    private static List<String> splitTemplateArgs(String args) {
        List<String> result = new ArrayList<>();
        int depth = 0;
        int start = 0;
        for (int i = 0; i < args.length(); i++) {
            char c = args.charAt(i);
            if (c == '<') {
                depth++;
            } else if (c == '>') {
                depth--;
            } else if (c == ',' && depth == 0) {
                result.add(args.substring(start, i));
                start = i + 1;
            }
        }
        result.add(args.substring(start));
        return result;
    }

    /**
     * Extracts the inner type from a std::optional<T> type declaration, correctly
     * handling nested angle brackets.
     * <ul>
     *   <li>{@code std::optional<std::string>} → {@code std::string}</li>
     *   <li>{@code std::optional<std::vector<int>>} → {@code std::vector<int>}</li>
     *   <li>{@code std::optional<MyModel>} → {@code MyModel}</li>
     *   <li>{@code std::string} → {@code null}</li>
     * </ul>
     *
     * @return the inner type, or null if the input does not start with "std::optional<"
     */
    private static String extractOptionalInnerType(String type) {
        if (type == null || !type.startsWith("std::optional<")) {
            return null;
        }
        // Strip prefix "std::optional<" (14 chars) and find matching '>'
        int depth = 0;
        int start = 14; // length of "std::optional<"
        for (int i = start; i < type.length(); i++) {
            char c = type.charAt(i);
            if (c == '<') {
                depth++;
            } else if (c == '>') {
                if (depth == 0) {
                    return type.substring(start, i);
                }
                depth--;
            }
        }
        return null;
    }

    /**
     * Camelize the method name of the getter and setter, but keep underscores at the front
     *
     * @param name string to be camelized
     * @return Camelized string
     */
    @Override
    public String getterAndSetterCapitalize(String name) {
        if (name == null || name.length() == 0) {
            return name;
        }

        name = toVarName(name);

        if (name.startsWith("_")) {
            return "_" + camelize(name);
        }

        return camelize(name);
    }

    private static boolean isSchemaValidationSupportingFile(SupportingFile file) {
        String destination = file.getDestinationFilename();
        return "Oas31SchemaRegistry.h".equals(destination)
                || "schema_ir.generated.cpp".equals(destination)
                || "schema_validate.generated.cpp".equals(destination)
                || (destination.startsWith("schema_ir.generated.chunk")
                        && destination.endsWith(".cpp"));
    }

    @Override
    public void processOpts() {
        super.processOpts();
        packageName = additionalProperties.getOrDefault(
                CodegenConstants.PACKAGE_NAME, DEFAULT_PACKAGE_NAME).toString();
        if (StringUtils.isBlank(packageName)) {
            throw new IllegalArgumentException("packageName must not be blank");
        }
        additionalProperties.put(CodegenConstants.PACKAGE_NAME, packageName);
        String modelNamespace = modelPackage.replaceAll("\\.", "::");
        additionalProperties.put("modelNamespaceDeclarations", modelPackage.split("\\."));
        additionalProperties.put("modelNamespace", modelNamespace);
        additionalProperties.put("schemaValidationNamespace",
                modelNamespace + "::detail::schema_validation");
        additionalProperties.put("schemaValidationHeaderGuardPrefix",
                modelPackage.replaceAll("[^A-Za-z0-9]", "_").toUpperCase(Locale.ROOT));
        additionalProperties.put("apiNamespaceDeclarations", apiPackage.split("\\."));
        additionalProperties.put("apiNamespace", apiPackage.replaceAll("\\.", "::"));

        if (additionalProperties.containsKey("formatAssertionPolicy")) {
            String policy = additionalProperties.get("formatAssertionPolicy")
                    .toString().trim().toLowerCase(Locale.ROOT);
            if (!FORMAT_ASSERTION_POLICY_ANNOTATION.equals(policy)) {
                throw new IllegalArgumentException(
                        "formatAssertionPolicy supports only 'annotation'; "
                                + "format assertions are not implemented");
            }
        }
        formatAssertionPolicy = FORMAT_ASSERTION_POLICY_ANNOTATION;
        additionalProperties.put("formatAssertionPolicy", formatAssertionPolicy);

        // Configure whether SSE schemas describe the wire representation or the
        // parsed JSON event data. Unknown values use the documented default.
        if (additionalProperties.containsKey("sseSchemaMode")) {
            String raw = additionalProperties.get("sseSchemaMode").toString().trim();
            if (raw.equalsIgnoreCase(SSE_SCHEMA_MODE_JSON_EVENT_DATA)) {
                sseSchemaMode = SSE_SCHEMA_MODE_JSON_EVENT_DATA;
            } else if (raw.equalsIgnoreCase(SSE_SCHEMA_MODE_REPRESENTATION)) {
                sseSchemaMode = SSE_SCHEMA_MODE_REPRESENTATION;
            } else {
                LOGGER.warn("sseSchemaMode: unknown value '{}'; falling back to '{}'",
                        raw, SSE_SCHEMA_MODE_REPRESENTATION);
            }
        }
        additionalProperties.put("sseSchemaMode", sseSchemaMode);

        // compileWithValidation controls decode-time composition-branch checks.
        // Representation safety checks remain active regardless of this option.
        if (additionalProperties.containsKey("compileWithValidation")) {
            Object raw = additionalProperties.get("compileWithValidation");
            if (raw instanceof Boolean) {
                validateOnDecode = (Boolean) raw;
            } else {
                validateOnDecode = Boolean.parseBoolean(raw.toString().trim());
            }
        }
        additionalProperties.put("validateOnDecode", validateOnDecode);
        additionalProperties.put("compileWithValidation", validateOnDecode);
        if (!validateOnDecode) {
            supportingFiles.removeIf(CppBoostBeastClientCodegen::isSchemaValidationSupportingFile);
        }
        if (additionalProperties.containsKey("tolerateNonNullableNulls")) {
            Object raw = additionalProperties.get("tolerateNonNullableNulls");
            if (raw instanceof Boolean) {
                tolerateNonNullableNulls = (Boolean) raw;
            } else {
                tolerateNonNullableNulls = Boolean.parseBoolean(raw.toString().trim());
            }
        }
        additionalProperties.put("tolerateNonNullableNulls", tolerateNonNullableNulls);
    }

    /**
     * Location to write model files. You can use the modelPackage() as defined
     * when the class is instantiated
     */
    @Override
    public String modelFileFolder() {
        return (outputFolder + "/model").replace("/", File.separator);
    }

    /**
     * Location to write api files. You can use the apiPackage() as defined when
     * the class is instantiated
     */
    @Override
    public String apiFileFolder() {
        return (outputFolder + "/api").replace("/", File.separator);
    }

    @Override
    public String toModelImport(String name) {
        if (importMapping.containsKey(name)) {
            return importMapping.get(name);
        } else {
            return "#include \"" + name + ".h\"";
        }
    }

    @Override
    public CodegenModel fromModel(String name, Schema model) {
        // Flatten allOf into a synthetic schema with intersected properties and
        // unioned required names. Clearing allOf gives every property direct owned
        // storage rather than generated inheritance.
        Schema modelArg = model;
        if (model != null && model.getAllOf() != null && !model.getAllOf().isEmpty()) {
            AllOfIntersection intersection = allOfIntersections.get(
                    toModelName(name));
            if (intersection != null) {
                // Check for unsatisfiable required properties / scalar conflicts
                if (!intersection.isSatisfiable()) {
                    throw new AllOfRequiredUnsatisfiableException(
                            name, intersection.getUnsatisfiableReason());
                }

                Schema synthetic = Oas31CompositionLowering.buildSyntheticAllOfSchema(
                        name, intersection);
                // Copy top-level attributes from original model
                if (model.getDiscriminator() != null) {
                    synthetic.setDiscriminator(model.getDiscriminator());
                }
                if (Boolean.TRUE.equals(model.getNullable())) {
                    synthetic.setNullable(true);
                }
                if (model.getDescription() != null) {
                    synthetic.setDescription(model.getDescription());
                }
                if (model.getFormat() != null && intersection.getRootScalarType() != null) {
                    synthetic.setFormat(model.getFormat());
                }
                // Optional impossible properties retain their API surface but
                // reject any JSON object in which they are present.
                if (!intersection.getOptionalImpossibleProperties().isEmpty()) {
                    Map<String, Object> ext = synthetic.getExtensions();
                    if (ext == null) {
                        ext = new LinkedHashMap<>();
                        synthetic.setExtensions(ext);
                    }
                    ext.put("x-cpp-optional-impossible-properties",
                            new ArrayList<>(intersection.getOptionalImpossibleProperties()));
                }
                // Flat: allOf = null so super.fromModel sees no parent
                synthetic.setAllOf(null);
                modelArg = synthetic;
            }
        }

        // Pre-check: The OpenAPI 3.1 parser converts anyOf [T, null] into
        // {type: T, nullable: true} or {$ref: X, nullable: true}, consuming
        // the anyOf list.  Detect these nullable schemas and produce the
        // correct std::optional<T> type.
        //
        // For $ref schemas (normalised anyOf/oneOf [T, null] where T was a
        // $ref), getTypeDeclaration resolves the target and returns the
        // correct C++ type.  For arrays, getTypeDeclaration returns the
        // container type (e.g. std::vector<...>) without optional wrapping,
        // so we wrap it here.  Inline object schemas (type=object, no $ref)
        // are full class models — they stay out of the alias precomputation
        // because getTypeDeclaration would return the raw OAS type name
        // "object" instead of the model name.  They are handled separately
        // below via variant model registration.
        boolean isNullableSchema = model != null
            && Boolean.TRUE.equals(model.getNullable())
            && (model.get$ref() != null
                || (model.getType() != null && !"object".equals(model.getType())));
        String preComputedNullUnionType = null;
        if (isNullableSchema) {
            // Resolve the type to its C++ type and wrap in std::optional
            String innerType = getTypeDeclaration(model);
            // getTypeDeclaration already returns std::optional<T> for nullable.
            // Use it directly if it starts with std::optional<.
            if (innerType.startsWith("std::optional<")) {
                preComputedNullUnionType = innerType;
            } else {
                preComputedNullUnionType = "std::optional<" + innerType + ">";
            }
        } else if (model != null) {
            // Also try the anyOf/oneOf path for cases where the parser
            // preserved the composed schema structure.
            preComputedNullUnionType = detectNullUnion(model, name);
        }

        CodegenModel codegenModel = super.fromModel(name, modelArg);
        if (codegenModel == null) {
            return null;
        }

        codegenModel.vendorExtensions.put(
                "x-cpp-component-schema-id", componentSchemaId(name));

        // Post-check: Apply the pre-computed null union type if the default
        // pipeline consumed the composed schemas.
        if (preComputedNullUnionType != null) {
            codegenModel.dataType = preComputedNullUnionType;
            codegenModel.vendorExtensions.put("x-cpp-type", preComputedNullUnionType);
            codegenModel.vendorExtensions.put("x-cpp-composed-keyword",
                model.getAnyOf() != null ? "anyOf" : "oneOf");
            codegenModel.vendorExtensions.put("x-cpp-is-alias", true);
            codegenModel.vendorExtensions.put("x-cpp-is-optional", true);
            // Force a model header/source so Gate A inventory and $ref users get
            // `using NullableString = std::optional<std::string>;`. DefaultCodegen
            // marks plain nullable primitives as isAlias and skips file emission.
            codegenModel.isAlias = false;
            resolvedAliasTypes.put(name, preComputedNullUnionType);
            variantModels.add(name);
        }

        // Post-check: Inline nullable object schemas (type=object, nullable=true,
        // no $ref) are full class models with properties — they cannot use the
        // alias path. Register them as variant models so $ref references use value
        // semantics (std::shared_ptr<NullableObject> → NullableObject) and tag
        // the model as optional for correct null-value representation.
        if (model != null && model.get$ref() == null
                && "object".equals(model.getType())
                && Boolean.TRUE.equals(model.getNullable())) {
            variantModels.add(name);
            codegenModel.vendorExtensions.put("x-cpp-is-optional", true);
        }

        Set<String> oldImports = codegenModel.imports;
        codegenModel.imports = new HashSet<>();
        for (String imp : oldImports) {
            String newImp = toModelImport(imp);
            if (!newImp.isEmpty()) {
                codegenModel.imports.add(newImp);
            }
        }
        // Every model header declares vector conversion helpers.
        codegenModel.imports.add("#include <vector>");

        // Fixed-const properties: OAS 3.1 `const`, single-value `enum`, or optional
        // vendor extension `x-stainless-const`. Portable path is OAS `const` / single enum —
        // vendor extensions are never required for correct encode/decode.
        if (codegenModel.vars != null) {
            Map<String, Schema> allProps = new LinkedHashMap<>();
            if (model.getProperties() != null) {
                allProps.putAll(model.getProperties());
            }
            if (model.getAllOf() != null && openAPI != null) {
                for (Object parentObj : model.getAllOf()) {
                    if (parentObj instanceof Schema) {
                        Schema parentSchema = ModelUtils.getReferencedSchema(
                                openAPI, (Schema) parentObj);
                        if (parentSchema != null && parentSchema.getProperties() != null) {
                            allProps.putAll(parentSchema.getProperties());
                        }
                    }
                }
            }
            for (CodegenProperty var : codegenModel.vars) {
                Object rawProp = allProps.get(var.baseName);
                if (!(rawProp instanceof Schema)) {
                    continue;
                }
                Schema varSchema = (Schema) rawProp;
                boolean hasOasConst = varSchema.getConst() != null;
                boolean hasSingleValueEnum = varSchema.getEnum() != null
                        && varSchema.getEnum().size() == 1;
                boolean hasStainlessConst = varSchema.getExtensions() != null
                        && Boolean.TRUE.equals(varSchema.getExtensions().get("x-stainless-const"));
                if (!(hasOasConst || hasSingleValueEnum || hasStainlessConst)) {
                    continue;
                }
                String constRawValue = null;
                if (varSchema.getConst() != null) {
                    constRawValue = varSchema.getConst().toString();
                } else if (varSchema.getEnum() != null && !varSchema.getEnum().isEmpty()) {
                    constRawValue = varSchema.getEnum().get(0).toString();
                }
                if (constRawValue == null && var.example != null) {
                    constRawValue = var.example;
                }
                if (constRawValue == null) {
                    constRawValue = "std::string".equals(var.dataType) ? "" : "0";
                }
                String inlineValue;
                boolean isStringConst = "std::string".equals(var.dataType)
                        || "std::optional<std::string>".equals(var.dataType)
                        || (var.isString && !var.isInteger && !var.isLong && !var.isNumber
                        && !var.isBoolean);
                if ("std::optional<std::string>".equals(var.dataType)) {
                    inlineValue = "std::optional<std::string>{\""
                            + escapeCppStringContent(constRawValue) + "\"}";
                } else if (isStringConst || "std::string".equals(var.dataType)) {
                    inlineValue = "\"" + escapeCppStringContent(constRawValue) + "\"";
                } else {
                    inlineValue = constRawValue;
                }
                // Neutral OAS-first flag used by templates.
                var.vendorExtensions.put("x-cpp-const", true);
                var.vendorExtensions.put("x-cpp-const-value", constRawValue);
                var.vendorExtensions.put("x-cpp-const-inline-value", inlineValue);
                // Mustache is truthy on key presence — only set when string-typed.
                if (isStringConst || "std::string".equals(var.dataType)
                        || "std::optional<std::string>".equals(var.dataType)) {
                    var.vendorExtensions.put("x-cpp-const-is-string", true);
                } else if (var.isBoolean || "bool".equals(var.dataType)
                        || "std::optional<bool>".equals(var.dataType)) {
                    var.vendorExtensions.put("x-cpp-const-is-boolean", true);
                }
                // Keep stainless keys as aliases so older template forks still work.
                var.vendorExtensions.put("x-stainless-const", true);
                var.vendorExtensions.put("x-stainless-const-value", constRawValue);
                var.vendorExtensions.put("x-stainless-const-inline-value", inlineValue);
            }
        }

        addContainerPropertyNames(codegenModel.vars);
        return codegenModel;
    }

    @Override
    public CodegenParameter fromParameter(Parameter parameter, Set<String> imports) {
        CodegenParameter codegenParameter = super.fromParameter(parameter, imports);
        // Preserve serialization facts for every parameter location.
        codegenParameterStyled(parameter, codegenParameter);
        if (!codegenParameter.isQueryParam) {
            return codegenParameter;
        }

        if (!codegenParameter.required) {
            codegenParameter.vendorExtensions.put(X_CODEGEN_IS_OPTIONAL_QUERY_PARAMETER, true);
        }
        return codegenParameter;
    }

    /**
     * Records the OAS 3.1 serialization facts consumed by the C++ wire layer.
     * Style defaults to form for query/cookie and simple for path/header. Explode
     * defaults to true only for form. allowReserved is surfaced consistently;
     * allowEmptyValue applies only to form-style query parameters.
     */
    private void codegenParameterStyled(Parameter parameter,
                                        CodegenParameter codegenParameter) {
        String style = parameter.getStyle() == null
                ? null : parameter.getStyle().toString();
        if (style == null) {
            if (codegenParameter.isQueryParam || codegenParameter.isCookieParam) {
                style = "form";
            } else {
                style = "simple";   // path, header
            }
        }
        Boolean explode = Boolean.TRUE.equals(parameter.getExplode());
        if (parameter.getExplode() == null) {
            explode = "form".equals(style);     // spec default
        }
        codegenParameter.vendorExtensions.put(X_CODEGEN_PARAM_STYLE, style);
        codegenParameter.vendorExtensions.put(X_CODEGEN_PARAM_EXPLODE, explode);
        codegenParameter.vendorExtensions.put(X_CODEGEN_PARAM_ALLOW_RESERVED,
                Boolean.TRUE.equals(parameter.getAllowReserved()));
        codegenParameter.vendorExtensions.put(X_CODEGEN_PARAM_ALLOW_EMPTY_VALUE,
                Boolean.TRUE.equals(parameter.getAllowEmptyValue()));
    }

    private String queryCollectionDelimiter(Parameter.StyleEnum style) {
        if (style == Parameter.StyleEnum.SPACEDELIMITED) {
            return "%20";
        }
        if (style == Parameter.StyleEnum.PIPEDELIMITED) {
            return "%7C";
        }
        return ",";
    }

    private void addContainerPropertyNames(List<CodegenProperty> properties) {
        for (CodegenProperty property : properties) {
            CodegenProperty item = property.items;
            while (item != null) {
                item.vendorExtensions.put("x-container-property-name", property.name);
                item = item.items;
            }
        }
    }

    @Override
    public String toModelFilename(String name) {
        return toModelName(name);
    }

    @Override
    public String toApiFilename(String name) {
        return toApiName(name);
    }

    @Override
    public OperationsMap postProcessOperationsWithModels(
            OperationsMap objs, List<ModelMap> allModels) {
        return new CppBoostBeastTemplateModelAssembler(
                sourceOpenApi,
                webhookPreservation,
                operationCallbacks,
                operationLinks,
                composedKeywordsByModel,
                sseSchemaMode).assemble(objs, allModels);
    }


    /**
     * Optional - type declaration. This is a String which is used by the
     * templates to instantiate your types. There is typically special handling
     * for different property types
     *
     * @return a string value used as the `dataType` field for model templates,
     * `returnType` for api templates
     */
    @Override
    public String getTypeDeclaration(Schema p) {
        // Handle inline oneOf/anyOf composed schemas (apply lowering rules directly)
        if (ModelUtils.isComposedSchema(p) && (p.getOneOf() != null || p.getAnyOf() != null)) {
            return lowerInlineComposedSchema(p);
        }

        String openAPIType = getSchemaType(p);

        if (ModelUtils.isArraySchema(p)) {
            // Use getItems() directly to handle both OpenAPI 3.0 and 3.1
            Schema inner = p.getItems();
            String arrayType;
            if (inner != null) {
                arrayType = getSchemaType(p) + "<" + getTypeDeclaration(inner) + ">";
            } else {
                arrayType = "std::vector<boost::json::value>";
            }
            // Nullable arrays must be wrapped in std::optional so null JSON
            // values are representable. The array branch returns before the
            // nullable fallback checks at the end of this method.
            if (ModelUtils.isNullable(p)) {
                return "std::optional<" + arrayType + ">";
            }
            return arrayType;
        } else if (ModelUtils.isMapSchema(p)) {
            Schema inner = ModelUtils.getAdditionalProperties(p);
            String innerType = inner == null ? "boost::json::value" : getTypeDeclaration(inner);
            String mapType = getSchemaType(p) + "<std::string, " + innerType + ">";
            // Nullable maps must be wrapped in std::optional so null JSON
            // values are representable. The map branch returns before the
            // nullable fallback checks at the end of this method.
            if (ModelUtils.isNullable(p)) {
                return "std::optional<" + mapType + ">";
            }
            return mapType;
        } else if (ModelUtils.isByteArraySchema(p)) {
            return "std::string";
        } else if (ModelUtils.isStringSchema(p)
                || ModelUtils.isDateSchema(p)
                || ModelUtils.isDateTimeSchema(p) || ModelUtils.isFileSchema(p)
                || languageSpecificPrimitives.contains(openAPIType)
                || typeMapping.containsKey(openAPIType)
                || typeMapping.values().contains(openAPIType)) {
            // Resolve through type mapping for scalar allOf: composed schemas
            // return OAS raw types (e.g. "string") or mapped types (e.g.
            // "std::string") depending on branch resolution path.
            // Re-map if the value is already in the type mapping values.
            String resolved = typeMapping.containsKey(openAPIType)
                    ? typeMapping.get(openAPIType)
                    : toModelName(openAPIType);
            // OAS 3.0 nullable: true → std::optional<T>
            if (ModelUtils.isNullable(p)) {
                return "std::optional<" + resolved + ">";
            }
            return resolved;
        } else if (ModelUtils.isNullType(p)) {
            // Handle OpenAPI 3.1 null type
            return "std::nullptr_t";
        } else if (ModelUtils.isAnyType(p) || ModelUtils.isFreeFormObject(p, openAPI)) {
            return "boost::json::value";
        }

        // OAS 3.0 nullable: true → std::optional<T>
        if (ModelUtils.isNullable(p)) {
            return "std::optional<" + openAPIType + ">";
        }

        // Variant models use value semantics (no shared_ptr wrapping)
        if (variantModels.contains(openAPIType)) {
            return openAPIType;
        }

        // Object references use shared ownership because circular-reference facts
        // are unavailable when this declaration is computed. Variant aliases are
        // handled above as value types.
        return "std::shared_ptr<" + openAPIType + ">";
    }

    /**
     * Resolves an inline oneOf/anyOf schema to its lowered C++ type by computing
     * branch types and applying the same ordered lowering rules as model-level types.
     */
    private String lowerInlineComposedSchema(Schema p) {
        String composedKeyword;
        List<Schema> children;
        if (p.getOneOf() != null) {
            children = p.getOneOf();
            composedKeyword = "oneOf";
        } else {
            children = p.getAnyOf();
            composedKeyword = "anyOf";
        }

        List<ComposedBranch> composedBranches = new ArrayList<>();
        for (Schema child : children) {
            // Compute the branch type using the full type declaration pipeline
            // but strip shared_ptr for variant members (value semantics).
            String childType = stripSharedPtr(getTypeDeclaration(child));
            // Resolve $ref targets that are aliased to primitive types at the
            // declaration point, before resolvedAliasTypes is available (it is
            // populated during postProcessModels, which runs later). This handles
            // inline schemas like CreateAssistantRequest_model = oneOf [string,
            // $ref AssistantSupportedModels] where the target is anyOf [string,
            // string-enum] → std::string, collapsing to just std::string.
            Schema resolvedChild = child;
            if (!childType.startsWith("std::") && !childType.startsWith("boost::")
                    && !childType.startsWith("std::shared_ptr<")) {
                Schema resolvedTarget = child.get$ref() != null && openAPI != null
                        ? ModelUtils.getReferencedSchema(openAPI, child) : null;
                if (resolvedTarget != null) {
                    resolvedChild = resolvedTarget;
                    String resolved = getTypeDeclaration(resolvedTarget);
                    String stripped = stripSharedPtr(resolved);
                    if (!stripped.equals(childType)) {
                        childType = stripped;
                    }
                }
            }
            boolean isEnum = resolvedChild.getEnum() != null && !resolvedChild.getEnum().isEmpty();
            boolean isStringLike = ModelUtils.isStringSchema(resolvedChild)
                    || "std::string".equals(childType);
            composedBranches.add(new ComposedBranch(childType, isEnum, isStringLike, -1));
        }

        // Deduplicate inside lowerComposedTypes so oneOf branch identity survives
        // identical lowered C++ types.
        return Oas31CompositionLowering.lowerComposedTypes(
                composedBranches, composedKeyword, null, LOGGER::warn);
    }

    @Override
    public CodegenProperty fromProperty(String name, Schema p, boolean required,
                                        boolean schemaIsFromAdditionalProperties) {
        CodegenProperty prop = super.fromProperty(name, p, required, schemaIsFromAdditionalProperties);
        if (prop == null || p == null) {
            return prop;
        }
        // Tag inline composed properties so templates can honor oneOf vs anyOf
        // decode rules (exactly-one vs first-match) instead of always using
        // the generic JsonValueConverter exactly-one path.
        if (p.getOneOf() != null && !p.getOneOf().isEmpty()) {
            prop.vendorExtensions.put("x-cpp-composed-keyword", "oneOf");
            prop.vendorExtensions.put("x-cpp-is-oneof", true);
        } else if (p.getAnyOf() != null && !p.getAnyOf().isEmpty()) {
            prop.vendorExtensions.put("x-cpp-composed-keyword", "anyOf");
            prop.vendorExtensions.put("x-cpp-is-anyof", true);
        }
        return prop;
    }

    @Override
    public String toDefaultValue(Schema p) {
        if (ModelUtils.isStringSchema(p)) {
            if (p.getDefault() != null) {
                return "\"" + escapeCppStringContent(p.getDefault().toString()) + "\"";
            } else {
                return "\"\"";
            }
        } else if (ModelUtils.isBooleanSchema(p)) {
            if (p.getDefault() != null) {
                return p.getDefault().toString();
            } else {
                return "false";
            }
        } else if (ModelUtils.isDateSchema(p)) {
            if (p.getDefault() != null) {
                return "\"" + escapeCppStringContent(p.getDefault().toString()) + "\"";
            } else {
                return "\"\"";
            }
        } else if (ModelUtils.isDateTimeSchema(p)) {
            if (p.getDefault() != null) {
                return "\"" + escapeCppStringContent(p.getDefault().toString()) + "\"";
            } else {
                return "\"\"";
            }
        } else if (ModelUtils.isNumberSchema(p)) {
            if (ModelUtils.isFloatSchema(p)) { // float
                if (p.getDefault() != null) {
                    return p.getDefault().toString() + "f";
                } else {
                    return "0.0f";
                }
            } else { // double
                if (p.getDefault() != null) {
                    return p.getDefault().toString();
                } else {
                    return "0.0";
                }
            }
        } else if (ModelUtils.isIntegerSchema(p)) {
            if (ModelUtils.isLongSchema(p)) { // long
                if (p.getDefault() != null) {
                    return p.getDefault().toString() + "L";
                } else {
                    return "0L";
                }
            } else { // integer
                if (p.getDefault() != null) {
                    return p.getDefault().toString();
                } else {
                    return "0";
                }
            }
        } else if (ModelUtils.isByteArraySchema(p)) {
            if (p.getDefault() != null) {
                return "\"" + escapeCppStringContent(p.getDefault().toString()) + "\"";
            } else {
                return "\"\"";
            }
        } else if (ModelUtils.isMapSchema(p)) {
            Schema inner = ModelUtils.getAdditionalProperties(p);
            String innerType = inner == null ? "boost::json::value" : getTypeDeclaration(inner);
            return "std::map<std::string, " + innerType + ">()";
        } else if (ModelUtils.isArraySchema(p)) {
            // Use getItems() directly to handle OpenAPI 3.1 JsonSchema
            Schema inner = p.getItems();
            String innerType = inner != null ? getTypeDeclaration(inner) : "boost::json::value";
            return "std::vector<" + innerType + ">()";
        } else if (!StringUtils.isEmpty(p.get$ref())) {
            String refName = toModelName(ModelUtils.getSimpleRef(p.get$ref()));
            if (variantModels.contains(refName)) {
                return refName + "()";
            }
            return "std::make_shared<" + refName + ">()";
        } else if (ModelUtils.isNullType(p)) {
            return "nullptr";
        } else if (ModelUtils.isAnyType(p) || ModelUtils.isFreeFormObject(p, openAPI)) {
            return "boost::json::value()";
        }

        return "nullptr";
    }
    
    @Override
    public String toDefaultValue(CodegenProperty codegenProperty, Schema schema) {
        if (codegenProperty != null) {
            if (codegenProperty.dataType != null && codegenProperty.dataType.startsWith("std::shared_ptr<")) {
                return "nullptr";
            }
            if ("boost::json::value".equals(codegenProperty.dataType)) {
                return "boost::json::value()";
            }
            Schema referenceSchema = Oas31CompositionLowering.referenceSchemaOf(schema);
            if (referenceSchema != null && referenceSchema != schema
                    && schema.getDefault() == null) {
                Schema referencedTarget = ModelUtils.getReferencedSchema(openAPI, referenceSchema);
                if (referencedTarget != null && referencedTarget != referenceSchema
                        && codegenProperty.dataType != null
                        && codegenProperty.dataType.equals(getTypeDeclaration(referencedTarget))) {
                    return toDefaultValue(referencedTarget);
                }
            }
        }
        return super.toDefaultValue(codegenProperty, schema);
    }

    @Override
    public void setParameterEncodingValues(CodegenParameter codegenParameter, MediaType mediaType) {
        super.setParameterEncodingValues(codegenParameter, mediaType);
        // Detect Encoding Object headers that cannot be propagated to
        // multipart parts. When an Encoding Object specifies headers,
        // emit a diagnostic instead of silently dropping them.
        if (codegenParameter.isFormParam && mediaType != null
                && mediaType.getEncoding() != null) {
            io.swagger.v3.oas.models.media.Encoding encoding =
                    mediaType.getEncoding().get(codegenParameter.baseName);
            if (encoding != null && encoding.getHeaders() != null
                    && !encoding.getHeaders().isEmpty()) {
                LOGGER.warn("Encoding Object on form parameter '{}' specifies {} header(s) "
                        + "that are not propagated to the multipart part. "
                        + "Generated code uses only the contentType field. "
                        + "Header keys: {}",
                        codegenParameter.baseName,
                        encoding.getHeaders().size(),
                        encoding.getHeaders().keySet());
            }
        }
    }

    @Override
    public void postProcessParameter(CodegenParameter parameter) {
        super.postProcessParameter(parameter);

        boolean isPrimitiveType = parameter.isPrimitiveType == Boolean.TRUE;
        boolean isArray = parameter.isArray == Boolean.TRUE;
        boolean isMap = parameter.isMap == Boolean.TRUE;
        boolean isString = parameter.isString == Boolean.TRUE;
        parameter.vendorExtensions.put(X_CODEGEN_IS_RAW_BODY,
                isPrimitiveType || isString || parameter.isByteArray || parameter.isBinary
                        || "std::string".equals(parameter.dataType));

        if (!isPrimitiveType && !isArray && !isMap && !isString && !parameter.dataType.startsWith("std::shared_ptr")
                && !"boost::json::value".equals(parameter.dataType)
                && !"std::nullptr_t".equals(parameter.dataType)
                && !parameter.dataType.startsWith("std::variant<")
                && !parameter.dataType.startsWith("std::optional<")
                && !"std::monostate".equals(parameter.dataType)) {
            // Wrap non-primitive types in shared_ptr, unless:
            // - The type is a variant/optional model (value semantics)
            // - The type is a known variant model name from composed schemas
            if (!variantModels.contains(parameter.dataType)) {
                parameter.dataType = "std::shared_ptr<" + parameter.dataType + ">";
                parameter.defaultValue = "std::make_shared<" + parameter.dataType + ">()";
            }
        }

        // Post-hoc unwrap: if the type ended up as std::shared_ptr<VariantModel>,
        // strip the shared_ptr wrapper (value semantics for variant types).
        if (parameter.dataType != null && parameter.dataType.startsWith("std::shared_ptr<")
                && parameter.dataType.endsWith(">")) {
            String innerType = parameter.dataType.substring(16, parameter.dataType.length() - 1);
            if (variantModels.contains(innerType)) {
                parameter.dataType = innerType;
                parameter.defaultValue = null;
            }
        }

        // For form params, validate that encoding style/explode combinations
        // are representable in multipart/form-data. Only form-style is supported
        // for multipart (space-delimited, pipe-delimited, and deep-object styles
        // are not representable). Fail closed with a targeted diagnostic.
        if (parameter.isFormParam) {
            if (Boolean.TRUE.equals(parameter.isSpaceDelimited)) {
                throw new UnsupportedSchemaAssertionException(
                        parameter.baseName,
                        "encoding-style");
            }
            if (Boolean.TRUE.equals(parameter.isPipeDelimited)) {
                throw new UnsupportedSchemaAssertionException(
                        parameter.baseName,
                        "encoding-style");
            }
            if (Boolean.TRUE.equals(parameter.isDeepObject)) {
                throw new UnsupportedSchemaAssertionException(
                        parameter.baseName,
                        "encoding-style");
            }
        }

        // Tag variant form params for branch-aware multipart serialization.
        // When a form parameter's type is a variant, the template uses
        // addVariantFormParameter to dispatch binary branches as file parts
        // and object branches as JSON parts.
        // Only set for actual std::variant types, not for models that alias
        // to primitive types (e.g., VideoModel → std::string), which would
        // cause instantiation of addVariantFormParameter<std::string> and
        // an invalid std::visit call on a non-variant type.
        boolean isVariantParam = false;
        if (parameter.isFormParam && parameter.dataType != null) {
            if (parameter.dataType.startsWith("std::variant<")) {
                isVariantParam = true;
            } else if (variantModels.contains(parameter.dataType)) {
                String resolved = resolveThroughAliases(parameter.dataType);
                if (resolved != null && resolved.startsWith("std::variant<")) {
                    isVariantParam = true;
                }
            }
        }
        if (isVariantParam) {
            parameter.vendorExtensions.put("x-codegen-is-variant-form-param", true);
        }
    }

    /**
     * Optional - OpenAPI type conversion. This is used to map OpenAPI types in
     * a `Schema` into either language specific types via `typeMapping` or
     * into complex models if there is not a mapping.
     *
     * @return a string value of the type or complex model for this property
     */
    @Override
    public String getSchemaType(Schema p) {
        // Non-standard format (NOT core OAS vocabulary). Documented generator
        // convenience for corpora that use Unix-epoch integer timestamps.
        // Disable by not using format: unixtime in the source document.
        if (p != null && "unixtime".equals(p.getFormat())) {
            return "int64_t";
        }
        String openAPIType = super.getSchemaType(p);
        String type = null;
        String modelName;
        if (typeMapping.containsKey(openAPIType)) {
            type = typeMapping.get(openAPIType);
        } else {
            type = openAPIType;
        }

        modelName = toModelName(type);
        return modelName;
    }

    @Override
    public void updateCodegenPropertyEnum(CodegenProperty var) {
        // Remove prefix added by DefaultCodegen
        String originalDefaultValue = var.defaultValue;
        super.updateCodegenPropertyEnum(var);
        var.defaultValue = originalDefaultValue;
    }


    @Override
    public Map<String, Object> postProcessSupportingFileData(Map<String, Object> objs) {
        Map<String, Object> processed = super.postProcessSupportingFileData(objs);
        if (!validateOnDecode) {
            return processed;
        }
        // Model processing can replace inline branch schema objects after the
        // initial recovery pass; refresh the emitted graph from the raw spec.
        Oas31RawSpecRecovery.recoverPristineLiterals(openAPI, getInputSpec());
        Oas31SchemaIrEmitter emitter = new Oas31SchemaIrEmitter(
                openAPI, compositionDescriptors, additionalProperties());
        Map<String, Object> produced = emitter.produce(processed);
        supportingFiles.removeIf(file -> {
            String destination = file.getDestinationFilename();
            return destination.startsWith("schema_ir.generated.chunk")
                    && destination.endsWith(".cpp");
        });
        int chunkCount = ((Number) produced.get("oas31SchemaIrChunkCount")).intValue();
        for (int chunk = 0; chunk < chunkCount; chunk++) {
            supportingFiles.add(new SupportingFile(
                    Oas31SchemaIrEmitter.schemaIrChunkTemplate(chunk),
                    "model", Oas31SchemaIrEmitter.schemaIrChunkFilename(chunk)));
        }
        return produced;
    }

    }
