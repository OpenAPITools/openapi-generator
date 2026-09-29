package org.openapitools.codegen.languages;

import com.fasterxml.jackson.databind.JsonNode;

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

public class CppBoostBeastClientCodegen extends CppBoostBeastModelCodegen {

    public static final String DEFAULT_PACKAGE_NAME = "CppBoostBeastOpenAPIClient";
    public static final String EXPORT_MACRO = "exportMacro";
    private static final String HAS_EXPORT_MACRO = "hasExportMacro";

    /** Policy for format metadata in composition branch matching.
     *  Formats remain annotations and never affect branch match counts. */
    private String formatAssertionPolicy = "annotation";

    /** Value type for the formatAssertion option. */
    private static final String FORMAT_ASSERTION_POLICY_ANNOTATION = "annotation";

    /** SSE schema interpretation mode. */
    private String sseSchemaMode = "representation";
    private static final String SSE_SCHEMA_MODE_REPRESENTATION = "representation";
    private static final String SSE_SCHEMA_MODE_JSON_EVENT_DATA = "jsonEventData";
    /** Explicit conditional-streaming contracts, keyed by operationId. */
    private Set<String> sseOperationIds = Collections.emptySet();
    private Map<String, String> sseRequestPropertyMappings = Collections.emptyMap();
    private Map<String, String> sseEventTypeMappings = Collections.emptyMap();
    private boolean inferConditionalSseOperations = true;
    /** Controls composition-branch validation during model decoding. */
    private boolean validateOnDecode = true;
    /** Retains undeclared JSON object members in generated object models. */
    private boolean preserveAdditionalProperties = false;

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
    private Map<String, String> componentSchemaIdsByName = Collections.emptyMap();



    /** Starts an isolated state set for one generator invocation. */
    private void beginGeneration(OpenAPI openApi) {
        sourceOpenApi = openApi;
        variantModels = new HashSet<>();
        resolvedAliasTypes = new HashMap<>();
        composedKeywordsByModel = new HashMap<>();
        compositionDescriptors = new LinkedHashMap<>();
        compositionDescriptorSets = new LinkedHashMap<>();
        webhookPreservation = new ArrayList<>();
        operationCallbacks = new HashMap<>();
        operationLinks = new HashMap<>();
        allOfIntersections = new LinkedHashMap<>();
        refreshComponentSchemaIds(openApi);

    }

    /**
     * swagger-parser materializes the implicit root server as {@code /}, which
     * is indistinguishable from a source-level {@code servers: [{url: /}]} in
     * the model. Consult the raw document before server-precedence assembly.
     */
    private boolean detectExplicitRootServers() {
        String inputSpec = getInputSpec();
        if (inputSpec == null || inputSpec.isEmpty()) {
            // Programmatic OpenAPI instances have no parser-injected source.
            return true;
        }
        try {
            JsonNode document = Oas31RawSpecRecovery.readRawDocument(inputSpec);
            return document != null && document.isObject() && document.has("servers");
        } catch (Exception exception) {
            throw new IllegalStateException(
                    "Unable to inspect the source OpenAPI document for root servers", exception);
        }
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

    /**
     * Returns every composition descriptor present on a schema, in keyword
     * order: oneOf, anyOf, then allOf.
     */
    public List<CompositionDescriptor> getCompositionDescriptorsForSchema(String schemaName) {
        return compositionDescriptorSets.getOrDefault(schemaName, Collections.emptyList());
    }

    protected String packageName = DEFAULT_PACKAGE_NAME;
    private String exportMacro = "";

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
        hasExplicitRootServers = detectExplicitRootServers();

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
                List<CompositionDescriptor> descriptors =
                        Oas31CompositionLowering.buildCompositionDescriptors(
                                schemaName, schema, openAPI, schemas);
                if (!descriptors.isEmpty()) {
                    String modelName = toModelName(schemaName);
                    // The primary descriptor drives representation lowering;
                    // retain and validate every composition keyword separately.
                    compositionDescriptors.put(modelName, descriptors.get(0));
                    compositionDescriptorSets.put(modelName, Collections.unmodifiableList(
                            new ArrayList<>(descriptors)));
                    for (CompositionDescriptor descriptor : descriptors) {
                        Oas31CompositionLowering.validateDescriptorAssertions(descriptor);
                    }
                }
                // allOf affects object storage even when oneOf or anyOf selects
                // the public representation.
                if (schema.getAllOf() != null && !schema.getAllOf().isEmpty()) {
                    AllOfIntersection intersection =
                            Oas31CompositionLowering.computeAllOfIntersection(
                                    schemaName, schema, openAPI, schemas, new HashSet<>());
                    if (intersection != null) {
                        allOfIntersections.put(toModelName(schemaName), intersection);
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
        addOption(EXPORT_MACRO,
                "C++ export macro placed before public classes and functions. When non-empty,"
                + " ApiExport.h is generated for Windows DLL export/import handling.",
                exportMacro);
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
                + " media representation; callbacks receive an owning SseEvent with"
                + " raw data, event, id, and retry metadata. 'jsonEventData': decode"
                + " each complete event data payload against the response schema and"
                + " pass both the typed value and SseEvent metadata to the callback."
                + " Use x-sse-event-data-schema for per-operation typed decoding.");
        sseSchemaModeOption.defaultValue(SSE_SCHEMA_MODE_REPRESENTATION);
        sseSchemaModeOption.addEnum(SSE_SCHEMA_MODE_REPRESENTATION,
                "Schema describes the media representation; callback receives SseEvent");
        sseSchemaModeOption.addEnum(SSE_SCHEMA_MODE_JSON_EVENT_DATA,
                "Schema describes each JSON event data payload");
        cliOptions.add(sseSchemaModeOption);
        cliOptions.add(new CliOption("sseOperationIds",
                "Comma-separated operationIds whose JSON request body conditionally"
                + " selects text/event-stream (default request property: stream)."));
        cliOptions.add(new CliOption("sseRequestPropertyMappings",
                "Comma-separated operationId=property mappings for the boolean request"
                + " property that selects SSE."));
        cliOptions.add(new CliOption("sseEventTypeMappings",
                "Comma-separated operationId=Model mappings for the JSON schema of each"
                + " SSE event data payload."));
        CliOption inferConditionalSseOption = CliOption.newBoolean(
                "inferConditionalSseOperations",
                "Infer conditional SSE for dual JSON/SSE operations only when the"
                + " request selector and event model are unambiguous. Enabled by default.");
        inferConditionalSseOption.defaultValue(Boolean.TRUE.toString());
        cliOptions.add(inferConditionalSseOption);

        CliOption compileWithValidationOption = new CliOption("compileWithValidation",
                "Emit schema-validation IR and kValidateOnDecode=true in generated"
                + " ValidationTypes.h (default). Set to false to omit the IR for"
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
        CliOption preserveAdditionalPropertiesOption = new CliOption(
                "preserveAdditionalProperties",
                "Retain undeclared JSON object members in generated object models and"
                + " re-emit them. Composition validation accepts such members while"
                + " decoding; set to false for strict additionalProperties handling.");
        preserveAdditionalPropertiesOption.defaultValue(Boolean.FALSE.toString());
        cliOptions.add(preserveAdditionalPropertiesOption);


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
        supportingFiles.add(new SupportingFile(
                "oas31_exact_number_source.mustache", "model", "Oas31ExactNumber.cpp"));
        supportingFiles.add(new SupportingFile("oas31_schema_ir.mustache", "model", "Oas31SchemaIr.h"));
        supportingFiles.add(new SupportingFile("oas31_deep_equal.mustache", "model", "Oas31DeepEqual.h"));
        supportingFiles.add(new SupportingFile("oas31_exact_json.mustache", "model", "Oas31ExactJson.h"));
        supportingFiles.add(new SupportingFile("oas31_validator.mustache", "model", "Oas31Validator.h"));
        // Generation-time IR tables and optional bounded source chunks. Content
        // is rendered from supporting-file data.
        supportingFiles.add(new SupportingFile("oas31_schema_ir_header.mustache", "model", "Oas31SchemaRegistry.h"));
        supportingFiles.add(new SupportingFile("oas31_schema_ir_source.mustache", "model", "schema_ir.generated.cpp"));

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
                || (destination.startsWith("schema_ir.generated.chunk")
                        && destination.endsWith(".cpp"));
    }

    private static Set<String> parseNameSet(Object rawValue, String optionName) {
        if (rawValue == null || rawValue.toString().trim().isEmpty()) {
            return Collections.emptySet();
        }
        Collection<?> values = rawValue instanceof Collection
                ? (Collection<?>) rawValue
                : Arrays.asList(rawValue.toString().split(",", -1));
        Set<String> result = new LinkedHashSet<>();
        for (Object value : values) {
            String name = value == null ? "" : value.toString().trim();
            if (name.isEmpty()) {
                throw new IllegalArgumentException(optionName
                        + " contains an empty operationId");
            }
            result.add(name);
        }
        return Collections.unmodifiableSet(result);
    }

    private static Map<String, String> parseNameMappings(
            Object rawValue, String optionName) {
        if (rawValue == null || rawValue.toString().trim().isEmpty()) {
            return Collections.emptyMap();
        }
        Map<String, String> result = new LinkedHashMap<>();
        if (rawValue instanceof Map) {
            for (Map.Entry<?, ?> entry : ((Map<?, ?>) rawValue).entrySet()) {
                putNameMapping(result, entry.getKey(), entry.getValue(), optionName);
            }
        } else {
            for (String mapping : rawValue.toString().split(",", -1)) {
                int separator = mapping.indexOf('=');
                if (separator <= 0 || separator == mapping.length() - 1
                        || mapping.indexOf('=', separator + 1) >= 0) {
                    throw new IllegalArgumentException(optionName
                            + " entries must use operationId=value syntax: " + mapping);
                }
                putNameMapping(result, mapping.substring(0, separator),
                        mapping.substring(separator + 1), optionName);
            }
        }
        return Collections.unmodifiableMap(result);
    }

    private static void putNameMapping(Map<String, String> target,
            Object rawKey, Object rawValue, String optionName) {
        String key = rawKey == null ? "" : rawKey.toString().trim();
        String value = rawValue == null ? "" : rawValue.toString().trim();
        if (key.isEmpty() || value.isEmpty()) {
            throw new IllegalArgumentException(optionName
                    + " entries require non-empty operationId and value");
        }
        if (target.putIfAbsent(key, value) != null) {
            throw new IllegalArgumentException(optionName
                    + " contains duplicate operationId: " + key);
        }
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
        Object configuredExportMacro = additionalProperties.get(EXPORT_MACRO);
        exportMacro = configuredExportMacro == null
                ? "" : configuredExportMacro.toString().trim();
        if (!exportMacro.isEmpty()
                && !exportMacro.matches("[A-Za-z_][A-Za-z0-9_]*")) {
            throw new IllegalArgumentException(
                    "exportMacro must be empty or a valid C preprocessor identifier: "
                            + exportMacro);
        }
        additionalProperties.put(EXPORT_MACRO, exportMacro);
        additionalProperties.put(HAS_EXPORT_MACRO, !exportMacro.isEmpty());
        supportingFiles.removeIf(file -> "ApiExport.h".equals(
                file.getDestinationFilename()));
        if (!exportMacro.isEmpty()) {
            String exportPrefix = toPreprocessorIdentifier(packageName);
            additionalProperties.put("exportDefine", exportPrefix + "_EXPORTS");
            additionalProperties.put("exportHeaderGuard",
                    exportPrefix.toUpperCase(Locale.ROOT) + "_API_EXPORT_H_");
            supportingFiles.add(new SupportingFile(
                    "api-export.mustache", "api", "ApiExport.h"));
        } else {
            additionalProperties.remove("exportDefine");
            additionalProperties.remove("exportHeaderGuard");
        }
        String modelNamespace = modelPackage.replaceAll("\\.", "::");
        additionalProperties.put("modelNamespaceDeclarations", modelPackage.split("\\."));
        additionalProperties.put("modelNamespace", modelNamespace);
        additionalProperties.put("schemaValidationNamespace",
                modelNamespace + "::detail::schema_validation");
        additionalProperties.put("schemaValidationHeaderGuardPrefix",
                modelPackage.replaceAll("[^A-Za-z0-9]", "_").toUpperCase(Locale.ROOT));
        additionalProperties.put("apiHeaderGuardPrefix",
                apiPackage.replaceAll("[^A-Za-z0-9]", "_").toUpperCase(Locale.ROOT));
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
                throw new IllegalArgumentException("sseSchemaMode must be '"
                        + SSE_SCHEMA_MODE_REPRESENTATION + "' or '"
                        + SSE_SCHEMA_MODE_JSON_EVENT_DATA + "': " + raw);
            }
        }
        additionalProperties.put("sseSchemaMode", sseSchemaMode);

        sseOperationIds = parseNameSet(
                additionalProperties.get("sseOperationIds"), "sseOperationIds");
        sseRequestPropertyMappings = parseNameMappings(
                additionalProperties.get("sseRequestPropertyMappings"),
                "sseRequestPropertyMappings");
        sseEventTypeMappings = parseNameMappings(
                additionalProperties.get("sseEventTypeMappings"),
                "sseEventTypeMappings");
        if (additionalProperties.containsKey("inferConditionalSseOperations")) {
            Object raw = additionalProperties.get("inferConditionalSseOperations");
            if (raw instanceof Boolean) {
                inferConditionalSseOperations = (Boolean) raw;
            } else {
                String value = raw.toString().trim();
                if (!"true".equalsIgnoreCase(value) && !"false".equalsIgnoreCase(value)) {
                    throw new IllegalArgumentException(
                            "inferConditionalSseOperations must be true or false: " + value);
                }
                inferConditionalSseOperations = Boolean.parseBoolean(value);
            }
        }
        additionalProperties.put("inferConditionalSseOperations",
                inferConditionalSseOperations);

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
        preserveAdditionalProperties = false;
        if (additionalProperties.containsKey("preserveAdditionalProperties")) {
            Object raw = additionalProperties.get("preserveAdditionalProperties");
            if (raw instanceof Boolean) {
                preserveAdditionalProperties = (Boolean) raw;
            } else {
                String value = raw.toString().trim();
                if (!"true".equalsIgnoreCase(value) && !"false".equalsIgnoreCase(value)) {
                    throw new IllegalArgumentException(
                            "preserveAdditionalProperties must be true or false: " + value);
                }
                preserveAdditionalProperties = Boolean.parseBoolean(value);
            }
        }
        additionalProperties.put("preserveAdditionalProperties", preserveAdditionalProperties);
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
                "x-cpp-component-schema-id",
                componentSchemaId(name, componentSchemaIdsByName));

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
        if (preserveAdditionalProperties) {
            codegenModel.imports.add("#include <map>");
            codegenModel.imports.add("#include <string>");
            codegenModel.imports.add("#include <utility>");
            reserveExtraJsonPropertyIdentifiers(codegenModel);
        }

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

    private void reserveExtraJsonPropertyIdentifiers(CodegenModel codegenModel) {
        Set<String> propertyAccessors = new HashSet<>();
        Set<String> propertyMembers = new HashSet<>();
        if (codegenModel.allVars != null) {
            for (CodegenProperty property : codegenModel.allVars) {
                if (property.getter != null) {
                    propertyAccessors.add(property.getter);
                }
                if (property.setter != null) {
                    propertyAccessors.add(property.setter);
                }
                if (property.name != null) {
                    propertyMembers.add("m_" + property.name);
                }
            }
        }

        int maxSuffix = propertyAccessors.size() + propertyMembers.size() + 2;
        for (int suffix = 1; suffix <= maxSuffix; suffix++) {
            String suffixText = suffix == 1 ? "" : Integer.toString(suffix);
            String getter = "getExtraJsonProperties" + suffixText;
            String setter = "setExtraJsonProperties" + suffixText;
            String member = "m_extraJsonProperties" + suffixText;
            if (propertyAccessors.contains(getter) || propertyAccessors.contains(setter)
                    || propertyMembers.contains(member)) {
                continue;
            }
            codegenModel.vendorExtensions.put("x-cpp-extra-json-properties-getter", getter);
            codegenModel.vendorExtensions.put("x-cpp-extra-json-properties-setter", setter);
            codegenModel.vendorExtensions.put("x-cpp-extra-json-properties-member", member);
            return;
        }
        throw new IllegalStateException("Unable to reserve C++ extra JSON property identifiers");
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
                sseSchemaMode,
                sseOperationIds,
                sseRequestPropertyMappings,
                sseEventTypeMappings,
                inferConditionalSseOperations,
                hasExplicitRootServers).assemble(objs, allModels);
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
        if (Oas31RawSpecRecovery.hasExplicitDefault(p)) {
            String defaultValue = explicitScalarDefaultValue(prop, p);
            if (defaultValue != null) {
                prop.defaultValue = defaultValue;
                prop.vendorExtensions.put("x-cpp-has-explicit-default", true);
                prop.vendorExtensions.put(X_CPP_EXPLICIT_DEFAULT_SCALAR, defaultValue);
                prop.vendorExtensions.put("x-cpp-default-is-null",
                        "null".equals(Oas31RawSpecRecovery.defaultJsonOf(p)));
            }
        }
        return prop;
    }

    private String explicitScalarDefaultValue(CodegenProperty property, Schema schema) {
        String json = Oas31RawSpecRecovery.defaultJsonOf(schema);
        if (json == null) {
            return null;
        }

        com.fasterxml.jackson.databind.JsonNode value;
        try {
            value = io.swagger.v3.core.util.Json31.mapper().readTree(json);
        } catch (com.fasterxml.jackson.core.JsonProcessingException exception) {
            throw new IllegalArgumentException(
                    "Unable to parse default for property '" + property.baseName + "'", exception);
        }
        if (value == null || !value.isValueNode()) {
            return null;
        }

        Object nullableInner = property.vendorExtensions.get(
                "x-cpp-nullable-field-inner-type");
        if (value.isNull()) {
            if (nullableInner != null) {
                return "NullableField<" + nullableInner + ">::makeDefaultNull()";
            }
            if (property.dataType != null
                    && property.dataType.startsWith("std::optional<")) {
                return "std::nullopt";
            }
            if ("std::nullptr_t".equals(property.dataType)) {
                return "nullptr";
            }
            if ("boost::json::value".equals(property.dataType)) {
                return "boost::json::value(nullptr)";
            }
            if (property.dataType != null
                    && property.dataType.startsWith("std::shared_ptr<")) {
                // A branch-local default:null is an annotation, not a model value.
                // Ignore it rather than rejecting an otherwise legal schema.
                return null;
            }

            throw new IllegalArgumentException(
                    "JSON null default is not representable by C++ property '"
                            + property.baseName + "' of type " + property.dataType);
        }

        String expression;
        if (value.isTextual()) {
            expression = "\"" + escapeCppStringContent(value.textValue()) + "\"";
        } else if (value.isBoolean()) {
            expression = value.booleanValue() ? "true" : "false";
        } else if (value.isNumber()) {
            expression = explicitNumericDefault(property, value.decimalValue());
        } else {
            return null;
        }

        if (nullableInner != null) {
            return "NullableField<" + nullableInner + ">::makeDefaultValue("
                    + expression + ")";
        }
        return expression;
    }

    private static String explicitNumericDefault(
            CodegenProperty property, java.math.BigDecimal value) {
        if (property.isInteger || property.isLong) {
            java.math.BigInteger integer;
            try {
                integer = value.toBigIntegerExact();
            } catch (ArithmeticException exception) {
                throw new IllegalArgumentException(
                        "Non-integral default is not representable by integer property '"
                                + property.baseName + "'", exception);
            }
            if (property.isLong || "std::int64_t".equals(property.dataType)) {
                java.math.BigInteger min = java.math.BigInteger.valueOf(Long.MIN_VALUE);
                java.math.BigInteger max = java.math.BigInteger.valueOf(Long.MAX_VALUE);
                if (integer.compareTo(min) < 0 || integer.compareTo(max) > 0) {
                    throw new IllegalArgumentException(
                            "Default is outside int64 range for property '"
                                    + property.baseName + "'");
                }
                if (integer.equals(min)) {
                    return "std::int64_t{-9223372036854775807LL - 1LL}";
                }
                return "std::int64_t{" + integer + "LL}";
            }
            try {
                return "std::int32_t{" + integer.intValueExact() + "}";
            } catch (ArithmeticException exception) {
                throw new IllegalArgumentException(
                        "Default is outside int32 range for property '"
                                + property.baseName + "'", exception);
            }
        }

        String literal = value.toString();
        boolean hasFloatingMarker = literal.indexOf('.') >= 0
                || literal.indexOf('e') >= 0 || literal.indexOf('E') >= 0;
        if (!hasFloatingMarker) {
            literal += ".0";
        }
        if (property.isFloat || "float".equals(property.dataType)) {
            float narrowed = value.floatValue();
            if (!Float.isFinite(narrowed)
                    || (value.signum() != 0 && narrowed == 0.0f)) {
                throw new IllegalArgumentException(
                        "Default is outside finite float range for property '"
                                + property.baseName + "'");
            }
            return literal + "F";
        }
        double narrowed = value.doubleValue();
        if (!Double.isFinite(narrowed)
                || (value.signum() != 0 && narrowed == 0.0)) {
            throw new IllegalArgumentException(
                    "Default is outside finite double range for property '"
                            + property.baseName + "'");
        }
        return literal;
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
    public Map<String, ModelsMap> updateAllModels(Map<String, ModelsMap> objs) {
        Map<String, ModelsMap> updatedModels = super.updateAllModels(objs);
        refreshComponentSchemaIds(openAPI);
        for (Map.Entry<String, ModelsMap> entry : updatedModels.entrySet()) {
            for (ModelMap modelMap : entry.getValue().getModels()) {
                CodegenModel model = modelMap.getModel();
                String schemaName = model.schemaName != null ? model.schemaName : entry.getKey();
                model.vendorExtensions.put("x-cpp-component-schema-id",
                        componentSchemaId(schemaName, componentSchemaIdsByName));
            }
        }
        return updatedModels;
    }

    private void refreshComponentSchemaIds(OpenAPI openApi) {
        if (openApi == null || openApi.getComponents() == null
                || openApi.getComponents().getSchemas() == null) {
            componentSchemaIdsByName = Collections.emptyMap();
            return;
        }
        componentSchemaIdsByName = componentSchemaIds(
                openApi.getComponents().getSchemas().keySet());
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
        refreshComponentSchemaIds(openAPI);
        Oas31SchemaIrEmitter emitter = new Oas31SchemaIrEmitter(
                openAPI, compositionDescriptors, additionalProperties(), componentSchemaIdsByName);
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
