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
    private static final String HAS_EXPORT_MACRO = "hasExportMacro";


    /** SSE schema interpretation mode. */
    private String sseSchemaMode = "representation";
    private static final String SSE_SCHEMA_MODE_REPRESENTATION = "representation";
    private static final String SSE_SCHEMA_MODE_JSON_EVENT_DATA = "jsonEventData";
    /** Explicit conditional-streaming contracts, keyed by operationId. */
    private Set<String> sseOperationIds = Collections.emptySet();
    private Map<String, String> sseRequestPropertyMappings = Collections.emptyMap();
    private Map<String, String> sseEventTypeMappings = Collections.emptyMap();
    private boolean inferConditionalSseOperations = true;





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


    // ========================================================================
    // OAS 3.1 dialect and schema policy
    // ========================================================================



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





    /**
     * Camelize the method name of the getter and setter, but keep underscores at the front
     *
     * @param name string to be camelized
     * @return Camelized string
     */


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
        applySharedCppOptions();

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



    /**
     * Optional - OpenAPI type conversion. This is used to map OpenAPI types in
     * a `Schema` into either language specific types via `typeMapping` or
     * into complex models if there is not a mapping.
     *
     * @return a string value of the type or complex model for this property
     */






    }
