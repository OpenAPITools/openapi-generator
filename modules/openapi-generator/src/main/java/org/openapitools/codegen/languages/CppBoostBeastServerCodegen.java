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
package org.openapitools.codegen.languages;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.PathItem;
import io.swagger.v3.oas.models.media.Content;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.parameters.RequestBody;
import io.swagger.v3.oas.models.responses.ApiResponse;
import io.swagger.v3.oas.models.security.SecurityScheme;
import org.openapitools.codegen.utils.ModelUtils;
import org.openapitools.codegen.CodegenType;
import org.openapitools.codegen.SupportingFile;
import org.openapitools.codegen.meta.GeneratorMetadata;
import org.openapitools.codegen.meta.Stability;
import org.openapitools.codegen.meta.features.DataTypeFeature;
import org.openapitools.codegen.meta.features.DocumentationFeature;
import org.openapitools.codegen.meta.features.GlobalFeature;
import org.openapitools.codegen.meta.features.ParameterFeature;
import org.openapitools.codegen.meta.features.SchemaSupportFeature;
import org.openapitools.codegen.meta.features.SecurityFeature;
import org.openapitools.codegen.meta.features.WireFormatFeature;
import org.openapitools.codegen.model.ModelMap;
import org.openapitools.codegen.model.OperationsMap;
import org.apache.commons.lang3.StringUtils;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

/**
 * C++ Boost.Beast HTTP server code generator. Emits an asynchronous
 * HTTP/1.1 server (Boost.Beast + Boost.Asio + Boost.URL) with typed
 * per-operation request/response contracts, OAS parameter deserialization,
 * a pluggable security authorizer seam, RFC 9457 problem responses, and
 * decode-time OAS 3.1 schema validation shared with the client generator.
 *
 * <p>Mustache templates are located in
 * {@code src/main/resources/cpp-boost-beast-server/} with shared
 * model/validation templates resolved from {@code cpp-boost-beast-common}.
 */
public class CppBoostBeastServerCodegen extends CppBoostBeastModelCodegen {

    public static final String DEFAULT_PACKAGE_NAME = "CppBoostBeastServer";
    public static final String ADD_API_IMPL_STUBS = "addApiImplStubs";

    protected String packageName = DEFAULT_PACKAGE_NAME;

    @Override
    public CodegenType getTag() {
        return CodegenType.SERVER;
    }

    @Override
    public String getName() {
        return "cpp-boost-beast-server";
    }

    @Override
    public String getHelp() {
        return "Generates a C++ Boost.Beast HTTP server.";
    }

    public CppBoostBeastServerCodegen() {
        super();
        openapiNormalizer.put("NORMALIZER_CLASS",
                CppBoostBeastClientCodegen.CppBoostBeastOpenAPINormalizer.class.getName());
        generatorMetadata = GeneratorMetadata.newBuilder(generatorMetadata)
                .stability(Stability.BETA)
                .build();
        modifyFeatureSet(features -> features
                .includeDocumentationFeatures(DocumentationFeature.Readme)
                .securityFeatures(EnumSet.of(
                        SecurityFeature.ApiKey,
                        SecurityFeature.BasicAuth,
                        SecurityFeature.BearerToken))
                .wireFormatFeatures(EnumSet.of(WireFormatFeature.JSON))
                .includeGlobalFeatures(
                        GlobalFeature.ParameterStyling
                )
                .excludeGlobalFeatures(
                        GlobalFeature.XMLStructureDefinitions,
                        GlobalFeature.Callbacks,
                        GlobalFeature.LinkObjects,
                        // The generated Router registers operation paths
                        // verbatim and the server binds the endpoint the
                        // caller passes to listen(): the document's server
                        // URLs (host and any base path) are not mounted, so
                        // host, base-path, and multi-server routing are not
                        // implemented.
                        GlobalFeature.Host,
                        GlobalFeature.BasePath,
                        GlobalFeature.MultiServer
                )
                .excludeParameterFeatures(
                        // Non-JSON request bodies degrade to "no typed body"
                        // (see collectSurfaceWarnings): the runtime never
                        // parses urlencoded or multipart payloads.
                        ParameterFeature.FormUnencoded,
                        ParameterFeature.FormMultipart
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
                        DataTypeFeature.Decimal,
                        DataTypeFeature.Date,
                        DataTypeFeature.DateTime,
                        DataTypeFeature.Uuid,
                        DataTypeFeature.Byte,
                        DataTypeFeature.Binary,
                        DataTypeFeature.Password
                )
                .includeParameterFeatures(
                        ParameterFeature.Cookie
                )
        );

        outputFolder = "generated-code" + File.separator + "cpp-boost-beast-server";
        modelTemplateFiles.put("model-header.mustache", ".h");
        modelTemplateFiles.put("model-source.mustache", ".cpp");
        apiTemplateFiles.put("api-header.mustache", ".h");
        apiTemplateFiles.put("api-source.mustache", ".cpp");

        embeddedTemplateDir = templateDir = "cpp-boost-beast-server";

        modelPackage = "org.openapitools.server.model";
        apiPackage = "org.openapitools.server.api";

        cliOptions.clear();

        addOption(org.openapitools.codegen.CodegenConstants.PACKAGE_NAME,
                "C++ package and library name.", DEFAULT_PACKAGE_NAME);
        addOption(org.openapitools.codegen.CodegenConstants.MODEL_PACKAGE,
                "C++ namespace for models (convention: name.space.model).", this.modelPackage);
        addOption(org.openapitools.codegen.CodegenConstants.API_PACKAGE,
                "C++ namespace for apis (convention: name.space.api).", this.apiPackage);
        org.openapitools.codegen.CliOption compileWithValidationOption =
                new org.openapitools.codegen.CliOption("compileWithValidation",
                        "Emit schema-validation IR and kValidateOnDecode=true in generated"
                                + " ValidationTypes.h (default). Set to false to omit the IR.");
        compileWithValidationOption.defaultValue(Boolean.TRUE.toString());
        cliOptions.add(compileWithValidationOption);
        org.openapitools.codegen.CliOption tolerateOption =
                new org.openapitools.codegen.CliOption(
                        "tolerateNonNullableNulls",
                        "Treat explicit JSON null values as absent for generated model"
                                + " properties whose schemas do not allow null. Enabled by"
                                + " default; set to false for strict schema decoding.");
        tolerateOption.defaultValue(Boolean.TRUE.toString());
        cliOptions.add(tolerateOption);
        org.openapitools.codegen.CliOption preserveOption =
                new org.openapitools.codegen.CliOption(
                        "preserveAdditionalProperties",
                        "Retain undeclared JSON object members in generated object models"
                                + " and re-emit them; set to false for strict handling.");
        preserveOption.defaultValue(Boolean.FALSE.toString());
        cliOptions.add(preserveOption);
        org.openapitools.codegen.CliOption stubsOption =
                org.openapitools.codegen.CliOption.newBoolean(
                        ADD_API_IMPL_STUBS,
                        "Generate API implementation stubs that answer 501 problem+json"
                                + " and a sample main.cpp for quick start");
        stubsOption.defaultValue(Boolean.FALSE.toString());
        cliOptions.add(stubsOption);

        supportingFiles.add(new SupportingFile("validation-types.mustache", "model", "ValidationTypes.h"));
        supportingFiles.add(new SupportingFile("NullableField.h.mustache", "model", "NullableField.h"));
        supportingFiles.add(new SupportingFile("anytype-header.mustache", "model", "AnyType.h"));
        supportingFiles.add(new SupportingFile(
                "oas31_exact_number.mustache", "model", "Oas31ExactNumber.h"));
        supportingFiles.add(new SupportingFile(
                "oas31_exact_number_source.mustache", "model", "Oas31ExactNumber.cpp"));
        supportingFiles.add(new SupportingFile("oas31_schema_ir.mustache", "model", "Oas31SchemaIr.h"));
        supportingFiles.add(new SupportingFile("oas31_deep_equal.mustache", "model", "Oas31DeepEqual.h"));
        supportingFiles.add(new SupportingFile("oas31_exact_json.mustache", "model", "Oas31ExactJson.h"));
        supportingFiles.add(new SupportingFile("oas31_validator.mustache", "model", "Oas31Validator.h"));
        supportingFiles.add(new SupportingFile(
                "oas31_schema_ir_header.mustache", "model", "Oas31SchemaRegistry.h"));
        supportingFiles.add(new SupportingFile(
                "oas31_schema_ir_source.mustache", "model", "schema_ir.generated.cpp"));

        supportingFiles.add(new SupportingFile("http-server-header.mustache", "server", "HttpServer.h"));
        supportingFiles.add(new SupportingFile("http-server-source.mustache", "server", "HttpServer.cpp"));
        supportingFiles.add(new SupportingFile("router-header.mustache", "server", "Router.h"));
        supportingFiles.add(new SupportingFile("responder-header.mustache", "server", "Responder.h"));
        supportingFiles.add(new SupportingFile("problem-header.mustache", "server", "Problem.h"));
        supportingFiles.add(new SupportingFile("authorizer-header.mustache", "server", "Authorizer.h"));
        supportingFiles.add(new SupportingFile("param-codecs-header.mustache", "server", "ParamCodecs.h"));
        supportingFiles.add(new SupportingFile("body-json-header.mustache", "server", "BodyJson.h"));
        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("CMakeLists.txt.mustache", "", "CMakeLists.txt"));

        languageSpecificPrimitives = new HashSet<String>(
                Arrays.asList("int", "char", "bool", "long", "float", "double",
                        "std::int32_t", "std::int64_t"));

        // Replace (do not inherit) the base maps: DefaultCodegen seeds
        // AnyType -> oas_any_type_not_mapped, a placeholder header that no
        // C++ template provides. Untyped schemas resolve through the model
        // pipeline's isAnyType branch (boost::json::value); mirroring the
        // client's wiped-map initialization keeps both generators on one
        // convention.
        super.typeMapping = new HashMap<String, String>();
        typeMapping.put("date", "std::string");
        typeMapping.put("DateTime", "std::string");
        typeMapping.put("string", "std::string");
        typeMapping.put("integer", "std::int32_t");
        typeMapping.put("long", "std::int64_t");
        typeMapping.put("boolean", "bool");
        typeMapping.put("array", "std::vector");
        typeMapping.put("set", "std::vector");
        typeMapping.put("map", "std::map");
        typeMapping.put("file", "std::string");
        typeMapping.put("object", "boost::json::value");
        typeMapping.put("number", "double");
        typeMapping.put("UUID", "std::string");
        typeMapping.put("URI", "std::string");
        typeMapping.put("ByteArray", "std::string");

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
    public void processOpts() {
        super.processOpts();
        packageName = additionalProperties.getOrDefault(
                org.openapitools.codegen.CodegenConstants.PACKAGE_NAME,
                DEFAULT_PACKAGE_NAME).toString();
        if (StringUtils.isBlank(packageName)) {
            throw new IllegalArgumentException("packageName must not be blank");
        }
        additionalProperties.put(
                org.openapitools.codegen.CodegenConstants.PACKAGE_NAME, packageName);
        applySharedCppOptions();

        boolean addStubs = Boolean.parseBoolean(
                additionalProperties.getOrDefault(ADD_API_IMPL_STUBS, Boolean.FALSE)
                        .toString());
        additionalProperties.put(ADD_API_IMPL_STUBS, addStubs);
        if (addStubs) {
            supportingFiles.add(new SupportingFile("main.mustache", "", "main.cpp"));
        }
    }

    @Override
    public void preprocessOpenAPI(OpenAPI openAPI) {
        super.preprocessOpenAPI(openAPI);
        for (String warning : collectSurfaceWarnings(openAPI)) {
            LOGGER.warn("cpp-boost-beast-server: {}", warning);
        }
        List<String> rejections = validateServerSupportSurface(openAPI);
        if (!rejections.isEmpty()) {
            throw new IllegalArgumentException(
                    "cpp-boost-beast-server: " + String.join("; ", rejections));
        }
    }

    /**
     * Surfaces the runtime cannot faithfully serve but CAN degrade safely
     * on. These warn instead of failing the build because the repository
     * contract (AllGeneratorsTest) requires every registered generator to
     * generate from the canonical petstore spec, which declares XML, form,
     * and multipart payloads plus an oauth2 scheme, and because real-world
     * corpora (the OpenAI spec) declare parameter and body shapes the JSON
     * runtime cannot decode. Degrade semantics:
     * <ul>
     *   <li>request media types are filtered to JSON; an operation left with
     *       no JSON media type loses its typed body and mixed bodies answer
     *       415 to non-JSON content types;</li>
     *   <li>parameters the runtime cannot decode from a raw string are
     *       dropped from the generated handler (the assembler applies the
     *       equivalent dataType-level rule);</li>
     *   <li>security schemes without a credential extractor deny all
     *       requests with 401.</li>
     * </ul>
     */
    List<String> collectSurfaceWarnings(OpenAPI openAPI) {
        List<String> warnings = new ArrayList<>();
        if (openAPI == null || openAPI.getPaths() == null) {
            return warnings;
        }
        for (Map.Entry<String, PathItem> pathEntry : openAPI.getPaths().entrySet()) {
            PathItem item = pathEntry.getValue();
            if (item == null || item.readOperationsMap() == null) {
                continue;
            }
            for (Map.Entry<PathItem.HttpMethod, Operation> opEntry
                    : item.readOperationsMap().entrySet()) {
                Operation operation = opEntry.getValue();
                if (operation == null) {
                    continue;
                }
                String operationId = operation.getOperationId() != null
                        ? operation.getOperationId()
                        : opEntry.getKey() + " " + pathEntry.getKey();
                RequestBody body = operation.getRequestBody() != null
                        ? ModelUtils.getReferencedRequestBody(
                                openAPI, operation.getRequestBody())
                        : null;
                if (body != null && body.getContent() != null
                        && !body.getContent().isEmpty()) {
                    boolean hasJson = false;
                    List<String> dropped = new ArrayList<>();
                    for (String mediaType : body.getContent().keySet()) {
                        // Mirrors the assembler's requestBodyFacts filter:
                        // only JSON family decodes; the wildcard and every
                        // other type is dropped (and warned here).
                        if (isSupportedRequestMediaType(mediaType)) {
                            hasJson = true;
                        } else {
                            dropped.add(mediaType);
                        }
                    }
                    if (!hasJson) {
                        warnings.add("operation '" + operationId
                                + "' declares no JSON request media type ("
                                + String.join(", ", dropped)
                                + "); the generated handler receives no typed body");
                    } else if (!dropped.isEmpty()) {
                        warnings.add("operation '" + operationId
                                + "' also declares request media types the JSON"
                                + " decoder cannot parse (" + String.join(", ", dropped)
                                + "); only the JSON declarations are accepted at"
                                + " runtime (415 for the rest)");
                    }
                }
                // Parameter-shape drops (content-style, objects, arrays with
                // non-scalar items, unsupported styles, heterogeneous enums,
                // empty types) are classified and warned by the assembler,
                // which emits the field and sees the resolved dataType.
                if (operation.getResponses() != null) {
                    for (Map.Entry<String, ApiResponse> respEntry
                            : operation.getResponses().entrySet()) {
                        ApiResponse response = respEntry.getValue();
                        if (response == null || response.getContent() == null) {
                            continue;
                        }
                        for (String mediaType : response.getContent().keySet()) {
                            if (!isSupportedMediaType(mediaType)) {
                                warnings.add("operation '" + operationId
                                        + "' response '" + respEntry.getKey()
                                        + "' declares media type '" + mediaType
                                        + "'; the generated responder serializes"
                                        + " responses as JSON");
                            }
                        }
                    }
                }
            }
        }
        for (String scheme : collectUnsupportedSecuritySchemes(openAPI)) {
            warnings.add("security scheme '" + scheme
                    + "' uses a type the runtime cannot extract credentials for"
                    + " (only apiKey and http are supported); operations"
                    + " requiring it deny all requests with 401");
        }
        return warnings;
    }

    @Override
    public OperationsMap postProcessOperationsWithModels(
            OperationsMap objs, List<ModelMap> allModels) {
        String modelNamespace = String.valueOf(
                additionalProperties.getOrDefault("modelNamespace", ""));
        String apiNamespace = String.valueOf(
                additionalProperties.getOrDefault("apiNamespace", ""));
        return new CppBoostBeastServerTemplateModelAssembler(
                sourceOpenApi, modelNamespace, apiNamespace,
                this::toModelImport).assemble(objs, allModels);
    }

    /**
     * The server runtime decodes JSON bodies only and never parses a
     * form-encoded payload, so flattened form fields (including their
     * spaceDelimited/pipeDelimited/deepObject encodings) are dropped by the
     * assembler with a warning rather than aborting generation. The shared
     * model pipeline's fail-closed reject exists for the client's multipart
     * writer, which does not apply here.
     */
    @Override
    protected boolean rejectsUnsupportedFormEncodingStyles() {
        return false;
    }
    /**
     * Generation-time rejection gate for shapes whose generated code could
     * not route deterministically: ambiguous path templates and ranged
     * status codes. Parameter and body shapes the runtime cannot decode
     * DEGRADE instead (dropped with a warning; see
     * {@link #collectSurfaceWarnings} and the assembler's dataType rule) so
     * real-world corpora still generate compileable code.
     */
    List<String> validateServerSupportSurface(OpenAPI openAPI) {
        List<String> diagnostics = new ArrayList<>();
        if (openAPI == null || openAPI.getPaths() == null) {
            return diagnostics;
        }
        Map<String, String> shapeOwners = new LinkedHashMap<>();
        for (Map.Entry<String, PathItem> pathEntry : openAPI.getPaths().entrySet()) {
            String pathTemplate = pathEntry.getKey();
            String malformed = pathTemplateIssue(pathTemplate);
            if (malformed != null) {
                diagnostics.add("path template '" + pathTemplate + "' has "
                        + malformed + "; the router cannot extract its parameters");
            }
            String previous = shapeOwners.putIfAbsent(
                    routeShapeKey(pathTemplate), pathTemplate);
            if (previous != null && !previous.equals(pathTemplate)) {
                diagnostics.add("path templates '" + previous + "' and '" + pathTemplate
                        + "' have the same shape; server routing requires distinct shapes");
            }
            PathItem item = pathEntry.getValue();
            if (item == null || item.readOperationsMap() == null) {
                continue;
            }
            for (Map.Entry<PathItem.HttpMethod, Operation> opEntry
                    : item.readOperationsMap().entrySet()) {
                Operation operation = opEntry.getValue();
                if (operation == null) {
                    continue;
                }
                String operationId = operation.getOperationId() != null
                        ? operation.getOperationId()
                        : opEntry.getKey() + " " + pathTemplate;
                if (operation.getResponses() != null) {
                    for (Map.Entry<String, ApiResponse> respEntry
                            : operation.getResponses().entrySet()) {
                        ApiResponse response = respEntry.getValue();
                        if (response == null) {
                            continue;
                        }
                        if (respEntry.getKey() != null
                                && respEntry.getKey().matches("[1-5]XX")) {
                            diagnostics.add("operation '" + operationId
                                    + "' declares ranged status code '"
                                    + respEntry.getKey()
                                    + "'; only concrete codes are supported");
                        }
                    }
                }
            }
        }
        return diagnostics;
    }

    /** Security scheme names whose type is not apiKey/http (always-401 stubs). */
    private static Set<String> collectUnsupportedSecuritySchemes(OpenAPI openAPI) {
        Set<String> unsupported = new LinkedHashSet<>();
        if (openAPI.getComponents() == null
                || openAPI.getComponents().getSecuritySchemes() == null) {
            return unsupported;
        }
        for (Map.Entry<String, SecurityScheme> entry
                : openAPI.getComponents().getSecuritySchemes().entrySet()) {
            SecurityScheme scheme = entry.getValue();
            if (scheme == null || scheme.getType() == null) {
                continue;
            }
            SecurityScheme.Type type = scheme.getType();
            if (type != SecurityScheme.Type.APIKEY
                    && type != SecurityScheme.Type.HTTP) {
                unsupported.add(entry.getKey() + " (" + type + ")");
            }
        }
        return unsupported;
    }

    /** Media types the generated runtime can serve as JSON: exact JSON,
     *  {+json} structured suffixes, and the wildcard. */
    static boolean isSupportedMediaType(String mediaType) {
        String normalized = normalizeMediaType(mediaType);
        return isJsonMediaType(normalized) || "*/*".equals(normalized);
    }

    /** Media types the generated runtime can DECODE a request body from.
     *  Unlike responses (where everything is serialized as JSON anyway), a
     *  request body must be parsed, and the wildcard {@code *\/​*} declares no
     *  JSON-specific representation: admitting it would make the handler
     *  answer a JSON parse error (400) to every non-JSON representation it
     *  cannot decode. Request media types are therefore narrowed to JSON
     *  (with a warning); everything else gets a clean 415. */
    static boolean isSupportedRequestMediaType(String mediaType) {
        return isJsonMediaType(normalizeMediaType(mediaType));
    }

    /** True for exact application/json and {+json} structured suffixes. */
    static boolean isJsonMediaType(String normalizedMediaType) {
        return "application/json".equals(normalizedMediaType)
                || normalizedMediaType.endsWith("+json");
    }

    /** Lowercase the media type and strip {@code ;parameter} suffixes. */
    static String normalizeMediaType(String mediaType) {
        String normalized = mediaType == null ? "" : mediaType.trim().toLowerCase(Locale.ROOT);
        int semicolon = normalized.indexOf(';');
        if (semicolon >= 0) {
            normalized = normalized.substring(0, semicolon).trim();
        }
        return normalized;
    }

    /** Styles whose wire format the generated codecs reproduce exactly. */
    static boolean isStyleAllowedForLocation(String in, String style) {
        switch (in) {
            case "header":
                return "simple".equals(style);
            case "cookie":
                return "form".equals(style);
            case "query":
                return "form".equals(style) || "spaceDelimited".equals(style)
                        || "pipeDelimited".equals(style) || "deepObject".equals(style);
            case "path":
                return "simple".equals(style) || "label".equals(style)
                        || "matrix".equals(style);
            default:
                return true;
        }
    }

    /** Whether a C++ dataType is one of the plain scalar parseScalar
     *  overloads (nullable and optional wrappers are NOT: the codec decodes
     *  into plain scalars and models carry nulls through their own field
     *  decoders instead). */
    static boolean isPlainScalarDataType(String dataType) {
        return "std::string".equals(dataType) || "bool".equals(dataType)
                || "std::int32_t".equals(dataType) || "std::int64_t".equals(dataType)
                || "float".equals(dataType) || "double".equals(dataType);
    }

    /** Names enums the constraint templates cannot render faithfully: null
     *  members and mixes of string with numeric/boolean members (integer +
     *  number mixes stay valid; both render into the long double
     *  allow-list). Returns a short reason (grammar: "<verb phrase>") or
     *  null for uniform enums. Shared with the assembler. */
    static String heterogeneousEnumIssue(Schema<?> schema) {
        if (schema == null || schema.getEnum() == null || schema.getEnum().isEmpty()) {
            return null;
        }
        boolean hasNull = false;
        boolean hasString = false;
        boolean hasNumber = false;
        boolean hasBoolean = false;
        for (Object value : schema.getEnum()) {
            if (value == null) {
                hasNull = true;
            } else if (value instanceof String) {
                hasString = true;
            } else if (value instanceof Number) {
                hasNumber = true;
            } else if (value instanceof Boolean) {
                hasBoolean = true;
            }
        }
        if (hasNull
                || (hasString && (hasNumber || hasBoolean))
                || (hasBoolean && (hasString || hasNumber))) {
            return "has heterogeneous enum members; the generated allow-list"
                    + " renders uniform string, numeric, or boolean enums only";
        }
        return null;
    }

    /** Routing-shape diagnostics for one path template, or null when the
     *  template is well-formed. Mirrors Router::tokenizeSegment: expressions
     *  must be balanced, non-nested, and non-empty; everything else (a stray
     *  '{', a nested '{', an empty '{}') would be treated as literal text by
     *  the router while the parameter extraction expects an expression, so
     *  the route could never fill its parameters. */
    static String pathTemplateIssue(String pathTemplate) {
        int start = 0;
        while (true) {
            int open = pathTemplate.indexOf('{', start);
            if (open < 0) {
                int stray = pathTemplate.indexOf('}', start);
                return stray < 0 ? null
                        : "unbalanced '}' outside an expression";
            }
            int close = pathTemplate.indexOf('}', open + 1);
            if (close < 0) {
                return "unclosed '{'";
            }
            int inner = pathTemplate.indexOf('{', open + 1);
            if (inner >= 0 && inner < close) {
                return "nested '{' inside an expression";
            }
            if (close == open + 1) {
                return "empty '{}' expression";
            }
            start = close + 1;
        }
    }

    /** Canonical routing shape: per segment, literal text stays, each
     *  expression contributes a "{}" marker. Mirrors Router::splitPath +
     *  Router::tokenizeSegment so templates the router can confuse (two
     *  token streams that match the same inputs, e.g. '/a/{x}-{y}' vs
     *  '/a/{z}-{w}' ordering ambiguities and '/x/{a}' vs '/x/{b}') are
     *  detected at generation time. Whole-segment and embedded expressions
     *  share one shape space: '/pets/{id}' and '/pets/p{id}' differ (extra
     *  literal), '/reports/{y}-{m}' and '/reports/{a}-{b}' collide. */
    private static String routeShapeKey(String pathTemplate) {
        StringBuilder key = new StringBuilder();
        int start = pathTemplate.startsWith("/") ? 1 : 0;
        while (start <= pathTemplate.length()) {
            int slash = pathTemplate.indexOf('/', start);
            String segment = slash < 0
                    ? pathTemplate.substring(start)
                    : pathTemplate.substring(start, slash);
            int cursor = 0;
            while (cursor < segment.length()) {
                int open = segment.indexOf('{', cursor);
                if (open < 0) {
                    key.append(segment, cursor, segment.length());
                    break;
                }
                key.append(segment, cursor, open);
                int close = segment.indexOf('}', open);
                int inner = segment.indexOf('{', open + 1);
                if (close < 0 || (inner >= 0 && inner < close) || close == open + 1) {
                    // Malformed remainder: literal, exactly as the router does.
                    key.append(segment, open, segment.length());
                    break;
                }
                key.append("{}");
                cursor = close + 1;
            }
            key.append('/');
            if (slash < 0) {
                break;
            }
            start = slash + 1;
        }
        return key.length() == 0 ? "/" : key.toString();
    }

    /** True when the `{baseName}` expression is the whole path segment (the
     *  only place label/matrix styles can appear per the OAS grammar). */
    static boolean isWholeSegmentPathParameter(String pathTemplate, String baseName) {
        String token = "{" + baseName + "}";
        int start = 0;
        while (start <= pathTemplate.length()) {
            int slash = pathTemplate.indexOf('/', start);
            String segment = slash < 0
                    ? pathTemplate.substring(start)
                    : pathTemplate.substring(start, slash);
            if (segment.equals(token)) {
                return true;
            }
            if (slash < 0) {
                return false;
            }
            start = slash + 1;
        }
        return false;
    }
}
