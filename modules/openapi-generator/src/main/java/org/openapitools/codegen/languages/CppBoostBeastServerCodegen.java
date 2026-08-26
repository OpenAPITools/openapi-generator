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
import io.swagger.v3.oas.models.parameters.Parameter;
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
                        GlobalFeature.ParameterStyling,
                        GlobalFeature.MultiServer
                )
                .excludeGlobalFeatures(
                        GlobalFeature.XMLStructureDefinitions,
                        GlobalFeature.Callbacks,
                        GlobalFeature.LinkObjects
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
     * and multipart payloads plus an oauth2 scheme. Degrade semantics:
     * the assembler filters declared request media types to JSON (a body
     * with no JSON media type loses its typed body; mixed bodies accept
     * JSON only and answer 415 to the rest), and security schemes the
     * runtime has no credential extractor for deny all requests with 401.
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
                    List<String> nonJson = new ArrayList<>();
                    for (String mediaType : body.getContent().keySet()) {
                        if (isSupportedMediaType(mediaType)) {
                            hasJson = true;
                        } else {
                            nonJson.add(mediaType);
                        }
                    }
                    if (!hasJson) {
                        warnings.add("operation '" + operationId
                                + "' declares only non-JSON request media types ("
                                + String.join(", ", nonJson)
                                + "); the generated handler receives no typed body");
                    } else if (!nonJson.isEmpty()) {
                        warnings.add("operation '" + operationId
                                + "' also declares non-JSON request media types ("
                                + String.join(", ", nonJson)
                                + "); only JSON is accepted at runtime"
                                + " (415 for the rest)");
                    }
                }
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
        return new CppBoostBeastServerTemplateModelAssembler(sourceOpenApi)
                .assemble(objs, allModels);
    }

    /**
     * Generation-time rejection gate for surfaces whose generated code
     * would not compile or not route deterministically. Fails closed with
     * precise diagnostics instead of emitting silent stubs. Media types and
     * security scheme types are NOT rejected here (they degrade; see
     * {@link #collectSurfaceWarnings}) because the repository harness
     * requires every generator to accept the canonical petstore spec.
     */
    List<String> validateServerSupportSurface(OpenAPI openAPI) {
        List<String> diagnostics = new ArrayList<>();
        if (openAPI == null || openAPI.getPaths() == null) {
            return diagnostics;
        }
        Map<String, String> shapeOwners = new LinkedHashMap<>();
        for (Map.Entry<String, PathItem> pathEntry : openAPI.getPaths().entrySet()) {
            String pathTemplate = pathEntry.getKey();
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
                appendParameterRejections(
                        openAPI, operation.getParameters(),
                        pathEntry.getKey() + ":" + opEntry.getKey(), diagnostics);
                appendParameterRejections(
                        openAPI, item.getParameters(),
                        pathEntry.getKey() + ":pathItem", diagnostics);
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

    /** Media types the generated runtime can serve: JSON, {+json} suffixes,
     *  and the wildcard. Shared with the assembler, which filters declared
     *  request media types down to this set. */
    static boolean isSupportedMediaType(String mediaType) {
        String normalized = mediaType == null ? "" : mediaType.trim().toLowerCase(Locale.ROOT);
        int semicolon = normalized.indexOf(';');
        if (semicolon >= 0) {
            normalized = normalized.substring(0, semicolon).trim();
        }
        return "application/json".equals(normalized)
                || normalized.endsWith("+json")
                || "*/*".equals(normalized);
    }

    private static void appendParameterRejections(
            OpenAPI openAPI, List<Parameter> parameters, String location,
            List<String> diagnostics) {
        if (parameters == null) {
            return;
        }
        for (Parameter parameter : parameters) {
            if (parameter == null) {
                continue;
            }
            String label = parameter.getName() != null
                    ? parameter.getName() : "(unnamed)";
            if (parameter.getContent() != null) {
                diagnostics.add("parameter '" + label + "' at " + location
                        + " uses content-style serialization; only schema"
                        + " parameters are supported");
                continue;
            }
            String in = parameter.getIn() == null ? "" : parameter.getIn();
            Schema<?> schema = resolvedParameterSchema(openAPI, parameter);
            if (schema != null) {
                String type = schema.getType();
                if ("array".equals(type)) {
                    if ("cookie".equals(in)) {
                        diagnostics.add("parameter '" + label + "' at " + location
                                + " must have a scalar schema; array and object"
                                + " cookie parameters are not supported");
                    } else {
                        Schema<?> items = schema.getItems() == null
                                ? null
                                : ModelUtils.getReferencedSchema(openAPI, schema.getItems());
                        String itemType = items == null ? null : items.getType();
                        if (itemType != null && !isScalarType(itemType)) {
                            diagnostics.add("array parameter '" + label + "' at "
                                    + location + " must have scalar items"
                                    + " (string, integer, number, or boolean)");
                        }
                    }
                } else if ("object".equals(type)) {
                    String styleForShape = parameter.getStyle() == null
                            ? null : parameter.getStyle().toString();
                    boolean deepObjectStringMap = "query".equals(in)
                            && "deepObject".equals(styleForShape)
                            && schema.getAdditionalProperties() instanceof Schema
                            && "string".equals(ModelUtils.getReferencedSchema(
                                    openAPI, (Schema<?>) schema.getAdditionalProperties())
                                    .getType());
                    if (!deepObjectStringMap) {
                        diagnostics.add("object-typed parameter '" + label + "' at "
                                + location + " is not supported; only query"
                                + " deepObject parameters mapping to string values"
                                + " are supported");
                    }
                }
                appendEnumRejections(schema, label, location, diagnostics);
            }
            String style = parameter.getStyle() == null
                    ? null : parameter.getStyle().toString();
            if (style == null) {
                continue;   // location default is allowed
            }
            boolean allowed;
            switch (in) {
                case "header":
                    allowed = "simple".equals(style);
                    break;
                case "cookie":
                    allowed = "form".equals(style);
                    break;
                case "query":
                    allowed = "form".equals(style) || "spaceDelimited".equals(style)
                            || "pipeDelimited".equals(style) || "deepObject".equals(style);
                    break;
                case "path":
                    allowed = "simple".equals(style) || "label".equals(style)
                            || "matrix".equals(style);
                    break;
                default:
                    allowed = true;
                    break;
            }
            if (!allowed) {
                diagnostics.add("parameter '" + label + "' at " + location
                        + " uses unsupported style '" + style + "' for in='" + in + "'");
            }
        }
    }

    /** Resolves a parameter's schema through $ref (parameter and schema level). */
    private static Schema<?> resolvedParameterSchema(
            OpenAPI openAPI, Parameter parameter) {
        Parameter resolved = parameter.get$ref() != null
                ? ModelUtils.getReferencedParameter(openAPI, parameter)
                : parameter;
        if (resolved == null || resolved.getSchema() == null) {
            return null;
        }
        return ModelUtils.getReferencedSchema(openAPI, resolved.getSchema());
    }

    /** Whether a schema type maps to a runtime scalar parseScalar overload. */
    private static boolean isScalarType(String type) {
        return "string".equals(type) || "integer".equals(type)
                || "number".equals(type) || "boolean".equals(type);
    }

    /** Rejects enums the parameter constraint templates cannot render
     *  faithfully: null members and mixes of string with numeric/boolean
     *  members (integer+number mixes stay valid; both render into the
     *  generated long double allow-list). */
    private static void appendEnumRejections(
            Schema<?> schema, String label, String location,
            List<String> diagnostics) {
        if (schema.getEnum() == null || schema.getEnum().isEmpty()) {
            return;
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
            diagnostics.add("parameter '" + label + "' at " + location
                    + " has heterogeneous enum members; only uniform string,"
                    + " numeric, or boolean enums are supported");
        }
    }

    /** Canonical routing shape: literal vs placeholder per path segment.
     *  Mirrors Router::splitPath (one leading empty segment skipped,
     *  interior and trailing empty segments preserved) so `/pets` and
     *  `/pets/` produce distinct keys exactly when the router treats them
     *  as distinct routes. */
    private static String routeShapeKey(String pathTemplate) {
        StringBuilder key = new StringBuilder();
        List<String> segments = new ArrayList<>();
        int start = pathTemplate.startsWith("/") ? 1 : 0;
        while (start <= pathTemplate.length()) {
            int slash = pathTemplate.indexOf('/', start);
            if (slash < 0) {
                segments.add(pathTemplate.substring(start));
                break;
            }
            segments.add(pathTemplate.substring(start, slash));
            start = slash + 1;
        }
        for (String segment : segments) {
            if (segment.startsWith("{") && segment.endsWith("}")) {
                key.append("/{");
            } else {
                key.append('/').append(segment);
            }
        }
        return key.length() == 0 ? "/" : key.toString();
    }
}
