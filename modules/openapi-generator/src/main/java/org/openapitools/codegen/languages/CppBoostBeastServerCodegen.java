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
import io.swagger.v3.oas.models.media.MediaType;
import io.swagger.v3.oas.models.parameters.Parameter;
import io.swagger.v3.oas.models.responses.ApiResponse;
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
        List<String> rejections = validateServerSupportSurface(openAPI);
        if (!rejections.isEmpty()) {
            throw new IllegalArgumentException(
                    "cpp-boost-beast-server: " + String.join("; ", rejections));
        }
    }

    @Override
    public OperationsMap postProcessOperationsWithModels(
            OperationsMap objs, List<ModelMap> allModels) {
        return new CppBoostBeastServerTemplateModelAssembler(sourceOpenApi)
                .assemble(objs, allModels);
    }

    /**
     * Generation-time rejection gate for surfaces the initial server runtime
     * does not implement. Fails closed with precise diagnostics instead of
     * emitting silent stubs.
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
                if (operation.getRequestBody() != null
                        && operation.getRequestBody().getContent() != null) {
                    collectMediaTypeRejections(
                            operation.getRequestBody().getContent(),
                            operationId, diagnostics);
                }
                if (operation.getResponses() != null) {
                    for (ApiResponse response : operation.getResponses().values()) {
                        if (response != null && response.getContent() != null) {
                            collectMediaTypeRejections(
                                    response.getContent(), operationId, diagnostics);
                        }
                    }
                }
                appendParameterRejections(operation.getParameters(),
                        pathEntry.getKey() + ":" + opEntry.getKey(), diagnostics);
                appendParameterRejections(item.getParameters(),
                        pathEntry.getKey() + ":pathItem", diagnostics);
            }
        }
        return diagnostics;
    }

    private static void collectMediaTypeRejections(
            Content content, String operationId, List<String> diagnostics) {
        for (String mediaType : content.keySet()) {
            if (!isSupportedMediaType(mediaType)) {
                diagnostics.add("operation '" + operationId
                        + "' uses unsupported media type '" + mediaType
                        + "'; only JSON bodies are supported");
            }
        }
    }

    private static boolean isSupportedMediaType(String mediaType) {
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
            List<Parameter> parameters, String location, List<String> diagnostics) {
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
            String style = parameter.getStyle() == null
                    ? null : parameter.getStyle().toString();
            if (style == null) {
                continue;   // location default is allowed
            }
            String in = parameter.getIn() == null ? "" : parameter.getIn();
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

    /** Canonical routing shape: literal vs placeholder per path segment. */
    private static String routeShapeKey(String pathTemplate) {
        StringBuilder key = new StringBuilder();
        for (String segment : pathTemplate.split("/")) {
            if (segment.isEmpty()) {
                continue;
            }
            if (segment.startsWith("{") && segment.endsWith("}")) {
                key.append("/{");
            } else {
                key.append('/').append(segment);
            }
        }
        return key.length() == 0 ? "/" : key.toString();
    }
}
