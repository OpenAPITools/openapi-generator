/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
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

import com.samskivert.mustache.Mustache;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.info.Info;
import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.parameters.Parameter;
import io.swagger.v3.oas.models.parameters.RequestBody;
import io.swagger.v3.oas.models.responses.ApiResponse;
import io.swagger.v3.oas.models.servers.Server;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.*;
import org.openapitools.codegen.meta.GeneratorMetadata;
import org.openapitools.codegen.meta.Stability;
import org.openapitools.codegen.meta.features.GlobalFeature;
import org.openapitools.codegen.meta.features.ParameterFeature;
import org.openapitools.codegen.meta.features.SchemaSupportFeature;
import org.openapitools.codegen.meta.features.WireFormatFeature;
import org.openapitools.codegen.model.ModelMap;
import org.openapitools.codegen.model.ModelsMap;
import org.openapitools.codegen.model.OperationMap;
import org.openapitools.codegen.model.OperationsMap;
import org.openapitools.codegen.utils.ModelUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.IOException;
import java.math.BigInteger;
import java.nio.file.Path;
import java.util.*;
import java.util.stream.Collectors;

import static org.openapitools.codegen.utils.StringUtils.camelize;
import static org.openapitools.codegen.utils.StringUtils.underscore;

public class RustAxumServerCodegen extends AbstractRustCodegen implements CodegenConfig {
    public static final String PROJECT_NAME = "openapi-server";
    private static final String apiPath = "rust-axum";

    private String packageName;
    private String packageVersion;
    private Boolean disableValidator = false;
    private Boolean allowBlockingValidator = false;
    private Boolean allowBlockingResponseSerialize = false;
    private String externCrateName;

    // Types
    private static final String uuidType = "uuid::Uuid";
    private static final String bytesType = "ByteArray";
    private static final String dateType = "chrono::naive::NaiveDate";
    private static final String dateTimeType = "chrono::DateTime::<chrono::Utc>";
    private static final String stringType = "String";
    private static final String objectType = "crate::types::Object";
    private static final String mapType = "std::collections::HashMap";
    private static final String vecType = "Vec";

    // Mime
    private static final String octetMimeType = "application/octet-stream";
    private static final String plainTextMimeType = "text/plain";
    private static final String xmlMimeType = "application/xml";
    private static final String textXmlMimeType = "text/xml";
    private static final String formUrlEncodedMimeType = "application/x-www-form-urlencoded";
    private static final String jsonMimeType = "application/json";
    // RFC 7386 support
    private static final String mergePatchJsonMimeType = "application/merge-patch+json";
    // RFC 7807 Support
    private static final String problemJsonMimeType = "application/problem+json";
    private static final String problemXmlMimeType = "application/problem+xml";

    // Grouping (Method, Operation) by Path.
    private final Map<String, ArrayList<MethodOperation>> pathMethodOpMap = new HashMap<>();

    // Logger
    private final Logger LOGGER = LoggerFactory.getLogger(RustAxumServerCodegen.class);

    public RustAxumServerCodegen() {
        super();

        modifyFeatureSet(features -> features
                .wireFormatFeatures(EnumSet.of(
                        WireFormatFeature.JSON,
                        WireFormatFeature.Custom
                ))
                .excludeGlobalFeatures(
                        GlobalFeature.Info,
                        GlobalFeature.ExternalDocumentation,
                        GlobalFeature.Examples,
                        GlobalFeature.XMLStructureDefinitions,
                        GlobalFeature.MultiServer,
                        GlobalFeature.ParameterizedServer,
                        GlobalFeature.ParameterStyling,
                        GlobalFeature.Callbacks,
                        GlobalFeature.LinkObjects
                )
                .excludeSchemaSupportFeatures(
                        SchemaSupportFeature.Polymorphism
                )
                .excludeParameterFeatures(
                        ParameterFeature.Cookie
                )
        );

        generatorMetadata = GeneratorMetadata.newBuilder(generatorMetadata)
                .stability(Stability.BETA)
                .build();

        // Show the generation timestamp by default
        hideGenerationTimestamp = Boolean.FALSE;

        // set the output folder here
        outputFolder = Path.of("generated-code", "rust-axum").toString();
        embeddedTemplateDir = templateDir = "rust-axum";

        importMapping = new HashMap<>();
        modelTemplateFiles.clear();
        apiTemplateFiles.clear();

        // types
        defaultIncludes = new HashSet<>(
                Set.of("map", "array")
        );

        languageSpecificPrimitives = new HashSet<>(
                Set.of(
                        "bool",
                        "char",
                        "i8",
                        "i16",
                        "i32",
                        "i64",
                        "u8",
                        "u16",
                        "u32",
                        "u64",
                        "isize",
                        "usize",
                        "f32",
                        "f64",
                        "str",
                        stringType)
        );
        assert languageSpecificPrimitives.size() == 16;

        instantiationTypes = new HashMap<>(
                Map.of(
                        "array", vecType,
                        "map", mapType
                )
        );
        assert instantiationTypes.size() == 2;

        typeMapping = new HashMap<>(Map.ofEntries(
                new AbstractMap.SimpleEntry<>("number", "f64"),
                new AbstractMap.SimpleEntry<>("integer", "i32"),
                new AbstractMap.SimpleEntry<>("long", "i64"),
                new AbstractMap.SimpleEntry<>("float", "f32"),
                new AbstractMap.SimpleEntry<>("double", "f64"),
                new AbstractMap.SimpleEntry<>("string", stringType),
                new AbstractMap.SimpleEntry<>("UUID", uuidType),
                new AbstractMap.SimpleEntry<>("URI", stringType),
                new AbstractMap.SimpleEntry<>("byte", "u8"),
                new AbstractMap.SimpleEntry<>("ByteArray", bytesType),
                new AbstractMap.SimpleEntry<>("binary", bytesType),
                new AbstractMap.SimpleEntry<>("boolean", "bool"),
                new AbstractMap.SimpleEntry<>("date", dateType),
                new AbstractMap.SimpleEntry<>("DateTime", dateTimeType),
                new AbstractMap.SimpleEntry<>("password", stringType),
                new AbstractMap.SimpleEntry<>("File", bytesType),
                new AbstractMap.SimpleEntry<>("file", bytesType),
                new AbstractMap.SimpleEntry<>("array", vecType),
                new AbstractMap.SimpleEntry<>("map", mapType),
                new AbstractMap.SimpleEntry<>("object", objectType),
                new AbstractMap.SimpleEntry<>("AnyType", objectType)
        ));
        assert typeMapping.size() == 21;

        // cli options
        CliOption optDisableValidator = new CliOption("disableValidator", "Disable validating request-data (header, path, query, body) " +
                "against OpenAPI Schema Specification.");
        optDisableValidator.setType("bool");
        optDisableValidator.defaultValue(disableValidator.toString());

        CliOption optAllowBlockingValidator = new CliOption("allowBlockingValidator",
                String.join("",
                        "By default, validation process, which might perform a lot of compute in a ",
                        "future without yielding, is executed on a blocking thread via tokio::task::spawn_blocking. ",
                        "Set this option to true will override this behaviour and allow blocking call to happen. ",
                        "It helps to improve the performance when validating request-data (header, path, query, body) ",
                        "is low cost."));
        optAllowBlockingValidator.setType("bool");
        optAllowBlockingValidator.defaultValue(allowBlockingValidator.toString());

        CliOption optAllowBlockingResponseSerialize = new CliOption("allowBlockingResponseSerialize",
                String.join("", "By default, json/form-urlencoded response serialization, which might ",
                        "perform a lot of compute in a future without yielding, is executed on a blocking thread ",
                        "via tokio::task::spawn_blocking. Set this option to true will override this behaviour and ",
                        "allow blocking call to happen. It helps to improve the performance when response ",
                        "serialization (e.g. returns tiny data) is low cost."));
        optAllowBlockingResponseSerialize.setType("bool");
        optAllowBlockingResponseSerialize.defaultValue(allowBlockingResponseSerialize.toString());

        cliOptions = new ArrayList<>(
                List.of(
                        new CliOption(CodegenConstants.PACKAGE_NAME,
                                "Rust crate name (convention: snake_case).")
                                .defaultValue("openapi"),
                        new CliOption(CodegenConstants.PACKAGE_VERSION,
                                "Rust crate version."),
                        optDisableValidator,
                        optAllowBlockingValidator,
                        optAllowBlockingResponseSerialize
                )
        );

        supportingFiles.add(new SupportingFile("Cargo.mustache", "", "Cargo.toml"));
        supportingFiles.add(new SupportingFile("gitignore", "", ".gitignore"));
        supportingFiles.add(new SupportingFile("lib.mustache", "src", "lib.rs"));
        supportingFiles.add(new SupportingFile("models.mustache", "src", "models.rs"));
        supportingFiles.add(new SupportingFile("types.mustache", "src", "types.rs"));
        supportingFiles.add(new SupportingFile("header.mustache", "src", "header.rs"));
        supportingFiles.add(new SupportingFile("server-mod.mustache", "src/server", "mod.rs"));
        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md")
                .doNotOverwrite());
    }

    public CodegenType getTag() {
        return CodegenType.SERVER;
    }

    public String getName() {
        return "rust-axum";
    }

    public String getHelp() {
        return "Generates a Rust server library which bases on Axum.";
    }

    @Override
    public Mustache.Compiler processCompiler(Mustache.Compiler compiler) {
        return compiler
                .emptyStringIsFalse(true)
                .zeroIsFalse(true);
    }

    @Override
    public void processOpts() {
        super.processOpts();

        if (StringUtils.isEmpty(System.getenv("RUST_POST_PROCESS_FILE"))) {
            LOGGER.info("Environment variable RUST_POST_PROCESS_FILE not defined. rustfmt will be used" +
                    " by default. To choose a different tool, try" +
                    " 'export RUST_POST_PROCESS_FILE=\"/usr/local/bin/rustfmt\"' (Linux/Mac)");
            LOGGER.info("NOTE: To enable file post-processing, 'enablePostProcessFile' must be set to `true` " +
                    " (--enable-post-process-file for CLI).");
        }

        if (!Boolean.TRUE.equals(ModelUtils.isGenerateAliasAsModel())) {
            LOGGER.warn("generateAliasAsModel is set to false, which means array/map will be generated as model instead and the resulting code may have issues. Please enable `generateAliasAsModel` to address the issue.");
        }

        setPackageName((String) additionalProperties.getOrDefault(CodegenConstants.PACKAGE_NAME, "openapi"));

        if (additionalProperties.containsKey(CodegenConstants.PACKAGE_VERSION)) {
            setPackageVersion((String) additionalProperties.get(CodegenConstants.PACKAGE_VERSION));
        }

        additionalProperties.put(CodegenConstants.PACKAGE_NAME, packageName);
        additionalProperties.put("externCrateName", externCrateName);

        if (additionalProperties.containsKey("disableValidator")) {
            disableValidator = convertPropertyToBooleanAndWriteBack("disableValidator");
        } else {
            additionalProperties.put("disableValidator", disableValidator);
        }

        if (additionalProperties.containsKey("allowBlockingValidator")) {
            allowBlockingValidator = convertPropertyToBooleanAndWriteBack("allowBlockingValidator");
        } else {
            additionalProperties.put("allowBlockingValidator", allowBlockingValidator);
        }

        if (additionalProperties.containsKey("allowBlockingResponseSerialize")) {
            allowBlockingResponseSerialize = convertPropertyToBooleanAndWriteBack("allowBlockingResponseSerialize");
        } else {
            additionalProperties.put("allowBlockingResponseSerialize", allowBlockingResponseSerialize);
        }
    }

    private void setPackageName(String packageName) {
        this.packageName = packageName;

        // Also set the extern crate name, which has any '-' replace with a '_'.
        this.externCrateName = packageName.replace('-', '_');
    }

    private void setPackageVersion(String packageVersion) {
        this.packageVersion = packageVersion;
    }

    @Override
    public String apiPackage() {
        return apiPath;
    }

    @Override
    public void preprocessOpenAPI(OpenAPI openAPI) {
        Info info = openAPI.getInfo();

        if (packageVersion == null || packageVersion.isEmpty()) {
            List<String> versionComponents = new ArrayList<>(Arrays.asList(info.getVersion().split("[.]")));
            if (versionComponents.isEmpty()) {
                versionComponents.add("1");
            }
            while (versionComponents.size() < 3) {
                versionComponents.add("0");
            }

            setPackageVersion(String.join(".", versionComponents));
        }

        additionalProperties.put(CodegenConstants.PACKAGE_VERSION, packageVersion);
    }

    @Override
    public String toApiName(String name) {
        return name.isEmpty() ?
                "default" :
                sanitizeIdentifier(name, CasingType.SNAKE_CASE, "api", "API", true);
    }

    /**
     * Location to write api files.  You can use the apiPackage() as defined when the class is
     * instantiated
     */
    @Override
    public String apiFileFolder() {
        return Path.of(outputFolder, apiPackage().replace('.', File.separatorChar)).toString();
    }

    @Override
    public String toOperationId(String operationId) {
        return sanitizeIdentifier(operationId, CasingType.CAMEL_CASE, "call", "method", true);
    }

    @Override
    public String toEnumValue(String value, String datatype) {
        return "\"" + super.toEnumValue(value, datatype) + "\"";
    }

    private boolean isObjectType(String type) {
        return "object".equals(type);
    }

    private boolean isMimetypeXml(String mimetype) {
        return mimetype.toLowerCase(Locale.ROOT).startsWith(xmlMimeType) ||
                mimetype.toLowerCase(Locale.ROOT).startsWith(problemXmlMimeType) ||
                mimetype.toLowerCase(Locale.ROOT).startsWith(textXmlMimeType);
    }

    private boolean isMimetypeJson(String mimetype) {
        return mimetype.toLowerCase(Locale.ROOT).startsWith(jsonMimeType) ||
                mimetype.toLowerCase(Locale.ROOT).startsWith(mergePatchJsonMimeType) ||
                mimetype.toLowerCase(Locale.ROOT).startsWith(problemJsonMimeType);
    }

    private boolean isMimetypeWwwFormUrlEncoded(String mimetype) {
        return mimetype.toLowerCase(Locale.ROOT).startsWith(formUrlEncodedMimeType);
    }

    private boolean isMimetypeMultipartFormData(String mimetype) {
        return mimetype.toLowerCase(Locale.ROOT).startsWith("multipart/form-data");
    }

    private boolean isMimetypeMultipartRelated(String mimetype) {
        return mimetype.toLowerCase(Locale.ROOT).startsWith("multipart/related");
    }

    private boolean isMimetypeUnknown(String mimetype) {
        return "*/*".equals(mimetype);
    }

    boolean isMimetypePlain(String mimetype) {
        return !(isMimetypeUnknown(mimetype) ||
                isMimetypeJson(mimetype) ||
                isMimetypeWwwFormUrlEncoded(mimetype) ||
                isMimetypeMultipartFormData(mimetype) ||
                isMimetypeMultipartRelated(mimetype));
    }

    @Override
    public CodegenOperation fromOperation(String path, String httpMethod, Operation operation, List<Server> servers) {
        CodegenOperation op = super.fromOperation(path, httpMethod, operation, servers);

        String underscoredOperationId = underscore(op.operationId);
        op.vendorExtensions.put("x-operation-id", underscoredOperationId);
        op.vendorExtensions.put("x-uppercase-operation-id", underscoredOperationId.toUpperCase(Locale.ROOT));

        if (!op.isCallbackRequest) {
            // group route by path
            String axumPath = op.path;
            for (CodegenParameter param : op.pathParams) {
                // Replace {baseName} with {paramName} for format string
                String paramSearch = "{" + param.baseName + "}";
                String paramReplace = ":" + param.paramName;

                axumPath = axumPath.replace(paramSearch, paramReplace);
            }
            pathMethodOpMap
                    .computeIfAbsent(axumPath, (key) -> new ArrayList<>())
                    .add(new MethodOperation(op.httpMethod.toLowerCase(Locale.ROOT), underscoredOperationId));
        }

        // Determine the types that this operation produces. `getProducesInfo`
        // simply lists all the types, and then we add the correct imports to
        // the generated library.
        Set<String> producesInfo = getProducesInfo(openAPI, operation);
        boolean producesPlainText = false;
        boolean producesFormUrlEncoded = false;
        if (producesInfo != null && !producesInfo.isEmpty()) {
            List<Map<String, String>> produces = new ArrayList<>(producesInfo.size());

            for (String mimeType : producesInfo) {
                if (isMimetypeWwwFormUrlEncoded(mimeType)) {
                    producesFormUrlEncoded = true;
                } else if (isMimetypePlain(mimeType)) {
                    producesPlainText = true;
                }

                Map<String, String> mediaType = new HashMap<>();
                mediaType.put("mediaType", mimeType);

                produces.add(mediaType);
            }

            op.produces = produces;
            op.hasProduces = true;
        }

        // Set for deduplication of response IDs
        for (CodegenResponse rsp : op.responses) {
            // Get the original API response, so we get process the schema
            // directly.
            ApiResponse original;
            if ("0".equals(rsp.code)) {
                original = operation.getResponses().get("default");
            } else {
                original = operation.getResponses().get(rsp.code);
            }

            // Create a unique responseID for this response.
            String[] words = rsp.message.split("[^A-Za-z ]");

            // build responseId from both status code and description
            String responseId = "Status" + rsp.code + (
                    ((words.length != 0) && (!words[0].trim().isEmpty())) ?
                            "_" + camelize(words[0].replace(" ", "_")) : ""
            );

            rsp.vendorExtensions.put("x-response-id", responseId);
            if (rsp.dataType != null) {
                // Get the mimetype which is produced by this response. Note
                // that although in general responses produces a set of
                // different mimetypes currently we only support 1 per
                // response.
                String firstProduces = null;

                if (original.getContent() != null) {
                    firstProduces = original.getContent().keySet().stream().findFirst().orElse(null);
                }

                // The output mime type. This allows us to do sensible fallback
                // to JSON rather than using only the default operation
                // mimetype.
                String outputMime;

                if (firstProduces == null) {
                    if (producesFormUrlEncoded) {
                        outputMime = formUrlEncodedMimeType;
                    } else if (producesPlainText) {
                        if (bytesType.equals(rsp.dataType)) {
                            outputMime = octetMimeType;
                        } else {
                            outputMime = plainTextMimeType;
                        }
                    } else {
                        outputMime = jsonMimeType;
                    }
                } else {
                    if (isMimetypeWwwFormUrlEncoded(firstProduces)) {
                        producesFormUrlEncoded = true;
                        producesPlainText = false;
                    } else if (isMimetypePlain(firstProduces)) {
                        producesFormUrlEncoded = false;
                        producesPlainText = true;
                    } else {
                        producesFormUrlEncoded = false;
                        producesPlainText = false;
                    }

                    outputMime = firstProduces;

                    // As we don't support XML, fallback to plain text
                    if (isMimetypeXml(outputMime)) {
                        outputMime = plainTextMimeType;
                    }
                }

                rsp.vendorExtensions.put("x-mime-type", outputMime);

                if (producesFormUrlEncoded) {
                    rsp.vendorExtensions.put("x-produces-form-urlencoded", true);
                } else if (producesPlainText) {
                    // Plain text means that there is not structured data in
                    // this response. So it'll either be a UTF-8 encoded string
                    // 'plainText' or some generic 'bytes'.
                    //
                    // Note that we don't yet distinguish between string/binary
                    // and string/bytes - that is we don't auto-detect whether
                    // base64 encoding should be done. They both look like
                    // 'producesBytes'.
                    if (bytesType.equals(rsp.dataType)) {
                        rsp.vendorExtensions.put("x-produces-bytes", true);
                    } else {
                        rsp.vendorExtensions.put("x-produces-plain-text", true);
                    }
                } else {
                    rsp.vendorExtensions.put("x-produces-json", true);
                    if (isObjectType(rsp.dataType)) {
                        rsp.dataType = objectType;
                    }
                }
            }
        }

        return op;
    }

    @Override
    public OperationsMap postProcessOperationsWithModels(OperationsMap operationsMap, List<ModelMap> allModels) {
        OperationMap operations = operationsMap.getOperations();
        List<CodegenOperation> operationList = operations.getOperation();

        for (CodegenOperation op : operationList) {
            postProcessOperationWithModels(op);
        }

        return operationsMap;
    }

    private void postProcessOperationWithModels(CodegenOperation op) {
        boolean consumesJson = false;
        boolean consumesPlainText = false;
        boolean consumesFormUrlEncoded = false;

        if (op.consumes != null) {
            for (Map<String, String> consume : op.consumes) {
                final String mediaType = consume.get("mediaType");
                if (mediaType != null) {
                    if (isMimetypeJson(mediaType)) {
                        consumesJson = true;
                    } else if (isMimetypeWwwFormUrlEncoded(mediaType)) {
                        consumesFormUrlEncoded = true;
                    } else if (isMimetypePlain(mediaType)) {
                        consumesPlainText = true;
                    } else if (isMimetypeMultipartFormData(mediaType)) {
                        op.vendorExtensions.put("x-consumes-multipart", true);
                    } else if (isMimetypeMultipartRelated(mediaType)) {
                        op.vendorExtensions.put("x-consumes-multipart-related", true);
                    }
                }
            }
        }

        if (op.bodyParam != null) {
            if (consumesJson) {
                op.bodyParam.vendorExtensions.put("x-consumes-json", true);
            } else if (consumesFormUrlEncoded) {
                op.bodyParam.vendorExtensions.put("x-consumes-form-urlencoded", true);
            } else if (consumesPlainText) {
                op.bodyParam.vendorExtensions.put("x-consumes-plain-text", true);
            } else {
                op.bodyParam.vendorExtensions.put("x-consumes-json", true);
            }
        }

        for (CodegenParameter param : op.bodyParams) {
            // Default to producing json if nothing else is specified
            if (consumesJson) {
                param.vendorExtensions.put("x-consumes-json", true);
            } else if (consumesFormUrlEncoded) {
                param.vendorExtensions.put("x-consumes-form-urlencoded", true);
            } else if (consumesPlainText) {
                param.vendorExtensions.put("x-consumes-plain-text", true);
            } else {
                param.vendorExtensions.put("x-consumes-json", true);
            }
        }

        for (CodegenParameter param : op.queryParams) {
            // If the MIME type is JSON, mark it.  We don't currently support any other MIME types.
            if (param.contentType != null && isMimetypeJson(param.contentType)) {
                param.vendorExtensions.put("x-consumes-json", true);
            }
        }
    }

    @Override
    public boolean isDataTypeFile(final String dataType) {
        return dataType != null && dataType.equals(typeMapping.get("File"));
    }

    /**
     * Add operation to group
     *
     * @param tag          name of the tag
     * @param resourcePath path of the resource
     * @param operation    OAS Operation object
     * @param op           Codegen Operation object
     * @param operations   map of Codegen operations
     */
    @SuppressWarnings("static-method")
    @Override
    public void addOperationToGroup(String tag, String resourcePath, Operation operation, CodegenOperation
            op, Map<String, List<CodegenOperation>> operations) {
        // only generate operation for the first tag of the tags
        if (tag != null && op.tags.size() > 1) {
            String expectedTag = sanitizeTag(op.tags.get(0).getName());
            if (!tag.equals(expectedTag)) {
                LOGGER.info("generated skip additional tag `{}` with operationId={}", tag, op.operationId);
                return;
            }
        }
        super.addOperationToGroup(tag, resourcePath, operation, op, operations);
    }

    // This is a really terrible hack. We're working around the fact that the
    // base version of `fromRequestBody` checks to see whether the body is a
    // ref. If so, it unwraps the reference and replaces it with its inner
    // type. This causes problems in rust-axum, as it means that we use inner
    // types in the API, rather than the correct outer type.
    //
    // Thus, we grab the inner schema beforehand, and then tinker afterward to
    // restore things to sensible values.
    @Override
    public CodegenParameter fromRequestBody(RequestBody body, Set<String> imports, String bodyParameterName) {
        Schema original_schema = ModelUtils.getSchemaFromRequestBody(body);
        CodegenParameter codegenParameter = super.fromRequestBody(body, imports, bodyParameterName);

        if (StringUtils.isNotBlank(original_schema.get$ref())) {
            // Undo the mess `super.fromRequestBody` made - re-wrap the inner
            // type.
            codegenParameter.dataType = getTypeDeclaration(original_schema);
            codegenParameter.isPrimitiveType = false;
            codegenParameter.isArray = false;
            codegenParameter.isString = false;
            codegenParameter.isByteArray = ModelUtils.isByteArraySchema(original_schema);
        }

        return codegenParameter;
    }

    @Override
    public String toInstantiationType(Schema p) {
        if (ModelUtils.isArraySchema(p)) {
            Schema inner = ModelUtils.getSchemaItems(p);
            return instantiationTypes.get("array") + "<" + getSchemaType(inner) + ">";
        } else if (ModelUtils.isMapSchema(p)) {
            Schema inner = ModelUtils.getAdditionalProperties(p);
            return instantiationTypes.get("map") + "<" + typeMapping.get("string") + ", " + getSchemaType(inner) + ">";
        } else {
            return null;
        }
    }

    @Override
    public Map<String, Object> postProcessSupportingFileData(Map<String, Object> bundle) {
        generateYAMLSpecFile(bundle);

        final List<PathMethodOperations> pathMethodOps = pathMethodOpMap.entrySet().stream()
                .map(entry -> {
                    ArrayList<MethodOperation> methodOps = entry.getValue();
                    methodOps.sort(Comparator.comparing(a -> a.method));
                    return new PathMethodOperations(entry.getKey(), methodOps);
                })
                .sorted(Comparator.comparing(a -> a.path))
                .collect(Collectors.toList());
        bundle.put("pathMethodOps", pathMethodOps);

        return super.postProcessSupportingFileData(bundle);
    }

    @Override
    public String toDefaultValue(Schema p) {
        String defaultValue = null;
        if ((ModelUtils.isNullable(p)) && (p.getDefault() != null) && ("null".equalsIgnoreCase(p.getDefault().toString())))
            return "Nullable::Null";

        if (ModelUtils.isBooleanSchema(p)) {
            if (p.getDefault() != null) {
                if ("false".equalsIgnoreCase(p.getDefault().toString()))
                    defaultValue = "false";
                else
                    defaultValue = "true";
            }
        } else if (ModelUtils.isNumberSchema(p)) {
            if (p.getDefault() != null) {
                defaultValue = p.getDefault().toString();
            }
        } else if (ModelUtils.isIntegerSchema(p)) {
            if (p.getDefault() != null) {
                defaultValue = p.getDefault().toString();
            }
        } else if (ModelUtils.isStringSchema(p)) {
            if (p.getDefault() != null) {
                defaultValue = "\"" + p.getDefault() + "\".to_string()";
            }
        }

        if ((defaultValue != null) && (ModelUtils.isNullable(p)))
            defaultValue = "Nullable::Present(" + defaultValue + ")";

        return defaultValue;
    }

    @Override
    public void postProcessModelProperty(CodegenModel model, CodegenProperty property) {
        super.postProcessModelProperty(model, property);

        if (!languageSpecificPrimitives.contains(property.dataType)) {
            final int position = property.dataType.lastIndexOf(":");
            if (position != -1) {
                property.dataType = property.dataType.substring(0, position) + camelize(property.dataType.substring(position));
            } else {
                property.dataType = camelize(property.dataType);
            }
            property.isPrimitiveType = property.isContainer && languageSpecificPrimitives.contains(typeMapping.get(property.complexType));
        } else {
            property.isPrimitiveType = true;
        }

        // Integer type fitting
        if (Objects.equals(property.baseType, "integer")) {
            BigInteger minimum = Optional.ofNullable(property.getMinimum()).map(BigInteger::new).orElse(null);
            BigInteger maximum = Optional.ofNullable(property.getMaximum()).map(BigInteger::new).orElse(null);
            property.dataType = bestFittingIntegerType(
                    minimum, property.getExclusiveMinimum(),
                    maximum, property.getExclusiveMaximum(),
                    true);
        }

        property.name = underscore(property.name);

        if (!property.required) {
            property.defaultValue = (property.defaultValue != null) ? "Some(" + property.defaultValue + ")" : "None";
        }

        if (isObjectType(property.baseType)) {
            property.dataType = objectType;
            property.isNullable = false;
        }
    }

    @Override
    public ModelsMap postProcessModels(ModelsMap modelsMap) {
        for (ModelMap mo : modelsMap.getModels()) {
            CodegenModel cm = mo.getModel();

            LOGGER.trace("Post processing model: {}", cm);

            if (isObjectType(cm.dataType)) {
                // Object isn't a sensible default. Instead, we set it to
                // 'null'. This ensures that we treat this model as a struct
                // with multiple parameters.
                cm.dataType = null;
            } else if ("map".equals(cm.dataType)) {
                if (!cm.allVars.isEmpty() || cm.additionalPropertiesType == null) {
                    LOGGER.warn("Ignoring additionalProperties (see https://github.com/OpenAPITools/openapi-generator/issues/318) alongside defined properties");
                    cm.dataType = null;
                } else {
                    cm.dataType = mapType + "<String, " + cm.additionalPropertiesType + ">";
                }
            } else if (cm.dataType != null) {
                // We need to hack about with single-parameter models to
                // get them recognised correctly.
                cm.isAlias = false;
                cm.dataType = typeMapping.get(cm.dataType);
            }

            cm.vendorExtensions.put("x-is-string", stringType.equals(cm.dataType));
        }
        return super.postProcessModelsEnum(modelsMap);
    }

    @Override
    public void postProcessFile(File file, String fileType) {
        if (file == null) {
            return;
        }

        final String fileName = file.toString();

        String[] command;

        String cmd = System.getenv("RUST_POST_PROCESS_FILE");
        if (StringUtils.isEmpty(cmd)) {
            cmd = "rustfmt";
            command = new String[]{cmd, "--edition", "2021", fileName};
        } else {
            command = new String[]{cmd, fileName};
        }

        // only process files with .rs extension
        if ("rs".equals(FilenameUtils.getExtension(fileName))) {
            try {
                Process p = Runtime.getRuntime().exec(command);
                int exitValue = p.waitFor();
                if (exitValue != 0) {
                    LOGGER.error("Error running the command ({} {}). Exit code: {}", cmd, file, exitValue);
                } else {
                    LOGGER.info("Successfully executed: {} {}", cmd, file);
                }
            } catch (InterruptedException | IOException e) {
                LOGGER.error("Error running the command ({} {}). Exception: {}", cmd, file, e.getMessage());
                Thread.currentThread().interrupt();
            }
        }
    }

    @Override
    protected void updateParameterForString(CodegenParameter codegenParameter, Schema parameterSchema) {
        if (ModelUtils.isEmailSchema(parameterSchema)) {
            codegenParameter.isEmail = true;
        } else if (ModelUtils.isUUIDSchema(parameterSchema)) {
            codegenParameter.setIsString(false);
            codegenParameter.isUuid = true;
        } else if (ModelUtils.isByteArraySchema(parameterSchema)) {
            codegenParameter.setIsString(false);
            codegenParameter.isByteArray = true;
            codegenParameter.isPrimitiveType = true;
        } else if (ModelUtils.isBinarySchema(parameterSchema)) {
            codegenParameter.isBinary = true;
            codegenParameter.isFile = true; // file = binary in OAS3
            codegenParameter.isPrimitiveType = true;
        } else if (ModelUtils.isDateSchema(parameterSchema)) {
            codegenParameter.setIsString(false); // for backward compatibility with 2.x
            codegenParameter.isDate = true;
            codegenParameter.isPrimitiveType = true;
        } else if (ModelUtils.isDateTimeSchema(parameterSchema)) {
            codegenParameter.setIsString(false); // for backward compatibility with 2.x
            codegenParameter.isDateTime = true;
            codegenParameter.isPrimitiveType = true;
        } else if (ModelUtils.isDecimalSchema(parameterSchema)) { // type: string, format: number
            codegenParameter.setIsString(false);
            codegenParameter.isDecimal = true;
            codegenParameter.isPrimitiveType = true;
        }
        if (Boolean.TRUE.equals(codegenParameter.isString)) {
            codegenParameter.isPrimitiveType = true;
        }
    }

    @Override
    protected void updatePropertyForAnyType(CodegenProperty property, Schema p) {
        // The 'null' value is allowed when the OAS schema is 'any type'.
        // See https://github.com/OAI/OpenAPI-Specification/issues/1389
        if (Boolean.FALSE.equals(p.getNullable())) {
            LOGGER.warn("Schema '{}' is any type, which includes the 'null' value. 'nullable' cannot be set to 'false'", p.getName());
        }
        if (languageSpecificPrimitives.contains(property.dataType)) {
            property.isPrimitiveType = true;
        }
        if (ModelUtils.isMapSchema(p)) {
            // an object or anyType composed schema that has additionalProperties set
            // some of our code assumes that any type schema with properties defined will be a map
            // even though it should allow in any type and have map constraints for properties
            updatePropertyForMap(property, p);
        }
    }

    @Override
    protected String getParameterDataType(Parameter parameter, Schema schema) {
        if (parameter.get$ref() != null) {
            String refName = ModelUtils.getSimpleRef(parameter.get$ref());
            return toModelName(refName);
        }
        return null;
    }

    static class PathMethodOperations {
        public String path;
        public ArrayList<MethodOperation> methodOperations;

        PathMethodOperations(String path, ArrayList<MethodOperation> methodOperations) {
            this.path = path;
            this.methodOperations = methodOperations;
        }
    }

    static class MethodOperation {
        public String method;
        public String operationID;

        MethodOperation(String method, String operationID) {
            this.method = method;
            this.operationID = operationID;
        }
    }
}
