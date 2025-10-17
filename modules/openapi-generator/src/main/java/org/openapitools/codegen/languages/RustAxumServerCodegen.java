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
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.parameters.Parameter;
import io.swagger.v3.oas.models.parameters.RequestBody;
import io.swagger.v3.oas.models.responses.ApiResponse;
import io.swagger.v3.oas.models.servers.Server;
import io.swagger.v3.oas.models.tags.Tag;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.*;
import org.openapitools.codegen.meta.GeneratorMetadata;
import org.openapitools.codegen.meta.Stability;
import org.openapitools.codegen.meta.features.GlobalFeature;
import org.openapitools.codegen.meta.features.SchemaSupportFeature;
import org.openapitools.codegen.meta.features.SecurityFeature;
import org.openapitools.codegen.meta.features.WireFormatFeature;
import org.openapitools.codegen.model.ModelMap;
import org.openapitools.codegen.model.ModelsMap;
import org.openapitools.codegen.model.OperationMap;
import org.openapitools.codegen.model.OperationsMap;
import org.openapitools.codegen.utils.ModelUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.math.BigInteger;
import java.nio.file.Path;
import java.util.*;
import java.util.stream.Collectors;

import static org.openapitools.codegen.utils.StringUtils.camelize;
import static org.openapitools.codegen.utils.StringUtils.underscore;

public class RustAxumServerCodegen extends AbstractRustCodegen implements CodegenConfig {
    public static final String PROJECT_NAME = "openapi-server";

    private String packageName;
    private String packageVersion;
    private Boolean disableValidator = false;
    private Boolean allowBlockingValidator = false;
    private Boolean allowBlockingResponseSerialize = false;
    private String externCrateName;
    private Boolean basicAuthorization = false;
    private Boolean basicAnalytic = false;
    private Boolean ownedRequest = false;

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
    private boolean havingAuthMethods = false;
    private boolean havingBasicAuthMethods = false;

    // Logger
    private final Logger LOGGER = LoggerFactory.getLogger(RustAxumServerCodegen.class);

    public RustAxumServerCodegen() {
        super();

        // The `#[validate(nested)]` macro relies on an internal field named `errors` to accumulate validation results. Therefore, defining a struct like this will fail:
        //
        // ```rust
        // struct A {
        //   #[validate(nested)]
        //   errors: B,
        // }
        // ```
        //
        // To avoid this, either rename the field to something other than "errors", or reserve it.
        this.reservedWords.add("errors");

        modifyFeatureSet(features -> features
                .wireFormatFeatures(EnumSet.of(
                        WireFormatFeature.JSON,
                        WireFormatFeature.Custom
                ))
                .securityFeatures(EnumSet.of(
                        SecurityFeature.ApiKey,
                        SecurityFeature.BasicAuth,
                        SecurityFeature.BearerToken
                ))
                .schemaSupportFeatures(EnumSet.of(
                        SchemaSupportFeature.Simple,
                        SchemaSupportFeature.Composite,
                        SchemaSupportFeature.oneOf,
                        SchemaSupportFeature.anyOf,
                        SchemaSupportFeature.allOf
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
        apiTemplateFiles.put("apis.mustache", ".rs");

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
        supportingFiles.add(new SupportingFile("types.mustache", "src", "types.rs"));
        supportingFiles.add(new SupportingFile("header.mustache", "src", "header.rs"));
        supportingFiles.add(new SupportingFile("models.mustache", "src", "models.rs"));
        supportingFiles.add(new SupportingFile("apis-mod.mustache", apiPackage().replace('.', File.separatorChar), "mod.rs"));
        supportingFiles.add(new SupportingFile("server-mod.mustache", "src/server", "mod.rs"));
        supportingFiles.add(new SupportingFile("lib.mustache", "src", "lib.rs"));
        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md").doNotOverwrite());
    }

    @Override
    public CodegenType getTag() {
        return CodegenType.SERVER;
    }

    @Override
    public String getName() {
        return "rust-axum";
    }

    @Override
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
        } else if (!this.isEnablePostProcessFile()) {
            LOGGER.info("Warning: Environment variable 'RUST_POST_PROCESS_FILE' is set but file post-processing is not enabled. To enable file post-processing, 'enablePostProcessFile' must be set to `true` (--enable-post-process-file for CLI).");
        }

        if (!ModelUtils.isGenerateAliasAsModel()) {
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

        if (additionalProperties.containsKey("basicAuthorization")) {
            basicAuthorization = convertPropertyToBooleanAndWriteBack("basicAuthorization");
        } else {
            additionalProperties.put("basicAuthorization", basicAuthorization);
        }

        if (additionalProperties.containsKey("basicAnalytic")) {
            basicAnalytic = convertPropertyToBooleanAndWriteBack("basicAnalytic");
        } else {
            additionalProperties.put("basicAnalytic", basicAnalytic);
        }

        if (additionalProperties.containsKey("ownedRequest")) {
            ownedRequest = convertPropertyToBooleanAndWriteBack("ownedRequest");
        } else {
            additionalProperties.put("ownedRequest", ownedRequest);
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
        return "src" + File.separator + "apis";
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

    @Override
    public String toApiFilename(String name) {
        return toApiName(name);
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
        String axumPath = op.path;
        for (CodegenParameter param : op.pathParams) {
            // Replace {baseName} with {paramName} for format string
            String paramSearch = "{" + param.baseName + "}";
            String paramReplace = "{" + param.paramName + "}";

            axumPath = axumPath.replace(paramSearch, paramReplace);
        }
        ArrayList<MethodOperation> pathMethods = pathMethodOpMap.get(axumPath);

        // Prevent multiple declarations of the same operation
        if (pathMethods != null && pathMethods.stream().anyMatch(pathMethod ->
                pathMethod.operationID.equals(underscoredOperationId))) {
            return op;
        }

        op.vendorExtensions.put("x-operation-id", underscoredOperationId);
        op.vendorExtensions.put("x-uppercase-operation-id", underscoredOperationId.toUpperCase(Locale.ROOT));

        if (!op.isCallbackRequest) {
            // group route by path
            pathMethodOpMap
                    .computeIfAbsent(axumPath, (key) -> new ArrayList<>())
                    .add(new MethodOperation(
                            op.httpMethod.toLowerCase(Locale.ROOT),
                            underscoredOperationId,
                            op.vendorExtensions));
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
            original = ModelUtils.getReferencedApiResponse(openAPI, original);

            // Create a unique responseID for this response, if one is not already specified with the "x-response-id" extension
            if (!rsp.vendorExtensions.containsKey("x-response-id")) {
                String[] words = rsp.message.split("[^A-Za-z ]");

                // build responseId from both status code and description
                String responseId = "Status" + rsp.code + (
                        ((words.length != 0) && (!words[0].trim().isEmpty())) ?
                                "_" + camelize(words[0].replace(" ", "_")) : ""
                );
                rsp.vendorExtensions.put("x-response-id", responseId);
            }

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

            for (CodegenProperty header : rsp.headers) {
                if (uuidType.equals(header.dataType)) {
                    additionalProperties.put("apiUsesUuid", true);
                }
                header.nameInPascalCase = toModelName(header.baseName);
                header.nameInLowerCase = header.baseName.toLowerCase(Locale.ROOT);
            }
        }

        for (CodegenParameter header : op.headerParams) {
            header.nameInLowerCase = header.baseName.toLowerCase(Locale.ROOT);
        }

        for (CodegenProperty header : op.responseHeaders) {
            if (uuidType.equals(header.dataType)) {
                additionalProperties.put("apiUsesUuid", true);
            }
            header.nameInPascalCase = toModelName(header.baseName);
            header.nameInLowerCase = header.baseName.toLowerCase(Locale.ROOT);
        }

        // Include renderUuidConversionImpl exactly once in the vendorExtensions map when
        // at least one `uuid::Uuid` converted from a header value in the resulting Rust code.
        final boolean renderUuidConversionImpl = op.headerParams.stream().anyMatch(h -> h.getDataType().equals(uuidType));
        if (renderUuidConversionImpl)
            additionalProperties.put("renderUuidConversionImpl", "true");

        return op;
    }

    private void postProcessPolymorphism(final List<ModelMap> allModels) {
        final HashMap<String, List<String>> discriminatorsForModel = new HashMap<>();

        for (final ModelMap mo : allModels) {
            final CodegenModel cm = mo.getModel();

            final CodegenComposedSchemas cs = cm.getComposedSchemas();
            if (cs != null) {
                final List<CodegenProperty> csOneOf = cs.getOneOf();
                if (csOneOf != null) {
                    processPolymorphismDataType(csOneOf);
                    cs.setOneOf(csOneOf);
                    cm.setComposedSchemas(cs);
                }

                final List<CodegenProperty> csAnyOf = cs.getAnyOf();
                if (csAnyOf != null) {
                    processPolymorphismDataType(csAnyOf);
                    cs.setAnyOf(csAnyOf);
                    cm.setComposedSchemas(cs);
                }
            }

            if (cm.discriminator != null) {
                for (final String model : cm.oneOf) {
                    final List<String> discriminators = discriminatorsForModel.getOrDefault(model, new ArrayList<>());
                    discriminators.add(cm.discriminator.getPropertyName());
                    discriminatorsForModel.put(model, discriminators);
                }

                for (final String model : cm.anyOf) {
                    final List<String> discriminators = discriminatorsForModel.getOrDefault(model, new ArrayList<>());
                    discriminators.add(cm.discriminator.getPropertyName());
                    discriminatorsForModel.put(model, discriminators);
                }
            }
        }

        final var blocking = new HashSet<String>();
        for (ModelMap mo : allModels) {
            final CodegenModel cm = mo.getModel();

            final List<String> discriminators = discriminatorsForModel.get(cm.getSchemaName());
            if (discriminators != null) {
                // If the discriminator field is not a defined attribute in the variant structure, create it.
                if (!discriminating(discriminators, cm)) {
                    final String discriminator = discriminators.get(0);

                    CodegenProperty property = new CodegenProperty();

                    // Static attributes
                    // Only strings are supported by serde for tag field types, so it's the only one we'll deal with
                    property.openApiType = "string";
                    property.complexType = "string";
                    property.dataType = "String";
                    property.datatypeWithEnum = "String";
                    property.baseType = "string";
                    property.required = true;
                    property.isPrimitiveType = true;
                    property.isString = true;
                    property.isDiscriminator = true;

                    // Attributes based on the discriminator value
                    property.baseName = discriminator;
                    property.name = discriminator;
                    property.nameInCamelCase = camelize(discriminator);
                    property.nameInPascalCase = property.nameInCamelCase.substring(0, 1).toUpperCase(Locale.ROOT) + property.nameInCamelCase.substring(1);
                    property.nameInSnakeCase = underscore(discriminator).toUpperCase(Locale.ROOT);
                    property.getter = String.format(Locale.ROOT, "get%s", property.nameInPascalCase);
                    property.setter = String.format(Locale.ROOT, "set%s", property.nameInPascalCase);
                    property.defaultValueWithParam = String.format(Locale.ROOT, " = data.%s;", property.name);

                    // Attributes based on the model name
                    property.defaultValue = String.format(Locale.ROOT, "r#\"%s\"#.to_string()", cm.getSchemaName());
                    property.jsonSchema = String.format(Locale.ROOT, "{ \"default\":\"%s\"; \"type\":\"string\" }", cm.getSchemaName());

                    cm.vars.add(property);
                }
            }

            if (cm.vars.stream().noneMatch(v -> v.isDiscriminator)) {
                blocking.add(cm.getSchemaName());
            }
        }

        for (final ModelMap mo : allModels) {
            final CodegenModel cm = mo.getModel();
            if (cm.discriminator != null) {
                // if no discriminator in any of variant -> disable discriminator
                if (cm.oneOf.stream().anyMatch(blocking::contains) || cm.anyOf.stream().anyMatch(blocking::contains)) {
                    cm.discriminator = null;
                }
            }
        }
    }

    private static boolean discriminating(final List<String> discriminatorsForModel, final CodegenModel cm) {
        resetDiscriminatorProperty(cm);

        // Discriminator will be presented as enum tag -> One and only one tag is allowed
        int countString = 0;
        int countNonString = 0;
        for (final CodegenProperty var : cm.vars) {
            if (discriminatorsForModel.stream().anyMatch(discriminator -> var.baseName.equals(discriminator) || var.name.equals(discriminator))) {
                if (var.isString) {
                    var.isDiscriminator = true;
                    ++countString;
                } else
                    ++countNonString;
            }
        }

        if (countString > 0 && (countNonString > 0 || countString > 1)) {
            // at least two discriminator, one of them is string -> should not render serde tag
            resetDiscriminatorProperty(cm);
        }

        return countNonString > 0 || countString > 0;
    }

    private static void resetDiscriminatorProperty(final CodegenModel cm) {
        for (final CodegenProperty var : cm.vars) {
            var.isDiscriminator = false;
        }
    }

    private static void processPolymorphismDataType(final List<CodegenProperty> cp) {
        final HashSet<String> dedupDataTypeWithEnum = new HashSet<>();
        final HashMap<String, Integer> dedupDataType = new HashMap<>();

        int idx = 0;
        for (CodegenProperty model : cp) {
            // Generate a valid name for the enum variant.
            // Mainly needed for primitive types.
            model.datatypeWithEnum = camelize(model.dataType.replaceAll("(?:\\w+::)+(\\w+)", "$1")
                    .replace("<", "Of").replace(">", "")).replace(" ", "").replace(",", "");
            if (!dedupDataTypeWithEnum.add(model.datatypeWithEnum)) {
                model.datatypeWithEnum += ++idx;
            }

            dedupDataType.put(model.getDataType(), dedupDataType.getOrDefault(model.getDataType(), 0) + 1);

            if (!model.getDataType().matches(String.format(Locale.ROOT, ".*::%s", model.getDatatypeWithEnum()))) {
                model.isPrimitiveType = true;
            }
        }

        for (CodegenProperty model : cp) {
            if (dedupDataType.get(model.getDataType()) == 1) {
                model.vendorExtensions.put("x-from-trait", true);
            }
        }
    }

    @Override
    public OperationsMap postProcessOperationsWithModels(final OperationsMap operationsMap, List<ModelMap> allModels) {
        postProcessPolymorphism(allModels);

        final OperationMap operations = operationsMap.getOperations();
        operations.put("classnamePascalCase", camelize(operations.getClassname()));

        final boolean hasAuthMethod = operations.getOperation().stream()
                .map(this::postProcessOperationWithModels)
                .reduce(false, (a, b) -> a || b);
        if (hasAuthMethod) {
            operations.put("havingAuthMethod", true);
            operations.getOperation().forEach(op -> op.vendorExtensions.put("havingAuthMethod", true));
            this.havingAuthMethods = true;

            if (basicAuthorization) {
                operations.put("basicAuthorization", true);
                operations.getOperation().forEach(op -> op.vendorExtensions.put("basicAuthorization", true));
            }
        }

        if (basicAnalytic) {
            operations.put("basicAnalytic", true);
            operations.getOperation().forEach(op -> op.vendorExtensions.put("basicAnalytic", true));
        }

        if (ownedRequest) {
            operations.put("ownedRequest", true);
            operations.getOperation().forEach(op -> op.vendorExtensions.put("ownedRequest", true));
        }

        return operationsMap;
    }

    private boolean postProcessOperationWithModels(final CodegenOperation op) {
        boolean consumesJson = false;
        boolean consumesPlainText = false;
        boolean consumesFormUrlEncoded = false;
        boolean hasAuthMethod = false;

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
            final var dataType = op.bodyParam.dataType;
            if (dataType.startsWith(vecType + "<String")) {
                op.bodyParam.vendorExtensions.put("is-vec-string", true);
            } else if (dataType.startsWith(vecType + "<models::")) {
                op.bodyParam.vendorExtensions.put("is-vec-nested", true);
            } else if (dataType.startsWith(mapType + "<String, String")) {
                op.bodyParam.vendorExtensions.put("is-map-string", true);
            } else if (dataType.startsWith(mapType + "<String, models::")) {
                op.bodyParam.vendorExtensions.put("is-map-nested", true);
            } else if (dataType.startsWith(mapType + "<String")) {
                op.bodyParam.vendorExtensions.put("is-map", true);
            } else if (dataType.startsWith("models::")) {
                op.bodyParam.isModel = true;
            } else if (dataType.equals("String")) {
                op.bodyParam.isString = true;
                op.bodyParam.vendorExtensions.put("is-string", true);
            }

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

        if (op.authMethods != null) {
            for (CodegenSecurity s : op.authMethods) {
                if (s.isApiKey && (s.isKeyInCookie || s.isKeyInHeader)) {
                    if (s.isKeyInCookie) {
                        op.vendorExtensions.put("x-has-cookie-auth-methods", true);
                        op.vendorExtensions.put("x-api-key-cookie-name", s.keyParamName);
                    } else {
                        op.vendorExtensions.put("x-has-header-auth-methods", true);
                        op.vendorExtensions.put("x-api-key-header-name", s.keyParamName);
                    }

                    op.vendorExtensions.put("x-has-auth-methods", true);
                    hasAuthMethod = true;
                } else if (s.isBasic) {
                    op.vendorExtensions.put("x-has-basic-auth-methods", true);
                    op.vendorExtensions.put("x-is-basic-bearer", s.isBasicBearer);
                    op.vendorExtensions.put("x-api-auth-header-name", "authorization");

                    op.vendorExtensions.put("x-has-auth-methods", true);
                    hasAuthMethod = true;

                    if (!this.havingBasicAuthMethods)
                        this.havingBasicAuthMethods = true;
                }
            }
        }

        return hasAuthMethod;
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
        // If more than one tag, combine into a single unique group
        if (tag != null && op.tags.size() > 1) {
            // Skip any tags other than the first one. This is because we
            // override the tag with a combined version of all the tags.
            String expectedTag = sanitizeTag(op.tags.get(0).getName());
            if (!tag.equals(expectedTag)) {
                LOGGER.info("generated skip additional tag `{}` with operationId={}", tag, op.operationId);
                return;
            }

            // Get all tags sorted by name & Combine into a single group
            final String combinedTag = op.tags.stream()
                    .map(Tag::getName).sorted()
                    .collect(Collectors.joining("-"));
            // Add to group
            super.addOperationToGroup(combinedTag, resourcePath, operation, op, operations);

            return;
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
        final var original_schema = ModelUtils.getSchemaFromRequestBody(body);
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
    public String toInstantiationType(final Schema p) {
        if (ModelUtils.isArraySchema(p)) {
            final var inner = ModelUtils.getSchemaItems(p);
            return instantiationTypes.get("array") + "<" + getSchemaType(inner) + ">";
        } else if (ModelUtils.isMapSchema(p)) {
            final var inner = ModelUtils.getAdditionalProperties(p);
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
                    final ArrayList<MethodOperation> methodOps = entry.getValue();
                    methodOps.sort(Comparator.comparing(a -> a.method));
                    return new PathMethodOperations(entry.getKey(), methodOps);
                })
                .sorted(Comparator.comparing(a -> a.path))
                .collect(Collectors.toList());
        bundle.put("pathMethodOps", pathMethodOps);
        if (havingAuthMethods) bundle.put("havingAuthMethods", true);
        if (havingBasicAuthMethods) bundle.put("havingBasicAuthMethods", true);

        return super.postProcessSupportingFileData(bundle);
    }

    @Override
    public String toDefaultValue(final Schema p) {
        String defaultValue = null;

        if (ModelUtils.isEnumSchema(p))
            return null;

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
                if (!defaultValue.contains(".")) {
                    defaultValue += ".0";
                }
            }
        } else if (ModelUtils.isIntegerSchema(p)) {
            if (p.getDefault() != null) {
                defaultValue = p.getDefault().toString();
            }
        } else if (ModelUtils.isStringSchema(p)) {
            if (p.getDefault() != null) {
                defaultValue = "r#\"" + p.getDefault() + "\"#.to_string()";
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

        if (property.dataType.startsWith(vecType + "<String")) {
            property.vendorExtensions.put("is-vec-string", true);
        } else if (property.dataType.startsWith(vecType + "<models::")) {
            property.vendorExtensions.put("is-vec-nested", true);
        } else if (property.dataType.startsWith(mapType + "<String, String")) {
            property.vendorExtensions.put("is-map-string", true);
        } else if (property.dataType.startsWith(mapType + "<String, models::")) {
            property.vendorExtensions.put("is-map-nested", true);
        } else if (property.dataType.startsWith(mapType + "<String")) {
            property.vendorExtensions.put("is-map", true);
        } else if (property.dataType.startsWith("models::")) {
            property.vendorExtensions.put("is-nested", true);
        } else if (stringType.equals(property.dataType)) {
            property.vendorExtensions.put("is-string", true);
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
        super.postProcessFile(file, fileType);
        if (file == null) {
            return;
        }

        final String fileName = file.toString();

        String[] command;

        String cmd = System.getenv("RUST_POST_PROCESS_FILE");
        if (StringUtils.isEmpty(cmd)) {
            cmd = "rustfmt";
            command = new String[]{cmd, "--edition", "2024", fileName};
        } else {
            command = new String[]{cmd, fileName};
        }

        // only process files with .rs extension
        if ("rs".equals(FilenameUtils.getExtension(fileName))) {
            this.executePostProcessor(command);
        }
    }

    @Override
    protected void updateParameterForString(CodegenParameter codegenParameter, final Schema parameterSchema) {
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
        if (codegenParameter.isString) {
            codegenParameter.isPrimitiveType = true;
        }
    }

    @Override
    protected void updatePropertyForAnyType(final CodegenProperty property, final Schema p) {
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
    protected String getParameterDataType(final Parameter parameter, final Schema schema) {
        if (parameter.get$ref() != null) {
            String refName = ModelUtils.getSimpleRef(parameter.get$ref());
            return toModelName(refName);
        }
        return null;
    }

    @Override
    public String toVarName(String name) {
        final var varName = super.toVarName(name);

        if (varName.startsWith("r#"))
            return "r_" + varName.substring(2);

        return varName;
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
        public Map<String, Object> vendorExtensions;

        MethodOperation(String method, String operationID, Map<String, Object> vendorExtensions) {
            this.method = method;
            this.operationID = operationID;
            this.vendorExtensions = vendorExtensions;
        }
    }
}
