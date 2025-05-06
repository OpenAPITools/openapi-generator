/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 * Copyright 2018 SmartBear Software
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

import com.google.common.collect.ImmutableMap;
import com.samskivert.mustache.Mustache;
import com.samskivert.mustache.Mustache.Lambda;
import com.samskivert.mustache.Template;
import io.swagger.v3.oas.models.media.Discriminator;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.parser.util.SchemaTypeUtil;
import joptsimple.internal.Strings;
import lombok.AccessLevel;
import lombok.Setter;
import org.openapitools.codegen.*;
import org.openapitools.codegen.meta.features.*;
import org.openapitools.codegen.model.ModelMap;
import org.openapitools.codegen.model.ModelsMap;
import org.openapitools.codegen.model.OperationMap;
import org.openapitools.codegen.model.OperationsMap;
import org.openapitools.codegen.templating.mustache.ReplaceAllLambda;
import org.openapitools.codegen.utils.ModelUtils;
import org.openapitools.codegen.utils.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.IOException;
import java.io.Writer;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.*;
import java.util.stream.Collectors;

public class RustClientCodegen extends AbstractRustCodegen implements CodegenConfig {
    private final Logger LOGGER = LoggerFactory.getLogger(RustClientCodegen.class);
    @Setter(AccessLevel.PRIVATE) private boolean useSingleRequestParameter = false;
    @Setter(AccessLevel.PRIVATE) private boolean supportAsync = true;
    @Setter(AccessLevel.PRIVATE) private boolean supportMiddleware = false;
    @Setter(AccessLevel.PRIVATE) private boolean supportTokenSource = false;
    private boolean supportMultipleResponses = false;
    private boolean withAWSV4Signature = false;
    @Setter private boolean preferUnsignedInt = false;
    @Setter private boolean bestFitInt = false;
    @Setter private boolean avoidBoxedModels = false;

    public static final String PACKAGE_NAME = "packageName";
    public static final String EXTERN_CRATE_NAME = "externCrateName";
    public static final String PACKAGE_VERSION = "packageVersion";
    public static final String HYPER_LIBRARY = "hyper";
    public static final String HYPER0X_LIBRARY = "hyper0x";
    public static final String REQWEST_LIBRARY = "reqwest";
    public static final String REQWEST_TRAIT_LIBRARY = "reqwest-trait";
    public static final String REQWEST_TRAIT_LIBRARY_ATTR = "reqwestTrait";
    public static final String SUPPORT_ASYNC = "supportAsync";
    public static final String SUPPORT_MIDDLEWARE = "supportMiddleware";
    public static final String SUPPORT_TOKEN_SOURCE = "supportTokenSource";
    public static final String SUPPORT_MULTIPLE_RESPONSES = "supportMultipleResponses";
    public static final String PREFER_UNSIGNED_INT = "preferUnsignedInt";
    public static final String BEST_FIT_INT = "bestFitInt";
    public static final String AVOID_BOXED_MODELS = "avoidBoxedModels";
    public static final String TOP_LEVEL_API_CLIENT = "topLevelApiClient";
    public static final String MOCKALL = "mockall";
    public static final String BON_BUILDER = "useBonBuilder";

    @Setter protected String packageName = "openapi";
    @Setter protected String packageVersion = "1.0.0";
    protected String apiDocPath = "docs/";
    protected String modelDocPath = "docs/";
    protected String apiFolder = "src/apis";
    protected String modelFolder = "src/models";
    // The API has at least one UUID type.
    // If the API does not contain any UUIDs we do not need depend on the `uuid` crate
    private boolean hasUUIDs = false;

    @Override
    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    @Override
    public String getName() {
        return "rust";
    }

    @Override
    public String getHelp() {
        return "Generates a Rust client library (beta).";
    }

    public RustClientCodegen() {
        super();

        modifyFeatureSet(features -> features
                .includeDocumentationFeatures(DocumentationFeature.Readme)
                .wireFormatFeatures(EnumSet.of(WireFormatFeature.JSON, WireFormatFeature.XML, WireFormatFeature.Custom))
                .securityFeatures(EnumSet.of(
                        SecurityFeature.BasicAuth,
                        SecurityFeature.BearerToken,
                        SecurityFeature.ApiKey,
                        SecurityFeature.OAuth2_Implicit,
                        SecurityFeature.AWSV4Signature
                ))
                .excludeGlobalFeatures(
                        GlobalFeature.XMLStructureDefinitions,
                        GlobalFeature.Callbacks,
                        GlobalFeature.LinkObjects,
                        GlobalFeature.ParameterStyling
                )
                .excludeSchemaSupportFeatures(
                        SchemaSupportFeature.Polymorphism
                )
                .excludeParameterFeatures(
                        ParameterFeature.Cookie
                )
                .includeSchemaSupportFeatures(
                        SchemaSupportFeature.oneOf
                )
                .includeClientModificationFeatures(
                        ClientModificationFeature.BasePath,
                        ClientModificationFeature.UserAgent
                )
        );

        outputFolder = "generated-code/rust";
        modelTemplateFiles.put("model.mustache", ".rs");

        modelDocTemplateFiles.put("model_doc.mustache", ".md");
        apiDocTemplateFiles.put("api_doc.mustache", ".md");

        // default HIDE_GENERATION_TIMESTAMP to true
        hideGenerationTimestamp = Boolean.TRUE;

        embeddedTemplateDir = templateDir = "rust";

        defaultIncludes = new HashSet<>(
                Arrays.asList(
                        "map",
                        "array")
        );

        languageSpecificPrimitives = new HashSet<>(
                Arrays.asList(
                        "i8", "i16", "i32", "i64",
                        "u8", "u16", "u32", "u64",
                        "f32", "f64", "isize", "usize",
                        "char", "bool", "str", "String")
        );

        instantiationTypes.clear();
        /*instantiationTypes.put("array", "GoArray");
        instantiationTypes.put("map", "GoMap");*/

        typeMapping.clear();
        typeMapping.put("integer", "i32");
        typeMapping.put("long", "i64");
        typeMapping.put("number", "f64");
        typeMapping.put("float", "f32");
        typeMapping.put("double", "f64");
        typeMapping.put("boolean", "bool");
        typeMapping.put("string", "String");
        typeMapping.put("array", "Vec");
        typeMapping.put("map", "std::collections::HashMap");
        typeMapping.put("UUID", "uuid::Uuid");
        typeMapping.put("URI", "String");
        typeMapping.put("date", "string");
        typeMapping.put("DateTime", "String");
        typeMapping.put("password", "String");
        typeMapping.put("decimal", "String");

        // TODO(bcourtine): review file mapping.
        // I tried to map as "std::io::File", but Reqwest multipart file requires a "AsRef<Path>" param.
        // Getting a file from a Path is simple, but the opposite is difficult. So I map as "std::path::Path".
        typeMapping.put("file", "std::path::PathBuf");
        typeMapping.put("binary", "crate::models::File");
        typeMapping.put("ByteArray", "String");
        typeMapping.put("object", "serde_json::Value");
        typeMapping.put("AnyType", "serde_json::Value");

        // no need for rust
        //importMapping = new HashMap<String, String>();

        cliOptions.clear();
        cliOptions.add(new CliOption(CodegenConstants.PACKAGE_NAME, "Rust package name (convention: lowercase).")
                .defaultValue("openapi"));
        cliOptions.add(new CliOption(CodegenConstants.PACKAGE_VERSION, "Rust package version.")
                .defaultValue("1.0.0"));
        cliOptions.add(new CliOption(CodegenConstants.HIDE_GENERATION_TIMESTAMP, CodegenConstants.HIDE_GENERATION_TIMESTAMP_DESC)
                .defaultValue(Boolean.TRUE.toString()));
        cliOptions.add(new CliOption(CodegenConstants.USE_SINGLE_REQUEST_PARAMETER, CodegenConstants.USE_SINGLE_REQUEST_PARAMETER_DESC, SchemaTypeUtil.BOOLEAN_TYPE)
                .defaultValue(Boolean.FALSE.toString()));
        cliOptions.add(new CliOption(SUPPORT_ASYNC, "If set, generate async function call instead. This option is for 'reqwest' library only", SchemaTypeUtil.BOOLEAN_TYPE)
                .defaultValue(Boolean.TRUE.toString()));
        cliOptions.add(new CliOption(SUPPORT_MIDDLEWARE, "If set, add support for reqwest-middleware. This option is for 'reqwest' and 'reqwest-trait' library only", SchemaTypeUtil.BOOLEAN_TYPE)
                .defaultValue(Boolean.FALSE.toString()));
        cliOptions.add(new CliOption(SUPPORT_TOKEN_SOURCE, "If set, add support for google-cloud-token. This option is for 'reqwest' and 'reqwest-trait' library only and requires the 'supportAsync' option", SchemaTypeUtil.BOOLEAN_TYPE)
                .defaultValue(Boolean.FALSE.toString()));
        cliOptions.add(new CliOption(SUPPORT_MULTIPLE_RESPONSES, "If set, return type wraps an enum of all possible 2xx schemas. This option is for 'reqwest' and 'reqwest-trait' library only", SchemaTypeUtil.BOOLEAN_TYPE)
                .defaultValue(Boolean.FALSE.toString()));
        cliOptions.add(new CliOption(CodegenConstants.ENUM_NAME_SUFFIX, CodegenConstants.ENUM_NAME_SUFFIX_DESC).defaultValue(this.enumSuffix));
        cliOptions.add(new CliOption(CodegenConstants.WITH_AWSV4_SIGNATURE_COMMENT, CodegenConstants.WITH_AWSV4_SIGNATURE_COMMENT_DESC, SchemaTypeUtil.BOOLEAN_TYPE)
                .defaultValue(Boolean.FALSE.toString()));
        cliOptions.add(new CliOption(PREFER_UNSIGNED_INT, "Prefer unsigned integers where minimum value is >= 0", SchemaTypeUtil.BOOLEAN_TYPE)
                .defaultValue(Boolean.FALSE.toString()));
        cliOptions.add(new CliOption(BEST_FIT_INT, "Use best fitting integer type where minimum or maximum is set", SchemaTypeUtil.BOOLEAN_TYPE)
                .defaultValue(Boolean.FALSE.toString()));
        cliOptions.add(new CliOption(AVOID_BOXED_MODELS, "If set, `Box<T>` will not be used for models", SchemaTypeUtil.BOOLEAN_TYPE)
                .defaultValue(Boolean.FALSE.toString()));
        cliOptions.add(new CliOption(MOCKALL, "Adds `#[automock]` from the mockall crate to api traits. This option is for 'reqwest-trait' library only", SchemaTypeUtil.BOOLEAN_TYPE)
                .defaultValue(Boolean.FALSE.toString()));
        cliOptions.add(new CliOption(TOP_LEVEL_API_CLIENT, "Creates a top level `Api` trait and `ApiClient` struct that contain all Apis. This option is for 'reqwest-trait' library only", SchemaTypeUtil.BOOLEAN_TYPE)
                .defaultValue(Boolean.FALSE.toString()));
        cliOptions.add(new CliOption(BON_BUILDER, "Use the bon crate for building parameter types. This option is for the 'reqwest-trait' library only", SchemaTypeUtil.BOOLEAN_TYPE)
                .defaultValue(Boolean.FALSE.toString()));

        supportedLibraries.put(HYPER_LIBRARY, "HTTP client: Hyper (v1.x).");
        supportedLibraries.put(HYPER0X_LIBRARY, "HTTP client: Hyper (v0.x).");
        supportedLibraries.put(REQWEST_LIBRARY, "HTTP client: Reqwest.");
        supportedLibraries.put(REQWEST_TRAIT_LIBRARY, "HTTP client: Reqwest (trait based).");


        CliOption libraryOption = new CliOption(CodegenConstants.LIBRARY, "library template (sub-template) to use.");
        libraryOption.setEnum(supportedLibraries);
        // set reqwest as the default
        libraryOption.setDefault(REQWEST_LIBRARY);
        cliOptions.add(libraryOption);
        setLibrary(REQWEST_LIBRARY);
    }

    @Override
    public CodegenModel fromModel(String name, Schema model) {
        CodegenModel mdl = super.fromModel(name, model);

        // set correct names and baseNames to oneOf in composed-schema to use as enum variant names & mapping
        if (mdl.getComposedSchemas() != null && mdl.getComposedSchemas().getOneOf() != null
                && !mdl.getComposedSchemas().getOneOf().isEmpty()) {

            List<CodegenProperty> newOneOfs = mdl.getComposedSchemas().getOneOf().stream()
                    .map(CodegenProperty::clone)
                    .collect(Collectors.toList());
            List<Schema> schemas = ModelUtils.getInterfaces(model);
            if (newOneOfs.size() != schemas.size()) {
                // For safety reasons, this should never happen unless there is an error in the code
                throw new RuntimeException("oneOf size does not match the model");
            }

            Map<String, String> refsMapping = Optional.ofNullable(model.getDiscriminator())
                    .map(Discriminator::getMapping).orElse(Collections.emptyMap());

            // Reverse mapped references to use as baseName for oneOF, but different keys may point to the same $ref.
            // Thus, we group them by the value
            Map<String, List<String>> mappedNamesByRef = refsMapping.entrySet().stream()
                    .collect(Collectors.groupingBy(Map.Entry::getValue,
                            Collectors.mapping(Map.Entry::getKey, Collectors.toList())
                    ));

            for (int i = 0; i < newOneOfs.size(); i++) {
                CodegenProperty oneOf = newOneOfs.get(i);
                Schema schema = schemas.get(i);

                if (mappedNamesByRef.containsKey(schema.get$ref())) {
                    // prefer mapped names if present
                    // remove mapping not in order not to reuse for the next occurrence of the ref
                    List<String> names = mappedNamesByRef.get(schema.get$ref());
                    String mappedName = names.remove(0);
                    oneOf.setBaseName(mappedName);
                    oneOf.setName(toModelName(mappedName));
                } else if (!org.apache.commons.lang3.StringUtils.isEmpty(schema.get$ref())) {
                    // use $ref if it's reference
                    String refName = ModelUtils.getSimpleRef(schema.get$ref());
                    if (refName != null) {
                        String modelName = toModelName(refName);
                        oneOf.setName(modelName);
                        oneOf.setBaseName(refName);
                    }
                } else {
                    // In-placed type (primitive), because there is no mapping or ref for it.
                    // use camelized `title` if present, otherwise use `type`
                    String oneOfName = Optional.ofNullable(schema.getTitle()).orElseGet(schema::getType);
                    oneOf.setName(toModelName(oneOfName));
                }
            }

            mdl.getComposedSchemas().setOneOf(newOneOfs);
        }

        return mdl;
    }

    @Override
    public ModelsMap postProcessModels(ModelsMap objs) {
        for (ModelMap model : objs.getModels()) {
            CodegenModel cm = model.getModel();

            // Remove the discriminator field from the model, serde will take care of this
            if (cm.discriminator != null) {
                String reserved_var_name = cm.discriminator.getPropertyBaseName();

                for (CodegenProperty cp : cm.vars) {
                    if (cp.baseName.equals(reserved_var_name)) {
                        cm.vars.remove(cp);
                        break;
                    }
                }

                // Check if there are duplicate mappings in the discriminator
                // ie.
                // ```
                // mappings:
                //   student: '#/components/schemas/Person'
                //   teacher: '#/components/schemas/Person'
                //   car: '#/components/schemas/Vehicle'
                // ```
                //
                // Should be mapped to an enum with `PersonStudent`, `PersonTeacher`, `Vehicle` to 2 `Person` enum variants. (a compiler error)
                if (cm.discriminator.getMapping() != null) {
                    if (hasDuplicateValues(cm.discriminator.getMapping())) {
                        var inverted = invertMap(cm.discriminator.getMapping());
                        for (var s : inverted.entrySet()) {
                            if (s.getValue().size() > 1) {
                                LOGGER.debug("Found duplicated enum model (" + s.getKey() + ") in model " + cm.name + ". Adding suffix to model names.");
                                for (var m : cm.discriminator.getMappedModels()) {
                                    if (s.getValue().contains(m.getMappingName())) {
                                        m.setModelName(m.getModelName() + StringUtils.camelize(m.getMappingName()));
                                    }
                                }
                            }
                        }
                    }
                }
            }

            // Flag structs with byteArrays in them so that we can annotate them with the serde_as macro
            for (CodegenProperty cp : cm.vars) {
                if (cp.isByteArray) {
                    cm.vendorExtensions.put("x-rust-has-byte-array", true);
                    break;
                }
            }
        }
        // process enum in models
        return postProcessModelsEnum(objs);
    }

    @Override
    public void processOpts() {
        super.processOpts();

        if (additionalProperties.containsKey(CodegenConstants.WITH_AWSV4_SIGNATURE_COMMENT)) {
            withAWSV4Signature = Boolean.parseBoolean(additionalProperties.get(CodegenConstants.WITH_AWSV4_SIGNATURE_COMMENT).toString());
        }

        if (additionalProperties.containsKey(CodegenConstants.ENUM_NAME_SUFFIX)) {
            enumSuffix = additionalProperties.get(CodegenConstants.ENUM_NAME_SUFFIX).toString();
        }

        if (additionalProperties.containsKey(CodegenConstants.PACKAGE_NAME)) {
            setPackageName((String) additionalProperties.get(CodegenConstants.PACKAGE_NAME));
        } else {
            setPackageName("openapi");
        }

        // If no version is provided in additional properties, version from API specification is used.
        // If none of them is provided then fallback to default version
        if (additionalProperties.containsKey(CodegenConstants.PACKAGE_VERSION)) {
            setPackageVersion((String) additionalProperties.get(CodegenConstants.PACKAGE_VERSION));
        } else if (openAPI != null && openAPI.getInfo() != null && openAPI.getInfo().getVersion() != null) {
            setPackageVersion(openAPI.getInfo().getVersion());
        }

        if (additionalProperties.containsKey(CodegenConstants.USE_SINGLE_REQUEST_PARAMETER)) {
            this.setUseSingleRequestParameter(convertPropertyToBoolean(CodegenConstants.USE_SINGLE_REQUEST_PARAMETER));
        }
        writePropertyBack(CodegenConstants.USE_SINGLE_REQUEST_PARAMETER, getUseSingleRequestParameter());

        if (additionalProperties.containsKey(SUPPORT_ASYNC)) {
            this.setSupportAsync(convertPropertyToBoolean(SUPPORT_ASYNC));
        }
        writePropertyBack(SUPPORT_ASYNC, getSupportAsync());

        if (additionalProperties.containsKey(SUPPORT_MIDDLEWARE)) {
            this.setSupportMiddleware(convertPropertyToBoolean(SUPPORT_MIDDLEWARE));
        }
        writePropertyBack(SUPPORT_MIDDLEWARE, getSupportMiddleware());

        if (additionalProperties.containsKey(SUPPORT_TOKEN_SOURCE)) {
            this.setSupportTokenSource(convertPropertyToBoolean(SUPPORT_TOKEN_SOURCE));
        }
        writePropertyBack(SUPPORT_TOKEN_SOURCE, getSupportTokenSource());

        if (additionalProperties.containsKey(SUPPORT_MULTIPLE_RESPONSES)) {
            this.setSupportMultipleReturns(convertPropertyToBoolean(SUPPORT_MULTIPLE_RESPONSES));
        }
        writePropertyBack(SUPPORT_MULTIPLE_RESPONSES, getSupportMultipleReturns());

        if (additionalProperties.containsKey(PREFER_UNSIGNED_INT)) {
            this.setPreferUnsignedInt(convertPropertyToBoolean(PREFER_UNSIGNED_INT));
        }
        writePropertyBack(PREFER_UNSIGNED_INT, getPreferUnsignedInt());

        if (additionalProperties.containsKey(BEST_FIT_INT)) {
            this.setBestFitInt(convertPropertyToBoolean(BEST_FIT_INT));
        }
        writePropertyBack(BEST_FIT_INT, getBestFitInt());

        if (additionalProperties.containsKey(AVOID_BOXED_MODELS)) {
            this.setAvoidBoxedModels(convertPropertyToBoolean(AVOID_BOXED_MODELS));
        }
        writePropertyBack(AVOID_BOXED_MODELS, getAvoidBoxedModels());

        additionalProperties.put(CodegenConstants.PACKAGE_NAME, packageName);
        additionalProperties.put(CodegenConstants.PACKAGE_VERSION, packageVersion);
        additionalProperties.put(EXTERN_CRATE_NAME, getExternCrateName());

        additionalProperties.put("apiDocPath", apiDocPath);
        additionalProperties.put("modelDocPath", modelDocPath);

        if (HYPER_LIBRARY.equals(getLibrary())) {
            additionalProperties.put(HYPER_LIBRARY, "true");
        } else if (HYPER0X_LIBRARY.equals(getLibrary())) {
            additionalProperties.put(HYPER_LIBRARY, "true");
            additionalProperties.put(HYPER0X_LIBRARY, "true");
        } else if (REQWEST_LIBRARY.equals(getLibrary())) {
            additionalProperties.put(REQWEST_LIBRARY, "true");
        } else if (REQWEST_TRAIT_LIBRARY.equals(getLibrary())) {
            additionalProperties.put(REQWEST_TRAIT_LIBRARY_ATTR, "true");
        } else {
            LOGGER.error("Unknown library option (-l/--library): {}", getLibrary());
        }

        apiTemplateFiles.put(getLibrary() + "/api.mustache", ".rs");

        modelPackage = packageName;
        apiPackage = packageName;

        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("git_push.sh.mustache", "", "git_push.sh"));
        supportingFiles.add(new SupportingFile("gitignore.mustache", "", ".gitignore"));
        supportingFiles.add(new SupportingFile(".travis.yml", "", ".travis.yml"));
        supportingFiles.add(new SupportingFile("model_mod.mustache", modelFolder, "mod.rs"));
        supportingFiles.add(new SupportingFile("lib.mustache", "src", "lib.rs"));
        supportingFiles.add(new SupportingFile("Cargo.mustache", "", "Cargo.toml"));

        supportingFiles.add(new SupportingFile(getLibrary() + "/api_mod.mustache", apiFolder, "mod.rs"));
        supportingFiles.add(new SupportingFile(getLibrary() + "/configuration.mustache", apiFolder, "configuration.rs"));
        if (HYPER_LIBRARY.equals(getLibrary())) {
            supportingFiles.add(new SupportingFile("request.rs", apiFolder, "request.rs"));
            supportingFiles.add(new SupportingFile(getLibrary() + "/client.mustache", apiFolder, "client.rs"));
        }

        // add lambda for sanitize version (e.g. v1.2.3-beta => 1.2.3-beta)
        additionalProperties.put("lambdaVersion", new Mustache.Lambda() {
            @Override
            public void execute(Template.Fragment fragment, Writer writer) throws IOException {
                String content = fragment.execute();
                // remove v or V
                content = content.trim().replace("v", "");
                content = content.replace("V", "");

                String[] contents = content.split("[.]");
                if (contents.length == 1) {
                    // convert 5 to 5.0.0 for example
                    content += ".0.0";
                } else if (contents.length == 2) {
                    // convert 5.2 to 5.2.0 for example
                    content += ".0";
                }

                writer.write(content);
            }
        });

    }

    private String getExternCrateName() {
        // The external name used when importing a crate has all '-' replaced with '_'.
        return packageName.replace('-', '_');
    }

    private boolean getSupportAsync() {
        return supportAsync;
    }

    private boolean getSupportMiddleware() {
        return supportMiddleware;
    }

    private boolean getSupportTokenSource() {
        return supportTokenSource;
    }

    public boolean getSupportMultipleReturns() {
        return supportMultipleResponses;
    }

    public void setSupportMultipleReturns(boolean supportMultipleResponses) {
        this.supportMultipleResponses = supportMultipleResponses;
    }

    public boolean getPreferUnsignedInt() {
        return preferUnsignedInt;
    }

    public boolean getBestFitInt() {
        return bestFitInt;
    }

    private boolean getUseSingleRequestParameter() {
        return useSingleRequestParameter;
    }

    public boolean getAvoidBoxedModels() {
        return avoidBoxedModels;
    }

    @Override
    public String apiFileFolder() {
        return (outputFolder + File.separator + apiFolder).replace("/", File.separator);
    }

    @Override
    public String modelFileFolder() {
        return (outputFolder + File.separator + modelFolder).replace("/", File.separator);
    }

    @Override
    public String modelFilename(String templateName, String modelName) {
        String suffix = modelTemplateFiles().get(templateName);
        return modelFileFolder() + File.separator + toModelFilename(modelName) + suffix;
    }

    @Override
    public String apiDocFileFolder() {
        return (outputFolder + "/" + apiDocPath).replace('/', File.separatorChar);
    }

    @Override
    public String modelDocFileFolder() {
        return (outputFolder + "/" + modelDocPath).replace('/', File.separatorChar);
    }

    @Override
    public String getTypeDeclaration(Schema p) {
        // use unaliased schema for client-side
        Schema unaliasSchema = unaliasSchema(p);
        return super.getTypeDeclaration(unaliasSchema);
    }

    @Override
    public String getSchemaType(Schema p) {
        String schemaType = super.getSchemaType(p);
        String type = typeMapping.getOrDefault(schemaType, schemaType);

        // Implement integer type fitting (when property is enabled)
        if (Objects.equals(p.getType(), "integer")) {
            boolean bestFit = convertPropertyToBoolean(BEST_FIT_INT);
            boolean preferUnsigned = convertPropertyToBoolean(PREFER_UNSIGNED_INT);

            BigInteger minimum = Optional.ofNullable(p.getMinimum()).map(BigDecimal::toBigInteger).orElse(null);
            boolean exclusiveMinimum = Optional.ofNullable(p.getExclusiveMinimum()).orElse(false);

            boolean unsigned = preferUnsigned && canFitIntoUnsigned(minimum, exclusiveMinimum);

            if (Strings.isNullOrEmpty(p.getFormat())) {
                if (bestFit) {
                    return bestFittingIntegerType(
                            minimum,
                            exclusiveMinimum,
                            Optional.ofNullable(p.getMaximum()).map(BigDecimal::toBigInteger).orElse(null),
                            Optional.ofNullable(p.getExclusiveMaximum()).orElse(false),
                            preferUnsigned);
                } else {
                    return unsigned ? "u32" : "i32";
                }
            } else {
                switch (p.getFormat()) {
                    case "int32":
                        return unsigned ? "u32" : "i32";
                    case "int64":
                        return unsigned ? "u64" : "i64";
                }
            }
        }

        return type;
    }

    @Override
    public void postProcessModelProperty(CodegenModel model, CodegenProperty property) {
        super.postProcessModelProperty(model, property);

        // If a property is both nullable and non-required then we represent this using a double Option
        // which requires the `serde_with` extension crate for deserialization.
        // See: https://docs.rs/serde_with/latest/serde_with/rust/double_option/index.html
        if (property.isNullable && !property.required) {
            additionalProperties.put("serdeWith", true);
        }

        // Add a field for checking if a field is with optional or required in templates.
        // This is useful in Mustache templates as it's not possible to do OR logic between variables.
        property.vendorExtensions.put("isMandatory", !property.isNullable && property.required);

        // If a property is a base64-encoded byte array, use `serde_with` for deserialization.
        if (property.isByteArray) {
            additionalProperties.put("serdeWith", true);
            // If a byte array is both nullable and not required we need to include our own
            // custom double option as serde_as does not work with serde_with's double_option.
            if (property.isNullable && !property.required) {
                additionalProperties.put("serdeAsDoubleOption", true);
            }
        }
    }

    @Override
    public void postProcessParameter(CodegenParameter parameter) {
        super.postProcessParameter(parameter);
        // in order to avoid name conflicts, we map parameters inside the functions
        String inFunctionIdentifier = "";
        if (this.useSingleRequestParameter) {
            inFunctionIdentifier = "params." + parameter.paramName;
        } else {
            if (parameter.paramName.startsWith("r#")) {
                inFunctionIdentifier = "p_" + parameter.paramName.substring(2);
            } else {
                inFunctionIdentifier = "p_" + parameter.paramName;
            }
        }
        if (!parameter.vendorExtensions.containsKey(this.VENDOR_EXTENSION_PARAM_IDENTIFIER)) { // allow to overwrite this value
            parameter.vendorExtensions.put(this.VENDOR_EXTENSION_PARAM_IDENTIFIER, inFunctionIdentifier);
        }
    }

    @Override
    public OperationsMap postProcessOperationsWithModels(OperationsMap objs, List<ModelMap> allModels) {
        OperationMap objectMap = objs.getOperations();
        List<CodegenOperation> operations = objectMap.getOperation();
        for (CodegenOperation operation : operations) {
            if (operation.pathParams != null && operation.pathParams.size() > 0) {

                // For types with `isAnyType` we assume it's a `serde_json::Value` type.
                // However for path, query, and headers it's unlikely to be JSON so we default to `String`.
                // Note that we keep the default `serde_json::Value` for body parameters.
                for (var param : operation.allParams) {
                    if (param.isAnyType && (param.isPathParam || param.isQueryParam || param.isHeaderParam)) {
                        param.dataType = "String";
                        param.isPrimitiveType = true;
                        param.isString = true;
                    }
                }

                for (var pathParam : operation.pathParams) {
                    if (!pathParam.baseName.contains("-")) {
                        continue;
                    }

                    var newName = pathParam.baseName.replace("-", "_");
                    LOGGER.info(pathParam.baseName + " cannot be used as a path param. Renamed to " + newName);

                    operation.path = operation.path.replace("{" + pathParam.baseName + "}", "{" + newName + "}");
                    pathParam.baseName = newName;
                }
            }

            for (var param : operation.allParams) {
                if (!hasUUIDs && param.isUuid) {
                    hasUUIDs = true;
                    break;
                }
            }

            // http method verb conversion, depending on client library (e.g. Hyper: PUT => Put, Reqwest: PUT => put)
            if (HYPER_LIBRARY.equals(getLibrary())) {
                operation.httpMethod = StringUtils.camelize(operation.httpMethod.toLowerCase(Locale.ROOT));
            } else if (REQWEST_LIBRARY.equals(getLibrary()) || REQWEST_TRAIT_LIBRARY.equals(getLibrary())) {
                operation.httpMethod = operation.httpMethod.toUpperCase(Locale.ROOT);
            }

            // add support for single request parameter using x-group-parameters
            if (!operation.vendorExtensions.containsKey("x-group-parameters") && useSingleRequestParameter) {
                operation.vendorExtensions.put("x-group-parameters", Boolean.TRUE);
            }

            if (operation.producesTextPlain() && "String".equals(operation.returnType)) {
                operation.vendorExtensions.put("x-supports-plain-text", Boolean.TRUE);
            }

            // update return type to conform to rust standard
            /*
            if (operation.returnType != null) {
                if ( operation.returnType.startsWith("Vec") && !languageSpecificPrimitives.contains(operation.returnBaseType)) {
                    // array of model
                    String rt = operation.returnType;
                    int end = rt.lastIndexOf(">");
                    if ( end > 0 ) {
                        operation.vendorExtensions.put("x-returnTypeInMethod", "Vec<super::" + rt.substring("Vec<".length(), end).trim() + ">");
                        operation.returnContainer = "List";
                    }
                } else if (operation.returnType.startsWith("::std::collections::HashMap<String, ") && !languageSpecificPrimitives.contains(operation.returnBaseType)) {
                    LOGGER.info("return base type:" + operation.returnBaseType);
                    // map of model
                    String rt = operation.returnType;
                    int end = rt.lastIndexOf(">");
                    if ( end > 0 ) {
                        operation.vendorExtensions.put("x-returnTypeInMethod", "::std::collections::HashMap<String, super::" + rt.substring("::std::collections::HashMap<String, ".length(), end).trim() + ">");
                        operation.returnContainer = "Map";
                    }
                } else if (!languageSpecificPrimitives.contains(operation.returnType)) {
                    // add super:: to model, e.g. super::pet
                    operation.vendorExtensions.put("x-returnTypeInMethod", "super::" + operation.returnType);
                } else {
                    // primitive type or array/map of primitive type
                    operation.vendorExtensions.put("x-returnTypeInMethod", operation.returnType);
                }
            }

            for (CodegenParameter p : operation.allParams) {
                if (p.isArray && !languageSpecificPrimitives.contains(p.dataType)) {
                    // array of model
                    String rt = p.dataType;
                    int end = rt.lastIndexOf(">");
                    if ( end > 0 ) {
                        p.dataType = "Vec<" + rt.substring("Vec<".length(), end).trim() + ">";
                    }
                } else if (p.isMap && !languageSpecificPrimitives.contains(p.dataType)) {
                    // map of model
                    String rt = p.dataType;
                    int end = rt.lastIndexOf(">");
                    if ( end > 0 ) {
                        p.dataType = "::std::collections::HashMap<String, super::" + rt.substring("::std::collections::HashMap<String, ".length(), end).trim() + ">";
                    }
                } else if (!languageSpecificPrimitives.contains(p.dataType)) {
                    // add super:: to model, e.g. super::pet
                    p.dataType = "super::" + p.dataType;
                }
            }*/
        }

        if (!hasUUIDs) {
            for (var map : allModels) {
                CodegenModel m = map.getModel();
                if (m.getIsUuid() || hasUuidInProperties(m.vars)) {
                    hasUUIDs = true;
                    LOGGER.debug("found UUID in model: " + m.name);
                    break;
                }
            }
        }

        this.additionalProperties.put("hasUUIDs", hasUUIDs);
        return objs;
    }

    /**
     * Recursively searches for a model's properties for a UUID type field.
     */
    private boolean hasUuidInProperties(List<CodegenProperty> properties) {
        for (CodegenProperty property : properties) {
            if (property.isUuid) {
                return true;
            }
            // Check nested properties
            if (property.items != null && hasUuidInProperties(Collections.singletonList(property.items))) {
                return true;
            }
            if (property.additionalProperties != null && hasUuidInProperties(Collections.singletonList(property.additionalProperties))) {
                return true;
            }
            if (property.vars != null && hasUuidInProperties(property.vars)) {
                return true;
            }
        }
        return false;
    }

    @Override
    public String toDefaultValue(Schema p) {
        if (p.getDefault() != null) {
            return p.getDefault().toString();
        } else {
            return null;
        }
    }

    @Override
    protected ImmutableMap.Builder<String, Lambda> addMustacheLambdas() {
        return super.addMustacheLambdas()
                // Convert variable names to lifetime names.
                // Generally they are the same, but `#` is not valid in lifetime names.
                // Rust uses `r#` prefix for variables that are also keywords.
                .put("lifetimeName", new ReplaceAllLambda("^r#", "r_"));
    }

    public static <K, V> Map<V, List<K>> invertMap(Map<K, V> map) {
        Map<V, List<K>> invertedMap = new HashMap<>();

        for (Map.Entry<K, V> entry : map.entrySet()) {
            invertedMap.computeIfAbsent(entry.getValue(), k -> new ArrayList<>()).add(entry.getKey());
        }

        return invertedMap;
    }

    public static <K, V> boolean hasDuplicateValues(Map<K, V> map) {
        Set<V> uniqueValues = new HashSet<>(map.values());
        return uniqueValues.size() < map.size();
    }
}
