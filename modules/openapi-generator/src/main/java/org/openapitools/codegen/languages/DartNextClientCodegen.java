package org.openapitools.codegen.languages;

import static org.openapitools.codegen.CodegenConstants.SKIP_FORM_MODEL;
import static org.openapitools.codegen.utils.CamelizeOption.LOWERCASE_FIRST_CHAR;
import static org.openapitools.codegen.utils.CamelizeOption.LOWERCASE_FIRST_LETTER;
import static org.openapitools.codegen.utils.StringUtils.camelize;
import static org.openapitools.codegen.utils.StringUtils.escape;
import static org.openapitools.codegen.utils.StringUtils.underscore;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.CodegenModel;
import org.openapitools.codegen.CodegenParameter;
import org.openapitools.codegen.CodegenProperty;
import org.openapitools.codegen.CodegenType;
import org.openapitools.codegen.DefaultCodegen;
import org.openapitools.codegen.SupportingFile;
import org.openapitools.codegen.config.GlobalSettings;
import org.openapitools.codegen.meta.GeneratorMetadata;
import org.openapitools.codegen.meta.Stability;
import org.openapitools.codegen.meta.features.ClientModificationFeature;
import org.openapitools.codegen.meta.features.DataTypeFeature;
import org.openapitools.codegen.meta.features.DocumentationFeature;
import org.openapitools.codegen.meta.features.GlobalFeature;
import org.openapitools.codegen.meta.features.ParameterFeature;
import org.openapitools.codegen.meta.features.SchemaSupportFeature;
import org.openapitools.codegen.meta.features.SecurityFeature;
import org.openapitools.codegen.meta.features.WireFormatFeature;
import org.openapitools.codegen.model.ModelMap;
import org.openapitools.codegen.model.ModelsMap;
import org.openapitools.codegen.model.OperationsMap;
import org.openapitools.codegen.utils.ModelUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.collect.Sets;
import com.samskivert.mustache.DefaultCollector;
import com.samskivert.mustache.Mustache;
import com.samskivert.mustache.Mustache.Compiler;

import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.parameters.Parameter;
import lombok.Getter;
import lombok.Setter;

public class DartNextClientCodegen extends DefaultCodegen {
    public static final String PROJECT_NAME = "projectName";

    private final Logger LOGGER = LoggerFactory.getLogger(DartNextClientCodegen.class);

    public static final String PUB_LIBRARY = "pubLibrary";
    public static final String PUB_NAME = "pubName";
    public static final String PUB_VERSION = "pubVersion";
    public static final String PUB_DESCRIPTION = "pubDescription";
    public static final String PUB_AUTHOR = "pubAuthor";
    public static final String PUB_AUTHOR_EMAIL = "pubAuthorEmail";
    public static final String PUB_HOMEPAGE = "pubHomepage";
    public static final String PUB_REPOSITORY = "pubRepository";
    public static final String PUB_PUBLISH_TO = "pubPublishTo";
    public static final String USE_ENUM_EXTENSION = "useEnumExtension";
    private static final String CLIENT_NAME = "clientName";

    @Getter
    @Setter
    private String clientName;
    @Setter
    protected String pubLibrary = "openapi.api";
    @Setter
    protected String pubName = "openapi";
    @Setter
    protected String pubVersion = "1.0.0";
    @Setter
    protected String pubDescription = "OpenAPI API client";
    @Setter
    protected String pubAuthor = "Author";
    @Setter
    protected String pubAuthorEmail = "author@homepage";
    @Setter
    protected String pubHomepage = "homepage";
    @Setter
    protected String pubRepository = null;
    @Setter
    protected String pubPublishTo = null;
    @Setter
    protected boolean useEnumExtension = false;
    @Setter
    protected String sourceFolder = "src";
    @Setter
    protected boolean camelCaseDollarSign = false;

    protected String libPath = "lib" + File.separator;

    protected String srcPath() {
        return libPath + sourceFolder + File.separator;
    }

    protected String testPath() {
        return "test" + File.separator;
    }

    protected String modelsPath() {
        return srcPath() + modelPackage();
    }

    protected String apisPath() {
        return srcPath() + apiPackage();
    }

    protected String networkingPath() {
        return srcPath() + "networking" + File.separator;
    }

    protected String serializationPath() {
        return srcPath() + "serialization" + File.separator;
    }

    protected String apiDocFolder = "doc" + File.separator;
    protected String modelDocFolder = "doc" + File.separator;
    protected String apiTestFolder = "test" + File.separator + "apis" + File.separator;
    protected String modelTestFolder = "test" + File.separator + "models" + File.separator;

    protected Map<String, String> imports = new HashMap<>();

    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    public String getName() {
        return "dart-next";
    }

    public String getHelp() {
        return "Generates a dart-next client.";
    }

    public DartNextClientCodegen() {
        super();

        outputFolder = "generated-code" + File.separator + "dart-next";

        embeddedTemplateDir = templateDir = "dart-next";
        apiPackage = "apis";
        modelPackage = "models";

        generatorMetadata = GeneratorMetadata.newBuilder(generatorMetadata)
                .stability(Stability.EXPERIMENTAL)
                .build();

        supportsMixins = true;
        supportsInheritance = true;
        supportsMultipleInheritance = true;
        legacyDiscriminatorBehavior = false;
        supportsAdditionalPropertiesWithComposedSchema = true;
        disallowAdditionalPropertiesIfNotPresent = false;
        modifyFeatureSet(features -> features
                .includeDataTypeFeatures(DataTypeFeature.AnyType, DataTypeFeature.Custom)
                .includeDocumentationFeatures(DocumentationFeature.Readme)
                .includeSchemaSupportFeatures(
                        SchemaSupportFeature.Polymorphism,
                        SchemaSupportFeature.Union,
                        SchemaSupportFeature.Composite,
                        SchemaSupportFeature.allOf,
                        SchemaSupportFeature.oneOf,
                        SchemaSupportFeature.anyOf)

                .securityFeatures(EnumSet.of(
                        SecurityFeature.OAuth2_Implicit,
                        SecurityFeature.BasicAuth,
                        SecurityFeature.BearerToken,
                        SecurityFeature.ApiKey))
                .excludeGlobalFeatures(
                        GlobalFeature.XMLStructureDefinitions,
                        GlobalFeature.Callbacks,
                        GlobalFeature.LinkObjects,
                        GlobalFeature.ParameterStyling)
                .includeParameterFeatures(
                        ParameterFeature.Cookie)
                .includeClientModificationFeatures(
                        ClientModificationFeature.BasePath)
                .includeWireFormatFeatures(WireFormatFeature.Custom));

        // modelDocTemplateFiles.put("object_doc.mustache", ".md");
        // apiDocTemplateFiles.put("api_doc.mustache", ".md");

        // modelTestTemplateFiles.put("model_test.mustache", ".dart");
        // apiTestTemplateFiles.put("api_test.mustache", ".dart");

        final List<String> reservedWordsList = new ArrayList<>();
        try (BufferedReader reader = new BufferedReader(
                new InputStreamReader(DartClientCodegen.class.getResourceAsStream("/dart/dart-keywords.txt"),
                        StandardCharsets.UTF_8))) {
            while (reader.ready()) {
                reservedWordsList.add(reader.readLine());
            }
        } catch (Exception e) {
            LOGGER.error("Error reading dart keywords. Exception: {}", e.getMessage());
        }
        setReservedWordsLowerCase(reservedWordsList);

        // These types return isPrimitive=true in templates
        languageSpecificPrimitives = Sets.newHashSet(
                "String",
                "bool",
                "int",
                "num",
                "double");

        typeMapping = new HashMap<>();
        typeMapping.put("Array", "List");
        typeMapping.put("array", "List");
        typeMapping.put("map", "Map");
        typeMapping.put("List", "List");
        typeMapping.put("set", "Set");
        typeMapping.put("boolean", "bool");
        typeMapping.put("string", "String");
        typeMapping.put("char", "String");
        typeMapping.put("int", "int");
        typeMapping.put("long", "int");
        typeMapping.put("short", "int");
        typeMapping.put("number", "num");
        typeMapping.put("float", "double");
        typeMapping.put("double", "double");
        typeMapping.put("decimal", "double");
        typeMapping.put("integer", "int");
        typeMapping.put("Date", "DateTime");
        typeMapping.put("date", "DateTime");
        typeMapping.put("DateTime", "DateTime");
        typeMapping.put("file", "XFile");
        typeMapping.put("binary", "Uint8List");
        typeMapping.put("UUID", "String");
        typeMapping.put("URI", "Uri");
        typeMapping.put("ByteArray", "Uint8List");
        typeMapping.put("object", "$FreeFormObject");
        typeMapping.put("AnyType", "Object");

        // Data types of the above values which are automatically imported
        defaultIncludes = Sets.newHashSet(
                "String",
                "bool",
                "int",
                "num",
                "double",
                "List",
                "Set",
                "Map",
                "DateTime",
                "Object");

        imports.put("String", "dart:core");
        imports.put("bool", "dart:core");
        imports.put("int", "dart:core");
        imports.put("num", "dart:core");
        imports.put("double", "dart:core");
        imports.put("List", "dart:core");
        imports.put("Set", "dart:core");
        imports.put("Map", "dart:core");
        imports.put("DateTime", "dart:core");
        imports.put("Object", "dart:core");
        // imports.put("MultipartFile", "package:http/http.dart");

        addOption(PUB_LIBRARY, "Library name in generated code", pubLibrary);
        addOption(PUB_NAME, "Name in generated pubspec", pubName);
        addOption(PUB_VERSION, "Version in generated pubspec", pubVersion);
        addOption(PUB_DESCRIPTION, "Description in generated pubspec", pubDescription);
        addOption(PUB_AUTHOR, "Author name in generated pubspec", pubAuthor);
        addOption(PUB_AUTHOR_EMAIL, "Email address of the author in generated pubspec", pubAuthorEmail);
        addOption(PUB_HOMEPAGE, "Homepage in generated pubspec", pubHomepage);
        addOption(PUB_REPOSITORY, "Repository in generated pubspec", pubRepository);
        addOption(PUB_PUBLISH_TO, "Publish_to in generated pubspec", pubPublishTo);
        addOption(USE_ENUM_EXTENSION, "Allow the 'x-enum-values' extension for enums",
                String.valueOf(useEnumExtension));
        addOption(CodegenConstants.SOURCE_FOLDER, CodegenConstants.SOURCE_FOLDER_DESC, sourceFolder);
    }

    @Override
    public Compiler processCompiler(Compiler compiler) {

        // see https://github.com/samskivert/jmustache/issues/82#issuecomment-226284364
        return super.processCompiler(compiler).withCollector(new DefaultCollector() {
            @Override
            public Mustache.VariableFetcher createFetcher(Object ctx, String name) {
                if (ctx instanceof Map<?, ?> && name.equals("entrySet")) {
                    return new Mustache.VariableFetcher() {
                        public Object get(Object ctx, String name) throws Exception {
                            return ((Map<?, ?>) ctx).entrySet();
                        }
                    };
                } else if (ctx instanceof Map.Entry<?, ?>) {
                    if (name.equals("key")) {
                        return new Mustache.VariableFetcher() {
                            public Object get(Object ctx, String name) throws Exception {
                                return ((Map.Entry<?, ?>) ctx).getKey();
                            }
                        };
                    } else if (name.equals("value")) {
                        return new Mustache.VariableFetcher() {
                            public Object get(Object ctx, String name) throws Exception {
                                return ((Map.Entry<?, ?>) ctx).getValue();
                            }
                        };
                    }
                }
                return super.createFetcher(ctx, name);
            }
        });
    }

    @Override
    protected String toMediaTypeSchemaName(String contentType, String mediaTypeSchemaSuffix) {
        contentType = contentType.replaceAll("\\*", "Any");
        return toModelName(contentType);
    }

    @Override
    public void processOpts() {
        super.processOpts();
        allowUnicodeIdentifiers = true;

        // Since we generate models for apis anyway, we need to always enable this.
        GlobalSettings.setProperty(SKIP_FORM_MODEL, "false");

        // Fix a couple Java notation properties
        modelPackage = modelPackage.replace('.', '/');
        apiPackage = apiPackage.replace('.', '/');
        // And overwrite them in the additional properties
        additionalProperties.put(CodegenConstants.MODEL_PACKAGE, modelPackage);
        additionalProperties.put(CodegenConstants.API_PACKAGE, apiPackage);

        if (StringUtils.isEmpty(System.getenv("DART_POST_PROCESS_FILE"))) {
            LOGGER.info(
                    "Environment variable DART_POST_PROCESS_FILE not defined so the Dart code may not be properly formatted. To define it, try `export DART_POST_PROCESS_FILE=\"dart format\"` (Linux/Mac)");
            LOGGER.info(
                    "NOTE: To enable file post-processing, 'enablePostProcessFile' must be set to `true` (--enable-post-process-file for CLI).");
        }

        if (additionalProperties.containsKey(PUB_NAME)) {
            this.setPubName((String) additionalProperties.get(PUB_NAME));
        } else {
            // not set, use to be passed to template
            additionalProperties.put(PUB_NAME, pubName);
        }

        if (additionalProperties.containsKey(PUB_LIBRARY)) {
            this.setPubLibrary((String) additionalProperties.get(PUB_LIBRARY));
        } else {
            // not set, use to be passed to template
            additionalProperties.put(PUB_LIBRARY, pubLibrary);
        }

        if (additionalProperties.containsKey(PUB_VERSION)) {
            this.setPubVersion((String) additionalProperties.get(PUB_VERSION));
        } else {
            // not set, use to be passed to template
            additionalProperties.put(PUB_VERSION, pubVersion);
        }

        if (additionalProperties.containsKey(PUB_DESCRIPTION)) {
            this.setPubDescription((String) additionalProperties.get(PUB_DESCRIPTION));
        } else {
            // not set, use to be passed to template
            additionalProperties.put(PUB_DESCRIPTION, pubDescription);
        }

        if (additionalProperties.containsKey(PUB_AUTHOR)) {
            this.setPubAuthor((String) additionalProperties.get(PUB_AUTHOR));
        } else {
            // not set, use to be passed to template
            additionalProperties.put(PUB_AUTHOR, pubAuthor);
        }

        if (additionalProperties.containsKey(PUB_AUTHOR_EMAIL)) {
            this.setPubAuthorEmail((String) additionalProperties.get(PUB_AUTHOR_EMAIL));
        } else {
            // not set, use to be passed to template
            additionalProperties.put(PUB_AUTHOR_EMAIL, pubAuthorEmail);
        }

        if (additionalProperties.containsKey(PUB_HOMEPAGE)) {
            this.setPubHomepage((String) additionalProperties.get(PUB_HOMEPAGE));
        } else {
            // not set, use to be passed to template
            additionalProperties.put(PUB_HOMEPAGE, pubHomepage);
        }

        if (additionalProperties.containsKey(PUB_REPOSITORY)) {
            this.setPubRepository((String) additionalProperties.get(PUB_REPOSITORY));
        } else {
            // not set, use to be passed to template
            additionalProperties.put(PUB_REPOSITORY, pubRepository);
        }

        if (additionalProperties.containsKey(PUB_PUBLISH_TO)) {
            this.setPubPublishTo((String) additionalProperties.get(PUB_PUBLISH_TO));
        } else {
            // not set, use to be passed to template
            additionalProperties.put(PUB_PUBLISH_TO, pubPublishTo);
        }

        if (additionalProperties.containsKey(USE_ENUM_EXTENSION)) {
            this.setUseEnumExtension(convertPropertyToBooleanAndWriteBack(USE_ENUM_EXTENSION));
        } else {
            // Not set, use to be passed to template.
            additionalProperties.put(USE_ENUM_EXTENSION, useEnumExtension);
        }

        if (additionalProperties.containsKey(CodegenConstants.SOURCE_FOLDER)) {
            String srcFolder = (String) additionalProperties.get(CodegenConstants.SOURCE_FOLDER);
            this.setSourceFolder(srcFolder.replace('/', File.separatorChar));
        }
        additionalProperties.put(CodegenConstants.SOURCE_FOLDER, sourceFolder);

        if (!additionalProperties.containsKey(CLIENT_NAME)) {
            final String name = org.openapitools.codegen.utils.StringUtils.camelize(pubName);
            additionalProperties.put(CLIENT_NAME, name);
            LOGGER.debug("Client name not set, using default {}", name);
        }
        setClientName(additionalProperties.get(CLIENT_NAME).toString());

        // make api and model doc path available in mustache template
        additionalProperties.put("apiDocPath", apiDocFolder);
        additionalProperties.put("modelDocPath", modelDocFolder);

        // check to not overwrite a custom templateDir
        if (templateDir == null) {
            embeddedTemplateDir = templateDir = "dart-next";
        }

        String apisMustache = "lib/src/apis/";
        String modelsMustache = "lib/src/models/";
        String testsMustache = "test/";
        String modelsTestsMustache = testsMustache + "models/";
        String apisTestsMustache = testsMustache + "apis/";

        modelTemplateFiles.put(modelsMustache + "model.mustache", ".dart");
        modelTemplateFiles.put(modelsMustache + "model.reflection.mustache", ".reflection.dart");
        // modelTemplateFiles.put(modelsMustache + "model.serialization.mustache", ".serialization.dart");

        modelTestTemplateFiles.put(modelsTestsMustache + "model_tests.mustache", ".dart");
        apiTestTemplateFiles.put(apisTestsMustache + "api_tests.mustache", ".dart");

        apiTemplateFiles.put(apisMustache + "api.mustache", ".dart");
        apiTemplateFiles.put(apisMustache + "api_requests.mustache", ".requests.dart");
        apiTemplateFiles.put(apisMustache + "api_responses.mustache", ".responses.dart");

        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md").doNotOverwrite());
        supportingFiles.add(new SupportingFile("pubspec.mustache", "", "pubspec.yaml").doNotOverwrite());
        supportingFiles.add(new SupportingFile("CHANGELOG.md", "", "CHANGELOG.md").doNotOverwrite());
        supportingFiles.add(new SupportingFile("lib/internal_exports.mustache", "lib", "_internal.dart"));
        supportingFiles.add(new SupportingFile("lib/public_exports.mustache", "lib", pubName + ".dart"));
        supportingFiles.add(new SupportingFile("lib/src/facade.mustache", srcPath(), "api_facade.dart"));
        supportingFiles.add(new SupportingFile(modelsMustache + "_exports.mustache", modelsPath(), "_exports.dart"));
        supportingFiles.add(new SupportingFile(apisMustache + "_exports.mustache", apisPath(), "_exports.dart"));

        supportingFiles.add(new SupportingFile(testsMustache + "utils.mustache", testPath(), "utils.dart"));

        String networkingMustache = "lib/src/networking/";
        supportingFiles
                .add(new SupportingFile(networkingMustache + "_exports.mustache", networkingPath(), "_exports.dart"));
        supportingFiles
                .add(new SupportingFile(networkingMustache + "_internal.mustache", networkingPath(), "_internal.dart"));
        supportingFiles
                .add(new SupportingFile(networkingMustache + "helpers.mustache", networkingPath(), "helpers.dart"));
        supportingFiles
                .add(new SupportingFile(networkingMustache + "property_encoding_rule.mustache", networkingPath(),
                        "property_encoding_rule.dart"));
        supportingFiles
                .add(new SupportingFile(networkingMustache + "wire_serialization_options.mustache", networkingPath(),
                        "wire_serialization_options.dart"));
        supportingFiles
                .add(new SupportingFile(networkingMustache + "multipart.mustache", networkingPath(), "multipart.dart"));
        supportingFiles
                .add(new SupportingFile(networkingMustache + "package_http_client.mustache", networkingPath(),
                        "package_http_client.dart"));
        supportingFiles
                .add(new SupportingFile(testsMustache + "networking/helpers_test.mustache",
                        testPath() + File.separator + "networking",
                        "helpers_test.dart"));

        String serializationMustache = "lib/src/serialization/";
        supportingFiles
                .add(new SupportingFile(serializationMustache + "_exports.mustache", serializationPath(),
                        "_exports.dart"));
        supportingFiles
                .add(new SupportingFile(serializationMustache + "_internal.mustache", serializationPath(),
                        "_internal.dart"));

        supportingFiles
                .add(new SupportingFile(serializationMustache + "additional_properties.mustache", serializationPath(),
                        "additional_properties.dart"));
        supportingFiles
                .add(new SupportingFile(serializationMustache + "examples.mustache", serializationPath(),
                        "examples.dart"));
        supportingFiles
                .add(new SupportingFile(serializationMustache + "open_api_object.mustache", serializationPath(),
                        "open_api_object.dart"));
        supportingFiles
                .add(new SupportingFile(serializationMustache + "parameter_serialization.mustache", serializationPath(),
                        "parameter_serialization.dart"));
        supportingFiles
                .add(new SupportingFile(serializationMustache + "json_extensions.mustache", serializationPath(),
                        "json_extensions.dart"));
        supportingFiles
                .add(new SupportingFile(serializationMustache + "reflection.mustache", serializationPath(),
                        "reflection.dart"));
        supportingFiles
                .add(new SupportingFile(serializationMustache + "model_reflection.mustache", serializationPath(),
                        "model_reflection.dart"));
        supportingFiles
                .add(new SupportingFile(serializationMustache + "container_reflection.mustache", serializationPath(),
                        "container_reflection.dart"));
        supportingFiles
                .add(new SupportingFile(serializationMustache + "generated_reflections.mustache", serializationPath(),
                        "generated_reflections.dart"));
        supportingFiles
                .add(new SupportingFile(serializationMustache + "primitive_reflection.mustache", serializationPath(),
                        "primitive_reflection.dart"));
        supportingFiles
                .add(new SupportingFile(serializationMustache + "context.mustache", serializationPath(),
                        "context.dart"));
        supportingFiles
                .add(new SupportingFile(serializationMustache + "xml_extensions.mustache", serializationPath(),
                        "xml_extensions.dart"));
        supportingFiles
                .add(new SupportingFile(serializationMustache + "xml_reflection.mustache", serializationPath(),
                        "xml_reflection.dart"));
        supportingFiles
                .add(new SupportingFile(testsMustache + "serialization/helpers_test.mustache",
                        testPath() + File.separator + "serialization",
                        "helpers_test.dart"));
        // TODO: add all files inside shared_infrastructure to root folder of generated
        // addSharedInfrastructureFiles();
    }

    private final String kIsChild = "x-is-child";
    private final String kIsParent = "x-is-parent";
    private final String kIsPure = "x-is-pure";
    private final String kSelfOnlyProps = "x-self-only-props";
    private final String kHasSelfOnlyProps = "x-has-self-only-props";
    private final String kAncestorOnlyProps = "x-ancestor-only-props";
    private final String kHasAncestorOnlyProps = "x-has-ancestor-only-props";
    private final String kSelfAndAncestorOnlyProps = "x-self-and-ancestor-only-props";
    private final String kHasSelfAndAncestorOnlyProps = "x-has-self-and-ancestor-only-props";
    /// Whether the model has any variable at all.
    /// If this is false, the modal MUST be serialized to a scalar value via oneOf or anyOf.
    private final String kHasAnyVars = "x-has-any-vars";
    private final String kParentDiscriminator = "x-parent-discriminator";

    Map<String, Object> getInheritanceVariablesLocation(CodegenModel mo) {
        return mo.vendorExtensions;
    }


    // Workaround weird case in DefaultCodegen where parameter can have isInteger but isNumeric == false.
    @Override
    public CodegenParameter fromParameter(Parameter parameter, Set<String> imports) {
        var result = super.fromParameter(parameter, imports);
        if (result!=null) {
            if (result.isInteger || result.isFloat || result.isDouble || result.isLong || result.isShort) {
                result.isNumeric = true;
            }
        }
        return result;
    }

    // adapts codegen models and property to dart rules of inheritance
    private Map<String, ModelsMap> adaptToDartInheritance(Map<String, ModelsMap> objs) {
        // get all models
        Map<String, CodegenModel> allModels = new HashMap<>();
        for (ModelsMap modelsEntries : objs.values()) {
            for (ModelMap modelsMap : modelsEntries.getModels()) {
                CodegenModel model = modelsMap.getModel();
                var hasAnyOfOrOneOf = ((model.anyOf != null && !model.anyOf.isEmpty())
                        || (model.oneOf != null && !model.oneOf.isEmpty()));
                model.vendorExtensions.put("hasAnyOfOrOneOf", hasAnyOfOrOneOf);
                // if (model.discriminator != null) {
                // //if there is a discriminator, make sure it exists in vars.
                // var discriminatorPropertyResult = model.requiredVars.stream()
                // .filter(t ->
                // t.baseName.equals(model.discriminator.getPropertyBaseName())).findFirst();

                // if (discriminatorPropertyResult.isPresent()) {
                // final CodegenProperty discriminatorProperty =
                // discriminatorPropertyResult.get();
                // if (!model.vars.stream().anyMatch(v ->
                // v.baseName.equals(discriminatorProperty.baseName))) {
                // model.vars.add(discriminatorProperty);
                // }
                // }
                // }
                allModels.put(model.getClassname(), model);
            }
        }

        // all ancestors
        Set<String> allAncestorsForAllModelsFlat = new HashSet<>();
        // maps a model to its ancestors
        Map<String, Set<String>> allAncestorsForAllModels = new HashMap<>();
        for (java.util.Map.Entry<String, CodegenModel> cm : allModels.entrySet()) {
            Set<String> allAncestors = new HashSet<>();
            // get all ancestors
            // TODO: optimize this logic ?
            getAncestors(cm.getValue(), allModels, allAncestors);
            // just in case, a model can't be its own ancestor
            allAncestors.remove(cm.getKey());

            allAncestorsForAllModels.put(cm.getKey(), allAncestors);
            allAncestorsForAllModelsFlat.addAll(allAncestors);
        }

        Set<String> allPureClasses = new HashSet<>();
        // set isChild,isParent,isPure
        for (java.util.Map.Entry<String, CodegenModel> cmEntry : allModels.entrySet()) {
            String key = cmEntry.getKey();
            CodegenModel cm = cmEntry.getValue();
            // get all ancestors
            Set<String> allAncestors = allAncestorsForAllModels.get(key);

            // a class is a parent when it's an ancestor to another class
            boolean isParent = allAncestorsForAllModelsFlat.contains(key);
            // a class is a child when it has any ancestor
            boolean isChild = !allAncestors.isEmpty();
            // a class is pure when it's not a child, and has no oneOf nor anyOf
            boolean isPure = !isChild && (cm.oneOf == null || cm.oneOf.isEmpty())
                    && (cm.anyOf == null || cm.anyOf.isEmpty());

            Map<String, Object> varLocation = getInheritanceVariablesLocation(cm);
            varLocation.put(kIsChild, isChild);
            varLocation.put(kIsParent, isParent);
            varLocation.put(kIsPure, isPure);
            // if (!isParent && (cm.oneOf == null || cm.oneOf.isEmpty())) {
            // // discriminator has no meaning here
            // if (cm.discriminator != null) {
            // varLocation.put(kParentDiscriminator, cm.discriminator);
            // cm.discriminator = null;
            // }
            // }
            // when pure:
            // vars = allVars = selfOnlyProperties = kSelfAndAncestorOnlyProps
            // ancestorOnlyProps = empty
            if (isPure) {
                varLocation.put(kHasAnyVars, cm.getAllVars() != null && cm.getAllVars().size() > 0);
                varLocation.put(kSelfOnlyProps, new ArrayList<>(cm.getVars()));
                varLocation.put(kHasSelfOnlyProps, !cm.getVars().isEmpty());
                varLocation.put(kAncestorOnlyProps, cm.parentVars = new ArrayList<CodegenProperty>());
                varLocation.put(kHasAncestorOnlyProps, false);
                varLocation.put(kSelfAndAncestorOnlyProps, cm.allVars = new ArrayList<>(cm.getVars()));
                varLocation.put(kHasSelfAndAncestorOnlyProps, !cm.getVars().isEmpty());

                allPureClasses.add(key);
            }
        }

        // handle impure models
        for (java.util.Map.Entry<String, CodegenModel> cmEntry : allModels.entrySet()) {
            String key = cmEntry.getKey();
            CodegenModel cm = cmEntry.getValue();
            if (allPureClasses.contains(key)) {
                continue;
            }
            // get all ancestors
            Set<String> allAncestors = allAncestorsForAllModels.get(key);

            // get direct parents
            // Set<String> directParentNames = cm.allOf == null ? new HashSet<>() :
            // cm.allOf;
            Set<String> compositeProperties = new HashSet<>();

            Set<String> compositeModelNames = new HashSet<String>();
            compositeModelNames.addAll(ObjectUtils.firstNonNull(cm.oneOf, new HashSet<>()));
            compositeModelNames.addAll(ObjectUtils.firstNonNull(cm.anyOf, new HashSet<>()));
            compositeModelNames.addAll(allAncestors);

            for (String compositeModelName : compositeModelNames) {
                CodegenModel model = allModels.get(compositeModelName);
                if (model == null)
                    continue;
                List<CodegenProperty> allVars = ObjectUtils.firstNonNull(model.getAllVars(), new ArrayList<>());
                for (CodegenProperty prop : allVars) {
                    compositeProperties.add(prop.getName());
                }
            }
            // dart classes declare selfOnlyProperties as direct members (they exist in
            // "vars")
            // for pure models, this will equal vars
            Map<String, CodegenProperty> selfOnlyProperties = new HashMap<>();

            // ancestorOnlyProperties are properties defined by all ancestors
            // NOTE: oneOf,anyOf are NOT considered ancestors
            // since a child in dart must implement ALL OF the parent (using implements)
            Map<String, CodegenProperty> ancestorOnlyProperties = new HashMap<>();

            // combines both selfOnlyProperties and ancestorOnlyProperties
            // this will be used by the custom serializer as "x-handled-vars" and
            // "x-has-handled-vars"
            Map<String, CodegenProperty> selfAndAncestorOnlyProperties = new HashMap<>();

            // STEP 1: calculating selfOnlyProperties
            // get all vars of all ancestors and add them to ancestorPropNames
            // Set<String> _ancestorPropNames = new HashSet<>();
            for (String ancestorKey : allAncestors) {
                CodegenModel ancestorCM = allModels.get(ancestorKey);
                if (ancestorCM != null) {
                    for (CodegenProperty prop : ancestorCM.getVars()) {
                        ancestorOnlyProperties.put(prop.getName(), prop);
                    }
                }
            }
            for (CodegenProperty p : cm.getVars()) {
                p.isInherited = ancestorOnlyProperties.containsKey(p.getName());
                if (!p.isInherited && (!compositeProperties.contains(p.getName()) || p.isDiscriminator)) {
                    selfOnlyProperties.put(p.getName(), p);
                }
            }
            selfAndAncestorOnlyProperties.putAll(selfOnlyProperties);
            selfAndAncestorOnlyProperties.putAll(ancestorOnlyProperties);

            Map<String, Object> varLocation = getInheritanceVariablesLocation(cm);
            varLocation.put(kHasAnyVars, cm.getAllVars() != null && cm.getAllVars().size() > 0);
            varLocation.put(kSelfOnlyProps, cm.vars = new ArrayList<>(selfOnlyProperties.values()));
            varLocation.put(kHasSelfOnlyProps, !selfOnlyProperties.isEmpty());
            varLocation.put(kAncestorOnlyProps, cm.parentVars = new ArrayList<>(ancestorOnlyProperties.values()));
            varLocation.put(kHasAncestorOnlyProps, !ancestorOnlyProperties.isEmpty());
            varLocation.put(kSelfAndAncestorOnlyProps,
                    cm.allVars = new ArrayList<>(selfAndAncestorOnlyProperties.values()));
            varLocation.put(kHasSelfAndAncestorOnlyProps, !selfAndAncestorOnlyProperties.isEmpty());
        }

        return objs;
    }

    /// Gets all ancestors of a given model, and puts it in accumulator
    private void getAncestors(CodegenModel cm, Map<String, CodegenModel> allModels, Set<String> accumulator) {

        // get direct parents
        Set<String> directParentNames = cm.allOf;
        if (directParentNames != null && !directParentNames.isEmpty()) {
            for (String directParentName : directParentNames) {
                if (accumulator.add(directParentName)) {
                    CodegenModel parent = allModels.get(directParentName);
                    if (parent != null) {
                        getAncestors(parent, allModels, accumulator);
                    }
                }
            }
        }
    }

    @Override
    public Map<String, ModelsMap> postProcessAllModels(Map<String, ModelsMap> objs) {
        objs = super.postProcessAllModels(objs);
        objs = super.updateAllModels(objs);
        objs = adaptToDartInheritance(objs);

        return objs;
    }

    // private void addSharedInfrastructureFiles() {

    // String sharedInfrastructureFolder = "shared_infrastructure" + File.separator;

    // String libFolder = sharedInfrastructureFolder + "lib" + File.separator;
    // String srcFolder = libFolder + "src" + File.separator;
    // String networkingFolder = srcFolder + "networking" + File.separator;
    // String serializationFolder = srcFolder + "serialization" + File.separator;
    // supportingFiles.add(new SupportingFile(sharedInfrastructureFolder +
    // "pubspec.mustache", sharedInfrastructureFolder,
    // "pubspec.yaml"));
    // supportingFiles.add(
    // new SupportingFile(libFolder + "shared_infrastructure.dart", libFolder,
    // "shared_infrastructure.dart"));
    // // Networking
    // supportingFiles.add(
    // new SupportingFile(networkingFolder + "_exports.mustache", networkingFolder,
    // "_exports.dart"));
    // supportingFiles.add(
    // new SupportingFile(networkingFolder + "client.mustache", networkingFolder,
    // "client.dart"));
    // supportingFiles.add(
    // new SupportingFile(networkingFolder + "helpers.mustache", networkingFolder,
    // "helpers.dart"));
    // supportingFiles.add(
    // new SupportingFile(networkingFolder + "http_packets.mustache",
    // networkingFolder, "http_packets.dart"));
    // supportingFiles.add(
    // new SupportingFile(networkingFolder + "request.mustache", networkingFolder,
    // "request.dart"));
    // supportingFiles.add(
    // new SupportingFile(networkingFolder + "request.multipart.mustache",
    // networkingFolder, "request.multipart.dart"));
    // supportingFiles.add(
    // new SupportingFile(networkingFolder + "response.mustache", networkingFolder,
    // "response.dart"));

    // // Serialization
    // supportingFiles.add(
    // new SupportingFile(serializationFolder + "_exports.mustache",
    // serializationFolder, "_exports.dart"));
    // supportingFiles.add(
    // new SupportingFile(serializationFolder + "helpers.mustache",
    // serializationFolder, "helpers.dart"));
    // supportingFiles.add(
    // new SupportingFile(serializationFolder + "undefined_wrapper.mustache",
    // serializationFolder,
    // "undefined_wrapper.dart"));

    // }

    @Override
    public OperationsMap postProcessOperationsWithModels(OperationsMap objs, List<ModelMap> allModels) {
        var result = super.postProcessOperationsWithModels(objs, allModels);
        var operations = result.getOperations();
        for (var operation : operations.getOperation()) {
            var bodyParam = operation.bodyParam;
            if (bodyParam == null) {
                continue;
            }
            var resultContentMap = bodyParam.getContent();
            if (resultContentMap == null) {
                continue;
            }
            var betterConsumes = new ArrayList<Map<String, Object>>();
            for (var consumeEntry : resultContentMap.entrySet()) {
                var key = consumeEntry.getKey();
                var resultContent = consumeEntry.getValue();
                var betterConsume = new HashMap<String, Object>();
                Map<String, String> originalConsume = null;
                if (operation.consumes != null) {
                    for (var x : operation.consumes) {
                        var mediaType = x.get("mediaType");
                        if (key.equals(mediaType)) {
                            originalConsume = x;
                            break;
                        }
                    }
                }
                if (originalConsume != null) {
                    betterConsume.putAll(originalConsume);
                }
                betterConsume.put("key", key);
                var mimePatternList = key.toLowerCase(Locale.ROOT).split(";")[0].split("/");
                betterConsume.put("is_mime_" + mimePatternList[0], true);
                betterConsume.put("is_mime_" + mimePatternList[0] + "_" + mimePatternList[1], true);
                betterConsume.put("content", resultContent);
                var extraParameters = new ArrayList<CodegenParameter>();
                if (resultContent != null && resultContent.getEncoding() != null) {
                    for (var propEncoding : resultContent.getEncoding().entrySet()) {
                        if (propEncoding.getValue() == null || propEncoding.getValue().getHeaders() == null) {
                            continue;
                        }
                        var propBaseName = propEncoding.getKey();
                        var headers = propEncoding.getValue().getHeaders();
                        for (var header : headers) {
                            if (header.paramName != null) {
                                header.paramName = toParamName(propBaseName + "_" + header.paramName + "_header");
                            }
                            extraParameters.add(header);
                        }
                    }
                }

                betterConsume.put("extraParameters", extraParameters);
                betterConsumes.add(betterConsume);
            }
            operation.vendorExtensions.put("better-consumes", betterConsumes);
        }
        for (var operation : operations.getOperation()) {
            if (operation.responses == null) {
                continue;
            }
            for (var response : operation.responses) {
                if (response.getContent() == null) {
                    continue;
                }
                for (var content : response.getContent().entrySet()) {
                    var key = content.getKey();
                    var value = content.getValue();
                    if (value == null) {
                        continue;
                    }

                    var mimePatternList = key.toLowerCase(Locale.ROOT).split(";")[0].split("/");
                    value.vendorExtensions.put("is_mime_" + mimePatternList[0], true);
                    value.vendorExtensions.put("is_mime_" + mimePatternList[0] + "_" + mimePatternList[1], true);
                    if (value.getSchema() != null) {
                        value.getSchema().vendorExtensions.putAll(value.vendorExtensions);
                    }
                }
            }
        }
        return result;
    }

    @Override
    protected boolean needToImport(String type) {
        // Import everything, unless it is from dart:core.
        return StringUtils.isNotBlank(type) && (!imports.containsKey(type) || !imports.get(type).equals("dart:core"));
    }

    @Override
    protected boolean isReservedWord(String word) {
        // consider everything as reserved that is
        // * a keyword
        // * a word that has been mapped in the reservedWordsMappings
        // * a default included type or a type include through some library
        return super.isReservedWord(word) || reservedWordsMappings().containsKey(word)
                || defaultIncludes().contains(word);
    }

    @Override
    public String escapeReservedWord(String name) {
        if (reservedWordsMappings().containsKey(name)) {
            return reservedWordsMappings().get(name);
        }
        return "$" + name;
    }

    @Override
    public String apiFileFolder() {
        return (outputFolder + File.separator + libPath + sourceFolder + File.separator + apiPackage()).replace('/',
                File.separatorChar);
    }

    @Override
    public String modelFileFolder() {
        return (outputFolder + File.separator + libPath + sourceFolder + File.separator + modelPackage()).replace('/',
                File.separatorChar);
    }

    @Override
    public String apiTestFileFolder() {
        return outputFolder + File.separator + apiTestFolder;
    }

    @Override
    public String modelTestFileFolder() {
        return outputFolder + File.separator + modelTestFolder;
    }

    @Override
    public String apiDocFileFolder() {
        return outputFolder + File.separator + apiDocFolder;
    }

    @Override
    public String modelDocFileFolder() {
        return outputFolder + File.separator + modelDocFolder;
    }

    @Override
    public String toVarName(String name) {
        // obtain the name from nameMapping directly if provided
        if (nameMapping.containsKey(name)) {
            return nameMapping.get(name);
        }

        // sanitize name
        name = sanitizeName(name, "\\W-[\\$]"); // FIXME: a parameter should not be assigned. Also declare the methods
                                                // parameters as 'final'.

        if (name.toLowerCase(Locale.ROOT).matches("^_*class$")) {
            return "propertyClass";
        }

        if ("_".equals(name)) {
            name = "_u";
        }

        // numbers are not allowed at the beginning
        if (name.matches("^\\d.*")) {
            name = "_" + name;
        }

        // if it's all upper case, do nothing
        if (name.matches("^[A-Z0-9_]*$")) {
            return name;
        }

        if (startsWithTwoUppercaseLetters(name)) {
            name = name.substring(0, 2).toLowerCase(Locale.ROOT) + name.substring(2);
        }

        // If name contains special chars -> replace them.
        if ((((CharSequence) name).chars()
                .anyMatch(character -> specialCharReplacements.containsKey(String.valueOf((char) character))))) {
            List<String> allowedCharacters = new ArrayList<>();
            allowedCharacters.add("_");
            allowedCharacters.add("$");
            name = escape(name, specialCharReplacements, allowedCharacters, "_");
        }

        // camelize (lower first character) the variable name
        // pet_id => petId
        if (camelCaseDollarSign) {
            name = camelize(name, LOWERCASE_FIRST_CHAR);
        } else {
            name = camelize(name, LOWERCASE_FIRST_LETTER);
        }

        // for reserved word or word starting with number, append _
        if (isReservedWord(name) || name.matches("^\\d.*")) {
            name = escapeReservedWord(name);
        }

        return name;
    }

    private boolean startsWithTwoUppercaseLetters(String name) {
        boolean startsWithTwoUppercaseLetters = false;
        if (name.length() > 1) {
            startsWithTwoUppercaseLetters = name.substring(0, 2).equals(name.substring(0, 2).toUpperCase(Locale.ROOT));
        }
        return startsWithTwoUppercaseLetters;
    }

    @Override
    public String toParamName(String name) {
        if (parameterNameMapping.containsKey(name)) {
            return parameterNameMapping.get(name);
        }

        // should be the same as variable name
        return toVarName(name);
    }

    @Override
    public String toModelName(final String name) {
        // obtain the name from modelNameMapping directly if provided
        if (modelNameMapping.containsKey(name)) {
            return modelNameMapping.get(name);
        }

        String sanitizedName = sanitizeName(name);

        if (!StringUtils.isEmpty(modelNamePrefix)) {
            // add '_' so that model name can be camelized correctly
            sanitizedName = modelNamePrefix + "_" + sanitizedName;
        }

        if (!StringUtils.isEmpty(modelNameSuffix)) {
            // add '_' so that model name can be camelized correctly
            sanitizedName = sanitizedName + "_" + modelNameSuffix;
        }

        // camelize the model name
        // phone_number => PhoneNumber
        final String camelizedName = camelize(sanitizedName);

        // Check if there is a mapping that can be used
        if (typeMapping().containsKey(camelizedName)) {
            String typeName = typeMapping().get(camelizedName);
            if (imports.containsKey(typeName)) {
                // Anything with an import mapping is likely
                // generator specific and can not be used as model name.
                final String modelName = "$" + camelizedName;
                LOGGER.warn("{} (existing type) cannot be used as model name. Renamed to {}", camelizedName, modelName);
                return modelName;
            }
            return typeName;
        }

        // model name cannot use reserved keyword, e.g. return
        if (isReservedWord(camelizedName)) {
            final String modelName = "$" + camelizedName;
            LOGGER.warn("{} (reserved word) cannot be used as model name. Renamed to {}", camelizedName, modelName);
            return modelName;
        }

        // model name starts with number
        if (camelizedName.matches("^\\d.*")) {
            final String modelName = "$" + camelizedName; // e.g. 200Response => $200Response (after camelize)
            LOGGER.warn("{} (model name starts with number) cannot be used as model name. Renamed to {}", name,
                    modelName);
            return modelName;
        }

        return camelizedName;
    }

    @Override
    public String toModelFilename(String name) {
        return underscore(toModelName(name));
    }

    @Override
    public String toModelDocFilename(String name) {
        return toModelName(name);
    }

    @Override
    public String toApiFilename(String name) {
        return underscore(toApiName(name));
    }

    @Override
    public String toApiTestFilename(String name) {
        return toApiFilename(name) + "_test";
    }

    @Override
    public String toModelTestFilename(String name) {
        return toModelFilename(name) + "_test";
    }

    @Override
    public String toDefaultValue(Schema schema) {
        if (schema.getDefault() != null) {
            // if (ModelUtils.isMapSchema(schema) || ModelUtils.isSet(schema)) {
            // return "const {}";
            // }
            // if (ModelUtils.isArraySchema(schema)) {
            // return "const []";
            // }
            if (ModelUtils.isDateSchema(schema) || ModelUtils.isDateTimeSchema(schema)) {
                // this is currently not supported and would create compile errors
                return null;
            }
            if (ModelUtils.isStringSchema(schema)) {
                return "'" + schema.getDefault().toString().replace("'", "\\'") + "'";
            }
            return schema.getDefault().toString();
        }
        return null;

    }

    @Override
    public CodegenProperty fromProperty(String name, Schema p, boolean required,
            boolean schemaIsFromAdditionalProperties) {
        var result = super.fromProperty(name, p, required,
                schemaIsFromAdditionalProperties);
        // Fix for DefaultGenerator setting isModel to false when it has both
        // properties + additionalProperties.
        Schema referencedSchema = ModelUtils.getReferencedSchema(this.openAPI, p);
        if (referencedSchema != null && !result.isModel &&
                ModelUtils.isModel(referencedSchema) &&
                referencedSchema.getProperties() != null &&
                !referencedSchema.getProperties().isEmpty()) {
            result.isModel = true;
        }
        return result;
    }

    @Override
    public String getTypeDeclaration(Schema p) {
        Schema<?> schema = unaliasSchema(p);
        Schema<?> target = ModelUtils.isGenerateAliasAsModel() ? p : schema;
        return super.getTypeDeclaration(target);
    }
    // @Override
    // public String getTypeDeclaration(Schema p) {
    // Schema<?> schema = unaliasSchema(p);
    // Schema<?> target = ModelUtils.isGenerateAliasAsModel() ? p : schema;
    // if (ModelUtils.isArraySchema(target)) {
    // Schema<?> items = ModelUtils.getSchemaItems(schema);
    // return getSchemaType(target) + "<" + getTypeDeclaration(items) + ">";
    // }
    // if (ModelUtils.isMapSchema(target)) {
    // // Note: ModelUtils.isMapSchema(p) returns true when p is a composed schema
    // that also defines
    // // additionalproperties: true
    // Schema<?> inner = ModelUtils.getAdditionalProperties(target);
    // if (inner == null) {
    // LOGGER.error("`{}` (map property) does not have a proper inner type defined.
    // Default to type:string", p.getName());
    // inner = new StringSchema().description("TODO default missing map inner type
    // to string");
    // p.setAdditionalProperties(inner);
    // }
    // return getSchemaType(target) + "<String, " + getTypeDeclaration(inner) + ">";
    // }
    // return super.getTypeDeclaration(p);
    // }

    @Override
    public String getSchemaType(Schema p) {
        String openAPIType = super.getSchemaType(p);

        // don't apply renaming on types from the typeMapping
        if (typeMapping.containsKey(openAPIType)) {
            return typeMapping.get(openAPIType);
        }

        if (null == openAPIType) {
            LOGGER.error("No Type defined for Schema {}", p);
        }
        return toModelName(openAPIType);
    }

    @Override
    public ModelsMap postProcessModels(ModelsMap objs) {
        objs = postProcessModelsEnum(objs);

        return objs;
    }

    @Override
    public void updateCodegenPropertyEnum(CodegenProperty var, CodegenModel cm) {
        super.updateCodegenPropertyEnum(var);
        if (var.enumName != null) {
            propagateCorrectEnumName(var, cm.classname + var.enumName);
        }
    }

    protected void propagateCorrectEnumName(CodegenProperty var, String enumName) {
        if (var == null) {
            return;
        }
        if (var.enumName != null && !var.enumName.equals(enumName)) {
            var oldEnumName = var.enumName;
            LOGGER.info("enumName is not correct for property {}, renaming from {} to {}", var.name, var.enumName,
                    enumName);

            var.enumName = enumName;
            if (oldEnumName.equals(var.datatypeWithEnum)) {
                var.datatypeWithEnum = enumName;
            }
        }
        propagateCorrectEnumName(var.items, enumName);
        propagateCorrectEnumName(var.additionalProperties, enumName);
    }

    @Override
    public String toEnumName(CodegenProperty property) {
        return sanitizeName(camelize(property.name)) + "Enum";
    }

    @Override
    public String toEnumVarName(String value, String datatype) {
        if (enumNameMapping.containsKey(value)) {
            return enumNameMapping.get(value);
        }

        if (value.length() == 0) {
            return "empty";
        }
        if (("number".equalsIgnoreCase(datatype) ||
                "double".equalsIgnoreCase(datatype) ||
                "int".equalsIgnoreCase(datatype)) &&
                value.matches("^-?\\d.*")) {
            // Only rename numeric values when the datatype is numeric
            // AND the name is not changed by enum extensions (matches a numeric value).
            boolean isNegative = value.startsWith("-");
            return toVarName("number" + (isNegative ? "_negative" : "") + value);
        }
        return toVarName(value);
    }

    @Override
    public String toEnumValue(String value, String datatype) {
        if ("String".equalsIgnoreCase(datatype) ||
                "string".equalsIgnoreCase(datatype)) {
            return "'" + escapeText(value) + "'";
        } else {
            return value;
        }
    }

    @Override
    public String toOperationId(String operationId) {
        operationId = super.toOperationId(operationId);

        operationId = camelize(sanitizeName(operationId), LOWERCASE_FIRST_LETTER);

        // method name cannot use reserved keyword, e.g. return
        if (isReservedWord(operationId)) {
            String newOperationId = camelize("call_" + operationId, LOWERCASE_FIRST_LETTER);
            LOGGER.warn("{} (reserved word) cannot be used as method name. Renamed to {}", operationId, newOperationId);
            return newOperationId;
        }

        // operationId starts with a number
        if (operationId.matches("^\\d.*")) {
            String newOperationId = "$" + operationId;
            LOGGER.warn("{} (starting with a number) cannot be used as method name. Renamed to {}", operationId,
                    newOperationId);
            operationId = newOperationId;
        }

        return operationId;
    }

    @Override
    public String escapeQuotationMark(String input) {
        // remove " to avoid code injection
        return input.replace("\"", "");
    }

    @Override
    public String escapeUnsafeCharacters(String input) {
        return input.replace("*/", "*_/").replace("/*", "/_*");
    }

    @Override
    public void postProcessFile(File file, String fileType) {
        super.postProcessFile(file, fileType);
        if (file == null) {
            return;
        }

        String dartPostProcessFile = System.getenv("DART_POST_PROCESS_FILE");
        if (StringUtils.isEmpty(dartPostProcessFile)) {
            return; // skip if DART_POST_PROCESS_FILE env variable is not defined
        }

        // process all files with dart extension
        if ("dart".equals(FilenameUtils.getExtension(file.toString()))) {
            // currently supported is "dartfmt -w" and "dart format"
            String command = dartPostProcessFile + " " + file;
            try {
                Process p = Runtime.getRuntime().exec(command);
                int exitValue = p.waitFor();
                if (exitValue != 0) {
                    LOGGER.error("Error running the command ({}). Exit code: {}", command, exitValue);
                } else {
                    LOGGER.info("Successfully executed: {}", command);
                }
            } catch (InterruptedException | IOException e) {
                LOGGER.error("Error running the command ({}). Exception: {}", command, e.getMessage());
                // Restore interrupted state
                Thread.currentThread().interrupt();
            }
        }
    }
}
