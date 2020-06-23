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

package org.openapitools.codegen;

import com.github.benmanes.caffeine.cache.Cache;
import com.github.benmanes.caffeine.cache.Caffeine;
import com.github.benmanes.caffeine.cache.Ticker;
import com.google.common.base.CaseFormat;
import com.google.common.collect.ImmutableMap;
import com.samskivert.mustache.Mustache;
import com.samskivert.mustache.Mustache.Compiler;
import com.samskivert.mustache.Mustache.Lambda;

import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringEscapeUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.openapitools.codegen.CodegenDiscriminator.MappedModel;
import org.openapitools.codegen.api.TemplatingEngineAdapter;
import org.openapitools.codegen.config.GlobalSettings;
import org.openapitools.codegen.examples.ExampleGenerator;
import org.openapitools.codegen.meta.FeatureSet;
import org.openapitools.codegen.meta.GeneratorMetadata;
import org.openapitools.codegen.meta.Stability;
import org.openapitools.codegen.meta.features.*;
import org.openapitools.codegen.serializer.SerializerUtils;
import org.openapitools.codegen.templating.MustacheEngineAdapter;
import org.openapitools.codegen.templating.mustache.*;
import org.openapitools.codegen.utils.ModelUtils;
import org.openapitools.codegen.utils.OneOfImplementorAdditionalData;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.*;
import java.util.Map.Entry;
import java.util.concurrent.TimeUnit;
import java.util.function.Consumer;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import io.swagger.v3.core.util.Json;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.PathItem;
import io.swagger.v3.oas.models.callbacks.Callback;
import io.swagger.v3.oas.models.examples.Example;
import io.swagger.v3.oas.models.headers.Header;
import io.swagger.v3.oas.models.media.*;
import io.swagger.v3.oas.models.parameters.*;
import io.swagger.v3.oas.models.responses.ApiResponse;
import io.swagger.v3.oas.models.responses.ApiResponses;
import io.swagger.v3.oas.models.security.OAuthFlow;
import io.swagger.v3.oas.models.security.OAuthFlows;
import io.swagger.v3.oas.models.security.SecurityScheme;
import io.swagger.v3.oas.models.servers.Server;
import io.swagger.v3.oas.models.servers.ServerVariable;
import io.swagger.v3.parser.util.SchemaTypeUtil;

import static org.openapitools.codegen.utils.OnceLogger.once;
import static org.openapitools.codegen.utils.StringUtils.*;

public class DefaultCodegen implements CodegenConfig {
    private static final Logger LOGGER = LoggerFactory.getLogger(DefaultCodegen.class);

    public static FeatureSet DefaultFeatureSet;

    // A cache of sanitized words. The sanitizeName() method is invoked many times with the same
    // arguments, this cache is used to optimized performance.
    private static Cache<SanitizeNameOptions, String> sanitizedNameCache;

    static {
        DefaultFeatureSet = FeatureSet.newBuilder()
                .includeDataTypeFeatures(
                        DataTypeFeature.Int32, DataTypeFeature.Int64, DataTypeFeature.Float, DataTypeFeature.Double,
                        DataTypeFeature.Decimal, DataTypeFeature.String, DataTypeFeature.Byte, DataTypeFeature.Binary,
                        DataTypeFeature.Boolean, DataTypeFeature.Date, DataTypeFeature.DateTime, DataTypeFeature.Password,
                        DataTypeFeature.File, DataTypeFeature.Array, DataTypeFeature.Maps, DataTypeFeature.CollectionFormat,
                        DataTypeFeature.CollectionFormatMulti, DataTypeFeature.Enum, DataTypeFeature.ArrayOfEnum, DataTypeFeature.ArrayOfModel,
                        DataTypeFeature.ArrayOfCollectionOfPrimitives, DataTypeFeature.ArrayOfCollectionOfModel, DataTypeFeature.ArrayOfCollectionOfEnum,
                        DataTypeFeature.MapOfEnum, DataTypeFeature.MapOfModel, DataTypeFeature.MapOfCollectionOfPrimitives,
                        DataTypeFeature.MapOfCollectionOfModel, DataTypeFeature.MapOfCollectionOfEnum
                        // Custom types are template specific
                )
                .includeDocumentationFeatures(
                        DocumentationFeature.Api, DocumentationFeature.Model
                        // README is template specific
                )
                .includeGlobalFeatures(
                        GlobalFeature.Host, GlobalFeature.BasePath, GlobalFeature.Info, GlobalFeature.PartialSchemes,
                        GlobalFeature.Consumes, GlobalFeature.Produces, GlobalFeature.ExternalDocumentation, GlobalFeature.Examples,
                        GlobalFeature.Callbacks
                        // TODO: xml structures, styles, link objects, parameterized servers, full schemes for OAS 2.0
                )
                .includeSchemaSupportFeatures(
                        SchemaSupportFeature.Simple, SchemaSupportFeature.Composite,
                        SchemaSupportFeature.Polymorphism
                        // Union (OneOf) not 100% yet.
                )
                .includeParameterFeatures(
                        ParameterFeature.Path, ParameterFeature.Query, ParameterFeature.Header, ParameterFeature.Body,
                        ParameterFeature.FormUnencoded, ParameterFeature.FormMultipart, ParameterFeature.Cookie
                )
                .includeSecurityFeatures(
                        SecurityFeature.BasicAuth, SecurityFeature.ApiKey, SecurityFeature.BearerToken,
                        SecurityFeature.OAuth2_Implicit, SecurityFeature.OAuth2_Password,
                        SecurityFeature.OAuth2_ClientCredentials, SecurityFeature.OAuth2_AuthorizationCode
                        // OpenIDConnect not yet supported
                )
                .includeWireFormatFeatures(
                        WireFormatFeature.JSON, WireFormatFeature.XML
                        // PROTOBUF and Custom are generator specific
                )
                .build();

        int cacheSize = Integer.parseInt(GlobalSettings.getProperty(NAME_CACHE_SIZE_PROPERTY, "500"));
        int cacheExpiry = Integer.parseInt(GlobalSettings.getProperty(NAME_CACHE_EXPIRY_PROPERTY, "10"));
        sanitizedNameCache = Caffeine.newBuilder()
                .maximumSize(cacheSize)
                .expireAfterAccess(cacheExpiry, TimeUnit.SECONDS)
                .ticker(Ticker.systemTicker())
                .build();
    }

    protected GeneratorMetadata generatorMetadata;
    protected String inputSpec;
    protected String outputFolder = "";
    protected Set<String> defaultIncludes = new HashSet<String>();
    protected Map<String, String> typeMapping = new HashMap<String, String>();
    protected Map<String, String> instantiationTypes = new HashMap<String, String>();
    protected Set<String> reservedWords = new HashSet<String>();
    protected Set<String> languageSpecificPrimitives = new HashSet<String>();
    protected Map<String, String> importMapping = new HashMap<String, String>();
    protected String modelPackage = "", apiPackage = "", fileSuffix;
    protected String modelNamePrefix = "", modelNameSuffix = "";
    protected String apiNamePrefix = "", apiNameSuffix = "Api";
    protected String testPackage = "";
    /*
    apiTemplateFiles are for API outputs only (controllers/handlers).
    API templates may be written multiple times; APIs are grouped by tag and the file is written once per tag group.
    */
    protected Map<String, String> apiTemplateFiles = new HashMap<String, String>();
    protected Map<String, String> modelTemplateFiles = new HashMap<String, String>();
    protected Map<String, String> apiTestTemplateFiles = new HashMap<String, String>();
    protected Map<String, String> modelTestTemplateFiles = new HashMap<String, String>();
    protected Map<String, String> apiDocTemplateFiles = new HashMap<String, String>();
    protected Map<String, String> modelDocTemplateFiles = new HashMap<String, String>();
    protected Map<String, String> reservedWordsMappings = new HashMap<String, String>();
    protected String templateDir;
    protected String embeddedTemplateDir;
    protected Map<String, Object> additionalProperties = new HashMap<String, Object>();
    protected Map<String, String> serverVariables = new HashMap<String, String>();
    protected Map<String, Object> vendorExtensions = new HashMap<String, Object>();
    /*
    Supporting files are those which aren't models, APIs, or docs.
    These get a different map of data bound to the templates. Supporting files are written once.
    See also 'apiTemplateFiles'.
    */
    protected List<SupportingFile> supportingFiles = new ArrayList<SupportingFile>();
    protected List<CliOption> cliOptions = new ArrayList<CliOption>();
    protected boolean skipOverwrite;
    protected boolean removeOperationIdPrefix;

    /**
     * True if the code generator supports multiple class inheritance.
     * This is used to model the parent hierarchy based on the 'allOf' composed schemas.
     */
    protected boolean supportsMultipleInheritance;
    /**
     * True if the code generator supports single class inheritance.
     * This is used to model the parent hierarchy based on the 'allOf' composed schemas.
     * Note: the single-class inheritance technique has inherent limitations because
     * a 'allOf' composed schema may have multiple $ref child schemas, each one
     * potentially representing a "parent" in the class inheritance hierarchy.
     * Some language generators also use class inheritance to implement the `additionalProperties`
     * keyword. For example, the Java code generator may generate 'extends HashMap'.
     */
    protected boolean supportsInheritance;
    /**
     * True if the language generator supports the 'additionalProperties' keyword
     * as sibling of a composed (allOf/anyOf/oneOf) schema.
     * Note: all language generators should support this to comply with the OAS specification.
     */
    protected boolean supportsAdditionalPropertiesWithComposedSchema;
    protected boolean supportsMixins;
    protected Map<String, String> supportedLibraries = new LinkedHashMap<String, String>();
    protected String library;
    protected Boolean sortParamsByRequiredFlag = true;
    protected Boolean sortModelPropertiesByRequiredFlag = false;
    protected Boolean ensureUniqueParams = true;
    protected Boolean allowUnicodeIdentifiers = false;
    protected String gitHost, gitUserId, gitRepoId, releaseNote;
    protected String httpUserAgent;
    protected Boolean hideGenerationTimestamp = true;
    // How to encode special characters like $
    // They are translated to words like "Dollar" and prefixed with '
    // Then translated back during JSON encoding and decoding
    protected Map<String, String> specialCharReplacements = new HashMap<String, String>();
    // When a model is an alias for a simple type
    protected Map<String, String> typeAliases = null;
    protected Boolean prependFormOrBodyParameters = false;
    // The extension of the generated documentation files (defaults to markdown .md)
    protected String docExtension;
    protected String ignoreFilePathOverride;
    // flag to indicate whether to use environment variable to post process file
    protected boolean enablePostProcessFile = false;
    private TemplatingEngineAdapter templatingEngine = new MustacheEngineAdapter();
    // flag to indicate whether to use the utils.OneOfImplementorAdditionalData related logic
    protected boolean useOneOfInterfaces = false;
    // whether or not the oneOf imports machinery should add oneOf interfaces as imports in implementing classes
    protected boolean addOneOfInterfaceImports = false;
    protected List<CodegenModel> addOneOfInterfaces = new ArrayList<CodegenModel>();

    // flag to indicate whether to only update files whose contents have changed
    protected boolean enableMinimalUpdate = false;

    // acts strictly upon a spec, potentially modifying it to have consistent behavior across generators.
    protected boolean strictSpecBehavior = true;
    // flag to indicate whether enum value prefixes are removed
    protected boolean removeEnumValuePrefix = true;

    // Support legacy logic for evaluating discriminators
    protected boolean legacyDiscriminatorBehavior = true;

    // Specify what to do if the 'additionalProperties' keyword is not present in a schema.
    // See CodegenConstants.java for more details.
    protected boolean disallowAdditionalPropertiesIfNotPresent = true;

    // make openapi available to all methods
    protected OpenAPI openAPI;

    public List<CliOption> cliOptions() {
        return cliOptions;
    }

    public void processOpts() {
        if (additionalProperties.containsKey(CodegenConstants.TEMPLATE_DIR)) {
            this.setTemplateDir((String) additionalProperties.get(CodegenConstants.TEMPLATE_DIR));
        }

        if (additionalProperties.containsKey(CodegenConstants.MODEL_PACKAGE)) {
            this.setModelPackage((String) additionalProperties.get(CodegenConstants.MODEL_PACKAGE));
        }

        if (additionalProperties.containsKey(CodegenConstants.API_PACKAGE)) {
            this.setApiPackage((String) additionalProperties.get(CodegenConstants.API_PACKAGE));
        }

        if (additionalProperties.containsKey(CodegenConstants.HIDE_GENERATION_TIMESTAMP)) {
            setHideGenerationTimestamp(convertPropertyToBooleanAndWriteBack(CodegenConstants.HIDE_GENERATION_TIMESTAMP));
        } else {
            additionalProperties.put(CodegenConstants.HIDE_GENERATION_TIMESTAMP, hideGenerationTimestamp);
        }

        if (additionalProperties.containsKey(CodegenConstants.SORT_PARAMS_BY_REQUIRED_FLAG)) {
            this.setSortParamsByRequiredFlag(Boolean.valueOf(additionalProperties
                    .get(CodegenConstants.SORT_PARAMS_BY_REQUIRED_FLAG).toString()));
        }

        if (additionalProperties.containsKey(CodegenConstants.SORT_MODEL_PROPERTIES_BY_REQUIRED_FLAG)) {
            this.setSortModelPropertiesByRequiredFlag(Boolean.valueOf(additionalProperties
                    .get(CodegenConstants.SORT_MODEL_PROPERTIES_BY_REQUIRED_FLAG).toString()));
        }

        if (additionalProperties.containsKey(CodegenConstants.PREPEND_FORM_OR_BODY_PARAMETERS)) {
            this.setPrependFormOrBodyParameters(Boolean.valueOf(additionalProperties
                    .get(CodegenConstants.PREPEND_FORM_OR_BODY_PARAMETERS).toString()));
        }

        if (additionalProperties.containsKey(CodegenConstants.ENSURE_UNIQUE_PARAMS)) {
            this.setEnsureUniqueParams(Boolean.valueOf(additionalProperties
                    .get(CodegenConstants.ENSURE_UNIQUE_PARAMS).toString()));
        }

        if (additionalProperties.containsKey(CodegenConstants.ALLOW_UNICODE_IDENTIFIERS)) {
            this.setAllowUnicodeIdentifiers(Boolean.valueOf(additionalProperties
                    .get(CodegenConstants.ALLOW_UNICODE_IDENTIFIERS).toString()));
        }

        if (additionalProperties.containsKey(CodegenConstants.API_NAME_PREFIX)) {
            this.setApiNamePrefix((String) additionalProperties.get(CodegenConstants.API_NAME_PREFIX));
        }

        if (additionalProperties.containsKey(CodegenConstants.API_NAME_SUFFIX)) {
            this.setApiNameSuffix((String) additionalProperties.get(CodegenConstants.API_NAME_SUFFIX));
        }

        if (additionalProperties.containsKey(CodegenConstants.MODEL_NAME_PREFIX)) {
            this.setModelNamePrefix((String) additionalProperties.get(CodegenConstants.MODEL_NAME_PREFIX));
        }

        if (additionalProperties.containsKey(CodegenConstants.MODEL_NAME_SUFFIX)) {
            this.setModelNameSuffix((String) additionalProperties.get(CodegenConstants.MODEL_NAME_SUFFIX));
        }

        if (additionalProperties.containsKey(CodegenConstants.REMOVE_OPERATION_ID_PREFIX)) {
            this.setRemoveOperationIdPrefix(Boolean.valueOf(additionalProperties
                    .get(CodegenConstants.REMOVE_OPERATION_ID_PREFIX).toString()));
        }

        if (additionalProperties.containsKey(CodegenConstants.DOCEXTENSION)) {
            this.setDocExtension(String.valueOf(additionalProperties
                    .get(CodegenConstants.DOCEXTENSION).toString()));
        }

        if (additionalProperties.containsKey(CodegenConstants.ENABLE_POST_PROCESS_FILE)) {
            this.setEnablePostProcessFile(Boolean.valueOf(additionalProperties
                    .get(CodegenConstants.ENABLE_POST_PROCESS_FILE).toString()));
        }

        if (additionalProperties.containsKey(CodegenConstants.GENERATE_ALIAS_AS_MODEL)) {
            ModelUtils.setGenerateAliasAsModel(Boolean.valueOf(additionalProperties
                    .get(CodegenConstants.GENERATE_ALIAS_AS_MODEL).toString()));
        }

        if (additionalProperties.containsKey(CodegenConstants.REMOVE_ENUM_VALUE_PREFIX)) {
            this.setRemoveEnumValuePrefix(Boolean.valueOf(additionalProperties
                    .get(CodegenConstants.REMOVE_ENUM_VALUE_PREFIX).toString()));
        }

        if (additionalProperties.containsKey(CodegenConstants.LEGACY_DISCRIMINATOR_BEHAVIOR)) {
            this.setLegacyDiscriminatorBehavior(Boolean.valueOf(additionalProperties
                    .get(CodegenConstants.LEGACY_DISCRIMINATOR_BEHAVIOR).toString()));
        }
        if (additionalProperties.containsKey(CodegenConstants.DISALLOW_ADDITIONAL_PROPERTIES_IF_NOT_PRESENT)) {
            this.setDisallowAdditionalPropertiesIfNotPresent(Boolean.valueOf(additionalProperties
                    .get(CodegenConstants.DISALLOW_ADDITIONAL_PROPERTIES_IF_NOT_PRESENT).toString()));
        }
    }

    /***
     * Preset map builder with commonly used Mustache lambdas.
     *
     * To extend the map, override addMustacheLambdas(), call parent method
     * first and then add additional lambdas to the returned builder.
     *
     * If common lambdas are not desired, override addMustacheLambdas() method
     * and return empty builder.
     *
     * @return preinitialized map builder with common lambdas
     */
    protected ImmutableMap.Builder<String, Lambda> addMustacheLambdas() {

        return new ImmutableMap.Builder<String, Mustache.Lambda>()
                .put("lowercase", new LowercaseLambda().generator(this))
                .put("uppercase", new UppercaseLambda())
                .put("titlecase", new TitlecaseLambda())
                .put("camelcase", new CamelCaseLambda(true).generator(this))
                .put("pascalcase", new CamelCaseLambda(false).generator(this))
                .put("indented", new IndentedLambda())
                .put("indented_8", new IndentedLambda(8, " "))
                .put("indented_12", new IndentedLambda(12, " "))
                .put("indented_16", new IndentedLambda(16, " "));
    }

    private void registerMustacheLambdas() {
        ImmutableMap<String, Lambda> lambdas = addMustacheLambdas().build();

        if (lambdas.size() == 0) {
            return;
        }

        if (additionalProperties.containsKey("lambda")) {
            LOGGER.error("A property called 'lambda' already exists in additionalProperties");
            throw new RuntimeException("A property called 'lambda' already exists in additionalProperties");
        }
        additionalProperties.put("lambda", lambdas);
    }

    // override with any special post-processing for all models
    @SuppressWarnings({"static-method", "unchecked"})
    public Map<String, Object> postProcessAllModels(Map<String, Object> objs) {
        if (this.useOneOfInterfaces) {
            // First, add newly created oneOf interfaces
            for (CodegenModel cm : addOneOfInterfaces) {
                Map<String, Object> modelValue = new HashMap<String, Object>() {{
                    putAll(additionalProperties());
                    put("model", cm);
                }};
                List<Object> modelsValue = Arrays.asList(modelValue);
                List<Map<String, String>> importsValue = new ArrayList<Map<String, String>>();
                Map<String, Object> objsValue = new HashMap<String, Object>() {{
                    put("models", modelsValue);
                    put("package", modelPackage());
                    put("imports", importsValue);
                    put("classname", cm.classname);
                    putAll(additionalProperties);
                }};
                objs.put(cm.name, objsValue);
            }

            // Gather data from all the models that contain oneOf into OneOfImplementorAdditionalData classes
            // (see docstring of that class to find out what information is gathered and why)
            Map<String, OneOfImplementorAdditionalData> additionalDataMap = new HashMap<String, OneOfImplementorAdditionalData>();
            for (Map.Entry modelsEntry : objs.entrySet()) {
                Map<String, Object> modelsAttrs = (Map<String, Object>) modelsEntry.getValue();
                List<Object> models = (List<Object>) modelsAttrs.get("models");
                List<Map<String, String>> modelsImports = (List<Map<String, String>>) modelsAttrs.getOrDefault("imports", new ArrayList<Map<String, String>>());
                for (Object _mo : models) {
                    Map<String, Object> mo = (Map<String, Object>) _mo;
                    CodegenModel cm = (CodegenModel) mo.get("model");
                    if (cm.oneOf.size() > 0) {
                        cm.vendorExtensions.put("x-is-one-of-interface", true);
                        for (String one : cm.oneOf) {
                            if (!additionalDataMap.containsKey(one)) {
                                additionalDataMap.put(one, new OneOfImplementorAdditionalData(one));
                            }
                            additionalDataMap.get(one).addFromInterfaceModel(cm, modelsImports);
                        }
                        // if this is oneOf interface, make sure we include the necessary imports for it
                        addImportsToOneOfInterface(modelsImports);
                    }
                }
            }

            // Add all the data from OneOfImplementorAdditionalData classes to the implementing models
            for (Map.Entry modelsEntry : objs.entrySet()) {
                Map<String, Object> modelsAttrs = (Map<String, Object>) modelsEntry.getValue();
                List<Object> models = (List<Object>) modelsAttrs.get("models");
                List<Map<String, String>> imports = (List<Map<String, String>>) modelsAttrs.get("imports");
                for (Object _implmo : models) {
                    Map<String, Object> implmo = (Map<String, Object>) _implmo;
                    CodegenModel implcm = (CodegenModel) implmo.get("model");
                    String modelName = toModelName(implcm.name);
                    if (additionalDataMap.containsKey(modelName)) {
                        additionalDataMap.get(modelName).addToImplementor(this, implcm, imports, addOneOfInterfaceImports);
                    }
                }
            }
        }

        return objs;
    }

    /**
     * Index all CodegenModels by model name.
     *
     * @param objs Map of models
     * @return map of all models indexed by names
     */
    public Map<String, CodegenModel> getAllModels(Map<String, Object> objs) {
        Map<String, CodegenModel> allModels = new HashMap<String, CodegenModel>();
        for (Entry<String, Object> entry : objs.entrySet()) {
            String modelName = toModelName(entry.getKey());
            Map<String, Object> inner = (Map<String, Object>) entry.getValue();
            List<Map<String, Object>> models = (List<Map<String, Object>>) inner.get("models");
            for (Map<String, Object> mo : models) {
                CodegenModel cm = (CodegenModel) mo.get("model");
                allModels.put(modelName, cm);
            }
        }
        return allModels;
    }

    /**
     * Loop through all models to update different flags (e.g. isSelfReference), children models, etc
     *
     * @param objs Map of models
     * @return maps of models with various updates
     */
    public Map<String, Object> updateAllModels(Map<String, Object> objs) {
        Map<String, CodegenModel> allModels = getAllModels(objs);

        // Fix up all parent and interface CodegenModel references.
        for (CodegenModel cm : allModels.values()) {
            if (cm.getParent() != null) {
                cm.setParentModel(allModels.get(cm.getParent()));
            }
            if (cm.getInterfaces() != null && !cm.getInterfaces().isEmpty()) {
                cm.setInterfaceModels(new ArrayList<CodegenModel>(cm.getInterfaces().size()));
                for (String intf : cm.getInterfaces()) {
                    CodegenModel intfModel = allModels.get(intf);
                    if (intfModel != null) {
                        cm.getInterfaceModels().add(intfModel);
                    }
                }
            }
        }

        // Let parent know about all its children
        for (String name : allModels.keySet()) {
            CodegenModel cm = allModels.get(name);
            CodegenModel parent = allModels.get(cm.getParent());
            // if a discriminator exists on the parent, don't add this child to the inheritance hierarchy
            // TODO Determine what to do if the parent discriminator name == the grandparent discriminator name
            while (parent != null) {
                if (parent.getChildren() == null) {
                    parent.setChildren(new ArrayList<CodegenModel>());
                }
                parent.getChildren().add(cm);
                parent.hasChildren = true;
                Schema parentSchema = this.openAPI.getComponents().getSchemas().get(parent.name);
                if (parentSchema.getDiscriminator() == null) {
                    parent = allModels.get(parent.getParent());
                } else {
                    parent = null;
                }
            }
        }

        // loop through properties of each model to detect self-reference
        for (Map.Entry<String, Object> entry : objs.entrySet()) {
            Map<String, Object> inner = (Map<String, Object>) entry.getValue();
            List<Map<String, Object>> models = (List<Map<String, Object>>) inner.get("models");
            for (Map<String, Object> mo : models) {
                CodegenModel cm = (CodegenModel) mo.get("model");
                for (CodegenProperty cp : cm.allVars) {
                    // detect self import
                    if (cp.dataType.equalsIgnoreCase(cm.classname) ||
                            (cp.isContainer && cp.items != null && cp.items.dataType.equalsIgnoreCase(cm.classname))) {
                        cm.imports.remove(cm.classname); // remove self import
                        cp.isSelfReference = true;
                    }
                }
            }
        }
        setCircularReferences(allModels);

        return objs;
    }

    public void setCircularReferences(Map<String, CodegenModel> models) {
        final Map<String, List<CodegenProperty>> dependencyMap = models.entrySet().stream()
                .collect(Collectors.toMap(Entry::getKey, entry -> getModelDependencies(entry.getValue())));

        models.keySet().forEach(name -> setCircularReferencesOnProperties(name, dependencyMap));
    }

    private List<CodegenProperty> getModelDependencies(CodegenModel model) {
        return model.getAllVars().stream()
                .map(prop -> {
                    if (prop.isContainer) {
                        return prop.items.dataType == null ? null : prop;
                    }
                    return prop.dataType == null ? null : prop;
                })
                .filter(prop -> prop != null)
                .collect(Collectors.toList());
    }

    private void setCircularReferencesOnProperties(final String root,
                                                   final Map<String, List<CodegenProperty>> dependencyMap) {
        dependencyMap.getOrDefault(root, new ArrayList<>()).stream()
                .forEach(prop -> {
                    final List<String> unvisited =
                            Collections.singletonList(prop.isContainer ? prop.items.dataType : prop.dataType);
                    prop.isCircularReference = isCircularReference(root,
                            new HashSet<>(),
                            new ArrayList<>(unvisited),
                            dependencyMap);
                });
    }

    private boolean isCircularReference(final String root,
                                        final Set<String> visited,
                                        final List<String> unvisited,
                                        final Map<String, List<CodegenProperty>> dependencyMap) {
        for (int i = 0; i < unvisited.size(); i++) {
            final String next = unvisited.get(i);
            if (!visited.contains(next)) {
                if (next.equals(root)) {
                    return true;
                }
                dependencyMap.getOrDefault(next, new ArrayList<>())
                        .forEach(prop -> unvisited.add(prop.isContainer ? prop.items.dataType : prop.dataType));
                visited.add(next);
            }
        }
        return false;
    }

    // override with any special post-processing
    @SuppressWarnings("static-method")
    public Map<String, Object> postProcessModels(Map<String, Object> objs) {
        return objs;
    }

    /**
     * post process enum defined in model's properties
     *
     * @param objs Map of models
     * @return maps of models with better enum support
     */
    public Map<String, Object> postProcessModelsEnum(Map<String, Object> objs) {
        List<Object> models = (List<Object>) objs.get("models");
        for (Object _mo : models) {
            Map<String, Object> mo = (Map<String, Object>) _mo;
            CodegenModel cm = (CodegenModel) mo.get("model");

            // for enum model
            if (Boolean.TRUE.equals(cm.isEnum) && cm.allowableValues != null) {
                Map<String, Object> allowableValues = cm.allowableValues;
                List<Object> values = (List<Object>) allowableValues.get("values");
                List<Map<String, Object>> enumVars = buildEnumVars(values, cm.dataType);
                // if "x-enum-varnames" or "x-enum-descriptions" defined, update varnames
                updateEnumVarsWithExtensions(enumVars, cm.getVendorExtensions());
                cm.allowableValues.put("enumVars", enumVars);
            }

            // update codegen property enum with proper naming convention
            // and handling of numbers, special characters
            for (CodegenProperty var : cm.vars) {
                updateCodegenPropertyEnum(var);
            }

            for (CodegenProperty var : cm.allVars) {
                updateCodegenPropertyEnum(var);
            }

            for (CodegenProperty var : cm.requiredVars) {
                updateCodegenPropertyEnum(var);
            }

            for (CodegenProperty var : cm.optionalVars) {
                updateCodegenPropertyEnum(var);
            }

            for (CodegenProperty var : cm.parentVars) {
                updateCodegenPropertyEnum(var);
            }

            for (CodegenProperty var : cm.readOnlyVars) {
                updateCodegenPropertyEnum(var);
            }

            for (CodegenProperty var : cm.readWriteVars) {
                updateCodegenPropertyEnum(var);
            }

        }
        return objs;
    }

    /**
     * Returns the common prefix of variables for enum naming if
     * two or more variables are present
     *
     * @param vars List of variable names
     * @return the common prefix for naming
     */
    public String findCommonPrefixOfVars(List<Object> vars) {
        if (vars.size() > 1) {
            try {
                String[] listStr = vars.toArray(new String[vars.size()]);
                String prefix = StringUtils.getCommonPrefix(listStr);
                // exclude trailing characters that should be part of a valid variable
                // e.g. ["status-on", "status-off"] => "status-" (not "status-o")
                return prefix.replaceAll("[a-zA-Z0-9]+\\z", "");
            } catch (ArrayStoreException e) {
                // do nothing, just return default value
            }
        }
        return "";
    }

    /**
     * Return the enum default value in the language specified format
     *
     * @param value    enum variable name
     * @param datatype data type
     * @return the default value for the enum
     */
    public String toEnumDefaultValue(String value, String datatype) {
        return datatype + "." + value;
    }

    /**
     * Return the enum value in the language specified format
     * e.g. status becomes "status"
     *
     * @param value    enum variable name
     * @param datatype data type
     * @return the sanitized value for enum
     */
    public String toEnumValue(String value, String datatype) {
        if ("number".equalsIgnoreCase(datatype)) {
            return value;
        } else {
            return "\"" + escapeText(value) + "\"";
        }
    }

    /**
     * Return the sanitized variable name for enum
     *
     * @param value    enum variable name
     * @param datatype data type
     * @return the sanitized variable name for enum
     */
    public String toEnumVarName(String value, String datatype) {
        if (value.length() == 0) {
            return "EMPTY";
        }

        String var = value.replaceAll("\\W+", "_").toUpperCase(Locale.ROOT);
        if (var.matches("\\d.*")) {
            return "_" + var;
        } else {
            return var;
        }
    }

    /**
     * Set the OpenAPI document.
     * This method is invoked when the input OpenAPI document has been parsed and validated.
     */
    @Override
    public void setOpenAPI(OpenAPI openAPI) {
        this.openAPI = openAPI;
        // Set global settings such that helper functions in ModelUtils can lookup the value
        // of the CLI option.
        ModelUtils.setDisallowAdditionalPropertiesIfNotPresent(getDisallowAdditionalPropertiesIfNotPresent());
    }

    // override with any special post-processing
    @SuppressWarnings("static-method")
    public Map<String, Object> postProcessOperationsWithModels(Map<String, Object> objs, List<Object> allModels) {
        return objs;
    }

    // override with any special post-processing
    @SuppressWarnings("static-method")
    public Map<String, Object> postProcessSupportingFileData(Map<String, Object> objs) {
        return objs;
    }

    // override to post-process any model properties
    @SuppressWarnings("unused")
    public void postProcessModelProperty(CodegenModel model, CodegenProperty property) {
    }

    // override to post-process any parameters
    @SuppressWarnings("unused")
    public void postProcessParameter(CodegenParameter parameter) {
    }

    //override with any special handling of the entire OpenAPI spec document
    @SuppressWarnings("unused")
    public void preprocessOpenAPI(OpenAPI openAPI) {
        if (useOneOfInterfaces) {
            // we process the openapi schema here to find oneOf schemas and create interface models for them
            Map<String, Schema> schemas = new HashMap<String, Schema>(openAPI.getComponents().getSchemas());
            if (schemas == null) {
                schemas = new HashMap<String, Schema>();
            }
            Map<String, PathItem> pathItems = openAPI.getPaths();

            // we need to add all request and response bodies to processed schemas
            if (pathItems != null) {
                for (Map.Entry<String, PathItem> e : pathItems.entrySet()) {
                    for (Map.Entry<PathItem.HttpMethod, Operation> op : e.getValue().readOperationsMap().entrySet()) {
                        String opId = getOrGenerateOperationId(op.getValue(), e.getKey(), op.getKey().toString());
                        // process request body
                        RequestBody b = ModelUtils.getReferencedRequestBody(openAPI, op.getValue().getRequestBody());
                        Schema requestSchema = null;
                        if (b != null) {
                            requestSchema = ModelUtils.getSchemaFromRequestBody(b);
                        }
                        if (requestSchema != null) {
                            schemas.put(opId, requestSchema);
                        }
                        // process all response bodies
                        for (Map.Entry<String, ApiResponse> ar : op.getValue().getResponses().entrySet()) {
                            ApiResponse a = ModelUtils.getReferencedApiResponse(openAPI, ar.getValue());
                            Schema responseSchema = ModelUtils.getSchemaFromResponse(a);
                            if (responseSchema != null) {
                                schemas.put(opId + ar.getKey(), responseSchema);
                            }
                        }
                    }
                }
            }

            // also add all properties of all schemas to be checked for oneOf
            Map<String, Schema> propertySchemas = new HashMap<String, Schema>();
            for (Map.Entry<String, Schema> e : schemas.entrySet()) {
                Schema s = e.getValue();
                Map<String, Schema> props = s.getProperties();
                if (props == null) {
                    props = new HashMap<String, Schema>();
                }
                for (Map.Entry<String, Schema> p : props.entrySet()) {
                    propertySchemas.put(e.getKey() + "/" + p.getKey(), p.getValue());
                }
            }
            schemas.putAll(propertySchemas);

            // go through all gathered schemas and add them as interfaces to be created
            for (Map.Entry<String, Schema> e : schemas.entrySet()) {
                String n = toModelName(e.getKey());
                Schema s = e.getValue();
                String nOneOf = toModelName(n + "OneOf");
                if (ModelUtils.isComposedSchema(s)) {
                    if (e.getKey().contains("/")) {
                        // if this is property schema, we also need to generate the oneOf interface model
                        addOneOfNameExtension((ComposedSchema) s, nOneOf);
                        addOneOfInterfaceModel((ComposedSchema) s, nOneOf, openAPI);
                    } else {
                        // else this is a component schema, so we will just use that as the oneOf interface model
                        addOneOfNameExtension((ComposedSchema) s, n);
                    }
                } else if (ModelUtils.isArraySchema(s)) {
                    Schema items = ((ArraySchema) s).getItems();
                    if (ModelUtils.isComposedSchema(items)) {
                        addOneOfNameExtension((ComposedSchema) items, nOneOf);
                        addOneOfInterfaceModel((ComposedSchema) items, nOneOf, openAPI);
                    }
                } else if (ModelUtils.isMapSchema(s)) {
                    Schema addProps = getAdditionalProperties(s);
                    if (addProps != null && ModelUtils.isComposedSchema(addProps)) {
                        addOneOfNameExtension((ComposedSchema) addProps, nOneOf);
                        addOneOfInterfaceModel((ComposedSchema) addProps, nOneOf, openAPI);
                    }
                }
            }
        }
    }

    // override with any special handling of the entire OpenAPI spec document
    @SuppressWarnings("unused")
    public void processOpenAPI(OpenAPI openAPI) {
    }

    // override with any special handling of the JMustache compiler
    @SuppressWarnings("unused")
    public Compiler processCompiler(Compiler compiler) {
        return compiler;
    }

    // override with any special handling for the templating engine
    @SuppressWarnings("unused")
    public TemplatingEngineAdapter processTemplatingEngine(TemplatingEngineAdapter templatingEngine) {
        return templatingEngine;
    }

    // override with any special text escaping logic
    @SuppressWarnings("static-method")
    public String escapeText(String input) {
        if (input == null) {
            return input;
        }

        // remove \t, \n, \r
        // replace \ with \\
        // replace " with \"
        // outter unescape to retain the original multi-byte characters
        // finally escalate characters avoiding code injection
        return escapeUnsafeCharacters(
                StringEscapeUtils.unescapeJava(
                        StringEscapeUtils.escapeJava(input)
                                .replace("\\/", "/"))
                        .replaceAll("[\\t\\n\\r]", " ")
                        .replace("\\", "\\\\")
                        .replace("\"", "\\\""));
    }

    /**
     * Escape characters while allowing new lines
     *
     * @param input String to be escaped
     * @return escaped string
     */
    public String escapeTextWhileAllowingNewLines(String input) {
        if (input == null) {
            return input;
        }

        // remove \t
        // replace \ with \\
        // replace " with \"
        // outter unescape to retain the original multi-byte characters
        // finally escalate characters avoiding code injection
        return escapeUnsafeCharacters(
                StringEscapeUtils.unescapeJava(
                        StringEscapeUtils.escapeJava(input)
                                .replace("\\/", "/"))
                        .replaceAll("[\\t]", " ")
                        .replace("\\", "\\\\")
                        .replace("\"", "\\\""));
    }

    // override with any special encoding and escaping logic
    @SuppressWarnings("static-method")
    public String encodePath(String input) {
        return escapeText(input);
    }

    /**
     * override with any special text escaping logic to handle unsafe
     * characters so as to avoid code injection
     *
     * @param input String to be cleaned up
     * @return string with unsafe characters removed or escaped
     */
    public String escapeUnsafeCharacters(String input) {
        LOGGER.warn("escapeUnsafeCharacters should be overridden in the code generator with proper logic to escape " +
                "unsafe characters");
        // doing nothing by default and code generator should implement
        // the logic to prevent code injection
        // later we'll make this method abstract to make sure
        // code generator implements this method
        return input;
    }

    /**
     * Escape single and/or double quote to avoid code injection
     *
     * @param input String to be cleaned up
     * @return string with quotation mark removed or escaped
     */
    public String escapeQuotationMark(String input) {
        LOGGER.warn("escapeQuotationMark should be overridden in the code generator with proper logic to escape " +
                "single/double quote");
        return input.replace("\"", "\\\"");
    }

    public Set<String> defaultIncludes() {
        return defaultIncludes;
    }

    public Map<String, String> typeMapping() {
        return typeMapping;
    }

    public Map<String, String> instantiationTypes() {
        return instantiationTypes;
    }

    public Set<String> reservedWords() {
        return reservedWords;
    }

    public Set<String> languageSpecificPrimitives() {
        return languageSpecificPrimitives;
    }

    public Map<String, String> importMapping() {
        return importMapping;
    }

    public String testPackage() {
        return testPackage;
    }

    public String modelPackage() {
        return modelPackage;
    }

    public String apiPackage() {
        return apiPackage;
    }

    public String fileSuffix() {
        return fileSuffix;
    }

    public String templateDir() {
        return templateDir;
    }

    public String embeddedTemplateDir() {
        if (embeddedTemplateDir != null) {
            return embeddedTemplateDir;
        } else {
            return templateDir;
        }
    }

    public Map<String, String> apiDocTemplateFiles() {
        return apiDocTemplateFiles;
    }

    public Map<String, String> modelDocTemplateFiles() {
        return modelDocTemplateFiles;
    }

    public Map<String, String> reservedWordsMappings() {
        return reservedWordsMappings;
    }

    public Map<String, String> apiTestTemplateFiles() {
        return apiTestTemplateFiles;
    }

    public Map<String, String> modelTestTemplateFiles() {
        return modelTestTemplateFiles;
    }

    public Map<String, String> apiTemplateFiles() {
        return apiTemplateFiles;
    }

    public Map<String, String> modelTemplateFiles() {
        return modelTemplateFiles;
    }

    public String apiFileFolder() {
        return outputFolder + File.separator + apiPackage().replace('.', File.separatorChar);
    }

    public String modelFileFolder() {
        return outputFolder + File.separator + modelPackage().replace('.', File.separatorChar);
    }

    public String apiTestFileFolder() {
        return outputFolder + File.separator + testPackage().replace('.', File.separatorChar);
    }

    public String modelTestFileFolder() {
        return outputFolder + File.separator + testPackage().replace('.', File.separatorChar);
    }

    public String apiDocFileFolder() {
        return outputFolder;
    }

    public String modelDocFileFolder() {
        return outputFolder;
    }

    public Map<String, Object> additionalProperties() {
        return additionalProperties;
    }

    public Map<String, String> serverVariableOverrides() {
        return serverVariables;
    }

    public Map<String, Object> vendorExtensions() {
        return vendorExtensions;
    }

    public List<SupportingFile> supportingFiles() {
        return supportingFiles;
    }

    public String outputFolder() {
        return outputFolder;
    }

    public void setOutputDir(String dir) {
        this.outputFolder = dir;
    }

    public String getOutputDir() {
        return outputFolder();
    }

    public String getInputSpec() {
        return inputSpec;
    }

    public void setInputSpec(String inputSpec) {
        this.inputSpec = inputSpec;
    }

    public void setTemplateDir(String templateDir) {
        this.templateDir = templateDir;
    }

    public void setModelPackage(String modelPackage) {
        this.modelPackage = modelPackage;
    }

    public String getModelNamePrefix() {
        return modelNamePrefix;
    }

    public void setModelNamePrefix(String modelNamePrefix) {
        this.modelNamePrefix = modelNamePrefix;
    }

    public String getModelNameSuffix() {
        return modelNameSuffix;
    }

    public void setModelNameSuffix(String modelNameSuffix) {
        this.modelNameSuffix = modelNameSuffix;
    }

    public String getApiNameSuffix() {
        return apiNameSuffix;
    }

    public void setApiNameSuffix(String apiNameSuffix) {
        this.apiNameSuffix = apiNameSuffix;
    }

    public String getApiNamePrefix() {
        return apiNamePrefix;
    }

    public void setApiNamePrefix(String apiNamePrefix) {
        this.apiNamePrefix = apiNamePrefix;
    }

    public void setApiPackage(String apiPackage) {
        this.apiPackage = apiPackage;
    }

    public Boolean getSortParamsByRequiredFlag() {
        return sortParamsByRequiredFlag;
    }

    public void setSortParamsByRequiredFlag(Boolean sortParamsByRequiredFlag) {
        this.sortParamsByRequiredFlag = sortParamsByRequiredFlag;
    }

    public Boolean getSortModelPropertiesByRequiredFlag() {
        return sortModelPropertiesByRequiredFlag;
    }

    public void setSortModelPropertiesByRequiredFlag(Boolean sortModelPropertiesByRequiredFlag) {
        this.sortModelPropertiesByRequiredFlag = sortModelPropertiesByRequiredFlag;
    }

    public Boolean getPrependFormOrBodyParameters() {
        return prependFormOrBodyParameters;
    }

    public void setPrependFormOrBodyParameters(Boolean prependFormOrBodyParameters) {
        this.prependFormOrBodyParameters = prependFormOrBodyParameters;
    }

    public Boolean getEnsureUniqueParams() {
        return ensureUniqueParams;
    }

    public void setEnsureUniqueParams(Boolean ensureUniqueParams) {
        this.ensureUniqueParams = ensureUniqueParams;
    }

    public Boolean getLegacyDiscriminatorBehavior() {
        return legacyDiscriminatorBehavior;
    }

    public void setLegacyDiscriminatorBehavior(boolean val) {
        this.legacyDiscriminatorBehavior = val;
    }

    public Boolean getDisallowAdditionalPropertiesIfNotPresent() {
        return disallowAdditionalPropertiesIfNotPresent;
    }

    public void setDisallowAdditionalPropertiesIfNotPresent(boolean val) {
        this.disallowAdditionalPropertiesIfNotPresent = val;
    }

    public Boolean getAllowUnicodeIdentifiers() {
        return allowUnicodeIdentifiers;
    }

    public void setAllowUnicodeIdentifiers(Boolean allowUnicodeIdentifiers) {
        this.allowUnicodeIdentifiers = allowUnicodeIdentifiers;
    }

    public Boolean getUseOneOfInterfaces() {
        return useOneOfInterfaces;
    }

    public void setUseOneOfInterfaces(Boolean useOneOfInterfaces) {
        this.useOneOfInterfaces = useOneOfInterfaces;
    }

    /**
     * Return the regular expression/JSON schema pattern (http://json-schema.org/latest/json-schema-validation.html#anchor33)
     *
     * @param pattern the pattern (regular expression)
     * @return properly-escaped pattern
     */
    public String toRegularExpression(String pattern) {
        return addRegularExpressionDelimiter(escapeText(pattern));
    }

    /**
     * Return the file name of the Api Test
     *
     * @param name the file name of the Api
     * @return the file name of the Api
     */
    public String toApiFilename(String name) {
        return toApiName(name);
    }

    /**
     * Return the file name of the Api Documentation
     *
     * @param name the file name of the Api
     * @return the file name of the Api
     */
    public String toApiDocFilename(String name) {
        return toApiName(name);
    }

    /**
     * Return the file name of the Api Test
     *
     * @param name the file name of the Api
     * @return the file name of the Api
     */
    public String toApiTestFilename(String name) {
        return toApiName(name) + "Test";
    }

    /**
     * Return the variable name in the Api
     *
     * @param name the varible name of the Api
     * @return the snake-cased variable name
     */
    public String toApiVarName(String name) {
        return lowerCamelCase(name);
    }

    /**
     * Return the capitalized file name of the model
     *
     * @param name the model name
     * @return the file name of the model
     */
    public String toModelFilename(String name) {
        return camelize(name);
    }

    /**
     * Return the capitalized file name of the model test
     *
     * @param name the model name
     * @return the file name of the model
     */
    public String toModelTestFilename(String name) {
        return camelize(name) + "Test";
    }

    /**
     * Return the capitalized file name of the model documentation
     *
     * @param name the model name
     * @return the file name of the model
     */
    public String toModelDocFilename(String name) {
        return camelize(name);
    }

    /**
     * Returns metadata about the generator.
     *
     * @return A provided {@link GeneratorMetadata} instance
     */
    @Override
    public GeneratorMetadata getGeneratorMetadata() {
        return generatorMetadata;
    }

    /**
     * Return the operation ID (method name)
     *
     * @param operationId operation ID
     * @return the sanitized method name
     */
    @SuppressWarnings("static-method")
    public String toOperationId(String operationId) {
        // throw exception if method name is empty
        if (StringUtils.isEmpty(operationId)) {
            throw new RuntimeException("Empty method name (operationId) not allowed");
        }

        return operationId;
    }

    /**
     * Return the variable name by removing invalid characters and proper escaping if
     * it's a reserved word.
     *
     * @param name the variable name
     * @return the sanitized variable name
     */
    public String toVarName(String name) {
        if (reservedWords.contains(name)) {
            return escapeReservedWord(name);
        } else if (((CharSequence) name).chars().anyMatch(character -> specialCharReplacements.keySet().contains("" + ((char) character)))) {
            return escape(name, specialCharReplacements, null, null);
        } else {
            return name;
        }
    }

    /**
     * Return the parameter name by removing invalid characters and proper escaping if
     * it's a reserved word.
     *
     * @param name Codegen property object
     * @return the sanitized parameter name
     */
    public String toParamName(String name) {
        name = removeNonNameElementToCamelCase(name); // FIXME: a parameter should not be assigned. Also declare the methods parameters as 'final'.
        if (reservedWords.contains(name)) {
            return escapeReservedWord(name);
        } else if (((CharSequence) name).chars().anyMatch(character -> specialCharReplacements.keySet().contains("" + ((char) character)))) {
            return escape(name, specialCharReplacements, null, null);
        }
        return name;
    }

    /**
     * Return the parameter name of array of model
     *
     * @param name name of the array model
     * @return the sanitized parameter name
     */
    public String toArrayModelParamName(String name) {
        return toParamName(name);
    }

    /**
     * Return the Enum name (e.g. StatusEnum given 'status')
     *
     * @param property Codegen property
     * @return the Enum name
     */
    @SuppressWarnings("static-method")
    public String toEnumName(CodegenProperty property) {
        return StringUtils.capitalize(property.name) + "Enum";
    }

    /**
     * Return the escaped name of the reserved word
     *
     * @param name the name to be escaped
     * @return the escaped reserved word
     * <p>
     * throws Runtime exception as reserved word is not allowed (default behavior)
     */
    @SuppressWarnings("static-method")
    public String escapeReservedWord(String name) {
        throw new RuntimeException("reserved word " + name + " not allowed");
    }

    /**
     * Return the fully-qualified "Model" name for import
     *
     * @param name the name of the "Model"
     * @return the fully-qualified "Model" name for import
     */
    public String toModelImport(String name) {
        if ("".equals(modelPackage())) {
            return name;
        } else {
            return modelPackage() + "." + name;
        }
    }

    /**
     * Return the fully-qualified "Api" name for import
     *
     * @param name the name of the "Api"
     * @return the fully-qualified "Api" name for import
     */
    public String toApiImport(String name) {
        return apiPackage() + "." + name;
    }

    /**
     * Default constructor.
     * This method will map between OAS type and language-specified type, as well as mapping
     * between OAS type and the corresponding import statement for the language. This will
     * also add some language specified CLI options, if any.
     * returns string presentation of the example path (it's a constructor)
     */
    public DefaultCodegen() {
        CodegenType codegenType = getTag();
        if (codegenType == null) {
            codegenType = CodegenType.OTHER;
        }

        generatorMetadata = GeneratorMetadata.newBuilder()
                .stability(Stability.STABLE)
                .featureSet(DefaultFeatureSet)
                .generationMessage(String.format(Locale.ROOT, "OpenAPI Generator: %s (%s)", getName(), codegenType.toValue()))
                .build();

        defaultIncludes = new HashSet<String>(
                Arrays.asList("double",
                        "int",
                        "long",
                        "short",
                        "char",
                        "float",
                        "String",
                        "boolean",
                        "Boolean",
                        "Double",
                        "Void",
                        "Integer",
                        "Long",
                        "Float")
        );

        typeMapping = new HashMap<String, String>();
        typeMapping.put("array", "List");
        typeMapping.put("set", "Set");
        typeMapping.put("map", "Map");
        typeMapping.put("List", "List");
        typeMapping.put("Set", "Set");
        typeMapping.put("boolean", "Boolean");
        typeMapping.put("string", "String");
        typeMapping.put("int", "Integer");
        typeMapping.put("float", "Float");
        typeMapping.put("number", "BigDecimal");
        typeMapping.put("DateTime", "Date");
        typeMapping.put("long", "Long");
        typeMapping.put("short", "Short");
        typeMapping.put("char", "String");
        typeMapping.put("double", "Double");
        typeMapping.put("object", "Object");
        typeMapping.put("integer", "Integer");
        typeMapping.put("ByteArray", "byte[]");
        typeMapping.put("binary", "File");
        typeMapping.put("file", "File");
        typeMapping.put("UUID", "UUID");
        typeMapping.put("URI", "URI");
        typeMapping.put("BigDecimal", "BigDecimal");
        typeMapping.put("AnyType", "oas_any_type_not_mapped");

        instantiationTypes = new HashMap<String, String>();

        reservedWords = new HashSet<String>();

        // TODO: Move Java specific import mappings out of DefaultCodegen.
        importMapping = new HashMap<String, String>();
        importMapping.put("BigDecimal", "java.math.BigDecimal");
        importMapping.put("UUID", "java.util.UUID");
        importMapping.put("URI", "java.net.URI");
        importMapping.put("File", "java.io.File");
        importMapping.put("Date", "java.util.Date");
        importMapping.put("Timestamp", "java.sql.Timestamp");
        importMapping.put("Map", "java.util.Map");
        importMapping.put("HashMap", "java.util.HashMap");
        importMapping.put("Array", "java.util.List");
        importMapping.put("ArrayList", "java.util.ArrayList");
        importMapping.put("List", "java.util.*");
        importMapping.put("Set", "java.util.*");
        importMapping.put("LinkedHashSet", "java.util.LinkedHashSet");
        importMapping.put("DateTime", "org.joda.time.*");
        importMapping.put("LocalDateTime", "org.joda.time.*");
        importMapping.put("LocalDate", "org.joda.time.*");
        importMapping.put("LocalTime", "org.joda.time.*");

        cliOptions.add(CliOption.newBoolean(CodegenConstants.SORT_PARAMS_BY_REQUIRED_FLAG,
                CodegenConstants.SORT_PARAMS_BY_REQUIRED_FLAG_DESC).defaultValue(Boolean.TRUE.toString()));
        cliOptions.add(CliOption.newBoolean(CodegenConstants.SORT_MODEL_PROPERTIES_BY_REQUIRED_FLAG,
                CodegenConstants.SORT_MODEL_PROPERTIES_BY_REQUIRED_FLAG_DESC).defaultValue(Boolean.TRUE.toString()));
        cliOptions.add(CliOption.newBoolean(CodegenConstants.ENSURE_UNIQUE_PARAMS, CodegenConstants
                .ENSURE_UNIQUE_PARAMS_DESC).defaultValue(Boolean.TRUE.toString()));
        // name formatting options
        cliOptions.add(CliOption.newBoolean(CodegenConstants.ALLOW_UNICODE_IDENTIFIERS, CodegenConstants
                .ALLOW_UNICODE_IDENTIFIERS_DESC).defaultValue(Boolean.FALSE.toString()));
        // option to change the order of form/body parameter
        cliOptions.add(CliOption.newBoolean(CodegenConstants.PREPEND_FORM_OR_BODY_PARAMETERS,
                CodegenConstants.PREPEND_FORM_OR_BODY_PARAMETERS_DESC).defaultValue(Boolean.FALSE.toString()));

        // option to change how we process + set the data in the discriminator mapping
        CliOption legacyDiscriminatorBehaviorOpt = CliOption.newBoolean(CodegenConstants.LEGACY_DISCRIMINATOR_BEHAVIOR, CodegenConstants.LEGACY_DISCRIMINATOR_BEHAVIOR_DESC).defaultValue(Boolean.TRUE.toString());
        Map<String, String> legacyDiscriminatorBehaviorOpts = new HashMap<>();
        legacyDiscriminatorBehaviorOpts.put("true", "The mapping in the discriminator includes descendent schemas that allOf inherit from self and the discriminator mapping schemas in the OAS document.");
        legacyDiscriminatorBehaviorOpts.put("false", "The mapping in the discriminator includes any descendent schemas that allOf inherit from self, any oneOf schemas, any anyOf schemas, any x-discriminator-values, and the discriminator mapping schemas in the OAS document AND Codegen validates that oneOf and anyOf schemas contain the required discriminator and throws an error if the discriminator is missing.");
        legacyDiscriminatorBehaviorOpt.setEnum(legacyDiscriminatorBehaviorOpts);
        cliOptions.add(legacyDiscriminatorBehaviorOpt);

        // option to change how we process + set the data in the 'additionalProperties' keyword.
        CliOption disallowAdditionalPropertiesIfNotPresentOpt = CliOption.newBoolean(
            CodegenConstants.DISALLOW_ADDITIONAL_PROPERTIES_IF_NOT_PRESENT,
            CodegenConstants.DISALLOW_ADDITIONAL_PROPERTIES_IF_NOT_PRESENT_DESC).defaultValue(Boolean.TRUE.toString());
        Map<String, String> disallowAdditionalPropertiesIfNotPresentOpts = new HashMap<>();
        disallowAdditionalPropertiesIfNotPresentOpts.put("false",
            "The 'additionalProperties' implementation is compliant with the OAS and JSON schema specifications.");
        disallowAdditionalPropertiesIfNotPresentOpts.put("true",
            "when the 'additionalProperties' keyword is not present in a schema, " +
            "the value of 'additionalProperties' is automatically set to false, i.e. no additional properties are allowed. " +
            "Note: this mode is not compliant with the JSON schema specification. " +
            "This is the original openapi-generator behavior.");
        disallowAdditionalPropertiesIfNotPresentOpt.setEnum(disallowAdditionalPropertiesIfNotPresentOpts);
        cliOptions.add(disallowAdditionalPropertiesIfNotPresentOpt);
        this.setDisallowAdditionalPropertiesIfNotPresent(true);
        
        // initialize special character mapping
        initalizeSpecialCharacterMapping();

        // Register common Mustache lambdas.
        registerMustacheLambdas();
    }

    /**
     * Initialize special character mapping
     */
    protected void initalizeSpecialCharacterMapping() {
        // Initialize special characters
        specialCharReplacements.put("$", "Dollar");
        specialCharReplacements.put("^", "Caret");
        specialCharReplacements.put("|", "Pipe");
        specialCharReplacements.put("=", "Equal");
        specialCharReplacements.put("*", "Star");
        specialCharReplacements.put("-", "Minus");
        specialCharReplacements.put("&", "Ampersand");
        specialCharReplacements.put("%", "Percent");
        specialCharReplacements.put("#", "Hash");
        specialCharReplacements.put("@", "At");
        specialCharReplacements.put("!", "Exclamation");
        specialCharReplacements.put("+", "Plus");
        specialCharReplacements.put(":", "Colon");
        specialCharReplacements.put(">", "Greater_Than");
        specialCharReplacements.put("<", "Less_Than");
        specialCharReplacements.put(".", "Period");
        specialCharReplacements.put("_", "Underscore");
        specialCharReplacements.put("?", "Question_Mark");
        specialCharReplacements.put(",", "Comma");
        specialCharReplacements.put("'", "Quote");
        specialCharReplacements.put("\"", "Double_Quote");
        specialCharReplacements.put("/", "Slash");
        specialCharReplacements.put("\\", "Back_Slash");
        specialCharReplacements.put("(", "Left_Parenthesis");
        specialCharReplacements.put(")", "Right_Parenthesis");
        specialCharReplacements.put("{", "Left_Curly_Bracket");
        specialCharReplacements.put("}", "Right_Curly_Bracket");
        specialCharReplacements.put("[", "Left_Square_Bracket");
        specialCharReplacements.put("]", "Right_Square_Bracket");
        specialCharReplacements.put("~", "Tilde");
        specialCharReplacements.put("`", "Backtick");

        specialCharReplacements.put("<=", "Less_Than_Or_Equal_To");
        specialCharReplacements.put(">=", "Greater_Than_Or_Equal_To");
        specialCharReplacements.put("!=", "Not_Equal");
    }

    /**
     * Return the symbol name of a symbol
     *
     * @param input Symbol (e.g. $)
     * @return Symbol name (e.g. Dollar)
     */
    protected String getSymbolName(String input) {
        return specialCharReplacements.get(input);
    }

    /**
     * Return the example path
     *
     * @param path      the path of the operation
     * @param operation OAS operation object
     * @return string presentation of the example path
     */
    @SuppressWarnings("static-method")
    public String generateExamplePath(String path, Operation operation) {
        StringBuilder sb = new StringBuilder();
        sb.append(path);

        if (operation.getParameters() != null) {
            int count = 0;

            for (Parameter param : operation.getParameters()) {
                if (param instanceof QueryParameter) {
                    StringBuilder paramPart = new StringBuilder();
                    QueryParameter qp = (QueryParameter) param;

                    if (count == 0) {
                        paramPart.append("?");
                    } else {
                        paramPart.append(",");
                    }
                    count += 1;
                    if (!param.getRequired()) {
                        paramPart.append("[");
                    }
                    paramPart.append(param.getName()).append("=");
                    paramPart.append("{");

                    // TODO support for multi, tsv?
                    if (qp.getStyle() != null) {
                        paramPart.append(param.getName()).append("1");
                        if (Parameter.StyleEnum.FORM.equals(qp.getStyle())) {
                            if (qp.getExplode() != null && qp.getExplode()) {
                                paramPart.append(",");
                            } else {
                                paramPart.append("&").append(param.getName()).append("=");
                                paramPart.append(param.getName()).append("2");
                            }
                        } else if (Parameter.StyleEnum.PIPEDELIMITED.equals(qp.getStyle())) {
                            paramPart.append("|");
                        } else if (Parameter.StyleEnum.SPACEDELIMITED.equals(qp.getStyle())) {
                            paramPart.append("%20");
                        } else {
                            LOGGER.warn("query parameter '" + param.getName() + "style not support: " + qp.getStyle());
                        }
                    } else {
                        paramPart.append(param.getName());
                    }

                    paramPart.append("}");
                    if (!param.getRequired()) {
                        paramPart.append("]");
                    }
                    sb.append(paramPart.toString());
                }
            }
        }

        return sb.toString();
    }

    /**
     * Return the instantiation type of the property, especially for map and array
     *
     * @param schema property schema
     * @return string presentation of the instantiation type of the property
     */
    public String toInstantiationType(Schema schema) {
        if (ModelUtils.isMapSchema(schema)) {
            Schema additionalProperties = getAdditionalProperties(schema);
            String inner = getSchemaType(additionalProperties);
            return instantiationTypes.get("map") + "<String, " + inner + ">";
        } else if (ModelUtils.isArraySchema(schema)) {
            ArraySchema arraySchema = (ArraySchema) schema;
            String inner = getSchemaType(getSchemaItems(arraySchema));
            String parentType;
            if (ModelUtils.isSet(schema)) {
                parentType = "set";
            } else {
                parentType = "array";
            }
            return instantiationTypes.get(parentType) + "<" + inner + ">";
        } else {
            return null;
        }
    }

    /**
     * Return the example value of the parameter.
     *
     * @param codegenParameter Codegen parameter
     */
    public void setParameterExampleValue(CodegenParameter codegenParameter) {

        // set the example value
        // if not specified in x-example, generate a default value
        // TODO need to revise how to obtain the example value
        if (codegenParameter.vendorExtensions != null && codegenParameter.vendorExtensions.containsKey("x-example")) {
            codegenParameter.example = Json.pretty(codegenParameter.vendorExtensions.get("x-example"));
        } else if (Boolean.TRUE.equals(codegenParameter.isBoolean)) {
            codegenParameter.example = "true";
        } else if (Boolean.TRUE.equals(codegenParameter.isLong)) {
            codegenParameter.example = "789";
        } else if (Boolean.TRUE.equals(codegenParameter.isInteger)) {
            codegenParameter.example = "56";
        } else if (Boolean.TRUE.equals(codegenParameter.isFloat)) {
            codegenParameter.example = "3.4";
        } else if (Boolean.TRUE.equals(codegenParameter.isDouble)) {
            codegenParameter.example = "1.2";
        } else if (Boolean.TRUE.equals(codegenParameter.isNumber)) {
            codegenParameter.example = "8.14";
        } else if (Boolean.TRUE.equals(codegenParameter.isBinary)) {
            codegenParameter.example = "BINARY_DATA_HERE";
        } else if (Boolean.TRUE.equals(codegenParameter.isByteArray)) {
            codegenParameter.example = "BYTE_ARRAY_DATA_HERE";
        } else if (Boolean.TRUE.equals(codegenParameter.isFile)) {
            codegenParameter.example = "/path/to/file.txt";
        } else if (Boolean.TRUE.equals(codegenParameter.isDate)) {
            codegenParameter.example = "2013-10-20";
        } else if (Boolean.TRUE.equals(codegenParameter.isDateTime)) {
            codegenParameter.example = "2013-10-20T19:20:30+01:00";
        } else if (Boolean.TRUE.equals(codegenParameter.isUuid)) {
            codegenParameter.example = "38400000-8cf0-11bd-b23e-10b96e4ef00d";
        } else if (Boolean.TRUE.equals(codegenParameter.isUri)) {
            codegenParameter.example = "https://openapi-generator.tech";
        } else if (Boolean.TRUE.equals(codegenParameter.isString)) {
            codegenParameter.example = codegenParameter.paramName + "_example";
        } else if (Boolean.TRUE.equals(codegenParameter.isFreeFormObject)) {
            codegenParameter.example = "Object";
        }

    }

    /**
     * Return the example value of the parameter.
     *
     * @param codegenParameter Codegen parameter
     * @param parameter        Parameter
     */
    public void setParameterExampleValue(CodegenParameter codegenParameter, Parameter parameter) {
        if (parameter.getExample() != null) {
            codegenParameter.example = parameter.getExample().toString();
            return;
        }

        if (parameter.getExamples() != null && !parameter.getExamples().isEmpty()) {
            Example example = parameter.getExamples().values().iterator().next();
            if (example.getValue() != null) {
                codegenParameter.example = example.getValue().toString();
                return;
            }
        }

        Schema schema = parameter.getSchema();
        if (schema != null && schema.getExample() != null) {
            codegenParameter.example = schema.getExample().toString();
            return;
        }

        setParameterExampleValue(codegenParameter);
    }

    /**
     * Return the example value of the parameter.
     *
     * @param codegenParameter Codegen parameter
     * @param requestBody      Request body
     */
    public void setParameterExampleValue(CodegenParameter codegenParameter, RequestBody requestBody) {
        Content content = requestBody.getContent();

        if (content.size() > 1) {
            // @see ModelUtils.getSchemaFromContent()
            once(LOGGER).warn("Multiple MediaTypes found, using only the first one");
        }

        MediaType mediaType = content.values().iterator().next();
        if (mediaType.getExample() != null) {
            codegenParameter.example = mediaType.getExample().toString();
            return;
        }

        if (mediaType.getExamples() != null && !mediaType.getExamples().isEmpty()) {
            Example example = mediaType.getExamples().values().iterator().next();
            if (example.getValue() != null) {
                codegenParameter.example = example.getValue().toString();
                return;
            }
        }

        setParameterExampleValue(codegenParameter);
    }

    /**
     * Sets the content type of the parameter based on the encoding specified in the request body.
     *
     * @param codegenParameter Codegen parameter
     * @param mediaType        MediaType from the request body
     */
    public void setParameterContentType(CodegenParameter codegenParameter, MediaType mediaType) {
        if (mediaType != null && mediaType.getEncoding() != null) {
            Encoding encoding = mediaType.getEncoding().get(codegenParameter.baseName);
            if (encoding != null) {
                codegenParameter.contentType = encoding.getContentType();
            } else {
                LOGGER.debug("encoding not specified for " + codegenParameter.baseName);
            }
        }
    }

    /**
     * Return the example value of the property
     *
     * @param schema Property schema
     * @return string presentation of the example value of the property
     */
    public String toExampleValue(Schema schema) {
        if (schema.getExample() != null) {
            return schema.getExample().toString();
        }

        return getPropertyDefaultValue(schema);
    }

    /**
     * Return the default value of the property
     *
     * @param schema Property schema
     * @return string presentation of the default value of the property
     */
    @SuppressWarnings("static-method")
    public String toDefaultValue(Schema schema) {
        if (schema.getDefault() != null) {
            return schema.getDefault().toString();
        }

        return getPropertyDefaultValue(schema);
    }

    /**
     * Return property value depending on property type.
     *
     * @param schema property type
     * @return property value
     */
    @SuppressWarnings("squid:S3923")
    private String getPropertyDefaultValue(Schema schema) {
        /**
         * Although all branches return null, this is left intentionally as examples for new contributors
         */
        if (ModelUtils.isBooleanSchema(schema)) {
            return "null";
        } else if (ModelUtils.isDateSchema(schema)) {
            return "null";
        } else if (ModelUtils.isDateTimeSchema(schema)) {
            return "null";
        } else if (ModelUtils.isNumberSchema(schema)) {
            return "null";
        } else if (ModelUtils.isIntegerSchema(schema)) {
            return "null";
        } else if (ModelUtils.isStringSchema(schema)) {
            return "null";
        } else if (ModelUtils.isObjectSchema(schema)) {
            return "null";
        } else {
            return "null";
        }
    }

    /**
     * Return the property initialized from a data object
     * Useful for initialization with a plain object in Javascript
     *
     * @param name   Name of the property object
     * @param schema Property schema
     * @return string presentation of the default value of the property
     */
    @SuppressWarnings("static-method")
    public String toDefaultValueWithParam(String name, Schema schema) {
        return " = data." + name + ";";
    }

    /**
     * returns the OpenAPI type for the property. Use getAlias to handle $ref of primitive type
     *
     * @param schema property schema
     * @return string presentation of the type
     **/
    @SuppressWarnings("static-method")
    public String getSchemaType(Schema schema) {
        if (schema instanceof ComposedSchema) { // composed schema
            ComposedSchema cs = (ComposedSchema) schema;
            // Get the interfaces, i.e. the set of elements under 'allOf', 'anyOf' or 'oneOf'.
            List<Schema> schemas = ModelUtils.getInterfaces(cs);

            List<String> names = new ArrayList<>();
            // Build a list of the schema types under each interface.
            // For example, if a 'allOf' composed schema has $ref children,
            // add the type of each child to the list of names.
            for (Schema s : schemas) {
                names.add(getSingleSchemaType(s));
            }

            if (cs.getAllOf() != null) {
                return toAllOfName(names, cs);
            } else if (cs.getAnyOf() != null) { // anyOf
                return toAnyOfName(names, cs);
            } else if (cs.getOneOf() != null) { // oneOf
                return toOneOfName(names, cs);
            }
        }

        return getSingleSchemaType(schema);

    }

    protected Schema<?> getSchemaItems(ArraySchema schema) {
        Schema<?> items = schema.getItems();
        if (items == null) {
            LOGGER.error("Undefined array inner type for `{}`. Default to String.", schema.getName());
            items = new StringSchema().description("TODO default missing array inner type to string");
            schema.setItems(items);
        }
        return items;
    }

    protected Schema<?> getSchemaAdditionalProperties(Schema schema) {
        Schema<?> inner = getAdditionalProperties(schema);
        if (inner == null) {
            LOGGER.error("`{}` (map property) does not have a proper inner type defined. Default to type:string", schema.getName());
            inner = new StringSchema().description("TODO default missing map inner type to string");
            schema.setAdditionalProperties(inner);
        }
        return inner;
    }

    /**
     * Return the name of the 'allOf' composed schema.
     *
     * @param names          List of names
     * @param composedSchema composed schema
     * @return name of the allOf schema
     */
    @SuppressWarnings("static-method")
    public String toAllOfName(List<String> names, ComposedSchema composedSchema) {
        Map<String, Object> exts = composedSchema.getExtensions();
        if (exts != null && exts.containsKey("x-all-of-name")) {
            return (String) exts.get("x-all-of-name");
        }
        if (names.size() == 0) {
            LOGGER.error("allOf has no member defined: {}. Default to ERROR_ALLOF_SCHEMA", composedSchema);
            return "ERROR_ALLOF_SCHEMA";
        } else if (names.size() == 1) {
            return names.get(0);
        } else {
            LOGGER.warn("allOf with multiple schemas defined. Using only the first one: {}", names.get(0));
            return names.get(0);
        }
    }

    /**
     * Return the name of the anyOf schema
     *
     * @param names          List of names
     * @param composedSchema composed schema
     * @return name of the anyOf schema
     */
    @SuppressWarnings("static-method")
    public String toAnyOfName(List<String> names, ComposedSchema composedSchema) {
        return "anyOf<" + String.join(",", names) + ">";
    }

    /**
     * Return the name of the oneOf schema.
     * <p>
     * This name is used to set the value of CodegenProperty.openApiType.
     * <p>
     * If the 'x-one-of-name' extension is specified in the OAS document, return that value.
     * Otherwise, a name is constructed by creating a comma-separated list of all the names
     * of the oneOf schemas.
     *
     * @param names          List of names
     * @param composedSchema composed schema
     * @return name of the oneOf schema
     */
    @SuppressWarnings("static-method")
    public String toOneOfName(List<String> names, ComposedSchema composedSchema) {
        Map<String, Object> exts = composedSchema.getExtensions();
        if (exts != null && exts.containsKey("x-one-of-name")) {
            return (String) exts.get("x-one-of-name");
        }
        return "oneOf<" + String.join(",", names) + ">";
    }

    /**
     * Return a string representation of the schema type, resolving aliasing and references if necessary.
     *
     * @param schema
     * @return the string representation of the schema type.
     */
    private String getSingleSchemaType(Schema schema) {
        Schema unaliasSchema = ModelUtils.unaliasSchema(this.openAPI, schema, importMapping);

        if (StringUtils.isNotBlank(unaliasSchema.get$ref())) { // reference to another definition/schema
            // get the schema/model name from $ref
            String schemaName = ModelUtils.getSimpleRef(unaliasSchema.get$ref());
            if (StringUtils.isNotEmpty(schemaName)) {
                if (importMapping.containsKey(schemaName)) {
                    return schemaName;
                }
                return getAlias(schemaName);
            } else {
                LOGGER.warn("Error obtaining the datatype from ref:" + unaliasSchema.get$ref() + ". Default to 'object'");
                return "object";
            }
        } else { // primitive type or model
            return getAlias(getPrimitiveType(unaliasSchema));
        }
    }

    /**
     * Return the OAI type (e.g. integer, long, etc) corresponding to a schema.
     * <pre>$ref</pre> is not taken into account by this method.
     * <p>
     * If the schema is free-form (i.e. 'type: object' with no properties) or inline
     * schema, the returned OAI type is 'object'.
     *
     * @param schema
     * @return type
     */
    private String getPrimitiveType(Schema schema) {
        if (schema == null) {
            throw new RuntimeException("schema cannot be null in getPrimitiveType");
        } else if (ModelUtils.isNullType(schema)) {
            // The 'null' type is allowed in OAS 3.1 and above. It is not supported by OAS 3.0.x,
            // though this tooling supports it.
            return "null";
        } else if (ModelUtils.isStringSchema(schema) && "number".equals(schema.getFormat())) {
            // special handle of type: string, format: number
            return "BigDecimal";
        } else if (ModelUtils.isByteArraySchema(schema)) {
            return "ByteArray";
        } else if (ModelUtils.isFileSchema(schema)) {
            return "file";
        } else if (ModelUtils.isBinarySchema(schema)) {
            return SchemaTypeUtil.BINARY_FORMAT;
        } else if (ModelUtils.isBooleanSchema(schema)) {
            return SchemaTypeUtil.BOOLEAN_TYPE;
        } else if (ModelUtils.isDateSchema(schema)) {
            return SchemaTypeUtil.DATE_FORMAT;
        } else if (ModelUtils.isDateTimeSchema(schema)) {
            return "DateTime";
        } else if (ModelUtils.isNumberSchema(schema)) {
            if (schema.getFormat() == null) { // no format defined
                return "number";
            } else if (ModelUtils.isFloatSchema(schema)) {
                return SchemaTypeUtil.FLOAT_FORMAT;
            } else if (ModelUtils.isDoubleSchema(schema)) {
                return SchemaTypeUtil.DOUBLE_FORMAT;
            } else {
                LOGGER.warn("Unknown `format` {} detected for type `number`. Defaulting to `number`", schema.getFormat());
                return "number";
            }
        } else if (ModelUtils.isIntegerSchema(schema)) {
            if (ModelUtils.isLongSchema(schema)) {
                return "long";
            } else {
                return schema.getType(); // integer
            }
        } else if (ModelUtils.isMapSchema(schema)) {
            return "map";
        } else if (ModelUtils.isArraySchema(schema)) {
            if (ModelUtils.isSet(schema)) {
                return "set";
            } else {
                return "array";
            }
        } else if (ModelUtils.isUUIDSchema(schema)) {
            return "UUID";
        } else if (ModelUtils.isURISchema(schema)) {
            return "URI";
        } else if (ModelUtils.isStringSchema(schema)) {
            if (typeMapping.containsKey(schema.getFormat())) {
                // If the format matches a typeMapping (supplied with the --typeMappings flag)
                // then treat the format as a primitive type.
                // This allows the typeMapping flag to add a new custom type which can then
                // be used in the format field.
                return schema.getFormat();
            }
            return "string";
        } else if (isFreeFormObject(schema)) {
            // Note: the value of a free-form object cannot be an arbitrary type. Per OAS specification,
            // it must be a map of string to values.
            return "object";
        } else if (schema.getProperties() != null && !schema.getProperties().isEmpty()) { // having property implies it's a model
            return "object";
        } else if (isAnyTypeSchema(schema)) {
            return "AnyType";
        } else if (StringUtils.isNotEmpty(schema.getType())) {
            LOGGER.warn("Unknown type found in the schema: " + schema.getType());
            return schema.getType();
        }
        // The 'type' attribute has not been set in the OAS schema, which means the value
        // can be an arbitrary type, e.g. integer, string, object, array, number...
        // TODO: we should return a different value to distinguish between free-form object
        // and arbitrary type.
        return "object";
    }

    /**
     * Return the lowerCamelCase of the string
     *
     * @param name string to be lowerCamelCased
     * @return lowerCamelCase string
     */
    @SuppressWarnings("static-method")
    public String lowerCamelCase(String name) {
        return (name.length() > 0) ? (Character.toLowerCase(name.charAt(0)) + name.substring(1)) : "";
    }

    /**
     * Output the language-specific type declaration of a given OAS name.
     *
     * @param name name
     * @return a string presentation of the type
     */
    @SuppressWarnings("static-method")
    public String getTypeDeclaration(String name) {
        return name;
    }

    /**
     * Output the language-specific type declaration of the property.
     *
     * @param schema property schema
     * @return a string presentation of the property type
     */
    public String getTypeDeclaration(Schema schema) {
        if (schema == null) {
            LOGGER.warn("Null schema found. Default type to `NULL_SCHEMA_ERR`");
            return "NULL_SCHEMA_ERR";
        }

        String oasType = getSchemaType(schema);
        if (typeMapping.containsKey(oasType)) {
            return typeMapping.get(oasType);
        }

        return oasType;
    }

    /**
     * Determine the type alias for the given type if it exists. This feature
     * was originally developed for Java because the language does not have an aliasing
     * mechanism of its own but later extends to handle other languages
     *
     * @param name The type name.
     * @return The alias of the given type, if it exists. If there is no alias
     * for this type, then returns the input type name.
     */
    public String getAlias(String name) {
        if (typeAliases != null && typeAliases.containsKey(name)) {
            return typeAliases.get(name);
        }
        return name;
    }

    /**
     * Output the Getter name for boolean property, e.g. getActive
     *
     * @param name the name of the property
     * @return getter name based on naming convention
     */
    public String toBooleanGetter(String name) {
        return "get" + getterAndSetterCapitalize(name);
    }

    /**
     * Output the Getter name, e.g. getSize
     *
     * @param name the name of the property
     * @return getter name based on naming convention
     */
    public String toGetter(String name) {
        return "get" + getterAndSetterCapitalize(name);
    }

    /**
     * Output the Setter name, e.g. setSize
     *
     * @param name the name of the property
     * @return setter name based on naming convention
     */
    public String toSetter(String name) {
        return "set" + getterAndSetterCapitalize(name);
    }

    /**
     * Output the API (class) name (capitalized) ending with the specified or default suffix
     * Return DefaultApi if name is empty
     *
     * @param name the name of the Api
     * @return capitalized Api name
     */
    public String toApiName(String name) {
        if (name.length() == 0) {
            return "DefaultApi";
        }
        return camelize(apiNamePrefix + "_" + name + "_" + apiNameSuffix);
    }

    /**
     * Output the proper model name (capitalized).
     * In case the name belongs to the TypeSystem it won't be renamed.
     *
     * @param name the name of the model
     * @return capitalized model name
     */
    public String toModelName(final String name) {
        return camelize(modelNamePrefix + "_" + name + "_" + modelNameSuffix);
    }

    /**
     * Convert OAS Model object to Codegen Model object
     *
     * @param name   the name of the model
     * @param schema OAS Model object
     * @return Codegen Model object
     */
    public CodegenModel fromModel(String name, Schema schema) {
        Map<String, Schema> allDefinitions = ModelUtils.getSchemas(this.openAPI);
        if (typeAliases == null) {
            // Only do this once during first call
            typeAliases = getAllAliases(allDefinitions);
        }

        // unalias schema
        schema = ModelUtils.unaliasSchema(this.openAPI, schema, importMapping);
        if (schema == null) {
            LOGGER.warn("Schema {} not found", name);
            return null;
        }

        CodegenModel m = CodegenModelFactory.newInstance(CodegenModelType.MODEL);

        if (reservedWords.contains(name)) {
            m.name = escapeReservedWord(name);
        } else {
            m.name = name;
        }
        m.title = escapeText(schema.getTitle());
        m.description = escapeText(schema.getDescription());
        m.unescapedDescription = schema.getDescription();
        m.classname = toModelName(name);
        m.classVarName = toVarName(name);
        m.classFilename = toModelFilename(name);
        m.modelJson = Json.pretty(schema);
        m.externalDocumentation = schema.getExternalDocs();
        if (schema.getExtensions() != null && !schema.getExtensions().isEmpty()) {
            m.getVendorExtensions().putAll(schema.getExtensions());
        }
        m.isAlias = (typeAliases.containsKey(name)
                || isAliasOfSimpleTypes(schema)); // check if the unaliased schema is an alias of simple OAS types
        m.discriminator = createDiscriminator(name, schema, this.openAPI);
        if (!this.getLegacyDiscriminatorBehavior()) {
            m.addDiscriminatorMappedModelsImports();
        }

        if (schema.getDeprecated() != null) {
            m.isDeprecated = schema.getDeprecated();
        }

        if (schema.getXml() != null) {
            m.xmlPrefix = schema.getXml().getPrefix();
            m.xmlNamespace = schema.getXml().getNamespace();
            m.xmlName = schema.getXml().getName();
        }
        if (isAnyTypeSchema(schema)) {
            // The 'null' value is allowed when the OAS schema is 'any type'.
            // See https://github.com/OAI/OpenAPI-Specification/issues/1389
            if (Boolean.FALSE.equals(schema.getNullable())) {
                LOGGER.error("Schema '{}' is any type, which includes the 'null' value. 'nullable' cannot be set to 'false'", name);
            }
            m.isNullable = true;
        }
        if (ModelUtils.isArraySchema(schema)) {
            m.isArrayModel = true;
            m.arrayModelType = fromProperty(name, schema).complexType;
            addParentContainer(m, name, schema);
        } else if (schema instanceof ComposedSchema) {
            final ComposedSchema composed = (ComposedSchema) schema;
            Map<String, Schema> properties = new LinkedHashMap<String, Schema>();
            List<String> required = new ArrayList<String>();
            Map<String, Schema> allProperties = new LinkedHashMap<String, Schema>();
            List<String> allRequired = new ArrayList<String>();

            // if schema has properties outside of allOf/oneOf/anyOf also add them to m
            if (composed.getProperties() != null && !composed.getProperties().isEmpty()) {
                if (composed.getOneOf() != null && !composed.getOneOf().isEmpty()) {
                    LOGGER.warn("'oneOf' is intended to include only the additional optional OAS extension discriminator object. " +
                            "For more details, see https://json-schema.org/draft/2019-09/json-schema-core.html#rfc.section.9.2.1.3 and the OAS section on 'Composition and Inheritance'.");
                }
                addVars(m, unaliasPropertySchema(composed.getProperties()), composed.getRequired(), null, null);
            }

            // parent model
            final String parentName = ModelUtils.getParentName(composed, allDefinitions);
            final List<String> allParents = ModelUtils.getAllParentsName(composed, allDefinitions, false);
            final Schema parent = StringUtils.isBlank(parentName) || allDefinitions == null ? null : allDefinitions.get(parentName);

            // TODO revise the logic below to set dicriminator, xml attributes
            if (supportsInheritance || supportsMixins) {
                m.allVars = new ArrayList<CodegenProperty>();
                if (composed.getAllOf() != null) {
                    int modelImplCnt = 0; // only one inline object allowed in a ComposedModel
                    int modelDiscriminators = 0; // only one discriminator allowed in a ComposedModel
                    for (Schema innerSchema : composed.getAllOf()) { // TODO need to work with anyOf, oneOf as well
                        if (m.discriminator == null && innerSchema.getDiscriminator() != null) {
                            LOGGER.debug("discriminator is set to null (not correctly set earlier): {}", name);
                            m.discriminator = createDiscriminator(name, innerSchema, this.openAPI);
                            if (!this.getLegacyDiscriminatorBehavior()) {
                                m.addDiscriminatorMappedModelsImports();
                            }
                            modelDiscriminators++;
                        }

                        if (innerSchema.getXml() != null) {
                            m.xmlPrefix = innerSchema.getXml().getPrefix();
                            m.xmlNamespace = innerSchema.getXml().getNamespace();
                            m.xmlName = innerSchema.getXml().getName();
                        }
                        if (modelDiscriminators > 1) {
                            LOGGER.error("Allof composed schema is inheriting >1 discriminator. Only use one discriminator: {}", composed);
                        }

                        if (modelImplCnt++ > 1) {
                            LOGGER.warn("More than one inline schema specified in allOf:. Only the first one is recognized. All others are ignored.");
                            break; // only one schema with discriminator allowed in allOf
                        }
                    }
                }
            }

            // interfaces (schemas defined in allOf, anyOf, oneOf)
            List<Schema> interfaces = ModelUtils.getInterfaces(composed);
            if (!interfaces.isEmpty()) {
                // m.interfaces is for backward compatibility
                if (m.interfaces == null)
                    m.interfaces = new ArrayList<String>();

                for (Schema interfaceSchema : interfaces) {
                    interfaceSchema = ModelUtils.unaliasSchema(this.openAPI, interfaceSchema, importMapping);

                    if (StringUtils.isBlank(interfaceSchema.get$ref())) {
                        // primitive type
                        String languageType = getTypeDeclaration(interfaceSchema);

                        if (composed.getAnyOf() != null) {
                            if (m.anyOf.contains(languageType)) {
                                LOGGER.warn("{} (anyOf schema) already has `{}` defined and therefore it's skipped.", m.name, languageType);
                            } else {
                                m.anyOf.add(languageType);
                            }
                        } else if (composed.getOneOf() != null) {
                            if (m.oneOf.contains(languageType)) {
                                LOGGER.warn("{} (oneOf schema) already has `{}` defined and therefore it's skipped.", m.name, languageType);
                            } else {
                                m.oneOf.add(languageType);
                            }
                        } else if (composed.getAllOf() != null) {
                            // no need to add primitive type to allOf, which should comprise of schemas (models) only
                        } else {
                            LOGGER.error("Composed schema has incorrect anyOf, allOf, oneOf defined: {}", composed);
                        }
                        continue;
                    }

                    // the rest of the section is for model
                    Schema refSchema = null;
                    String ref = ModelUtils.getSimpleRef(interfaceSchema.get$ref());
                    if (allDefinitions != null) {
                        refSchema = allDefinitions.get(ref);
                    }
                    final String modelName = toModelName(ref);
                    m.interfaces.add(modelName);
                    addImport(m, modelName);
                    if (allDefinitions != null && refSchema != null) {
                        if (allParents.contains(ref) && supportsMultipleInheritance) {
                            // multiple inheritance
                            addProperties(allProperties, allRequired, refSchema);
                        } else if (parentName != null && parentName.equals(ref) && supportsInheritance) {
                            // single inheritance
                            addProperties(allProperties, allRequired, refSchema);
                        } else {
                            // composition
                            addProperties(properties, required, refSchema);
                            addProperties(allProperties, allRequired, refSchema);
                        }
                    }

                    if (composed.getAnyOf() != null) {
                        m.anyOf.add(modelName);
                    } else if (composed.getOneOf() != null) {
                        m.oneOf.add(modelName);
                    } else if (composed.getAllOf() != null) {
                        m.allOf.add(modelName);
                    } else {
                        LOGGER.error("Composed schema has incorrect anyOf, allOf, oneOf defined: {}", composed);
                    }
                }
            }

            if (parent != null && composed.getAllOf() != null) { // set parent for allOf only
                m.parentSchema = parentName;
                m.parent = toModelName(parentName);

                if (supportsMultipleInheritance) {
                    m.allParents = new ArrayList<String>();
                    for (String pname : allParents) {
                        String pModelName = toModelName(pname);
                        m.allParents.add(pModelName);
                        addImport(m, pModelName);
                    }
                } else { // single inheritance
                    addImport(m, m.parent);
                }
            }

            // child schema (properties owned by the schema itself)
            for (Schema component : interfaces) {
                if (component.get$ref() == null) {
                    if (component != null) {
                        // component is the child schema
                        addProperties(properties, required, component);

                        // includes child's properties (all, required) in allProperties, allRequired
                        addProperties(allProperties, allRequired, component);
                    }
                    break; // at most one child only
                }
            }

            if (composed.getRequired() != null) {
                required.addAll(composed.getRequired());
                allRequired.addAll(composed.getRequired());
            }

            addVars(m, unaliasPropertySchema(properties), required, unaliasPropertySchema(allProperties), allRequired);

            // Per OAS specification, composed schemas may use the 'additionalProperties' keyword.
            if (supportsAdditionalPropertiesWithComposedSchema) {
                // Process the schema specified with the 'additionalProperties' keyword.
                // This will set the 'CodegenModel.additionalPropertiesType' field
                // and potentially 'Codegen.parent'.
                //
                // Note: it's not a good idea to use single class inheritance to implement
                // the 'additionalProperties' keyword. Code generators that use single class
                // inheritance sometimes use the 'Codegen.parent' field to implement the
                // 'additionalProperties' keyword. However, that would be in conflict with
                // 'allOf' composed schemas, because these code generators also want to set
                // 'Codegen.parent' to the first child schema of the 'allOf' schema.
                addAdditionPropertiesToCodeGenModel(m, schema);
            }

            if (Boolean.TRUE.equals(schema.getNullable())) {
                m.isNullable = Boolean.TRUE;
            }
            // end of code block for composed schema
        } else {
            m.dataType = getSchemaType(schema);
            if (schema.getEnum() != null && !schema.getEnum().isEmpty()) {
                m.isEnum = true;
                // comment out below as allowableValues is not set in post processing model enum
                m.allowableValues = new HashMap<String, Object>();
                m.allowableValues.put("values", schema.getEnum());
            }
            if (ModelUtils.isMapSchema(schema)) {
                addAdditionPropertiesToCodeGenModel(m, schema);
                m.isMapModel = true;
            } else if (ModelUtils.isIntegerSchema(schema)) { // integer type
                // NOTE: Integral schemas as CodegenModel is a rare use case and may be removed at a later date.
                // Sync of properties is done for consistency with other data types like CodegenParameter/CodegenProperty.
                ModelUtils.syncValidationProperties(schema, m);

                m.isNumeric = Boolean.TRUE;
                if (ModelUtils.isLongSchema(schema)) { // int64/long format
                    m.isLong = Boolean.TRUE;
                } else { // int32 format
                    m.isInteger = Boolean.TRUE;
                }
            } else if (ModelUtils.isStringSchema(schema)) {
                // NOTE: String schemas as CodegenModel is a rare use case and may be removed at a later date.
                // Sync of properties is done for consistency with other data types like CodegenParameter/CodegenProperty.
                ModelUtils.syncValidationProperties(schema, m);
                m.isString = Boolean.TRUE;
            } else if (ModelUtils.isNumberSchema(schema)) {
                // NOTE: Number schemas as CodegenModel is a rare use case and may be removed at a later date.
                // Sync of properties is done for consistency with other data types like CodegenParameter/CodegenProperty.
                ModelUtils.syncValidationProperties(schema, m);
                m.isNumeric = Boolean.TRUE;
                if (ModelUtils.isFloatSchema(schema)) { // float
                    m.isFloat = Boolean.TRUE;
                } else if (ModelUtils.isDoubleSchema(schema)) { // double
                    m.isDouble = Boolean.TRUE;
                } else { // type is number and without format
                    m.isNumber = Boolean.TRUE;
                }
            }

            if (Boolean.TRUE.equals(schema.getNullable())) {
                m.isNullable = Boolean.TRUE;
            }

            // passing null to allProperties and allRequired as there's no parent
            addVars(m, unaliasPropertySchema(schema.getProperties()), schema.getRequired(), null, null);
        }

        // remove duplicated properties
        m.removeAllDuplicatedProperty();

        // post process model properties
        if (m.vars != null) {
            for (CodegenProperty prop : m.vars) {
                postProcessModelProperty(m, prop);
            }
            m.hasVars = m.vars.size() > 0;
        }
        if (m.allVars != null) {
            for (CodegenProperty prop : m.allVars) {
                postProcessModelProperty(m, prop);
            }
        }

        // set isDiscriminator on the discriminator property
        if (m.discriminator != null) {
            String discPropName = m.discriminator.getPropertyBaseName();
            List<List<CodegenProperty>> listOLists = new ArrayList<List<CodegenProperty>>();
            listOLists.add(m.requiredVars);
            listOLists.add(m.vars);
            listOLists.add(m.allVars);
            for (List<CodegenProperty> theseVars : listOLists) {
                for (CodegenProperty requiredVar : theseVars) {
                    if (discPropName.equals(requiredVar.baseName)) {
                        requiredVar.isDiscriminator = true;
                    }
                }
            }
        }

        if (sortModelPropertiesByRequiredFlag) {
            Comparator<CodegenProperty> comparator = new Comparator<CodegenProperty>() {
                @Override
                public int compare(CodegenProperty one, CodegenProperty another) {
                    if (one.required == another.required) return 0;
                    else if (one.required) return -1;
                    else return 1;
                }
            };
            Collections.sort(m.vars, comparator);
            Collections.sort(m.allVars, comparator);
        }

        return m;
    }

    /**
     * Recursively look in Schema sc for the discriminator discPropName
     * and return a CodegenProperty with the dataType and required params set
     * the returned CodegenProperty may not be required and it may not be of type string
     *
     * @param composedSchemaName The name of the sc Schema
     * @param sc                 The Schema that may contain the discriminator
     * @param discPropName       The String that is the discriminator propertyName in the schema
     */
    private CodegenProperty discriminatorFound(String composedSchemaName, Schema sc, String discPropName, OpenAPI openAPI) {
        Schema refSchema = ModelUtils.getReferencedSchema(openAPI, sc);
        if (refSchema.getProperties() != null && refSchema.getProperties().get(discPropName) != null) {
            Schema discSchema = (Schema) refSchema.getProperties().get(discPropName);
            CodegenProperty cp = new CodegenProperty();
            if (ModelUtils.isStringSchema(discSchema)) {
                cp.isString = true;
            }
            cp.setRequired(false);
            if (refSchema.getRequired() != null && refSchema.getRequired().contains(discPropName)) {
                cp.setRequired(true);
            }
            return cp;
        }
        if (ModelUtils.isComposedSchema(refSchema)) {
            ComposedSchema composedSchema = (ComposedSchema) refSchema;
            if (composedSchema.getAllOf() != null) {
                // If our discriminator is in one of the allOf schemas break when we find it
                for (Schema allOf : composedSchema.getAllOf()) {
                    CodegenProperty cp = discriminatorFound(composedSchemaName, allOf, discPropName, openAPI);
                    if (cp != null) {
                        return cp;
                    }
                }
            }
            if (composedSchema.getOneOf() != null && composedSchema.getOneOf().size() != 0) {
                // All oneOf definitions must contain the discriminator
                CodegenProperty cp = new CodegenProperty();
                for (Schema oneOf : composedSchema.getOneOf()) {
                    String modelName = ModelUtils.getSimpleRef(oneOf.get$ref());
                    CodegenProperty thisCp = discriminatorFound(composedSchemaName, oneOf, discPropName, openAPI);
                    if (thisCp == null) {
                        throw new RuntimeException("'" + composedSchemaName + "' defines discriminator '" + discPropName + "', but the referenced OneOf schema '" + modelName + "' is missing " + discPropName);
                    }
                    if (cp.dataType == null) {
                        cp = thisCp;
                        continue;
                    }
                    if (cp != thisCp) {
                        throw new RuntimeException("'" + composedSchemaName + "' defines discriminator '" + discPropName + "', but the OneOf schema '" + modelName + "' has a different " + discPropName + " definition than the prior OneOf schema's. Make sure the " + discPropName + " type and required values are the same");
                    }
                }
                return cp;
            }
            if (composedSchema.getAnyOf() != null && composedSchema.getAnyOf().size() != 0) {
                // All anyOf definitions must contain the discriminator because a min of one must be selected
                CodegenProperty cp = new CodegenProperty();
                for (Schema anyOf : composedSchema.getAnyOf()) {
                    String modelName = ModelUtils.getSimpleRef(anyOf.get$ref());
                    CodegenProperty thisCp = discriminatorFound(composedSchemaName, anyOf, discPropName, openAPI);
                    if (thisCp == null) {
                        throw new RuntimeException("'" + composedSchemaName + "' defines discriminator '" + discPropName + "', but the referenced AnyOf schema '" + modelName + "' is missing " + discPropName);
                    }
                    if (cp.dataType == null) {
                        cp = thisCp;
                        continue;
                    }
                    if (cp != thisCp) {
                        throw new RuntimeException("'" + composedSchemaName + "' defines discriminator '" + discPropName + "', but the AnyOf schema '" + modelName + "' has a different " + discPropName + " definition than the prior AnyOf schema's. Make sure the " + discPropName + " type and required values are the same");
                    }
                }
                return cp;

            }
        }
        return null;
    }

    /**
     * Recursively look in Schema sc for the discriminator and return it
     * Schema sc location
     * OpenAPI openAPI the spec where we are searching for the discriminator
     *
     * @param sc The Schema that may contain the discriminator
     */
    private Discriminator recursiveGetDiscriminator(Schema sc, OpenAPI openAPI) {
        Schema refSchema = ModelUtils.getReferencedSchema(openAPI, sc);
        Discriminator foundDisc = refSchema.getDiscriminator();
        if (foundDisc != null) {
            return foundDisc;
        }
        if (!!this.getLegacyDiscriminatorBehavior()) {
            return null;
        }
        Discriminator disc = new Discriminator();
        if (ModelUtils.isComposedSchema(refSchema)) {
            ComposedSchema composedSchema = (ComposedSchema) refSchema;
            if (composedSchema.getAllOf() != null) {
                // If our discriminator is in one of the allOf schemas break when we find it
                for (Schema allOf : composedSchema.getAllOf()) {
                    foundDisc = recursiveGetDiscriminator(allOf, openAPI);
                    if (foundDisc != null) {
                        disc.setPropertyName(foundDisc.getPropertyName());
                        disc.setMapping(foundDisc.getMapping());
                        return disc;
                    }
                }
            }
            if (composedSchema.getOneOf() != null && composedSchema.getOneOf().size() != 0) {
                // All oneOf definitions must contain the discriminator
                Integer hasDiscriminatorCnt = 0;
                Integer hasNullTypeCnt = 0;
                Set<String> discriminatorsPropNames = new HashSet<>();
                for  (Schema oneOf: composedSchema.getOneOf()) {
                    if (ModelUtils.isNullType(oneOf)) {
                        // The null type does not have a discriminator. Skip.
                        hasNullTypeCnt++;
                        continue;
                    }
                    foundDisc = recursiveGetDiscriminator(oneOf, openAPI);
                    if (foundDisc != null) {
                        discriminatorsPropNames.add(foundDisc.getPropertyName());
                        hasDiscriminatorCnt++;
                    }
                }
                if (discriminatorsPropNames.size() > 1) {
                    throw new RuntimeException("The oneOf schemas have conflicting discriminator property names. " +
                        "oneOf schemas must have the same property name, but found " + String.join(", ", discriminatorsPropNames));
                }
                if ((hasDiscriminatorCnt + hasNullTypeCnt) == composedSchema.getOneOf().size() && discriminatorsPropNames.size() == 1) {
                    disc.setPropertyName(foundDisc.getPropertyName());
                    disc.setMapping(foundDisc.getMapping());
                    return disc;
                }
                // If the scenario when oneOf has two children and one of them is the 'null' type,
                // there is no need for a discriminator.
            }
            if (composedSchema.getAnyOf() != null && composedSchema.getAnyOf().size() != 0) {
                // All anyOf definitions must contain the discriminator because a min of one must be selected
                Integer hasDiscriminatorCnt = 0;
                Integer hasNullTypeCnt = 0;
                Set<String> discriminatorsPropNames = new HashSet<>();
                for  (Schema anyOf: composedSchema.getAnyOf()) {
                    if (ModelUtils.isNullType(anyOf)) {
                        // The null type does not have a discriminator. Skip.
                        hasNullTypeCnt++;
                        continue;
                    }
                    foundDisc = recursiveGetDiscriminator(anyOf, openAPI);
                    if (foundDisc != null) {
                        discriminatorsPropNames.add(foundDisc.getPropertyName());
                        hasDiscriminatorCnt++;
                    }
                }
                if (discriminatorsPropNames.size() > 1) {
                    throw new RuntimeException("The anyOf schemas have conflicting discriminator property names. " +
                        "anyOf schemas must have the same property name, but found " + String.join(", ", discriminatorsPropNames));
                }
                if ((hasDiscriminatorCnt + hasNullTypeCnt) == composedSchema.getAnyOf().size() && discriminatorsPropNames.size() == 1) {
                    disc.setPropertyName(foundDisc.getPropertyName());
                    disc.setMapping(foundDisc.getMapping());
                    return disc;
                }
                // If the scenario when anyOf has two children and one of them is the 'null' type,
                // there is no need for a discriminator.
            }
        }
        return null;
    }

    /**
     * This function is only used for composed schemas which have a discriminator
     * Process oneOf and anyOf models in a composed schema and adds them into
     * a list if the oneOf and anyOf models contain
     * the required discriminator. If they don't contain the required
     * discriminator or the discriminator is the wrong type then an error is
     * thrown
     *
     * @param composedSchemaName The String model name of the composed schema where we are setting the discriminator map
     * @param discPropName       The String that is the discriminator propertyName in the schema
     * @param c                  The ComposedSchema that contains the discriminator and oneOf/anyOf schemas
     * @param openAPI            The OpenAPI spec that we are using
     * @return the list of oneOf and anyOf MappedModel that need to be added to the discriminator map
     */
    protected List<MappedModel> getOneOfAnyOfDescendants(String composedSchemaName, String discPropName, ComposedSchema c, OpenAPI openAPI) {
        ArrayList<List<Schema>> listOLists = new ArrayList<List<Schema>>();
        listOLists.add(c.getOneOf());
        listOLists.add(c.getAnyOf());
        List<MappedModel> descendentSchemas = new ArrayList();
        for (List<Schema> schemaList : listOLists) {
            if (schemaList == null) {
                continue;
            }
            for  (Schema sc: schemaList) {
                if (ModelUtils.isNullType(sc)) {
                    continue;
                }
                String ref = sc.get$ref();
                if (ref == null) {
                    // for schemas with no ref, it is not possible to build the discriminator map
                    // because ref is how we get the model name
                    // we only hit this use case for a schema with inline composed schemas, and one of those
                    // schemas also has inline composed schemas
                    // Note: if it is only inline one level, then the inline model resolver will move it into its own
                    // schema and make it a $ref schema in the oneOf/anyOf location
                    throw new RuntimeException("Invalid inline schema defined in oneOf/anyOf in '" + composedSchemaName + "'. Per the OpenApi spec, for this case when a composed schema defines a discriminator, the oneOf/anyOf schemas must use $ref. Change this inline definition to a $ref definition");
                }
                CodegenProperty df = discriminatorFound(composedSchemaName, sc, discPropName, openAPI);
                String modelName = ModelUtils.getSimpleRef(ref);
                if (df == null || !df.isString || df.required != true) {
                    String msgSuffix = "";
                    if (df == null) {
                        msgSuffix += discPropName + " is missing from the schema, define it as required and type string";
                    } else {
                        if (!df.isString) {
                            msgSuffix += "invalid type for " + discPropName + ", set it to string";
                        }
                        if (df.required != true) {
                            String spacer = "";
                            if (msgSuffix.length() != 0) {
                                spacer = ". ";
                            }
                            msgSuffix += spacer + "invalid optional definition of " + discPropName + ", include it in required";
                        }
                    }
                    throw new RuntimeException("'" + composedSchemaName + "' defines discriminator '" + discPropName + "', but the referenced schema '" + modelName + "' is incorrect. " + msgSuffix);
                }
                MappedModel mm = new MappedModel(modelName, toModelName(modelName));
                descendentSchemas.add(mm);
                Schema cs = ModelUtils.getSchema(openAPI, modelName);
                Map<String, Object> vendorExtensions = cs.getExtensions();
                if (vendorExtensions != null && !vendorExtensions.isEmpty() && vendorExtensions.containsKey("x-discriminator-value")) {
                    String xDiscriminatorValue = (String) vendorExtensions.get("x-discriminator-value");
                    mm = new MappedModel(xDiscriminatorValue, toModelName(modelName));
                    descendentSchemas.add(mm);
                }

            }
        }
        return descendentSchemas;
    }

    protected List<MappedModel> getAllOfDescendants(String thisSchemaName, OpenAPI openAPI) {
        ArrayList<String> queue = new ArrayList();
        List<MappedModel> descendentSchemas = new ArrayList();
        Map<String, Schema> schemas = ModelUtils.getSchemas(openAPI);
        String currentSchemaName = thisSchemaName;
        while (true) {
            for (String childName : schemas.keySet()) {
                if (childName == thisSchemaName) {
                    continue;
                }
                Schema child = schemas.get(childName);
                if (ModelUtils.isComposedSchema(child)) {
                    ComposedSchema composedChild = (ComposedSchema) child;
                    List<Schema> parents = composedChild.getAllOf();
                    if (parents != null) {
                        for (Schema parent : parents) {
                            String ref = parent.get$ref();
                            if (ref == null) {
                                // for schemas with no ref, it is not possible to build the discriminator map
                                // because ref is how we get the model name
                                // we only hit this use case for a schema with inline composed schemas, and one of those
                                // schemas also has inline composed schemas
                                throw new RuntimeException("Invalid inline schema defined in allOf in '" + childName + "'. Per the OpenApi spec, for this case when a composed schema defines a discriminator, the allOf schemas must use $ref. Change this inline definition to a $ref definition");
                            }
                            String parentName = ModelUtils.getSimpleRef(ref);
                            if (parentName.equals(currentSchemaName)) {
                                if (queue.contains(childName) || descendentSchemas.contains(childName)) {
                                    throw new RuntimeException("Stack overflow hit when looking for " + thisSchemaName + " an infinite loop starting and ending at " + childName + " was seen");
                                }
                                queue.add(childName);
                                break;
                            }
                        }
                    }
                }
            }
            if (queue.size() == 0) {
                break;
            }
            currentSchemaName = queue.remove(0);
            MappedModel mm = new MappedModel(currentSchemaName, toModelName(currentSchemaName));
            descendentSchemas.add(mm);
            Schema cs = schemas.get(currentSchemaName);
            Map<String, Object> vendorExtensions = cs.getExtensions();
            if (vendorExtensions != null && !vendorExtensions.isEmpty() && vendorExtensions.containsKey("x-discriminator-value")) {
                String xDiscriminatorValue = (String) vendorExtensions.get("x-discriminator-value");
                mm = new MappedModel(xDiscriminatorValue, toModelName(currentSchemaName));
                descendentSchemas.add(mm);
            }
        }
        return descendentSchemas;
    }

    protected CodegenDiscriminator createDiscriminator(String schemaName, Schema schema, OpenAPI openAPI) {
        Discriminator sourceDiscriminator = recursiveGetDiscriminator(schema, openAPI);
        if (sourceDiscriminator == null) {
            return null;
        }
        CodegenDiscriminator discriminator = new CodegenDiscriminator();
        String discPropName = sourceDiscriminator.getPropertyName();
        discriminator.setPropertyName(toVarName(discPropName));
        discriminator.setPropertyBaseName(sourceDiscriminator.getPropertyName());
        discriminator.setPropertyGetter(toGetter(discriminator.getPropertyName()));
        // FIXME: for now, we assume that the discriminator property is String
        discriminator.setPropertyType(typeMapping.get("string"));
        discriminator.setMapping(sourceDiscriminator.getMapping());
        List<MappedModel> uniqueDescendants = new ArrayList();
        if (sourceDiscriminator.getMapping() != null && !sourceDiscriminator.getMapping().isEmpty()) {
            for (Entry<String, String> e : sourceDiscriminator.getMapping().entrySet()) {
                String nameOrRef = e.getValue();
                String name = nameOrRef.indexOf('/') >= 0 ? ModelUtils.getSimpleRef(nameOrRef) : nameOrRef;
                String modelName = toModelName(name);
                uniqueDescendants.add(new MappedModel(e.getKey(), modelName));
            }
        }

        boolean legacyUseCase = (this.getLegacyDiscriminatorBehavior() && uniqueDescendants.isEmpty());
        if (!this.getLegacyDiscriminatorBehavior() || legacyUseCase) {
            // for schemas that allOf inherit from this schema, add those descendants to this discriminator map
            List<MappedModel> otherDescendants = getAllOfDescendants(schemaName, openAPI);
            for (MappedModel otherDescendant : otherDescendants) {
                if (!uniqueDescendants.contains(otherDescendant)) {
                    uniqueDescendants.add(otherDescendant);
                }
            }
        }
        // if there are composed oneOf/anyOf schemas, add them to this discriminator
        if (ModelUtils.isComposedSchema(schema) && !this.getLegacyDiscriminatorBehavior()) {
            List<MappedModel> otherDescendants = getOneOfAnyOfDescendants(schemaName, discPropName, (ComposedSchema) schema, openAPI);
            for (MappedModel otherDescendant : otherDescendants) {
                if (!uniqueDescendants.contains(otherDescendant)) {
                    uniqueDescendants.add(otherDescendant);
                }
            }
        }
        if (!this.getLegacyDiscriminatorBehavior()) {
            Collections.sort(uniqueDescendants);
        }
        discriminator.getMappedModels().addAll(uniqueDescendants);
        return discriminator;
    }

    /**
     * Handle the model for the 'additionalProperties' keyword in the OAS schema.
     *
     * @param codegenModel The codegen representation of the schema.
     * @param schema the input OAS schema.
     */
    protected void addAdditionPropertiesToCodeGenModel(CodegenModel codegenModel, Schema schema) {
        addParentContainer(codegenModel, codegenModel.name, schema);
    }

    /**
     * Add schema's properties to "properties" and "required" list
     *
     * @param properties all properties
     * @param required   required property only
     * @param schema     schema in which the properties will be added to the lists
     */
    protected void addProperties(Map<String, Schema> properties, List<String> required, Schema schema) {
        if (schema instanceof ComposedSchema) {
            ComposedSchema composedSchema = (ComposedSchema) schema;

            if (composedSchema.getAllOf() != null) {
                for (Schema component : composedSchema.getAllOf()) {
                    addProperties(properties, required, component);
                }
            }

            if (schema.getRequired() != null) {
                required.addAll(schema.getRequired());
            }

            if (composedSchema.getOneOf() != null) {
                for (Schema component : composedSchema.getOneOf()) {
                    addProperties(properties, required, component);
                }
            }

            if (composedSchema.getAnyOf() != null) {
                for (Schema component : composedSchema.getAnyOf()) {
                    addProperties(properties, required, component);
                }
            }

            return;
        }

        if (StringUtils.isNotBlank(schema.get$ref())) {
            Schema interfaceSchema = ModelUtils.getReferencedSchema(this.openAPI, schema);
            addProperties(properties, required, interfaceSchema);
            return;
        }
        if (schema.getProperties() != null) {
            properties.putAll(schema.getProperties());
        }
        if (schema.getRequired() != null) {
            required.addAll(schema.getRequired());
        }
    }

    /**
     * Camelize the method name of the getter and setter
     *
     * @param name string to be camelized
     * @return Camelized string
     */
    public String getterAndSetterCapitalize(String name) {
        if (name == null || name.length() == 0) {
            return name;
        }
        return camelize(toVarName(name));
    }

    /**
     * Convert OAS Property object to Codegen Property object
     *
     * @param name name of the property
     * @param p    OAS property schema
     * @return Codegen Property object
     */
    public CodegenProperty fromProperty(String name, Schema p) {
        if (p == null) {
            LOGGER.error("Undefined property/schema for `{}`. Default to type:string.", name);
            return null;
        }
        LOGGER.debug("debugging fromProperty for " + name + " : " + p);

        // unalias schema
        p = ModelUtils.unaliasSchema(this.openAPI, p, importMapping);

        CodegenProperty property = CodegenModelFactory.newInstance(CodegenModelType.PROPERTY);

        ModelUtils.syncValidationProperties(p, property);

        property.name = toVarName(name);
        property.baseName = name;
        if (p.getType() == null) {
            property.openApiType = getSchemaType(p);
        } else {
            property.openApiType = p.getType();
        }
        property.nameInCamelCase = camelize(property.name, false);
        property.nameInSnakeCase = CaseFormat.UPPER_CAMEL.to(CaseFormat.UPPER_UNDERSCORE, property.nameInCamelCase);
        property.description = escapeText(p.getDescription());
        property.unescapedDescription = p.getDescription();
        property.title = p.getTitle();
        property.getter = toGetter(name);
        property.setter = toSetter(name);
        property.example = toExampleValue(p);
        property.defaultValue = toDefaultValue(p);
        property.defaultValueWithParam = toDefaultValueWithParam(name, p);
        property.jsonSchema = Json.pretty(p);

        if (p.getDeprecated() != null) {
            property.deprecated = p.getDeprecated();
        } else if (p.get$ref() != null) {
            // Since $ref should be replaced with the model it refers
            // to, $ref'ing a model with 'deprecated' set should cause
            // the property to reflect the model's 'deprecated' value.
            String ref = ModelUtils.getSimpleRef(p.get$ref());
            if (ref != null) {
                Schema referencedSchema = ModelUtils.getSchemas(this.openAPI).get(ref);
                if (referencedSchema != null && referencedSchema.getDeprecated() != null) {
                    property.deprecated = referencedSchema.getDeprecated();
                }
            }
        }
        if (p.getReadOnly() != null) {
            property.isReadOnly = p.getReadOnly();
        }
        if (p.getWriteOnly() != null) {
            property.isWriteOnly = p.getWriteOnly();
        }
        if (p.getNullable() != null) {
            property.isNullable = p.getNullable();
        }

        if (p.getXml() != null) {
            if (p.getXml().getAttribute() != null) {
                property.isXmlAttribute = p.getXml().getAttribute();
            }
            property.xmlPrefix = p.getXml().getPrefix();
            property.xmlName = p.getXml().getName();
            property.xmlNamespace = p.getXml().getNamespace();
        }
        if (p.getExtensions() != null && !p.getExtensions().isEmpty()) {
            property.getVendorExtensions().putAll(p.getExtensions());
        }

        String type = getSchemaType(p);
        if (ModelUtils.isIntegerSchema(p)) { // integer type
            property.isNumeric = Boolean.TRUE;
            if (ModelUtils.isLongSchema(p)) { // int64/long format
                property.isLong = Boolean.TRUE;
            } else { // int32 format
                property.isInteger = Boolean.TRUE;
            }

            if (p.getMinimum() != null) {
                property.minimum = String.valueOf(p.getMinimum().longValue());
            }
            if (p.getMaximum() != null) {
                property.maximum = String.valueOf(p.getMaximum().longValue());
            }
            if (p.getExclusiveMinimum() != null) {
                property.exclusiveMinimum = p.getExclusiveMinimum();
            }
            if (p.getExclusiveMaximum() != null) {
                property.exclusiveMaximum = p.getExclusiveMaximum();
            }
            if (p.getMultipleOf() != null) {
                property.multipleOf = p.getMultipleOf();
            }

            // check if any validation rule defined
            // exclusive* are noop without corresponding min/max
            if (property.minimum != null || property.maximum != null || p.getMultipleOf() != null)
                property.hasValidation = true;

        } else if (ModelUtils.isBooleanSchema(p)) { // boolean type
            property.isBoolean = true;
            property.getter = toBooleanGetter(name);
        } else if (ModelUtils.isDateSchema(p)) { // date format
            property.isString = false; // for backward compatibility with 2.x
            property.isDate = true;

        } else if (ModelUtils.isDateTimeSchema(p)) { // date-time format
            property.isString = false; // for backward compatibility with 2.x
            property.isDateTime = true;

        } else if (ModelUtils.isStringSchema(p)) {
            if (ModelUtils.isByteArraySchema(p)) {
                property.isByteArray = true;
            } else if (ModelUtils.isBinarySchema(p)) {
                property.isBinary = true;
                property.isFile = true; // file = binary in OAS3
            } else if (ModelUtils.isFileSchema(p)) {
                property.isFile = true;
            } else if (ModelUtils.isUUIDSchema(p)) {
                // keep isString to true to make it backward compatible
                property.isString = true;
                property.isUuid = true;
            } else if (ModelUtils.isURISchema(p)) {
                property.isString = true; // for backward compatibility
                property.isUri = true;
            } else if (ModelUtils.isEmailSchema(p)) {
                property.isString = true;
                property.isEmail = true;
            } else {
                property.isString = true;
            }

            property.maxLength = p.getMaxLength();
            property.minLength = p.getMinLength();
            property.pattern = toRegularExpression(p.getPattern());

            // check if any validation rule defined
            if (property.pattern != null || property.minLength != null || property.maxLength != null)
                property.hasValidation = true;

        } else if (ModelUtils.isNumberSchema(p)) {
            property.isNumeric = Boolean.TRUE;
            if (ModelUtils.isFloatSchema(p)) { // float
                property.isFloat = Boolean.TRUE;
            } else if (ModelUtils.isDoubleSchema(p)) { // double
                property.isDouble = Boolean.TRUE;
            } else { // type is number and without format
                property.isNumber = Boolean.TRUE;
            }

            if (p.getMinimum() != null) {
                property.minimum = String.valueOf(p.getMinimum());
            }
            if (p.getMaximum() != null) {
                property.maximum = String.valueOf(p.getMaximum());
            }
            if (p.getExclusiveMinimum() != null) {
                property.exclusiveMinimum = p.getExclusiveMinimum();
            }
            if (p.getExclusiveMaximum() != null) {
                property.exclusiveMaximum = p.getExclusiveMaximum();
            }
            if (p.getMultipleOf() != null) {
                property.multipleOf = p.getMultipleOf();
            }

            // check if any validation rule defined
            // exclusive* are noop without corresponding min/max
            if (property.minimum != null || property.maximum != null || p.getMultipleOf() != null) {
                property.hasValidation = true;
            }

        } else if (isFreeFormObject(p)) {
            property.isFreeFormObject = true;
        } else if (isAnyTypeSchema(p)) {
            // The 'null' value is allowed when the OAS schema is 'any type'.
            // See https://github.com/OAI/OpenAPI-Specification/issues/1389
            if (Boolean.FALSE.equals(p.getNullable())) {
                LOGGER.warn("Schema '{}' is any type, which includes the 'null' value. 'nullable' cannot be set to 'false'", p.getName());
            }
            property.isNullable = true;
            property.isAnyType = true;
        } else if (ModelUtils.isArraySchema(p)) {
            // default to string if inner item is undefined
            ArraySchema arraySchema = (ArraySchema) p;
            Schema innerSchema = ModelUtils.unaliasSchema(this.openAPI, getSchemaItems(arraySchema), importMapping);
        } else if (ModelUtils.isMapSchema(p)) {
            Schema innerSchema = ModelUtils.unaliasSchema(this.openAPI, getAdditionalProperties(p),
                    importMapping);
            if (innerSchema == null) {
                LOGGER.error("Undefined map inner type for `{}`. Default to String.", p.getName());
                innerSchema = new StringSchema().description("//TODO automatically added by openapi-generator due to undefined type");
                p.setAdditionalProperties(innerSchema);
            }
        }

        //Inline enum case:
        if (p.getEnum() != null && !p.getEnum().isEmpty()) {
            List<Object> _enum = p.getEnum();
            property._enum = new ArrayList<String>();
            for (Object i : _enum) {
                property._enum.add(String.valueOf(i));
            }
            property.isEnum = true;

            Map<String, Object> allowableValues = new HashMap<String, Object>();
            allowableValues.put("values", _enum);
            if (allowableValues.size() > 0) {
                property.allowableValues = allowableValues;
            }
        }

        Schema referencedSchema = ModelUtils.getReferencedSchema(this.openAPI, p);

        //Referenced enum case:
        if (referencedSchema.getEnum() != null && !referencedSchema.getEnum().isEmpty()) {
            List<Object> _enum = referencedSchema.getEnum();

            Map<String, Object> allowableValues = new HashMap<String, Object>();
            allowableValues.put("values", _enum);
            if (allowableValues.size() > 0) {
                property.allowableValues = allowableValues;
            }
        }

        if (referencedSchema.getNullable() != null) {
            property.isNullable = referencedSchema.getNullable();
        }

        property.dataType = getTypeDeclaration(p);
        property.dataFormat = p.getFormat();
        property.baseType = getSchemaType(p);

        // this can cause issues for clients which don't support enums
        if (property.isEnum) {
            property.datatypeWithEnum = toEnumName(property);
            property.enumName = toEnumName(property);
        } else {
            property.datatypeWithEnum = property.dataType;
        }

        if (ModelUtils.isArraySchema(p)) {
            property.isContainer = true;
            property.isListContainer = true;
            if (ModelUtils.isSet(p)) {
                property.containerType = "set";
            } else {
                property.containerType = "array";
            }
            property.baseType = getSchemaType(p);
            if (p.getXml() != null) {
                property.isXmlWrapped = p.getXml().getWrapped() == null ? false : p.getXml().getWrapped();
                property.xmlPrefix = p.getXml().getPrefix();
                property.xmlNamespace = p.getXml().getNamespace();
                property.xmlName = p.getXml().getName();
            }

            // handle inner property
            property.maxItems = p.getMaxItems();
            property.minItems = p.getMinItems();
            String itemName = null;
            if (p.getExtensions() != null && p.getExtensions().get("x-item-name") != null) {
                itemName = p.getExtensions().get("x-item-name").toString();
            }
            if (itemName == null) {
                itemName = property.name;
            }
            ArraySchema arraySchema = (ArraySchema) p;
            Schema innerSchema = ModelUtils.unaliasSchema(this.openAPI, getSchemaItems(arraySchema), importMapping);
            CodegenProperty cp = fromProperty(itemName, innerSchema);
            updatePropertyForArray(property, cp);
        } else if (ModelUtils.isMapSchema(p)) {
            property.isContainer = true;
            property.isMapContainer = true;
            property.containerType = "map";
            property.baseType = getSchemaType(p);
            property.minItems = p.getMinProperties();
            property.maxItems = p.getMaxProperties();

            // handle inner property
            Schema innerSchema = ModelUtils.unaliasSchema(this.openAPI, getAdditionalProperties(p),
                    importMapping);
            if (innerSchema == null) {
                LOGGER.error("Undefined map inner type for `{}`. Default to String.", p.getName());
                innerSchema = new StringSchema().description("//TODO automatically added by openapi-generator due to undefined type");
                p.setAdditionalProperties(innerSchema);
            }
            CodegenProperty cp = fromProperty("inner", innerSchema);
            updatePropertyForMap(property, cp);
        } else if (isFreeFormObject(p)) {
            property.isFreeFormObject = true;
            property.baseType = getSchemaType(p);
            if (languageSpecificPrimitives.contains(property.dataType)) {
                property.isPrimitiveType = true;
            }
        } else if (isAnyTypeSchema(p)) {
            property.isAnyType = true;
            property.baseType = getSchemaType(p);
            if (languageSpecificPrimitives.contains(property.dataType)) {
                property.isPrimitiveType = true;
            }
        } else { // model
            setNonArrayMapProperty(property, type);
            Schema refOrCurrent = ModelUtils.getReferencedSchema(this.openAPI, p);
            property.isModel = (ModelUtils.isComposedSchema(refOrCurrent) || ModelUtils.isObjectSchema(refOrCurrent)) && ModelUtils.isModel(refOrCurrent);
        }

        LOGGER.debug("debugging from property return: " + property);
        return property;
    }

    /**
     * Update property for array(list) container
     *
     * @param property      Codegen property
     * @param innerProperty Codegen inner property of map or list
     */
    protected void updatePropertyForArray(CodegenProperty property, CodegenProperty innerProperty) {
        if (innerProperty == null) {
            LOGGER.warn("skipping invalid array property " + Json.pretty(property));
            return;
        }
        property.dataFormat = innerProperty.dataFormat;
        if (!languageSpecificPrimitives.contains(innerProperty.baseType)) {
            property.complexType = innerProperty.baseType;
        } else {
            property.isPrimitiveType = true;
        }
        property.items = innerProperty;
        property.mostInnerItems = getMostInnerItems(innerProperty);
        // inner item is Enum
        if (isPropertyInnerMostEnum(property)) {
            // isEnum is set to true when the type is an enum
            // or the inner type of an array/map is an enum
            property.isEnum = true;
            // update datatypeWithEnum and default value for array
            // e.g. List<string> => List<StatusEnum>
            updateDataTypeWithEnumForArray(property);
            // set allowable values to enum values (including array/map of enum)
            property.allowableValues = getInnerEnumAllowableValues(property);
        }

    }

    /**
     * Update property for map container
     *
     * @param property      Codegen property
     * @param innerProperty Codegen inner property of map or list
     */
    protected void updatePropertyForMap(CodegenProperty property, CodegenProperty innerProperty) {
        if (innerProperty == null) {
            LOGGER.warn("skipping invalid map property " + Json.pretty(property));
            return;
        }
        if (!languageSpecificPrimitives.contains(innerProperty.baseType)) {
            property.complexType = innerProperty.baseType;
        } else {
            property.isPrimitiveType = true;
        }
        property.items = innerProperty;
        property.mostInnerItems = getMostInnerItems(innerProperty);
        property.dataFormat = innerProperty.dataFormat;
        // inner item is Enum
        if (isPropertyInnerMostEnum(property)) {
            // isEnum is set to true when the type is an enum
            // or the inner type of an array/map is an enum
            property.isEnum = true;
            // update datatypeWithEnum and default value for map
            // e.g. Dictionary<string, string> => Dictionary<string, StatusEnum>
            updateDataTypeWithEnumForMap(property);
            // set allowable values to enum values (including array/map of enum)
            property.allowableValues = getInnerEnumAllowableValues(property);
        }

    }

    /**
     * Update property for map container
     *
     * @param property Codegen property
     * @return True if the inner most type is enum
     */
    protected Boolean isPropertyInnerMostEnum(CodegenProperty property) {
        CodegenProperty currentProperty = getMostInnerItems(property);

        return currentProperty == null ? false : currentProperty.isEnum;
    }

    protected CodegenProperty getMostInnerItems(CodegenProperty property) {
        CodegenProperty currentProperty = property;
        while (currentProperty != null && (Boolean.TRUE.equals(currentProperty.isMapContainer)
                || Boolean.TRUE.equals(currentProperty.isListContainer))) {
            currentProperty = currentProperty.items;
        }
        return currentProperty;
    }

    protected Map<String, Object> getInnerEnumAllowableValues(CodegenProperty property) {
        CodegenProperty currentProperty = getMostInnerItems(property);

        return currentProperty == null ? new HashMap<String, Object>() : currentProperty.allowableValues;
    }

    /**
     * Update datatypeWithEnum for array container
     *
     * @param property Codegen property
     */
    protected void updateDataTypeWithEnumForArray(CodegenProperty property) {
        CodegenProperty baseItem = property.items;
        while (baseItem != null && (Boolean.TRUE.equals(baseItem.isMapContainer)
                || Boolean.TRUE.equals(baseItem.isListContainer))) {
            baseItem = baseItem.items;
        }
        if (baseItem != null) {
            // set both datatype and datetypeWithEnum as only the inner type is enum
            property.datatypeWithEnum = property.datatypeWithEnum.replace(baseItem.baseType, toEnumName(baseItem));

            // naming the enum with respect to the language enum naming convention
            // e.g. remove [], {} from array/map of enum
            property.enumName = toEnumName(property);

            // set default value for variable with inner enum
            if (property.defaultValue != null) {
                property.defaultValue = property.defaultValue.replace(baseItem.baseType, toEnumName(baseItem));
            }

            updateCodegenPropertyEnum(property);
        }
    }

    /**
     * Update datatypeWithEnum for map container
     *
     * @param property Codegen property
     */
    protected void updateDataTypeWithEnumForMap(CodegenProperty property) {
        CodegenProperty baseItem = property.items;
        while (baseItem != null && (Boolean.TRUE.equals(baseItem.isMapContainer)
                || Boolean.TRUE.equals(baseItem.isListContainer))) {
            baseItem = baseItem.items;
        }

        if (baseItem != null) {
            // set both datatype and datetypeWithEnum as only the inner type is enum
            property.datatypeWithEnum = property.datatypeWithEnum.replace(", " + baseItem.baseType, ", " + toEnumName(baseItem));

            // naming the enum with respect to the language enum naming convention
            // e.g. remove [], {} from array/map of enum
            property.enumName = toEnumName(property);

            // set default value for variable with inner enum
            if (property.defaultValue != null) {
                property.defaultValue = property.defaultValue.replace(", " + property.items.baseType, ", " + toEnumName(property.items));
            }

            updateCodegenPropertyEnum(property);
        }
    }

    protected void setNonArrayMapProperty(CodegenProperty property, String type) {
        property.isContainer = false;
        if (languageSpecificPrimitives().contains(type)) {
            property.isPrimitiveType = true;
        } else {
            property.complexType = property.baseType;
            property.isModel = true;
        }
    }

    /**
     * Override with any special handling of response codes
     *
     * @param responses OAS Operation's responses
     * @return default method response or <code>null</code> if not found
     */
    protected ApiResponse findMethodResponse(ApiResponses responses) {
        String code = null;
        for (String responseCode : responses.keySet()) {
            if (responseCode.startsWith("2") || responseCode.equals("default")) {
                if (code == null || code.compareTo(responseCode) > 0) {
                    code = responseCode;
                }
            }
        }
        if (code == null) {
            return null;
        }
        return responses.get(code);
    }

    /**
     * Set op's returnBaseType, returnType, examples etc.
     *
     * @param operation      endpoint Operation
     * @param schemas        a map of the schemas in the openapi spec
     * @param op             endpoint CodegenOperation
     * @param methodResponse the default ApiResponse for the endpoint
     */
    protected void handleMethodResponse(Operation operation,
                                        Map<String, Schema> schemas,
                                        CodegenOperation op,
                                        ApiResponse methodResponse) {
        handleMethodResponse(operation, schemas, op, methodResponse, Collections.<String, String>emptyMap());
    }

    /**
     * Set op's returnBaseType, returnType, examples etc.
     *
     * @param operation      endpoint Operation
     * @param schemas        a map of the schemas in the openapi spec
     * @param op             endpoint CodegenOperation
     * @param methodResponse the default ApiResponse for the endpoint
     * @param importMappings mappings of external types to be omitted by unaliasing
     */
    protected void handleMethodResponse(Operation operation,
                                        Map<String, Schema> schemas,
                                        CodegenOperation op,
                                        ApiResponse methodResponse,
                                        Map<String, String> importMappings) {
        Schema responseSchema = ModelUtils.unaliasSchema(this.openAPI, ModelUtils.getSchemaFromResponse(methodResponse), importMappings);

        if (responseSchema != null) {
            CodegenProperty cm = fromProperty("response", responseSchema);

            if (ModelUtils.isArraySchema(responseSchema)) {
                ArraySchema as = (ArraySchema) responseSchema;
                CodegenProperty innerProperty = fromProperty("response", getSchemaItems(as));
                op.returnBaseType = innerProperty.baseType;
            } else if (ModelUtils.isMapSchema(responseSchema)) {
                CodegenProperty innerProperty = fromProperty("response", getAdditionalProperties(responseSchema));
                op.returnBaseType = innerProperty.baseType;
            } else {
                if (cm.complexType != null) {
                    op.returnBaseType = cm.complexType;
                } else {
                    op.returnBaseType = cm.baseType;
                }
            }

            // generate examples
            String exampleStatusCode = "200";
            for (String key : operation.getResponses().keySet()) {
                if (operation.getResponses().get(key) == methodResponse && !key.equals("default")) {
                    exampleStatusCode = key;
                }
            }
            op.examples = new ExampleGenerator(schemas, this.openAPI).generateFromResponseSchema(exampleStatusCode, responseSchema, getProducesInfo(this.openAPI, operation));
            op.defaultResponse = toDefaultValue(responseSchema);
            op.returnType = cm.dataType;
            op.returnFormat = cm.dataFormat;
            op.hasReference = schemas != null && schemas.containsKey(op.returnBaseType);
             
            // lookup discriminator
            Schema schema = schemas.get(op.returnBaseType);
            if (schema != null) {
                CodegenModel cmod = fromModel(op.returnBaseType, schema);
                op.discriminator = cmod.discriminator;
            }

            if (cm.isContainer) {
                op.returnContainer = cm.containerType;
                if ("map".equals(cm.containerType)) {
                    op.isMapContainer = true;
                } else if ("list".equalsIgnoreCase(cm.containerType)) {
                    op.isListContainer = true;
                } else if ("array".equalsIgnoreCase(cm.containerType)) {
                    op.isListContainer = true;
                } else if ("set".equalsIgnoreCase(cm.containerType)) {
                    op.isListContainer = true;
                }
            } else {
                op.returnSimpleType = true;
            }
            if (languageSpecificPrimitives().contains(op.returnBaseType) || op.returnBaseType == null) {
                op.returnTypeIsPrimitive = true;
            }
        }
        addHeaders(methodResponse, op.responseHeaders);
    }

    /**
     * Convert OAS Operation object to Codegen Operation object
     *
     * @param httpMethod HTTP method
     * @param operation  OAS operation object
     * @param path       the path of the operation
     * @param servers    list of servers
     * @return Codegen Operation object
     */
    public CodegenOperation fromOperation(String path,
                                          String httpMethod,
                                          Operation operation,
                                          List<Server> servers) {
        LOGGER.debug("fromOperation => operation: " + operation);
        if (operation == null)
            throw new RuntimeException("operation cannot be null in fromOperation");

        Map<String, Schema> schemas = ModelUtils.getSchemas(this.openAPI);
        CodegenOperation op = CodegenModelFactory.newInstance(CodegenModelType.OPERATION);
        Set<String> imports = new HashSet<String>();
        if (operation.getExtensions() != null && !operation.getExtensions().isEmpty()) {
            op.vendorExtensions.putAll(operation.getExtensions());

            Object isCallbackRequest = op.vendorExtensions.remove("x-callback-request");
            op.isCallbackRequest = Boolean.TRUE.equals(isCallbackRequest);
        }

        // servers setting
        if (operation.getServers() != null && !operation.getServers().isEmpty()) {
            // use operation-level servers first if defined
            op.servers = fromServers(operation.getServers());
        } else if (servers != null && !servers.isEmpty()) {
            // use path-level servers
            op.servers = fromServers(servers);
        }

        // store the original operationId for plug-in
        op.operationIdOriginal = operation.getOperationId();

        String operationId = getOrGenerateOperationId(operation, path, httpMethod);
        // remove prefix in operationId
        if (removeOperationIdPrefix) {
            int offset = operationId.indexOf('_');
            if (offset > -1) {
                operationId = operationId.substring(offset + 1);
            }
        }
        operationId = removeNonNameElementToCamelCase(operationId);

        if (isStrictSpecBehavior() && !path.startsWith("/")) {
            // modifies an operation.path to strictly conform to OpenAPI Spec
            op.path = "/" + path;
        } else {
            op.path = path;
        }

        op.operationId = toOperationId(operationId);
        op.summary = escapeText(operation.getSummary());
        op.unescapedNotes = operation.getDescription();
        op.notes = escapeText(operation.getDescription());
        op.hasConsumes = false;
        op.hasProduces = false;
        if (operation.getDeprecated() != null) {
            op.isDeprecated = operation.getDeprecated();
        }

        addConsumesInfo(operation, op);

        if (operation.getResponses() != null && !operation.getResponses().isEmpty()) {
            ApiResponse methodResponse = findMethodResponse(operation.getResponses());
            for (String key : operation.getResponses().keySet()) {
                ApiResponse response = operation.getResponses().get(key);
                addProducesInfo(response, op);
                CodegenResponse r = fromResponse(key, response);
                r.hasMore = true;
                if (r.baseType != null &&
                        !defaultIncludes.contains(r.baseType) &&
                        !languageSpecificPrimitives.contains(r.baseType)) {
                    imports.add(r.baseType);
                }
                if ("set".equals(r.containerType) && typeMapping.containsKey(r.containerType)) {
                    op.uniqueItems = true;
                    imports.add(typeMapping.get(r.containerType));
                }

                op.responses.add(r);
                if (Boolean.TRUE.equals(r.isBinary) && Boolean.TRUE.equals(r.is2xx) && Boolean.FALSE.equals(op.isResponseBinary)) {
                    op.isResponseBinary = Boolean.TRUE;
                }
                if (Boolean.TRUE.equals(r.isFile) && Boolean.TRUE.equals(r.is2xx) && Boolean.FALSE.equals(op.isResponseFile)) {
                    op.isResponseFile = Boolean.TRUE;
                }
            }
            op.responses.sort((a, b) -> {
                int aDefault = "0".equals(a.code) ? 1 : 0;
                int bDefault = "0".equals(b.code) ? 1 : 0;
                return aDefault - bDefault;
            });
            op.responses.get(op.responses.size() - 1).hasMore = false;

            if (methodResponse != null) {
                handleMethodResponse(operation, schemas, op, methodResponse, importMapping);
            }
        }

        if (operation.getCallbacks() != null && !operation.getCallbacks().isEmpty()) {
            operation.getCallbacks().forEach((name, callback) -> {
                CodegenCallback c = fromCallback(name, callback, servers);
                c.hasMore = true;
                op.callbacks.add(c);
            });
            op.callbacks.get(op.callbacks.size() - 1).hasMore = false;
        }

        List<Parameter> parameters = operation.getParameters();
        List<CodegenParameter> allParams = new ArrayList<CodegenParameter>();
        List<CodegenParameter> bodyParams = new ArrayList<CodegenParameter>();
        List<CodegenParameter> pathParams = new ArrayList<CodegenParameter>();
        List<CodegenParameter> queryParams = new ArrayList<CodegenParameter>();
        List<CodegenParameter> headerParams = new ArrayList<CodegenParameter>();
        List<CodegenParameter> cookieParams = new ArrayList<CodegenParameter>();
        List<CodegenParameter> formParams = new ArrayList<CodegenParameter>();
        List<CodegenParameter> requiredParams = new ArrayList<CodegenParameter>();
        List<CodegenParameter> optionalParams = new ArrayList<CodegenParameter>();

        CodegenParameter bodyParam = null;
        RequestBody requestBody = operation.getRequestBody();
        if (requestBody != null) {
            String contentType = getContentType(requestBody);
            if (contentType != null) {
                contentType = contentType.toLowerCase(Locale.ROOT);
            }
            if (contentType != null &&
                    (contentType.startsWith("application/x-www-form-urlencoded") ||
                            contentType.startsWith("multipart"))) {
                // process form parameters
                formParams = fromRequestBodyToFormParameters(requestBody, imports);
                op.isMultipart = contentType.startsWith("multipart");
                for (CodegenParameter cp : formParams) {
                    setParameterContentType(cp, requestBody.getContent().get(contentType));
                    postProcessParameter(cp);
                }
                // add form parameters to the beginning of all parameter list
                if (prependFormOrBodyParameters) {
                    for (CodegenParameter cp : formParams) {
                        allParams.add(cp.copy());
                    }
                }
            } else {
                // process body parameter
                requestBody = ModelUtils.getReferencedRequestBody(this.openAPI, requestBody);

                String bodyParameterName = "";
                if (op.vendorExtensions != null && op.vendorExtensions.containsKey("x-codegen-request-body-name")) {
                    bodyParameterName = (String) op.vendorExtensions.get("x-codegen-request-body-name");
                }
                bodyParam = fromRequestBody(requestBody, imports, bodyParameterName);
                bodyParam.description = escapeText(requestBody.getDescription());
                postProcessParameter(bodyParam);

                bodyParams.add(bodyParam);

                if (prependFormOrBodyParameters) {
                    allParams.add(bodyParam);
                }

                // add example
                if (schemas != null) {
                    op.requestBodyExamples = new ExampleGenerator(schemas, this.openAPI).generate(null, new ArrayList<String>(getConsumesInfo(this.openAPI, operation)), bodyParam.baseType);
                }
            }
        }

        if (parameters != null) {
            for (Parameter param : parameters) {
                param = ModelUtils.getReferencedParameter(this.openAPI, param);

                CodegenParameter p = fromParameter(param, imports);

                // ensure unique params
                if (ensureUniqueParams) {
                    if (!isParameterNameUnique(p, allParams)) {
                        p.paramName = generateNextName(p.paramName);
                    }
                }

                allParams.add(p);

                if (param instanceof QueryParameter || "query".equalsIgnoreCase(param.getIn())) {
                    queryParams.add(p.copy());
                } else if (param instanceof PathParameter || "path".equalsIgnoreCase(param.getIn())) {
                    pathParams.add(p.copy());
                } else if (param instanceof HeaderParameter || "header".equalsIgnoreCase(param.getIn())) {
                    headerParams.add(p.copy());
                } else if (param instanceof CookieParameter || "cookie".equalsIgnoreCase(param.getIn())) {
                    cookieParams.add(p.copy());
                } else {
                    LOGGER.warn("Unknown parameter type " + p.baseType + " for " + p.baseName);
                }

            }
        }

        // add form/body parameter (if any) to the end of all parameter list
        if (!prependFormOrBodyParameters) {
            for (CodegenParameter cp : formParams) {
                allParams.add(cp.copy());
            }

            for (CodegenParameter cp : bodyParams) {
                allParams.add(cp.copy());
            }
        }

        // create optional, required parameters
        for (CodegenParameter cp : allParams) {
            if (cp.required) { //required parameters
                requiredParams.add(cp.copy());
            } else { // optional parameters
                optionalParams.add(cp.copy());
                op.hasOptionalParams = true;
            }
        }

        // add imports to operation import tag
        for (String i : imports) {
            if (needToImport(i)) {
                op.imports.add(i);
            }
        }

        op.bodyParam = bodyParam;
        op.httpMethod = httpMethod.toUpperCase(Locale.ROOT);

        // move "required" parameters in front of "optional" parameters
        if (sortParamsByRequiredFlag) {
            Collections.sort(allParams, new Comparator<CodegenParameter>() {
                @Override
                public int compare(CodegenParameter one, CodegenParameter another) {
                    if (one.required == another.required)
                        return 0;
                    else if (one.required)
                        return -1;
                    else
                        return 1;
                }
            });
        }

        op.allParams = addHasMore(allParams);
        op.bodyParams = addHasMore(bodyParams);
        op.pathParams = addHasMore(pathParams);
        op.queryParams = addHasMore(queryParams);
        op.headerParams = addHasMore(headerParams);
        op.cookieParams = addHasMore(cookieParams);
        op.formParams = addHasMore(formParams);
        op.requiredParams = addHasMore(requiredParams);
        op.optionalParams = addHasMore(optionalParams);
        op.externalDocs = operation.getExternalDocs();
        // legacy support
        op.nickname = op.operationId;

        if (op.allParams.size() > 0) {
            op.hasParams = true;
        }
        op.hasRequiredParams = op.requiredParams.size() > 0;

        // set Restful Flag
        op.isRestfulShow = op.isRestfulShow();
        op.isRestfulIndex = op.isRestfulIndex();
        op.isRestfulCreate = op.isRestfulCreate();
        op.isRestfulUpdate = op.isRestfulUpdate();
        op.isRestfulDestroy = op.isRestfulDestroy();
        op.isRestful = op.isRestful();

        return op;
    }

    public boolean isParameterNameUnique(CodegenParameter p, List<CodegenParameter> parameters) {
        for (CodegenParameter parameter : parameters) {
            if (System.identityHashCode(p) == System.identityHashCode(parameter)) {
                continue; // skip itself
            }

            if (p.paramName.equals(parameter.paramName)) {
                return false;
            }
        }

        return true;
    }

    /**
     * Convert OAS Response object to Codegen Response object
     *
     * @param responseCode HTTP response code
     * @param response     OAS Response object
     * @return Codegen Response object
     */
    public CodegenResponse fromResponse(String responseCode, ApiResponse response) {
        CodegenResponse r = CodegenModelFactory.newInstance(CodegenModelType.RESPONSE);

        if (response.getContent() != null && response.getContent().size() > 0) {
            // Ensure validation properties from a target schema are persisted on CodegenResponse.
            // This ignores any edge case where different schemas have different validations because we don't
            // have a way to indicate a preference for response schema and are effective 1:1.
            Schema contentSchema = null;
            for (MediaType mt : response.getContent().values()) {
                if (contentSchema != null) break;
                contentSchema = mt.getSchema();
            }
            if (contentSchema != null) {
                ModelUtils.syncValidationProperties(contentSchema, r);
            }
        }

        if ("default".equals(responseCode) || "defaultResponse".equals(responseCode)) {
            r.code = "0";
            r.isDefault = true;
        } else {
            r.code = responseCode;
            switch (r.code.charAt(0)) {
                case '1':
                    r.is1xx = true;
                    break;
                case '2':
                    r.is2xx = true;
                    break;
                case '3':
                    r.is3xx = true;
                    break;
                case '4':
                    r.is4xx = true;
                    break;
                case '5':
                    r.is5xx = true;
                    break;
                default:
                    throw new RuntimeException("Invalid response code " + responseCode);
            }
        }
        Schema responseSchema;
        if (this.openAPI != null && this.openAPI.getComponents() != null) {
            responseSchema = ModelUtils.unaliasSchema(this.openAPI, ModelUtils.getSchemaFromResponse(response),
                    importMapping);
        } else { // no model/alias defined
            responseSchema = ModelUtils.getSchemaFromResponse(response);
        }
        r.schema = responseSchema;
        if (responseSchema != null && responseSchema.getPattern() != null) {
            r.setPattern(toRegularExpression(responseSchema.getPattern()));
        }

        r.message = escapeText(response.getDescription());
        // TODO need to revise and test examples in responses
        // ApiResponse does not support examples at the moment
        //r.examples = toExamples(response.getExamples());
        r.jsonSchema = Json.pretty(response);
        if (response.getExtensions() != null && !response.getExtensions().isEmpty()) {
            r.vendorExtensions.putAll(response.getExtensions());
        }
        addHeaders(response, r.headers);
        r.hasHeaders = !r.headers.isEmpty();

        if (r.schema != null) {
            Map<String, Schema> allSchemas = null;
            CodegenProperty cp = fromProperty("response", responseSchema);

            if (ModelUtils.isArraySchema(responseSchema)) {
                ArraySchema as = (ArraySchema) responseSchema;
                CodegenProperty innerProperty = fromProperty("response", getSchemaItems(as));
                CodegenProperty innerCp = innerProperty;
                while (innerCp != null) {
                    r.baseType = innerCp.baseType;
                    innerCp = innerCp.items;
                }
            } else {
                if (cp.complexType != null) {
                    r.baseType = cp.complexType;
                    r.isModel = true;
                } else {
                    r.baseType = cp.baseType;
                }
            }

            r.dataType = cp.dataType;
            if (Boolean.TRUE.equals(cp.isString) && Boolean.TRUE.equals(cp.isEmail)) {
                r.isEmail = true;
            } else if (Boolean.TRUE.equals(cp.isString) && Boolean.TRUE.equals(cp.isUuid)) {
                r.isUuid = true;
            } else if (Boolean.TRUE.equals(cp.isByteArray)) {
                r.isByteArray = true;
            } else if (Boolean.TRUE.equals(cp.isString)) {
                r.isString = true;
            } else if (Boolean.TRUE.equals(cp.isBoolean)) {
                r.isBoolean = true;
            } else if (Boolean.TRUE.equals(cp.isLong)) {
                r.isLong = true;
                r.isNumeric = true;
            } else if (Boolean.TRUE.equals(cp.isInteger)) {
                r.isInteger = true;
                r.isNumeric = true;
            } else if (Boolean.TRUE.equals(cp.isNumber)) {
                r.isNumber = true;
                r.isNumeric = true;
            } else if (Boolean.TRUE.equals(cp.isDouble)) {
                r.isDouble = true;
                r.isNumeric = true;
            } else if (Boolean.TRUE.equals(cp.isFloat)) {
                r.isFloat = true;
                r.isNumeric = true;
            } else if (Boolean.TRUE.equals(cp.isBinary)) {
                r.isFile = true; // file = binary in OAS3
                r.isBinary = true;
            } else if (Boolean.TRUE.equals(cp.isFile)) {
                r.isFile = true;
            } else if (Boolean.TRUE.equals(cp.isDate)) {
                r.isDate = true;
            } else if (Boolean.TRUE.equals(cp.isDateTime)) {
                r.isDateTime = true;
            } else if (Boolean.TRUE.equals(cp.isFreeFormObject)) {
                r.isFreeFormObject = true;
            } else if (Boolean.TRUE.equals(cp.isAnyType)) {
                r.isAnyType = true;
            } else {
                LOGGER.debug("Property type is not primitive: " + cp.dataType);
            }

            if (cp.isContainer) {
                r.simpleType = false;
                r.containerType = cp.containerType;
                r.isMapContainer = "map".equals(cp.containerType);
                r.isListContainer = "list".equalsIgnoreCase(cp.containerType) ||
                    "array".equalsIgnoreCase(cp.containerType) ||
                    "set".equalsIgnoreCase(cp.containerType);
            } else {
                r.simpleType = true;
            }

            r.primitiveType = (r.baseType == null || languageSpecificPrimitives().contains(r.baseType));
        }

        if (r.baseType == null) {
            r.isMapContainer = false;
            r.isListContainer = false;
            r.primitiveType = true;
            r.simpleType = true;
        }

        return r;
    }

    /**
     * Convert OAS Callback object to Codegen Callback object
     *
     * @param name     callback name
     * @param callback OAS Callback object
     * @param servers  list of servers
     * @return Codegen Response object
     */
    public CodegenCallback fromCallback(String name, Callback callback, List<Server> servers) {
        CodegenCallback c = new CodegenCallback();
        c.name = name;

        if (callback.getExtensions() != null && !callback.getExtensions().isEmpty()) {
            c.vendorExtensions.putAll(callback.getExtensions());
        }

        callback.forEach((expression, pi) -> {
            CodegenCallback.Url u = new CodegenCallback.Url();
            u.expression = expression;
            u.hasMore = true;

            if (pi.getExtensions() != null && !pi.getExtensions().isEmpty()) {
                u.vendorExtensions.putAll(pi.getExtensions());
            }

            Stream.of(
                    Pair.of("get", pi.getGet()),
                    Pair.of("head", pi.getHead()),
                    Pair.of("put", pi.getPut()),
                    Pair.of("post", pi.getPost()),
                    Pair.of("delete", pi.getDelete()),
                    Pair.of("patch", pi.getPatch()),
                    Pair.of("options", pi.getOptions()))
                    .filter(p -> p.getValue() != null)
                    .forEach(p -> {
                        String method = p.getKey();
                        Operation op = p.getValue();

                        boolean genId = op.getOperationId() == null;
                        if (genId) {
                            op.setOperationId(getOrGenerateOperationId(op, c.name + "_" + expression.replaceAll("\\{\\$.*}", ""), method));
                        }

                        if (op.getExtensions() == null) {
                            op.setExtensions(new HashMap<>());
                        }
                        // This extension will be removed later by `fromOperation()` as it is only needed here to
                        // distinguish between normal operations and callback requests
                        op.getExtensions().put("x-callback-request", true);

                        CodegenOperation co = fromOperation(expression, method, op, servers);
                        if (genId) {
                            co.operationIdOriginal = null;
                            // legacy (see `fromOperation()`)
                            co.nickname = co.operationId;
                        }
                        u.requests.add(co);
                    });

            if (!u.requests.isEmpty()) {
                u.requests.get(u.requests.size() - 1).hasMore = false;
            }
            c.urls.add(u);
        });

        if (!c.urls.isEmpty()) {
            c.urls.get(c.urls.size() - 1).hasMore = false;
        }

        return c;
    }

    /**
     * Convert OAS Parameter object to Codegen Parameter object
     *
     * @param parameter OAS parameter object
     * @param imports   set of imports for library/package/module
     * @return Codegen Parameter object
     */
    public CodegenParameter fromParameter(Parameter parameter, Set<String> imports) {
        CodegenParameter codegenParameter = CodegenModelFactory.newInstance(CodegenModelType.PARAMETER);

        if (parameter.getContent() != null && parameter.getContent().size() > 0) {
            // Ensure validation properties from a target schema are persisted on CodegenParameter.
            // This ignores any edge case where different schemas have different validations because we don't
            // have a way to indicate a preference for parameter schema and are effective 1:1.
            Schema contentSchema = null;
            for (MediaType mt : parameter.getContent().values()) {
                if (contentSchema != null) break;
                contentSchema = mt.getSchema();
            }
            if (contentSchema != null) {
                ModelUtils.syncValidationProperties(contentSchema, codegenParameter);
            }
        }

        codegenParameter.baseName = parameter.getName();
        codegenParameter.description = escapeText(parameter.getDescription());
        codegenParameter.unescapedDescription = parameter.getDescription();
        if (parameter.getRequired() != null) {
            codegenParameter.required = parameter.getRequired();
        }
        codegenParameter.jsonSchema = Json.pretty(parameter);

        if (GlobalSettings.getProperty("debugParser") != null) {
            LOGGER.info("working on Parameter " + parameter.getName());
            LOGGER.info("JSON schema: " + codegenParameter.jsonSchema);
        }

        if (parameter.getExtensions() != null && !parameter.getExtensions().isEmpty()) {
            codegenParameter.vendorExtensions.putAll(parameter.getExtensions());
        }

        Schema parameterSchema;
        if (parameter.getSchema() != null) {
            parameterSchema = parameter.getSchema();
        } else if (parameter.getContent() != null) {
            Content content = parameter.getContent();
            if (content.size() > 1) {
                once(LOGGER).warn("Multiple schemas found in content, returning only the first one");
            }
            Map.Entry<String, MediaType> entry = content.entrySet().iterator().next();
            codegenParameter.contentType = entry.getKey();
            parameterSchema = entry.getValue().getSchema();
        } else {
            parameterSchema = null;
        }

        if (parameterSchema != null) {
            parameterSchema = ModelUtils.unaliasSchema(this.openAPI, parameterSchema);
            if (parameterSchema == null) {
                LOGGER.warn("warning!  Schema not found for parameter \"" + parameter.getName() + "\", using String");
                parameterSchema = new StringSchema().description("//TODO automatically added by openapi-generator due to missing type definition.");
            }

            if (Boolean.TRUE.equals(parameterSchema.getNullable())) { // use nullable defined in the spec
                codegenParameter.isNullable = true;
            }

            // set default value
            codegenParameter.defaultValue = toDefaultValue(parameterSchema);

            if (parameter.getStyle() != null) {
                codegenParameter.style = parameter.getStyle().toString();
            }

            // the default value is false
            // https://github.com/OAI/OpenAPI-Specification/blob/master/versions/3.0.2.md#user-content-parameterexplode
            codegenParameter.isExplode = parameter.getExplode() == null ? false : parameter.getExplode();

            // TODO revise collectionFormat
            String collectionFormat = null;
            if (ModelUtils.isArraySchema(parameterSchema)) { // for array parameter
                final ArraySchema arraySchema = (ArraySchema) parameterSchema;
                Schema inner = getSchemaItems(arraySchema);

                collectionFormat = getCollectionFormat(parameter);
                // default to csv:
                collectionFormat = StringUtils.isEmpty(collectionFormat) ? "csv" : collectionFormat;
                CodegenProperty codegenProperty = fromProperty("inner", inner);
                codegenParameter.items = codegenProperty;
                codegenParameter.mostInnerItems = codegenProperty.mostInnerItems;
                codegenParameter.baseType = codegenProperty.dataType;
                codegenParameter.isContainer = true;
                codegenParameter.isListContainer = true;

                // recursively add import
                while (codegenProperty != null) {
                    imports.add(codegenProperty.baseType);
                    codegenProperty = codegenProperty.items;
                }

            } else if (ModelUtils.isMapSchema(parameterSchema)) { // for map parameter
                CodegenProperty codegenProperty = fromProperty("inner", getAdditionalProperties(parameterSchema));
                codegenParameter.items = codegenProperty;
                codegenParameter.mostInnerItems = codegenProperty.mostInnerItems;
                codegenParameter.baseType = codegenProperty.dataType;
                codegenParameter.isContainer = true;
                codegenParameter.isMapContainer = true;

                // recursively add import
                while (codegenProperty != null) {
                    imports.add(codegenProperty.baseType);
                    codegenProperty = codegenProperty.items;
                }
            }
/* TODO revise the logic below
            } else {
                Map<PropertyId, Object> args = new HashMap<PropertyId, Object>();
                String format = qp.getFormat();
                args.put(PropertyId.ENUM, qp.getEnum());
                property = PropertyBuilder.build(type, format, args);
            }
*/

            CodegenProperty codegenProperty = fromProperty(parameter.getName(), parameterSchema);
            // TODO revise below which seems not working
            //if (parameterSchema.getRequired() != null && !parameterSchema.getRequired().isEmpty() && parameterSchema.getRequired().contains(codegenProperty.baseName)) {
            codegenProperty.required = Boolean.TRUE.equals(parameter.getRequired()) ? true : false;
            //}
            //codegenProperty.required = true;

            // set boolean flag (e.g. isString)
            setParameterBooleanFlagWithCodegenProperty(codegenParameter, codegenProperty);

            String parameterDataType = this.getParameterDataType(parameter, parameterSchema);
            if (parameterDataType != null) {
                codegenParameter.dataType = parameterDataType;
            } else {
                codegenParameter.dataType = codegenProperty.dataType;
            }
            if (ModelUtils.isObjectSchema(parameterSchema)) {
                codegenProperty.complexType = codegenParameter.dataType;
            }
            if (ModelUtils.isSet(parameterSchema)) {
                imports.add(codegenProperty.baseType);
            }
            codegenParameter.dataFormat = codegenProperty.dataFormat;
            codegenParameter.required = codegenProperty.required;

            if (codegenProperty.isEnum) {
                codegenParameter.datatypeWithEnum = codegenProperty.datatypeWithEnum;
                codegenParameter.enumName = codegenProperty.enumName;
            }

            // enum
            updateCodegenPropertyEnum(codegenProperty);
            codegenParameter.isEnum = codegenProperty.isEnum;
            codegenParameter._enum = codegenProperty._enum;
            codegenParameter.allowableValues = codegenProperty.allowableValues;

            if (codegenProperty.items != null && codegenProperty.items.isEnum) {
                codegenParameter.datatypeWithEnum = codegenProperty.datatypeWithEnum;
                codegenParameter.enumName = codegenProperty.enumName;
                codegenParameter.items = codegenProperty.items;
                codegenParameter.mostInnerItems = codegenProperty.mostInnerItems;
            }

            codegenParameter.collectionFormat = collectionFormat;
            if ("multi".equals(collectionFormat)) {
                codegenParameter.isCollectionFormatMulti = true;
            }
            codegenParameter.paramName = toParamName(parameter.getName());

            // import
            if (codegenProperty.complexType != null) {
                imports.add(codegenProperty.complexType);
            }

            // validation
            // handle maximum, minimum properly for int/long by removing the trailing ".0"
            if (ModelUtils.isIntegerSchema(parameterSchema)) {
                codegenParameter.maximum = parameterSchema.getMaximum() == null ? null : String.valueOf(parameterSchema.getMaximum().longValue());
                codegenParameter.minimum = parameterSchema.getMinimum() == null ? null : String.valueOf(parameterSchema.getMinimum().longValue());
            } else {
                codegenParameter.maximum = parameterSchema.getMaximum() == null ? null : String.valueOf(parameterSchema.getMaximum());
                codegenParameter.minimum = parameterSchema.getMinimum() == null ? null : String.valueOf(parameterSchema.getMinimum());
            }

            codegenParameter.exclusiveMaximum = parameterSchema.getExclusiveMaximum() == null ? false : parameterSchema.getExclusiveMaximum();
            codegenParameter.exclusiveMinimum = parameterSchema.getExclusiveMinimum() == null ? false : parameterSchema.getExclusiveMinimum();
            codegenParameter.maxLength = parameterSchema.getMaxLength();
            codegenParameter.minLength = parameterSchema.getMinLength();
            codegenParameter.pattern = toRegularExpression(parameterSchema.getPattern());
            codegenParameter.maxItems = parameterSchema.getMaxItems();
            codegenParameter.minItems = parameterSchema.getMinItems();
            codegenParameter.uniqueItems = parameterSchema.getUniqueItems() == null ? false : parameterSchema.getUniqueItems();
            codegenParameter.multipleOf = parameterSchema.getMultipleOf();

            // exclusive* are noop without corresponding min/max
            if (codegenParameter.maximum != null || codegenParameter.minimum != null ||
                    codegenParameter.maxLength != null || codegenParameter.minLength != null ||
                    codegenParameter.maxItems != null || codegenParameter.minItems != null ||
                    codegenParameter.pattern != null || codegenParameter.multipleOf != null) {
                codegenParameter.hasValidation = true;
            }

        } else {
            LOGGER.error("ERROR! Not handling  " + parameter + " as Body Parameter at the moment");
        }

        if (parameter instanceof QueryParameter || "query".equalsIgnoreCase(parameter.getIn())) {
            codegenParameter.isQueryParam = true;
        } else if (parameter instanceof PathParameter || "path".equalsIgnoreCase(parameter.getIn())) {
            codegenParameter.required = true;
            codegenParameter.isPathParam = true;
        } else if (parameter instanceof HeaderParameter || "header".equalsIgnoreCase(parameter.getIn())) {
            codegenParameter.isHeaderParam = true;
        } else if (parameter instanceof CookieParameter || "cookie".equalsIgnoreCase(parameter.getIn())) {
            codegenParameter.isCookieParam = true;
        } else {
            LOGGER.warn("Unknown parameter type: " + parameter.getName());
        }

        // default to UNKNOWN_PARAMETER_NAME if paramName is null
        if (codegenParameter.paramName == null) {
            LOGGER.warn("Parameter name not defined properly. Default to UNKNOWN_PARAMETER_NAME");
            codegenParameter.paramName = "UNKNOWN_PARAMETER_NAME";
        }

        // set the parameter example value
        // should be overridden by lang codegen
        setParameterExampleValue(codegenParameter, parameter);

        postProcessParameter(codegenParameter);
        LOGGER.debug("debugging codegenParameter return: " + codegenParameter);
        return codegenParameter;
    }

    /**
     * Returns the data type of a parameter.
     * Returns null by default to use the CodegenProperty.datatype value
     *
     * @param parameter Parameter
     * @param schema    Schema
     * @return data type
     */
    protected String getParameterDataType(Parameter parameter, Schema schema) {
        if (parameter.get$ref() != null) {
            String refName = ModelUtils.getSimpleRef(parameter.get$ref());
            return toModelName(refName);
        }
        return null;
    }

    // TODO revise below as it should be replaced by ModelUtils.isByteArraySchema(parameterSchema)
    public boolean isDataTypeBinary(String dataType) {
        if (dataType != null) {
            return dataType.toLowerCase(Locale.ROOT).startsWith("byte");
        } else {
            return false;
        }
    }
    
    // TODO revise below as it should be replaced by ModelUtils.isFileSchema(parameterSchema)
    public boolean isDataTypeFile(String dataType) {
        if (dataType != null) {
            return dataType.toLowerCase(Locale.ROOT).equals("file");
        } else {
            return false;
        }
    }

    /**
     * Convert map of OAS SecurityScheme objects to a list of Codegen Security objects
     *
     * @param securitySchemeMap a map of OAS SecuritySchemeDefinition object
     * @return a list of Codegen Security objects
     */
    @SuppressWarnings("static-method")
    public List<CodegenSecurity> fromSecurity(Map<String, SecurityScheme> securitySchemeMap) {
        if (securitySchemeMap == null) {
            return Collections.emptyList();
        }

        List<CodegenSecurity> codegenSecurities = new ArrayList<CodegenSecurity>(securitySchemeMap.size());
        for (String key : securitySchemeMap.keySet()) {
            final SecurityScheme securityScheme = securitySchemeMap.get(key);

            CodegenSecurity cs = CodegenModelFactory.newInstance(CodegenModelType.SECURITY);
            cs.name = key;
            cs.type = securityScheme.getType().toString();
            cs.isCode = cs.isPassword = cs.isApplication = cs.isImplicit = false;
            cs.isHttpSignature = false;
            cs.isBasicBasic = cs.isBasicBearer = false;
            cs.scheme = securityScheme.getScheme();
            if (securityScheme.getExtensions() != null) {
                cs.vendorExtensions.putAll(securityScheme.getExtensions());
            }

            if (SecurityScheme.Type.APIKEY.equals(securityScheme.getType())) {
                cs.isBasic = cs.isOAuth = false;
                cs.isApiKey = true;
                cs.keyParamName = securityScheme.getName();
                cs.isKeyInHeader = securityScheme.getIn() == SecurityScheme.In.HEADER;
                cs.isKeyInQuery = securityScheme.getIn() == SecurityScheme.In.QUERY;
                cs.isKeyInCookie = securityScheme.getIn() == SecurityScheme.In.COOKIE;  //it assumes a validation step prior to generation. (cookie-auth supported from OpenAPI 3.0.0)
            } else if (SecurityScheme.Type.HTTP.equals(securityScheme.getType())) {
                cs.isKeyInHeader = cs.isKeyInQuery = cs.isKeyInCookie = cs.isApiKey = cs.isOAuth = false;
                cs.isBasic = true;
                if ("basic".equals(securityScheme.getScheme())) {
                    cs.isBasicBasic = true;
                } else if ("bearer".equals(securityScheme.getScheme())) {
                    cs.isBasicBearer = true;
                    cs.bearerFormat = securityScheme.getBearerFormat();
                } else if ("signature".equals(securityScheme.getScheme())) {
                    // HTTP signature as defined in https://datatracker.ietf.org/doc/draft-cavage-http-signatures/
                    // The registry of security schemes is maintained by IANA.
                    // https://www.iana.org/assignments/http-authschemes/http-authschemes.xhtml
                    // As of January 2020, the "signature" scheme has not been registered with IANA yet.
                    // This scheme may have to be changed when it is officially registered with IANA.
                    cs.isHttpSignature = true;
                    once(LOGGER).warn("Security scheme 'HTTP signature' is a draft IETF RFC and subject to change.");
                }
            } else if (SecurityScheme.Type.OAUTH2.equals(securityScheme.getType())) {
                cs.isKeyInHeader = cs.isKeyInQuery = cs.isKeyInCookie = cs.isApiKey = cs.isBasic = false;
                cs.isOAuth = true;
                final OAuthFlows flows = securityScheme.getFlows();
                if (securityScheme.getFlows() == null) {
                    throw new RuntimeException("missing oauth flow in " + cs.name);
                }
                if (flows.getPassword() != null) {
                    setOauth2Info(cs, flows.getPassword());
                    cs.isPassword = true;
                    cs.flow = "password";
                } else if (flows.getImplicit() != null) {
                    setOauth2Info(cs, flows.getImplicit());
                    cs.isImplicit = true;
                    cs.flow = "implicit";
                } else if (flows.getClientCredentials() != null) {
                    setOauth2Info(cs, flows.getClientCredentials());
                    cs.isApplication = true;
                    cs.flow = "application";
                } else if (flows.getAuthorizationCode() != null) {
                    setOauth2Info(cs, flows.getAuthorizationCode());
                    cs.isCode = true;
                    cs.flow = "accessCode";
                } else {
                    throw new RuntimeException("Could not identify any oauth2 flow in " + cs.name);
                }
            }

            codegenSecurities.add(cs);
        }

        // sort auth methods to maintain the same order
        Collections.sort(codegenSecurities, new Comparator<CodegenSecurity>() {
            @Override
            public int compare(CodegenSecurity one, CodegenSecurity another) {
                return ObjectUtils.compare(one.name, another.name);
            }
        });
        // set 'hasMore'
        Iterator<CodegenSecurity> it = codegenSecurities.iterator();
        while (it.hasNext()) {
            final CodegenSecurity security = it.next();
            security.hasMore = it.hasNext();
        }

        return codegenSecurities;
    }

    protected void setReservedWordsLowerCase(List<String> words) {
        reservedWords = new HashSet<String>();
        for (String word : words) {
            reservedWords.add(word.toLowerCase(Locale.ROOT));
        }
    }

    protected boolean isReservedWord(String word) {
        return word != null && reservedWords.contains(word.toLowerCase(Locale.ROOT));
    }

    /**
     * Get operationId from the operation object, and if it's blank, generate a new one from the given parameters.
     *
     * @param operation  the operation object
     * @param path       the path of the operation
     * @param httpMethod the HTTP method of the operation
     * @return the (generated) operationId
     */
    protected String getOrGenerateOperationId(Operation operation, String path, String httpMethod) {
        String operationId = operation.getOperationId();
        if (StringUtils.isBlank(operationId)) {
            String tmpPath = path;
            tmpPath = tmpPath.replaceAll("\\{", "");
            tmpPath = tmpPath.replaceAll("\\}", "");
            String[] parts = (tmpPath + "/" + httpMethod).split("/");
            StringBuilder builder = new StringBuilder();
            if ("/".equals(tmpPath)) {
                // must be root tmpPath
                builder.append("root");
            }
            for (String part : parts) {
                if (part.length() > 0) {
                    if (builder.toString().length() == 0) {
                        part = Character.toLowerCase(part.charAt(0)) + part.substring(1);
                    } else {
                        part = camelize(part);
                    }
                    builder.append(part);
                }
            }
            operationId = sanitizeName(builder.toString());
            LOGGER.warn("Empty operationId found for path: " + httpMethod + " " + path + ". Renamed to auto-generated operationId: " + operationId);
        }
        return operationId;
    }

    /**
     * Check the type to see if it needs import the library/module/package
     *
     * @param type name of the type
     * @return true if the library/module/package of the corresponding type needs to be imported
     */
    protected boolean needToImport(String type) {
        return StringUtils.isNotBlank(type) && !defaultIncludes.contains(type)
                && !languageSpecificPrimitives.contains(type);
    }

    @SuppressWarnings("static-method")
    protected List<Map<String, Object>> toExamples(Map<String, Object> examples) {
        if (examples == null) {
            return null;
        }

        final List<Map<String, Object>> output = new ArrayList<Map<String, Object>>(examples.size());
        for (Map.Entry<String, Object> entry : examples.entrySet()) {
            final Map<String, Object> kv = new HashMap<String, Object>();
            kv.put("contentType", entry.getKey());
            kv.put("example", entry.getValue());
            output.add(kv);
        }
        return output;
    }

    /**
     * Add headers to codegen property
     *
     * @param response   API response
     * @param properties list of codegen property
     */
    protected void addHeaders(ApiResponse response, List<CodegenProperty> properties) {
        if (response.getHeaders() != null) {
            for (Map.Entry<String, Header> headerEntry : response.getHeaders().entrySet()) {
                String description = headerEntry.getValue().getDescription();
                // follow the $ref
                Header header = ModelUtils.getReferencedHeader(this.openAPI, headerEntry.getValue());

                Schema schema;
                if (header.getSchema() == null) {
                    LOGGER.warn("No schema defined for Header '" + headerEntry.getKey() + "', using a String schema");
                    schema = new StringSchema();
                } else {
                    schema = header.getSchema();
                }
                CodegenProperty cp = fromProperty(headerEntry.getKey(), schema);
                cp.setDescription(escapeText(description));
                cp.setUnescapedDescription(description);
                if (header.getRequired() != null) {
                    cp.setRequired(header.getRequired());
                } else {
                    cp.setRequired(false);
                }
                properties.add(cp);
            }
        }
    }

    private static List<CodegenParameter> addHasMore(List<CodegenParameter> objs) {
        if (objs != null) {
            for (int i = 0; i < objs.size(); i++) {
                if (i > 0) {
                    objs.get(i).secondaryParam = true;
                }
                if (i < objs.size() - 1) {
                    objs.get(i).hasMore = true;
                }
            }
        }
        return objs;
    }

    /**
     * Add operation to group
     *
     * @param tag          name of the tag
     * @param resourcePath path of the resource
     * @param operation    OAS Operation object
     * @param co           Codegen Operation object
     * @param operations   map of Codegen operations
     */
    @SuppressWarnings("static-method")
    public void addOperationToGroup(String tag, String resourcePath, Operation operation, CodegenOperation
            co, Map<String, List<CodegenOperation>> operations) {
        List<CodegenOperation> opList = operations.get(tag);
        if (opList == null) {
            opList = new ArrayList<CodegenOperation>();
            operations.put(tag, opList);
        }
        // check for operationId uniqueness
        String uniqueName = co.operationId;
        int counter = 0;
        for (CodegenOperation op : opList) {
            if (uniqueName.equals(op.operationId)) {
                uniqueName = co.operationId + "_" + counter;
                counter++;
            }
        }
        if (!co.operationId.equals(uniqueName)) {
            LOGGER.warn("generated unique operationId `" + uniqueName + "`");
        }
        co.operationId = uniqueName;
        co.operationIdLowerCase = uniqueName.toLowerCase(Locale.ROOT);
        co.operationIdCamelCase = camelize(uniqueName);
        co.operationIdSnakeCase = underscore(uniqueName);
        opList.add(co);
        co.baseName = tag;
    }

    /**
     * Sets the value of the 'model.parent' property in CodegenModel, based on the value
     * of the 'additionalProperties' keyword. Some language generator use class inheritance
     * to implement additional properties. For example, in Java the generated model class
     * has 'extends HashMap' to represent the additional properties.
     * 
     * TODO: it's not a good idea to use single class inheritance to implement
     * additionalProperties. That may work for non-composed schemas, but that does not
     * work for composed 'allOf' schemas. For example, in Java, if additionalProperties
     * is set to true (which it should be by default, per OAS spec), then the generated
     * code has extends HashMap. That wouldn't work for composed 'allOf' schemas.
     * 
     * @param model the codegen representation of the OAS schema.
     * @param name the name of the model.
     * @param schema the input OAS schema.
     */
    protected void addParentContainer(CodegenModel model, String name, Schema schema) {
        final CodegenProperty property = fromProperty(name, schema);
        addImport(model, property.complexType);
        model.parent = toInstantiationType(schema);
        final String containerType = property.containerType;
        final String instantiationType = instantiationTypes.get(containerType);
        if (instantiationType != null) {
            addImport(model, instantiationType);
        }

        final String mappedType = typeMapping.get(containerType);
        if (mappedType != null) {
            addImport(model, mappedType);
        }
    }

    /**
     * Generate the next name for the given name, i.e. append "2" to the base name if not ending with a number,
     * otherwise increase the number by 1. For example:
     * status    => status2
     * status2   => status3
     * myName100 => myName101
     *
     * @param name The base name
     * @return The next name for the base name
     */
    private static String generateNextName(String name) {
        Pattern pattern = Pattern.compile("\\d+\\z");
        Matcher matcher = pattern.matcher(name);
        if (matcher.find()) {
            String numStr = matcher.group();
            int num = Integer.parseInt(numStr) + 1;
            return name.substring(0, name.length() - numStr.length()) + num;
        } else {
            return name + "2";
        }
    }

    protected void addImport(CodegenModel m, String type) {
        if (type != null && needToImport(type)) {
            m.imports.add(type);
        }
    }

    /**
     * Loop through propertiies and unalias the reference if $ref (reference) is defined
     *
     * @param properties model properties (schemas)
     * @return model properties with direct reference to schemas
     */
    private Map<String, Schema> unaliasPropertySchema(Map<String, Schema> properties) {
        if (properties != null) {
            for (String key : properties.keySet()) {
                properties.put(key, ModelUtils.unaliasSchema(this.openAPI, properties.get(key), importMapping()));

            }
        }

        return properties;
    }

    private void addVars(CodegenModel m, Map<String, Schema> properties, List<String> required,
                         Map<String, Schema> allProperties, List<String> allRequired) {

        m.hasRequired = false;
        if (properties != null && !properties.isEmpty()) {
            m.hasVars = true;
            m.hasEnums = false; // TODO need to fix as its false in both cases

            Set<String> mandatory = required == null ? Collections.<String>emptySet()
                    : new TreeSet<String>(required);

            // update "vars" without parent's properties (all, required)
            addVars(m, m.vars, properties, mandatory);
            m.allMandatory = m.mandatory = mandatory;
        } else {
            m.emptyVars = true;
            m.hasVars = false;
            m.hasEnums = false; // TODO need to fix as its false in both cases
        }

        if (allProperties != null) {
            Set<String> allMandatory = allRequired == null ? Collections.<String>emptySet()
                    : new TreeSet<String>(allRequired);
            // update "vars" with parent's properties (all, required)
            addVars(m, m.allVars, allProperties, allMandatory);
            m.allMandatory = allMandatory;
        } else { // without parent, allVars and vars are the same
            m.allVars = m.vars;
            m.allMandatory = m.mandatory;
        }

        // loop through list to update property name with toVarName
        Set<String> renamedMandatory = new TreeSet<String>();
        Iterator<String> mandatoryIterator = m.mandatory.iterator();
        while (mandatoryIterator.hasNext()) {
            renamedMandatory.add(toVarName(mandatoryIterator.next()));
        }
        m.mandatory = renamedMandatory;

        Set<String> renamedAllMandatory = new TreeSet<String>();
        Iterator<String> allMandatoryIterator = m.allMandatory.iterator();
        while (allMandatoryIterator.hasNext()) {
            renamedAllMandatory.add(toVarName(allMandatoryIterator.next()));
        }
        m.allMandatory = renamedAllMandatory;
    }

    /**
     * Add variables (properties) to codegen model (list of properties, various flags, etc)
     *
     * @param m          Codegen model
     * @param vars       list of codegen properties (e.g. vars, allVars) to be updated with the new properties
     * @param properties a map of properties (schema)
     * @param mandatory  a set of required properties' name
     */
    private void addVars(CodegenModel m, List<CodegenProperty> vars, Map<String, Schema> properties, Set<String> mandatory) {
        for (Map.Entry<String, Schema> entry : properties.entrySet()) {

            final String key = entry.getKey();
            final Schema prop = entry.getValue();

            if (prop == null) {
                LOGGER.warn("Please report the issue. There shouldn't be null property for " + key);
            } else {
                final CodegenProperty cp = fromProperty(key, prop);
                cp.required = mandatory.contains(key);
                m.hasRequired = m.hasRequired || cp.required;
                m.hasOptional = m.hasOptional || !cp.required;
                if (cp.isEnum) {
                    // FIXME: if supporting inheritance, when called a second time for allProperties it is possible for
                    // m.hasEnums to be set incorrectly if allProperties has enumerations but properties does not.
                    m.hasEnums = true;
                }

                // set model's hasOnlyReadOnly to false if the property is read-only
                if (!Boolean.TRUE.equals(cp.isReadOnly)) {
                    m.hasOnlyReadOnly = false;
                }

                // TODO revise the logic to include map
                if (cp.isContainer) {
                    addImport(m, typeMapping.get("array"));
                }

                addImport(m, cp.baseType);
                CodegenProperty innerCp = cp;
                while (innerCp != null) {
                    addImport(m, innerCp.complexType);
                    innerCp = innerCp.items;
                }
                vars.add(cp);

                // if required, add to the list "requiredVars"
                if (Boolean.TRUE.equals(cp.required)) {
                    m.requiredVars.add(cp);
                } else { // else add to the list "optionalVars" for optional property
                    m.optionalVars.add(cp);
                }

                // if readonly, add to readOnlyVars (list of properties)
                if (Boolean.TRUE.equals(cp.isReadOnly)) {
                    m.readOnlyVars.add(cp);
                } else { // else add to readWriteVars (list of properties)
                    // duplicated properties will be removed by removeAllDuplicatedProperty later
                    m.readWriteVars.add(cp);
                }
            }
        }
    }

    /**
     * Determine all of the types in the model definitions (schemas) that are aliases of
     * simple types.
     *
     * @param schemas The complete set of model definitions (schemas).
     * @return A mapping from model name to type alias
     */
    Map<String, String> getAllAliases(Map<String, Schema> schemas) {
        if (schemas == null || schemas.isEmpty()) {
            return new HashMap<>();
        }

        Map<String, String> aliases = new HashMap<>();
        for (Map.Entry<String, Schema> entry : schemas.entrySet()) {
            Schema schema = entry.getValue();
            if (isAliasOfSimpleTypes(schema)) {
                String oasName = entry.getKey();
                String schemaType = getPrimitiveType(schema);
                aliases.put(oasName, schemaType);
            }

        }

        return aliases;
    }

    private static Boolean isAliasOfSimpleTypes(Schema schema) {
        return (!ModelUtils.isObjectSchema(schema)
                && !ModelUtils.isArraySchema(schema)
                && !ModelUtils.isMapSchema(schema)
                && !ModelUtils.isComposedSchema(schema)
                && schema.getEnum() == null);
    }

    /**
     * Remove characters not suitable for variable or method name from the input and camelize it
     *
     * @param name string to be camelize
     * @return camelized string
     */
    @SuppressWarnings("static-method")
    public String removeNonNameElementToCamelCase(String name) {
        return removeNonNameElementToCamelCase(name, "[-_:;#]");
    }

    /**
     * Remove characters that is not good to be included in method name from the input and camelize it
     *
     * @param name                  string to be camelize
     * @param nonNameElementPattern a regex pattern of the characters that is not good to be included in name
     * @return camelized string
     */
    protected String removeNonNameElementToCamelCase(final String name, final String nonNameElementPattern) {
        String result = Arrays.stream(name.split(nonNameElementPattern))
                .map(StringUtils::capitalize)
                .collect(Collectors.joining(""));
        if (result.length() > 0) {
            result = result.substring(0, 1).toLowerCase(Locale.ROOT) + result.substring(1);
        }
        return result;
    }

    public String apiFilename(String templateName, String tag) {
        String suffix = apiTemplateFiles().get(templateName);
        return apiFileFolder() + File.separator + toApiFilename(tag) + suffix;
    }

    public String modelFilename(String templateName, String modelName) {
        String suffix = modelTemplateFiles().get(templateName);
        return modelFileFolder() + File.separator + toModelFilename(modelName) + suffix;
    }

    /**
     * Return the full path and API documentation file
     *
     * @param templateName template name
     * @param tag          tag
     * @return the API documentation file name with full path
     */
    public String apiDocFilename(String templateName, String tag) {
        String docExtension = getDocExtension();
        String suffix = docExtension != null ? docExtension : apiDocTemplateFiles().get(templateName);
        return apiDocFileFolder() + File.separator + toApiDocFilename(tag) + suffix;
    }

    /**
     * Return the full path and API test file
     *
     * @param templateName template name
     * @param tag          tag
     * @return the API test file name with full path
     */
    public String apiTestFilename(String templateName, String tag) {
        String suffix = apiTestTemplateFiles().get(templateName);
        return apiTestFileFolder() + File.separator + toApiTestFilename(tag) + suffix;
    }

    public boolean shouldOverwrite(String filename) {
        return !(skipOverwrite && new File(filename).exists());
    }

    public boolean isSkipOverwrite() {
        return skipOverwrite;
    }

    public void setSkipOverwrite(boolean skipOverwrite) {
        this.skipOverwrite = skipOverwrite;
    }

    public boolean isRemoveOperationIdPrefix() {
        return removeOperationIdPrefix;
    }

    public void setRemoveOperationIdPrefix(boolean removeOperationIdPrefix) {
        this.removeOperationIdPrefix = removeOperationIdPrefix;
    }

    public boolean isHideGenerationTimestamp() {
        return hideGenerationTimestamp;
    }

    public void setHideGenerationTimestamp(boolean hideGenerationTimestamp) {
        this.hideGenerationTimestamp = hideGenerationTimestamp;
    }

    /**
     * All library templates supported.
     * (key: library name, value: library description)
     *
     * @return the supported libraries
     */
    public Map<String, String> supportedLibraries() {
        return supportedLibraries;
    }

    /**
     * Set library template (sub-template).
     *
     * @param library Library template
     */
    public void setLibrary(String library) {
        if (library != null && !supportedLibraries.containsKey(library)) {
            StringBuilder sb = new StringBuilder("Unknown library: " + library + "\nAvailable libraries:");
            if (supportedLibraries.size() == 0) {
                sb.append("\n  ").append("NONE");
            } else {
                for (String lib : supportedLibraries.keySet()) {
                    sb.append("\n  ").append(lib);
                }
            }
            throw new RuntimeException(sb.toString());
        }
        this.library = library;
    }

    /**
     * Library template (sub-template).
     *
     * @return Library template
     */
    public String getLibrary() {
        return library;
    }

    /**
     * Set Git host.
     *
     * @param gitHost Git host
     */
    public void setGitHost(String gitHost) {
        this.gitHost = gitHost;
    }

    /**
     * Git host.
     *
     * @return Git host
     */
    public String getGitHost() {
        return gitHost;
    }

    /**
     * Set Git user ID.
     *
     * @param gitUserId Git user ID
     */
    public void setGitUserId(String gitUserId) {
        this.gitUserId = gitUserId;
    }

    /**
     * Git user ID
     *
     * @return Git user ID
     */
    public String getGitUserId() {
        return gitUserId;
    }

    /**
     * Set Git repo ID.
     *
     * @param gitRepoId Git repo ID
     */
    public void setGitRepoId(String gitRepoId) {
        this.gitRepoId = gitRepoId;
    }

    /**
     * Git repo ID
     *
     * @return Git repo ID
     */
    public String getGitRepoId() {
        return gitRepoId;
    }

    /**
     * Set release note.
     *
     * @param releaseNote Release note
     */
    public void setReleaseNote(String releaseNote) {
        this.releaseNote = releaseNote;
    }

    /**
     * Release note
     *
     * @return Release note
     */
    public String getReleaseNote() {
        return releaseNote;
    }

    /**
     * Documentation files extension
     *
     * @return Documentation files extension
     */
    public String getDocExtension() {
        return docExtension;
    }

    /**
     * Set Documentation files extension
     *
     * @param userDocExtension documentation files extension
     */
    public void setDocExtension(String userDocExtension) {
        this.docExtension = userDocExtension;
    }

    /**
     * Set HTTP user agent.
     *
     * @param httpUserAgent HTTP user agent
     */
    public void setHttpUserAgent(String httpUserAgent) {
        this.httpUserAgent = httpUserAgent;
    }

    /**
     * HTTP user agent
     *
     * @return HTTP user agent
     */
    public String getHttpUserAgent() {
        return httpUserAgent;
    }

    @SuppressWarnings("static-method")
    protected CliOption buildLibraryCliOption(Map<String, String> supportedLibraries) {
        StringBuilder sb = new StringBuilder("library template (sub-template) to use:");
        for (String lib : supportedLibraries.keySet()) {
            sb.append("\n").append(lib).append(" - ").append(supportedLibraries.get(lib));
        }
        return new CliOption("library", sb.toString());
    }

    /**
     * Sanitize name (parameter, property, method, etc)
     *
     * @param name string to be sanitize
     * @return sanitized string
     */
    @SuppressWarnings("static-method")
    public String sanitizeName(String name) {
        return sanitizeName(name, "\\W");
    }

    @Override
    public void setTemplatingEngine(TemplatingEngineAdapter templatingEngine) {
        this.templatingEngine = templatingEngine;
    }

    @Override
    public TemplatingEngineAdapter getTemplatingEngine() {
        return this.templatingEngine;
    }

    /**
     * Sanitize name (parameter, property, method, etc)
     *
     * @param name            string to be sanitize
     * @param removeCharRegEx a regex containing all char that will be removed
     * @return sanitized string
     */
    public String sanitizeName(String name, String removeCharRegEx) {
        return sanitizeName(name, removeCharRegEx, new ArrayList<String>());
    }

    /**
     * Sanitize name (parameter, property, method, etc)
     *
     * @param name            string to be sanitize
     * @param removeCharRegEx a regex containing all char that will be removed
     * @param exceptionList   a list of matches which should not be sanitized (i.e exception)
     * @return sanitized string
     */
    @SuppressWarnings("static-method")
    public String sanitizeName(final String name, String removeCharRegEx, ArrayList<String> exceptionList) {
        // NOTE: performance wise, we should have written with 2 replaceAll to replace desired
        // character with _ or empty character. Below aims to spell out different cases we've
        // encountered so far and hopefully make it easier for others to add more special
        // cases in the future.

        // better error handling when map/array type is invalid
        if (name == null) {
            LOGGER.error("String to be sanitized is null. Default to ERROR_UNKNOWN");
            return "ERROR_UNKNOWN";
        }

        // if the name is just '$', map it to 'value' for the time being.
        if ("$".equals(name)) {
            return "value";
        }

        SanitizeNameOptions opts = new SanitizeNameOptions(name, removeCharRegEx, exceptionList);

        return sanitizedNameCache.get(opts, sanitizeNameOptions -> {
            String modifiable = sanitizeNameOptions.getName();
            List<String> exceptions = sanitizeNameOptions.getExceptions();
            // input[] => input
            modifiable = this.sanitizeValue(modifiable, "\\[\\]", "", exceptions);

            // input[a][b] => input_a_b
            modifiable = this.sanitizeValue(modifiable, "\\[", "_", exceptions);
            modifiable = this.sanitizeValue(modifiable, "\\]", "", exceptions);

            // input(a)(b) => input_a_b
            modifiable = this.sanitizeValue(modifiable, "\\(", "_", exceptions);
            modifiable = this.sanitizeValue(modifiable, "\\)", "", exceptions);

            // input.name => input_name
            modifiable = this.sanitizeValue(modifiable, "\\.", "_", exceptions);

            // input-name => input_name
            modifiable = this.sanitizeValue(modifiable, "-", "_", exceptions);

            // a|b => a_b
            modifiable = this.sanitizeValue(modifiable, "\\|", "_", exceptions);

            // input name and age => input_name_and_age
            modifiable = this.sanitizeValue(modifiable, " ", "_", exceptions);

            // /api/films/get => _api_films_get
            // \api\films\get => _api_films_get
            modifiable = modifiable.replaceAll("/", "_");
            modifiable = modifiable.replaceAll("\\\\", "_");

            // remove everything else other than word, number and _
            // $php_variable => php_variable
            if (allowUnicodeIdentifiers) { //could be converted to a single line with ?: operator
                modifiable = Pattern.compile(sanitizeNameOptions.getRemoveCharRegEx(), Pattern.UNICODE_CHARACTER_CLASS).matcher(modifiable).replaceAll("");
            } else {
                modifiable = modifiable.replaceAll(sanitizeNameOptions.getRemoveCharRegEx(), "");
            }
            return modifiable;
        });
    }

    private String sanitizeValue(String value, String replaceMatch, String replaceValue, List<String> exceptionList) {
        if (exceptionList.size() == 0 || !exceptionList.contains(replaceMatch)) {
            return value.replaceAll(replaceMatch, replaceValue);
        }
        return value;
    }

    /**
     * Sanitize tag
     *
     * @param tag Tag
     * @return Sanitized tag
     */
    public String sanitizeTag(String tag) {
        tag = camelize(sanitizeName(tag));

        // tag starts with numbers
        if (tag.matches("^\\d.*")) {
            tag = "Class" + tag;
        }

        return tag;
    }

    /**
     * Set CodegenParameter boolean flag using CodegenProperty.
     *
     * @param parameter Codegen Parameter
     * @param property  Codegen property
     */
    public void setParameterBooleanFlagWithCodegenProperty(CodegenParameter parameter, CodegenProperty property) {
        if (parameter == null) {
            LOGGER.error("Codegen Parameter cannot be null.");
            return;
        }

        if (property == null) {
            LOGGER.error("Codegen Property cannot be null.");
            return;
        }
        if (Boolean.TRUE.equals(property.isEmail) && Boolean.TRUE.equals(property.isString)) {
            parameter.isEmail = true;
        } else if (Boolean.TRUE.equals(property.isUuid) && Boolean.TRUE.equals(property.isString)) {
            parameter.isUuid = true;
        } else if (Boolean.TRUE.equals(property.isByteArray)) {
            parameter.isByteArray = true;
            parameter.isPrimitiveType = true;
        } else if (Boolean.TRUE.equals(property.isBinary)) {
            parameter.isBinary = true;
            parameter.isPrimitiveType = true;
        } else if (Boolean.TRUE.equals(property.isString)) {
            parameter.isString = true;
            parameter.isPrimitiveType = true;
        } else if (Boolean.TRUE.equals(property.isBoolean)) {
            parameter.isBoolean = true;
            parameter.isPrimitiveType = true;
        } else if (Boolean.TRUE.equals(property.isLong)) {
            parameter.isLong = true;
            parameter.isPrimitiveType = true;
        } else if (Boolean.TRUE.equals(property.isInteger)) {
            parameter.isInteger = true;
            parameter.isPrimitiveType = true;
        } else if (Boolean.TRUE.equals(property.isDouble)) {
            parameter.isDouble = true;
            parameter.isPrimitiveType = true;
        } else if (Boolean.TRUE.equals(property.isFloat)) {
            parameter.isFloat = true;
            parameter.isPrimitiveType = true;
        } else if (Boolean.TRUE.equals(property.isNumber)) {
            parameter.isNumber = true;
            parameter.isPrimitiveType = true;
        } else if (Boolean.TRUE.equals(property.isDate)) {
            parameter.isDate = true;
            parameter.isPrimitiveType = true;
        } else if (Boolean.TRUE.equals(property.isDateTime)) {
            parameter.isDateTime = true;
            parameter.isPrimitiveType = true;
        } else if (Boolean.TRUE.equals(property.isFreeFormObject)) {
            parameter.isFreeFormObject = true;
        } else if (Boolean.TRUE.equals(property.isAnyType)) {
            parameter.isAnyType = true;
        } else {
            LOGGER.debug("Property type is not primitive: " + property.dataType);
        }

        if (Boolean.TRUE.equals(property.isFile)) {
            parameter.isFile = true;
        }
        if (Boolean.TRUE.equals(property.isModel)) {
            parameter.isModel = true;
        }
    }

    /**
     * Update codegen property's enum by adding "enumVars" (with name and value)
     *
     * @param var list of CodegenProperty
     */
    public void updateCodegenPropertyEnum(CodegenProperty var) {
        Map<String, Object> allowableValues = var.allowableValues;

        // handle array
        if (var.mostInnerItems != null) {
            allowableValues = var.mostInnerItems.allowableValues;
        }

        if (allowableValues == null) {
            return;
        }

        List<Object> values = (List<Object>) allowableValues.get("values");
        if (values == null) {
            return;
        }

        String varDataType = var.mostInnerItems != null ? var.mostInnerItems.dataType : var.dataType;
        Optional<Schema> referencedSchema = ModelUtils.getSchemas(openAPI).entrySet().stream()
                .filter(entry -> Objects.equals(varDataType, toModelName(entry.getKey())))
                .map(Map.Entry::getValue)
                .findFirst();
        String dataType = (referencedSchema.isPresent()) ? getTypeDeclaration(referencedSchema.get()) : varDataType;
        List<Map<String, Object>> enumVars = buildEnumVars(values, dataType);

        // if "x-enum-varnames" or "x-enum-descriptions" defined, update varnames
        Map<String, Object> extensions = var.mostInnerItems != null ? var.mostInnerItems.getVendorExtensions() : var.getVendorExtensions();
        if (referencedSchema.isPresent()) {
            extensions = referencedSchema.get().getExtensions();
        }
        updateEnumVarsWithExtensions(enumVars, extensions);
        allowableValues.put("enumVars", enumVars);

        // handle default value for enum, e.g. available => StatusEnum.AVAILABLE
        if (var.defaultValue != null) {
            String enumName = null;
            final String enumDefaultValue;
            if ("string".equalsIgnoreCase(dataType)) {
                enumDefaultValue = toEnumValue(var.defaultValue, dataType);
            } else {
                enumDefaultValue = var.defaultValue;
            }
            for (Map<String, Object> enumVar : enumVars) {
                if (enumDefaultValue.equals(enumVar.get("value"))) {
                    enumName = (String) enumVar.get("name");
                    break;
                }
            }
            if (enumName != null) {
                var.defaultValue = toEnumDefaultValue(enumName, var.datatypeWithEnum);
            }
        }
    }

    protected List<Map<String, Object>> buildEnumVars(List<Object> values, String dataType) {
        List<Map<String, Object>> enumVars = new ArrayList<>();
        int truncateIdx = 0;

        if (isRemoveEnumValuePrefix()) {
            String commonPrefix = findCommonPrefixOfVars(values);
            truncateIdx = commonPrefix.length();
        }

        for (Object value : values) {
            Map<String, Object> enumVar = new HashMap<>();
            String enumName;
            if (truncateIdx == 0) {
                enumName = String.valueOf(value);
            } else {
                enumName = value.toString().substring(truncateIdx);
                if ("".equals(enumName)) {
                    enumName = value.toString();
                }
            }

            enumVar.put("name", toEnumVarName(enumName, dataType));
            enumVar.put("value", toEnumValue(String.valueOf(value), dataType));
            enumVar.put("isString", isDataTypeString(dataType));
            enumVars.add(enumVar);
        }
        return enumVars;
    }

    protected void updateEnumVarsWithExtensions(List<Map<String, Object>> enumVars, Map<String, Object> vendorExtensions) {
        if (vendorExtensions != null) {
            updateEnumVarsWithExtensions(enumVars, vendorExtensions, "x-enum-varnames", "name");
            updateEnumVarsWithExtensions(enumVars, vendorExtensions, "x-enum-descriptions", "enumDescription");
        }
    }

    private void updateEnumVarsWithExtensions(List<Map<String, Object>> enumVars, Map<String, Object> vendorExtensions, String extensionKey, String key) {
        if (vendorExtensions.containsKey(extensionKey)) {
            List<String> values = (List<String>) vendorExtensions.get(extensionKey);
            int size = Math.min(enumVars.size(), values.size());
            for (int i = 0; i < size; i++) {
                enumVars.get(i).put(key, values.get(i));
            }
        }
    }

    /**
     * If the pattern misses the delimiter, add "/" to the beginning and end
     * Otherwise, return the original pattern
     *
     * @param pattern the pattern (regular expression)
     * @return the pattern with delimiter
     */
    public String addRegularExpressionDelimiter(String pattern) {
        if (StringUtils.isEmpty(pattern)) {
            return pattern;
        }

        if (!pattern.matches("^/.*")) {
            return "/" + pattern.replaceAll("/", "\\\\/") + "/";
        }

        return pattern;
    }

    /**
     * reads propertyKey from additionalProperties, converts it to a boolean and
     * writes it back to additionalProperties to be usable as a boolean in
     * mustache files.
     *
     * @param propertyKey property key
     * @return property value as boolean
     */
    public boolean convertPropertyToBooleanAndWriteBack(String propertyKey) {
        boolean result = convertPropertyToBoolean(propertyKey);
        writePropertyBack(propertyKey, result);
        return result;
    }

    /**
     * Provides an override location, if any is specified, for the .openapi-generator-ignore.
     * <p>
     * This is originally intended for the first generation only.
     *
     * @return a string of the full path to an override ignore file.
     */
    public String getIgnoreFilePathOverride() {
        return ignoreFilePathOverride;
    }

    /**
     * Sets an override location for the '.openapi-generator-ignore' location for the first code generation.
     *
     * @param ignoreFileOverride The full path to an ignore file
     */
    public void setIgnoreFilePathOverride(final String ignoreFileOverride) {
        this.ignoreFilePathOverride = ignoreFileOverride;
    }

    public boolean convertPropertyToBoolean(String propertyKey) {
        final Object booleanValue = additionalProperties.get(propertyKey);
        Boolean result = Boolean.FALSE;
        if (booleanValue instanceof Boolean) {
            result = (Boolean) booleanValue;
        } else if (booleanValue instanceof String) {
            result = Boolean.parseBoolean((String) booleanValue);
        } else {
            LOGGER.warn("The value (generator's option) must be either boolean or string. Default to `false`.");
        }
        return result;
    }

    public void writePropertyBack(String propertyKey, boolean value) {
        additionalProperties.put(propertyKey, value);
    }

    protected String getContentType(RequestBody requestBody) {
        if (requestBody == null || requestBody.getContent() == null || requestBody.getContent().isEmpty()) {
            LOGGER.debug("Cannot determine the content type. Returning null.");
            return null;
        }
        return new ArrayList<>(requestBody.getContent().keySet()).get(0);
    }

    private void setOauth2Info(CodegenSecurity codegenSecurity, OAuthFlow flow) {
        codegenSecurity.authorizationUrl = flow.getAuthorizationUrl();
        codegenSecurity.tokenUrl = flow.getTokenUrl();

        if (flow.getScopes() != null && !flow.getScopes().isEmpty()) {
            List<Map<String, Object>> scopes = new ArrayList<Map<String, Object>>();
            int count = 0, numScopes = flow.getScopes().size();
            for (Map.Entry<String, String> scopeEntry : flow.getScopes().entrySet()) {
                Map<String, Object> scope = new HashMap<String, Object>();
                scope.put("scope", scopeEntry.getKey());
                scope.put("description", escapeText(scopeEntry.getValue()));

                count += 1;
                if (count < numScopes) {
                    scope.put("hasMore", "true");
                } else {
                    scope.put("hasMore", null);
                }

                scopes.add(scope);
            }
            codegenSecurity.scopes = scopes;
        }
    }

    private void addConsumesInfo(Operation operation, CodegenOperation codegenOperation) {
        RequestBody requestBody = ModelUtils.getReferencedRequestBody(this.openAPI, operation.getRequestBody());
        if (requestBody == null || requestBody.getContent() == null || requestBody.getContent().isEmpty()) {
            return;
        }

        Set<String> consumes = requestBody.getContent().keySet();
        List<Map<String, String>> mediaTypeList = new ArrayList<>();
        int count = 0;
        for (String key : consumes) {
            Map<String, String> mediaType = new HashMap<>();
            if ("*/*".equals(key)) {
                // skip as it implies `consumes` in OAS2 is not defined
                continue;
            } else {
                mediaType.put("mediaType", escapeText(escapeQuotationMark(key)));
            }

            count += 1;
            if (count < consumes.size()) {
                mediaType.put("hasMore", "true");
            } else {
                mediaType.put("hasMore", null);
            }

            mediaTypeList.add(mediaType);
        }

        if (!mediaTypeList.isEmpty()) {
            codegenOperation.consumes = mediaTypeList;
            codegenOperation.hasConsumes = true;
        }
    }

    public static Set<String> getConsumesInfo(OpenAPI openAPI, Operation operation) {
        RequestBody requestBody = ModelUtils.getReferencedRequestBody(openAPI, operation.getRequestBody());

        if (requestBody == null || requestBody.getContent() == null || requestBody.getContent().isEmpty()) {
            return Collections.emptySet(); // return empty set
        }
        return requestBody.getContent().keySet();
    }

    public boolean hasFormParameter(OpenAPI openAPI, Operation operation) {
        Set<String> consumesInfo = getConsumesInfo(openAPI, operation);

        if (consumesInfo == null || consumesInfo.isEmpty()) {
            return false;
        }

        for (String consume : consumesInfo) {
            if (consume != null &&
                    (consume.toLowerCase(Locale.ROOT).startsWith("application/x-www-form-urlencoded") ||
                            consume.toLowerCase(Locale.ROOT).startsWith("multipart"))) {
                return true;
            }
        }

        return false;
    }

    public boolean hasBodyParameter(OpenAPI openAPI, Operation operation) {
        RequestBody requestBody = ModelUtils.getReferencedRequestBody(openAPI, operation.getRequestBody());
        if (requestBody == null) {
            return false;
        }

        Schema schema = ModelUtils.getSchemaFromRequestBody(requestBody);
        return ModelUtils.getReferencedSchema(openAPI, schema) != null;
    }

    private void addProducesInfo(ApiResponse inputResponse, CodegenOperation codegenOperation) {
        ApiResponse response = ModelUtils.getReferencedApiResponse(this.openAPI, inputResponse);
        if (response == null || response.getContent() == null || response.getContent().isEmpty()) {
            return;
        }

        Set<String> produces = response.getContent().keySet();
        if (codegenOperation.produces == null) {
            codegenOperation.produces = new ArrayList<>();
        }

        Set<String> existingMediaTypes = new HashSet<>();
        for (Map<String, String> mediaType : codegenOperation.produces) {
            existingMediaTypes.add(mediaType.get("mediaType"));
        }

        int count = 0;
        for (String key : produces) {
            // escape quotation to avoid code injection, "*/*" is a special case, do nothing
            String encodedKey = "*/*".equals(key) ? key : escapeText(escapeQuotationMark(key));
            //Only unique media types should be added to "produces"
            if (!existingMediaTypes.contains(encodedKey)) {
                Map<String, String> mediaType = new HashMap<String, String>();
                mediaType.put("mediaType", encodedKey);

                count += 1;
                if (count < produces.size()) {
                    mediaType.put("hasMore", "true");
                } else {
                    mediaType.put("hasMore", null);
                }

                if (!codegenOperation.produces.isEmpty()) {
                    final Map<String, String> lastMediaType = codegenOperation.produces.get(codegenOperation.produces.size() - 1);
                    lastMediaType.put("hasMore", "true");
                }

                codegenOperation.produces.add(mediaType);
                codegenOperation.hasProduces = Boolean.TRUE;
            }
        }
    }

    /**
     * returns the list of MIME types the APIs can produce
     *
     * @param openAPI   current specification instance
     * @param operation Operation
     * @return a set of MIME types
     */
    public static Set<String> getProducesInfo(final OpenAPI openAPI, final Operation operation) {
        if (operation.getResponses() == null || operation.getResponses().isEmpty()) {
            return null;
        }

        Set<String> produces = new TreeSet<String>();

        for (ApiResponse r : operation.getResponses().values()) {
            ApiResponse response = ModelUtils.getReferencedApiResponse(openAPI, r);
            if (response.getContent() != null) {
                produces.addAll(response.getContent().keySet());
            }
        }

        return produces;
    }

    protected String getCollectionFormat(Parameter parameter) {
        if (Parameter.StyleEnum.FORM.equals(parameter.getStyle())) {
            // Ref: https://github.com/OAI/OpenAPI-Specification/blob/master/versions/3.0.1.md#style-values
            if (Boolean.TRUE.equals(parameter.getExplode())) { // explode is true (default)
                return "multi";
            } else {
                return "csv";
            }
        } else if (Parameter.StyleEnum.SIMPLE.equals(parameter.getStyle())) {
            return "csv";
        } else if (Parameter.StyleEnum.PIPEDELIMITED.equals(parameter.getStyle())) {
            return "pipes";
        } else if (Parameter.StyleEnum.SPACEDELIMITED.equals(parameter.getStyle())) {
            return "ssv";
        } else {
            return null;
        }
    }

    public CodegenType getTag() {
        return null;
    }

    public String getName() {
        return null;
    }

    public String getHelp() {
        return null;
    }

    public List<CodegenParameter> fromRequestBodyToFormParameters(RequestBody body, Set<String> imports) {
        List<CodegenParameter> parameters = new ArrayList<CodegenParameter>();
        LOGGER.debug("debugging fromRequestBodyToFormParameters= " + body);
        Schema schema = ModelUtils.getSchemaFromRequestBody(body);
        schema = ModelUtils.getReferencedSchema(this.openAPI, schema);
        if (schema.getProperties() != null && !schema.getProperties().isEmpty()) {
            Map<String, Schema> properties = schema.getProperties();
            for (Map.Entry<String, Schema> entry : properties.entrySet()) {
                CodegenParameter codegenParameter = CodegenModelFactory.newInstance(CodegenModelType.PARAMETER);
                // key => property name
                // value => property schema
                String collectionFormat = null;
                Schema s = entry.getValue();
                // array of schema
                if (ModelUtils.isArraySchema(s)) {
                    final ArraySchema arraySchema = (ArraySchema) s;
                    Schema inner = getSchemaItems(arraySchema);

                    codegenParameter = fromFormProperty(entry.getKey(), inner, imports);
                    CodegenProperty codegenProperty = fromProperty("inner", inner);
                    codegenParameter.items = codegenProperty;
                    codegenParameter.mostInnerItems = codegenProperty.mostInnerItems;
                    codegenParameter.baseType = codegenProperty.dataType;
                    codegenParameter.isPrimitiveType = false;
                    codegenParameter.isContainer = true;
                    codegenParameter.isListContainer = true;
                    codegenParameter.description = escapeText(s.getDescription());
                    codegenParameter.dataType = getTypeDeclaration(arraySchema);
                    if (codegenParameter.baseType != null && codegenParameter.enumName != null) {
                        codegenParameter.datatypeWithEnum = codegenParameter.dataType.replace(codegenParameter.baseType, codegenParameter.enumName);
                    } else {
                        LOGGER.warn("Could not compute datatypeWithEnum from " + codegenParameter.baseType + ", " + codegenParameter.enumName);
                    }
                    //TODO fix collectformat for form parameters
                    //collectionFormat = getCollectionFormat(s);
                    // default to csv:
                    codegenParameter.collectionFormat = StringUtils.isEmpty(collectionFormat) ? "csv" : collectionFormat;

                    // set nullable
                    setParameterNullable(codegenParameter, codegenProperty);

                    // recursively add import
                    while (codegenProperty != null) {
                        imports.add(codegenProperty.baseType);
                        codegenProperty = codegenProperty.items;
                    }

                } else if (ModelUtils.isMapSchema(s)) {
                    LOGGER.error("Map of form parameters not supported. Please report the issue to https://github.com/openapitools/openapi-generator if you need help.");
                    continue;
                } else {
                    codegenParameter = fromFormProperty(entry.getKey(), entry.getValue(), imports);
                }

                // Set 'required' flag defined in the schema element
                if (!codegenParameter.required && schema.getRequired() != null) {
                    codegenParameter.required = schema.getRequired().contains(entry.getKey());
                }

                parameters.add(codegenParameter);
            }
        }

        return parameters;
    }

    public CodegenParameter fromFormProperty(String name, Schema propertySchema, Set<String> imports) {
        CodegenParameter codegenParameter = CodegenModelFactory.newInstance(CodegenModelType.PARAMETER);

        LOGGER.debug("Debugging fromFormProperty {}: {}", name, propertySchema);
        CodegenProperty codegenProperty = fromProperty(name, propertySchema);

        ModelUtils.syncValidationProperties(propertySchema, codegenProperty);

        codegenParameter.isFormParam = Boolean.TRUE;
        codegenParameter.baseName = codegenProperty.baseName;
        codegenParameter.paramName = toParamName((codegenParameter.baseName));
        codegenParameter.baseType = codegenProperty.baseType;
        codegenParameter.dataType = codegenProperty.dataType;
        codegenParameter.dataFormat = codegenProperty.dataFormat;
        codegenParameter.description = escapeText(codegenProperty.description);
        codegenParameter.unescapedDescription = codegenProperty.getDescription();
        codegenParameter.jsonSchema = Json.pretty(propertySchema);
        codegenParameter.defaultValue = codegenProperty.getDefaultValue();

        if (codegenProperty.getVendorExtensions() != null && !codegenProperty.getVendorExtensions().isEmpty()) {
            codegenParameter.vendorExtensions = codegenProperty.getVendorExtensions();
        }
        if (propertySchema.getRequired() != null && !propertySchema.getRequired().isEmpty() && propertySchema.getRequired().contains(codegenProperty.baseName)) {
            codegenParameter.required = Boolean.TRUE;
        }

        // non-array/map
        updateCodegenPropertyEnum(codegenProperty);
        codegenParameter.isEnum = codegenProperty.isEnum;
        codegenParameter._enum = codegenProperty._enum;
        codegenParameter.allowableValues = codegenProperty.allowableValues;

        if (codegenProperty.isEnum) {
            codegenParameter.datatypeWithEnum = codegenProperty.datatypeWithEnum;
            codegenParameter.enumName = codegenProperty.enumName;
        }

        if (codegenProperty.items != null && codegenProperty.items.isEnum) {
            codegenParameter.items = codegenProperty.items;
            codegenParameter.mostInnerItems = codegenProperty.mostInnerItems;
        }

        // import
        if (codegenProperty.complexType != null) {
            imports.add(codegenProperty.complexType);
        }

        // validation
        // handle maximum, minimum properly for int/long by removing the trailing ".0"
        if (ModelUtils.isIntegerSchema(propertySchema)) {
            codegenParameter.maximum = propertySchema.getMaximum() == null ? null : String.valueOf(propertySchema.getMaximum().longValue());
            codegenParameter.minimum = propertySchema.getMinimum() == null ? null : String.valueOf(propertySchema.getMinimum().longValue());
        } else {
            codegenParameter.maximum = propertySchema.getMaximum() == null ? null : String.valueOf(propertySchema.getMaximum());
            codegenParameter.minimum = propertySchema.getMinimum() == null ? null : String.valueOf(propertySchema.getMinimum());
        }

        codegenParameter.exclusiveMaximum = propertySchema.getExclusiveMaximum() == null ? false : propertySchema.getExclusiveMaximum();
        codegenParameter.exclusiveMinimum = propertySchema.getExclusiveMinimum() == null ? false : propertySchema.getExclusiveMinimum();
        codegenParameter.maxLength = propertySchema.getMaxLength();
        codegenParameter.minLength = propertySchema.getMinLength();
        codegenParameter.pattern = toRegularExpression(propertySchema.getPattern());
        codegenParameter.maxItems = propertySchema.getMaxItems();
        codegenParameter.minItems = propertySchema.getMinItems();
        codegenParameter.uniqueItems = propertySchema.getUniqueItems() == null ? false : propertySchema.getUniqueItems();
        codegenParameter.multipleOf = propertySchema.getMultipleOf();

        // exclusive* are noop without corresponding min/max
        if (codegenParameter.maximum != null || codegenParameter.minimum != null ||
                codegenParameter.maxLength != null || codegenParameter.minLength != null ||
                codegenParameter.maxItems != null || codegenParameter.minItems != null ||
                codegenParameter.pattern != null || codegenParameter.multipleOf != null) {
            codegenParameter.hasValidation = true;
        }

        setParameterBooleanFlagWithCodegenProperty(codegenParameter, codegenProperty);
        setParameterExampleValue(codegenParameter);
        // set nullable
        setParameterNullable(codegenParameter, codegenProperty);

        //TODO collectionFormat for form parameter not yet supported
        //codegenParameter.collectionFormat = getCollectionFormat(propertySchema);
        return codegenParameter;
    }

    private void addBodyModelSchema(CodegenParameter codegenParameter, String name, Schema schema, Set<String> imports, String bodyParameterName, boolean forceSimpleRef) {
        CodegenModel codegenModel = null;
        if (StringUtils.isNotBlank(name)) {
            schema.setName(name);
            codegenModel = fromModel(name, schema);
        }
        if (codegenModel != null) {
            codegenParameter.isModel = true;
        }

        if (codegenModel != null && (codegenModel.hasVars || forceSimpleRef)) {
            if (StringUtils.isEmpty(bodyParameterName)) {
                codegenParameter.baseName = codegenModel.classname;
            } else {
                codegenParameter.baseName = bodyParameterName;
            }
            codegenParameter.paramName = toParamName(codegenParameter.baseName);
            codegenParameter.baseType = codegenModel.classname;
            codegenParameter.dataType = getTypeDeclaration(codegenModel.classname);
            codegenParameter.description = codegenModel.description;
            imports.add(codegenParameter.baseType);
        } else {
            CodegenProperty codegenProperty = fromProperty("property", schema);

            if (codegenProperty != null && codegenProperty.getComplexType() != null && codegenProperty.getComplexType().contains(" | ")) {
                List<String> parts = Arrays.asList(codegenProperty.getComplexType().split(" \\| "));
                imports.addAll(parts);
                String codegenModelName = codegenProperty.getComplexType();
                codegenParameter.baseName = codegenModelName;
                codegenParameter.paramName = toParamName(codegenParameter.baseName);
                codegenParameter.baseType = codegenParameter.baseName;
                codegenParameter.dataType = getTypeDeclaration(codegenModelName);
                codegenParameter.description = codegenProperty.getDescription();
            } else {
                if (ModelUtils.isMapSchema(schema)) {// http body is map
                    LOGGER.error("Map should be supported. Please report to openapi-generator github repo about the issue.");
                } else if (codegenProperty != null) {
                    String codegenModelName, codegenModelDescription;

                    if (codegenModel != null) {
                        codegenModelName = codegenModel.classname;
                        codegenModelDescription = codegenModel.description;
                    } else {
                        LOGGER.warn("The following schema has undefined (null) baseType. " +
                                "It could be due to form parameter defined in OpenAPI v2 spec with incorrect consumes. " +
                                "A correct 'consumes' for form parameters should be " +
                                "'application/x-www-form-urlencoded' or 'multipart/?'");
                        LOGGER.warn("schema: " + schema);
                        LOGGER.warn("codegenModel is null. Default to UNKNOWN_BASE_TYPE");
                        codegenModelName = "UNKNOWN_BASE_TYPE";
                        codegenModelDescription = "UNKNOWN_DESCRIPTION";
                    }

                    if (StringUtils.isEmpty(bodyParameterName)) {
                        codegenParameter.baseName = codegenModelName;
                    } else {
                        codegenParameter.baseName = bodyParameterName;
                    }

                    codegenParameter.paramName = toParamName(codegenParameter.baseName);
                    codegenParameter.baseType = codegenModelName;
                    codegenParameter.dataType = getTypeDeclaration(codegenModelName);
                    codegenParameter.description = codegenModelDescription;
                    imports.add(codegenParameter.baseType);

                    if (codegenProperty.complexType != null) {
                        imports.add(codegenProperty.complexType);
                    }
                }
            }

            setParameterBooleanFlagWithCodegenProperty(codegenParameter, codegenProperty);
            // set nullable
            setParameterNullable(codegenParameter, codegenProperty);
        }
    }

    public CodegenParameter fromRequestBody(RequestBody body, Set<String> imports, String bodyParameterName) {
        if (body == null) {
            LOGGER.error("body in fromRequestBody cannot be null!");
            throw new RuntimeException("body in fromRequestBody cannot be null!");
        }
        CodegenParameter codegenParameter = CodegenModelFactory.newInstance(CodegenModelType.PARAMETER);
        codegenParameter.baseName = "UNKNOWN_BASE_NAME";
        codegenParameter.paramName = "UNKNOWN_PARAM_NAME";
        codegenParameter.description = escapeText(body.getDescription());
        codegenParameter.required = body.getRequired() != null ? body.getRequired() : Boolean.FALSE;
        codegenParameter.isBodyParam = Boolean.TRUE;

        String name = null;
        LOGGER.debug("Request body = " + body);
        Schema schema = ModelUtils.getSchemaFromRequestBody(body);
        if (schema == null) {
            throw new RuntimeException("Request body cannot be null. Possible cause: missing schema in body parameter (OAS v2): " + body);
        }

        if (StringUtils.isNotBlank(schema.get$ref())) {
            name = ModelUtils.getSimpleRef(schema.get$ref());
        }
        schema = ModelUtils.getReferencedSchema(this.openAPI, schema);

        ModelUtils.syncValidationProperties(schema, codegenParameter);

        if (ModelUtils.isMapSchema(schema)) {
            // Schema with additionalproperties: true (including composed schemas with additionalproperties: true)
            if (ModelUtils.isGenerateAliasAsModel() && StringUtils.isNotBlank(name)) {
                this.addBodyModelSchema(codegenParameter, name, schema, imports, bodyParameterName, true);
            } else {
                Schema inner = getAdditionalProperties(schema);
                if (inner == null) {
                    LOGGER.error("No inner type supplied for map parameter `{}`. Default to type:string", schema.getName());
                    inner = new StringSchema().description("//TODO automatically added by openapi-generator");
                    schema.setAdditionalProperties(inner);
                }
                CodegenProperty codegenProperty = fromProperty("property", schema);

                imports.add(codegenProperty.baseType);

                CodegenProperty innerCp = codegenProperty;
                while (innerCp != null) {
                    if (innerCp.complexType != null) {
                        imports.add(innerCp.complexType);
                    }
                    innerCp = innerCp.items;
                }

                if (StringUtils.isEmpty(bodyParameterName)) {
                    codegenParameter.baseName = "request_body";
                } else {
                    codegenParameter.baseName = bodyParameterName;
                }
                codegenParameter.paramName = toParamName(codegenParameter.baseName);
                codegenParameter.items = codegenProperty.items;
                codegenParameter.mostInnerItems = codegenProperty.mostInnerItems;
                codegenParameter.dataType = getTypeDeclaration(schema);
                codegenParameter.baseType = getSchemaType(inner);
                codegenParameter.isContainer = Boolean.TRUE;
                codegenParameter.isMapContainer = Boolean.TRUE;

                setParameterBooleanFlagWithCodegenProperty(codegenParameter, codegenProperty);

                // set nullable
                setParameterNullable(codegenParameter, codegenProperty);
            }
        } else if (ModelUtils.isArraySchema(schema)) {
            if (ModelUtils.isGenerateAliasAsModel() && StringUtils.isNotBlank(name)) {
                this.addBodyModelSchema(codegenParameter, name, schema, imports, bodyParameterName, true);
            } else {
                final ArraySchema arraySchema = (ArraySchema) schema;
                Schema inner = getSchemaItems(arraySchema);
                CodegenProperty codegenProperty = fromProperty("property", arraySchema);
                imports.add(codegenProperty.baseType);
                CodegenProperty innerCp = codegenProperty;
                CodegenProperty mostInnerItem = innerCp;
                // loop through multidimensional array to add proper import
                // also find the most inner item
                while (innerCp != null) {
                    if (innerCp.complexType != null) {
                        imports.add(innerCp.complexType);
                    }
                    mostInnerItem = innerCp;
                    innerCp = innerCp.items;
                }

                if (StringUtils.isEmpty(bodyParameterName)) {
                    if (StringUtils.isEmpty(mostInnerItem.complexType)) {
                        codegenParameter.baseName = "request_body";
                    } else {
                        codegenParameter.baseName = mostInnerItem.complexType;
                    }
                } else {
                    codegenParameter.baseName = bodyParameterName;
                }
                codegenParameter.paramName = toArrayModelParamName(codegenParameter.baseName);
                codegenParameter.items = codegenProperty.items;
                codegenParameter.mostInnerItems = codegenProperty.mostInnerItems;
                codegenParameter.dataType = getTypeDeclaration(arraySchema);
                codegenParameter.baseType = getSchemaType(inner);
                codegenParameter.isContainer = Boolean.TRUE;
                codegenParameter.isListContainer = Boolean.TRUE;

                setParameterBooleanFlagWithCodegenProperty(codegenParameter, codegenProperty);
                // set nullable
                setParameterNullable(codegenParameter, codegenProperty);

                while (codegenProperty != null) {
                    imports.add(codegenProperty.baseType);
                    codegenProperty = codegenProperty.items;
                }
            }
        } else if (isFreeFormObject(schema)) {
            // HTTP request body is free form object
            CodegenProperty codegenProperty = fromProperty("FREE_FORM_REQUEST_BODY", schema);
            if (codegenProperty != null) {
                if (StringUtils.isEmpty(bodyParameterName)) {
                    codegenParameter.baseName = "body";  // default to body
                } else {
                    codegenParameter.baseName = bodyParameterName;
                }
                codegenParameter.isPrimitiveType = true;
                codegenParameter.baseType = codegenProperty.baseType;
                codegenParameter.dataType = codegenProperty.dataType;
                codegenParameter.description = codegenProperty.description;
                codegenParameter.paramName = toParamName(codegenParameter.baseName);
            }
            setParameterBooleanFlagWithCodegenProperty(codegenParameter, codegenProperty);
            // set nullable
            setParameterNullable(codegenParameter, codegenProperty);

        } else if (ModelUtils.isObjectSchema(schema) || ModelUtils.isComposedSchema(schema)) {
            this.addBodyModelSchema(codegenParameter, name, schema, imports, bodyParameterName, false);
        } else {
            // HTTP request body is primitive type (e.g. integer, string, etc)
            CodegenProperty codegenProperty = fromProperty("PRIMITIVE_REQUEST_BODY", schema);
            if (codegenProperty != null) {
                if (StringUtils.isEmpty(bodyParameterName)) {
                    codegenParameter.baseName = "body";  // default to body
                } else {
                    codegenParameter.baseName = bodyParameterName;
                }
                codegenParameter.isPrimitiveType = true;
                codegenParameter.baseType = codegenProperty.baseType;
                codegenParameter.dataType = codegenProperty.dataType;
                codegenParameter.description = codegenProperty.description;
                codegenParameter.paramName = toParamName(codegenParameter.baseName);
                codegenParameter.minimum = codegenProperty.minimum;
                codegenParameter.maximum = codegenProperty.maximum;
                codegenParameter.exclusiveMinimum = codegenProperty.exclusiveMinimum;
                codegenParameter.exclusiveMaximum = codegenProperty.exclusiveMaximum;
                codegenParameter.minLength = codegenProperty.minLength;
                codegenParameter.maxLength = codegenProperty.maxLength;
                codegenParameter.pattern = codegenProperty.pattern;

                if (codegenProperty.complexType != null) {
                    imports.add(codegenProperty.complexType);
                }

            }
            setParameterBooleanFlagWithCodegenProperty(codegenParameter, codegenProperty);
            // set nullable
            setParameterNullable(codegenParameter, codegenProperty);
        }

        addJsonSchemaForBodyRequestInCaseItsNotPresent(codegenParameter, body);

        // set the parameter's example value
        // should be overridden by lang codegen
        setParameterExampleValue(codegenParameter, body);

        return codegenParameter;
    }

    private void addJsonSchemaForBodyRequestInCaseItsNotPresent(CodegenParameter codegenParameter, RequestBody body) {
        if (codegenParameter.jsonSchema == null)
            codegenParameter.jsonSchema = Json.pretty(body);
    }

    protected void addOption(String key, String description, String defaultValue) {
        CliOption option = new CliOption(key, description);
        if (defaultValue != null)
            option.defaultValue(defaultValue);
        cliOptions.add(option);
    }

    protected void updateOption(String key, String defaultValue) {
        for (CliOption cliOption : cliOptions) {
            if (cliOption.getOpt().equals(key)) {
                cliOption.setDefault(defaultValue);
                break;
            }
        }
    }

    protected void removeOption(String key) {
        for (int i = 0; i < cliOptions.size(); i++) {
            if (key.equals(cliOptions.get(i).getOpt())) {
                cliOptions.remove(i);
                break;
            }
        }
    }

    protected void addSwitch(String key, String description, Boolean defaultValue) {
        CliOption option = CliOption.newBoolean(key, description);
        if (defaultValue != null)
            option.defaultValue(defaultValue.toString());
        cliOptions.add(option);
    }

    /**
     * generates OpenAPI specification file in JSON format
     *
     * @param objs map of object
     */
    protected void generateJSONSpecFile(Map<String, Object> objs) {
        OpenAPI openAPI = (OpenAPI) objs.get("openAPI");
        if (openAPI != null) {
            objs.put("openapi-json", SerializerUtils.toJsonString(openAPI));
        }
    }

    /**
     * generates OpenAPI specification file in YAML format
     *
     * @param objs map of object
     */
    public void generateYAMLSpecFile(Map<String, Object> objs) {
        OpenAPI openAPI = (OpenAPI) objs.get("openAPI");
        String yaml = SerializerUtils.toYamlString(openAPI);
        if (yaml != null) {
            objs.put("openapi-yaml", yaml);
        }
    }

    /**
     * checks if the data should be classified as "string" in enum
     * e.g. double in C# needs to be double-quoted (e.g. "2.8") by treating it as a string
     * In the future, we may rename this function to "isEnumString"
     *
     * @param dataType data type
     * @return true if it's a enum string
     */
    public boolean isDataTypeString(String dataType) {
        return "String".equals(dataType);
    }

    @Override
    public List<CodegenServer> fromServers(List<Server> servers) {
        if (servers == null) {
            return Collections.emptyList();
        }
        List<CodegenServer> codegenServers = new LinkedList<>();
        for (Server server : servers) {
            CodegenServer cs = new CodegenServer();
            cs.description = escapeText(server.getDescription());
            cs.url = server.getUrl();
            cs.variables = this.fromServerVariables(server.getVariables());
            codegenServers.add(cs);
        }
        return codegenServers;
    }

    @Override
    public List<CodegenServerVariable> fromServerVariables(Map<String, ServerVariable> variables) {
        if (variables == null) {
            return Collections.emptyList();
        }

        Map<String, String> variableOverrides = serverVariableOverrides();

        List<CodegenServerVariable> codegenServerVariables = new LinkedList<>();
        for (Entry<String, ServerVariable> variableEntry : variables.entrySet()) {
            CodegenServerVariable codegenServerVariable = new CodegenServerVariable();
            ServerVariable variable = variableEntry.getValue();
            List<String> enums = variable.getEnum();

            codegenServerVariable.defaultValue = variable.getDefault();
            codegenServerVariable.description = escapeText(variable.getDescription());
            codegenServerVariable.enumValues = enums;
            codegenServerVariable.name = variableEntry.getKey();

            // Sets the override value for a server variable pattern.
            // NOTE: OpenAPI Specification doesn't prevent multiple server URLs with variables. If multiple objects have the same
            //       variables pattern, user overrides will apply to _all_ of these patterns. We may want to consider indexed overrides.
            if (variableOverrides != null && !variableOverrides.isEmpty()) {
                String value = variableOverrides.getOrDefault(variableEntry.getKey(), variable.getDefault());
                codegenServerVariable.value = value;

                if (enums != null && !enums.isEmpty() && !enums.contains(value)) {
                    LOGGER.warn("Variable override of '{}' is not listed in the enum of allowed values ({}).", value, StringUtils.join(enums, ","));
                }
            } else {
                codegenServerVariable.value = variable.getDefault();
            }

            codegenServerVariables.add(codegenServerVariable);
        }
        return codegenServerVariables;
    }

    private void setParameterNullable(CodegenParameter parameter, CodegenProperty property) {
        if (parameter == null || property == null) {
            return;
        }
        parameter.isNullable = property.isNullable;
    }

    /**
     * Post-process the auto-generated file, e.g. using go-fmt to format the Go code. The file type can be "model-test",
     * "model-doc", "model", "api", "api-test", "api-doc", "supporting-mustache", "supporting-common",
     * "openapi-generator-ignore", "openapi-generator-version"
     * <p>
     * TODO: store these values in enum instead
     *
     * @param file     file to be processed
     * @param fileType file type
     */
    public void postProcessFile(File file, String fileType) {
        LOGGER.debug("Post processing file {} ({})", file, fileType);
    }

    /**
     * Boolean value indicating the state of the option for post-processing file using envirionment variables.
     *
     * @return true if the option is enabled
     */
    public boolean isEnablePostProcessFile() {
        return enablePostProcessFile;
    }

    /**
     * Set the boolean value indicating the state of the option for post-processing file using envirionment variables.
     *
     * @param enablePostProcessFile true to enable post-processing file
     */
    public void setEnablePostProcessFile(boolean enablePostProcessFile) {
        this.enablePostProcessFile = enablePostProcessFile;
    }

    /**
     * Get the boolean value indicating the state of the option for updating only changed files
     */
    @Override
    public boolean isEnableMinimalUpdate() {
        return enableMinimalUpdate;
    }

    /**
     * Set the boolean value indicating the state of the option for updating only changed files
     *
     * @param enableMinimalUpdate true to enable minimal update
     */
    @Override
    public void setEnableMinimalUpdate(boolean enableMinimalUpdate) {
        this.enableMinimalUpdate = enableMinimalUpdate;
    }

    /**
     * Indicates whether the codegen configuration should treat documents as strictly defined by the OpenAPI specification.
     *
     * @return true to act strictly upon spec documents, potentially modifying the spec to strictly fit the spec.
     */
    @Override
    public boolean isStrictSpecBehavior() {
        return this.strictSpecBehavior;
    }

    /**
     * Sets the boolean valid indicating whether generation will work strictly against the specification, potentially making
     * minor changes to the input document.
     *
     * @param strictSpecBehavior true if we will behave strictly, false to allow specification documents which pass validation to be loosely interpreted against the spec.
     */
    @Override
    public void setStrictSpecBehavior(final boolean strictSpecBehavior) {
        this.strictSpecBehavior = strictSpecBehavior;
    }

    @Override
    public FeatureSet getFeatureSet() {
        return this.generatorMetadata.getFeatureSet();
    }

    /**
     * Get the boolean value indicating whether to remove enum value prefixes
     */
    @Override
    public boolean isRemoveEnumValuePrefix() {
        return this.removeEnumValuePrefix;
    }

    /**
     * Set the boolean value indicating whether to remove enum value prefixes
     *
     * @param removeEnumValuePrefix true to enable enum value prefix removal
     */
    @Override
    public void setRemoveEnumValuePrefix(final boolean removeEnumValuePrefix) {
        this.removeEnumValuePrefix = removeEnumValuePrefix;
    }

    //// Following methods are related to the "useOneOfInterfaces" feature

    /**
     * Add "x-one-of-name" extension to a given oneOf schema (assuming it has at least 1 oneOf elements)
     *
     * @param s    schema to add the extension to
     * @param name name of the parent oneOf schema
     */
    public void addOneOfNameExtension(ComposedSchema s, String name) {
        if (s.getOneOf() != null && s.getOneOf().size() > 0) {
            s.addExtension("x-one-of-name", name);
        }
    }

    /**
     * Add a given ComposedSchema as an interface model to be generated, assuming it has `oneOf` defined
     *
     * @param cs      ComposedSchema object to create as interface model
     * @param type    name to use for the generated interface model
     * @param openAPI OpenAPI spec that we are using
     */
    public void addOneOfInterfaceModel(ComposedSchema cs, String type, OpenAPI openAPI) {
        if (cs.getOneOf() == null) {
            return;
        }

        CodegenModel cm = new CodegenModel();

        cm.discriminator = createDiscriminator("", (Schema) cs, openAPI);
        if (!this.getLegacyDiscriminatorBehavior()) {
            cm.addDiscriminatorMappedModelsImports();
        }
        for (Schema o : Optional.ofNullable(cs.getOneOf()).orElse(Collections.emptyList())) {
            if (o.get$ref() == null) {
                if (cm.discriminator != null && o.get$ref() == null) {
                    // OpenAPI spec states that inline objects should not be considered when discriminator is used
                    // https://github.com/OAI/OpenAPI-Specification/blob/master/versions/3.0.2.md#discriminatorObject
                    LOGGER.warn("Ignoring inline object in oneOf definition of {}, since discriminator is used", type);
                } else {
                    LOGGER.warn("Inline models are not supported in oneOf definition right now");
                }
                continue;
            }
            cm.oneOf.add(toModelName(ModelUtils.getSimpleRef(o.get$ref())));
        }
        cm.name = type;
        cm.classname = type;
        cm.vendorExtensions.put("x-is-one-of-interface", true);
        cm.interfaceModels = new ArrayList<CodegenModel>();

        addOneOfInterfaces.add(cm);
    }

    public void addImportsToOneOfInterface(List<Map<String, String>> imports) {
    }
    //// End of methods related to the "useOneOfInterfaces" feature

    protected void modifyFeatureSet(Consumer<FeatureSet.Builder> processor) {
        FeatureSet.Builder builder = getFeatureSet().modify();
        processor.accept(builder);
        this.generatorMetadata = GeneratorMetadata.newBuilder(generatorMetadata)
                .featureSet(builder.build()).build();
    }

    private static class SanitizeNameOptions {
        public SanitizeNameOptions(String name, String removeCharRegEx, List<String> exceptions) {
            this.name = name;
            this.removeCharRegEx = removeCharRegEx;
            if (exceptions != null) {
                this.exceptions = Collections.unmodifiableList(exceptions);
            } else {
                this.exceptions = Collections.unmodifiableList(new ArrayList<>());
            }
        }

        public String getName() {
            return name;
        }

        public String getRemoveCharRegEx() {
            return removeCharRegEx;
        }

        public List<String> getExceptions() {
            return exceptions;
        }

        private String name;
        private String removeCharRegEx;
        private List<String> exceptions;

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            SanitizeNameOptions that = (SanitizeNameOptions) o;
            return Objects.equals(getName(), that.getName()) &&
                    Objects.equals(getRemoveCharRegEx(), that.getRemoveCharRegEx()) &&
                    Objects.equals(getExceptions(), that.getExceptions());
        }

        @Override
        public int hashCode() {
            return Objects.hash(getName(), getRemoveCharRegEx(), getExceptions());
        }
    }

    /**
     * Return true if the schema value can be any type, i.e. it can be
     * the null value, integer, number, string, object or array.
     * One use case is when the "type" attribute in the OAS schema is unspecified.
     *
     * Examples:
     *
     *     arbitraryTypeValue:
     *       description: This is an arbitrary type schema.
     *         It is not a free-form object.
     *         The value can be any type except the 'null' value.
     *     arbitraryTypeNullableValue:
     *       description: This is an arbitrary type schema.
     *         It is not a free-form object.
     *         The value can be any type, including the 'null' value.
     *       nullable: true
     *
     * @param schema the OAS schema.
     * @return true if the schema value can be an arbitrary type.
     */
    public boolean isAnyTypeSchema(Schema schema) {
        if (schema == null) {
            once(LOGGER).error("Schema cannot be null in isAnyTypeSchema check");
            return false;
        }

        if (isFreeFormObject(schema)) {
            // make sure it's not free form object
            return false;
        }

        if (schema.getClass().equals(Schema.class) && schema.get$ref() == null && schema.getType() == null &&
                (schema.getProperties() == null || schema.getProperties().isEmpty()) &&
                schema.getAdditionalProperties() == null && schema.getNot() == null &&
                schema.getEnum() == null) {
            return true;
            // If and when type arrays are supported in a future OAS specification,
            // we could return true if the type array includes all possible JSON schema types.
        }
        return false;
    }

    /**
     * Check to see if the schema is a free form object.
     *
     * A free form object is an object (i.e. 'type: object' in a OAS document) that:
     * 1) Does not define properties, and
     * 2) Is not a composed schema (no anyOf, oneOf, allOf), and
     * 3) additionalproperties is not defined, or additionalproperties: true, or additionalproperties: {}.
     *
     * Examples:
     *
     * components:
     *   schemas:
     *     arbitraryObject:
     *       type: object
     *       description: This is a free-form object.
     *         The value must be a map of strings to values. The value cannot be 'null'.
     *         It cannot be array, string, integer, number.
     *     arbitraryNullableObject:
     *       type: object
     *       description: This is a free-form object.
     *         The value must be a map of strings to values. The value can be 'null',
     *         It cannot be array, string, integer, number.
     *       nullable: true
     *     arbitraryTypeValue:
     *       description: This is NOT a free-form object.
     *         The value can be any type except the 'null' value.
     *
     * @param schema potentially containing a '$ref'
     * @return true if it's a free-form object
     */
    protected boolean isFreeFormObject(Schema schema) {
        return ModelUtils.isFreeFormObject(this.openAPI, schema);
    }

    /**
     * Returns the additionalProperties Schema for the specified input schema.
     * 
     * The additionalProperties keyword is used to control the handling of additional, undeclared
     * properties, that is, properties whose names are not listed in the properties keyword.
     * The additionalProperties keyword may be either a boolean or an object.
     * If additionalProperties is a boolean and set to false, no additional properties are allowed.
     * By default when the additionalProperties keyword is not specified in the input schema,
     * any additional properties are allowed. This is equivalent to setting additionalProperties
     * to the boolean value True or setting additionalProperties: {}
     * 
     * @param schema the input schema that may or may not have the additionalProperties keyword.
     * @return the Schema of the additionalProperties. The null value is returned if no additional
     *         properties are allowed.
     */
    protected Schema getAdditionalProperties(Schema schema) {
        return ModelUtils.getAdditionalProperties(openAPI, schema);
    }

}
