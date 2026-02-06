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
import lombok.Getter;
import lombok.Setter;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.*;
import org.openapitools.codegen.languages.features.BeanValidationFeatures;
import org.openapitools.codegen.meta.features.*;
import org.openapitools.codegen.model.ModelMap;
import org.openapitools.codegen.model.ModelsMap;
import org.openapitools.codegen.model.OperationMap;
import org.openapitools.codegen.model.OperationsMap;
import org.openapitools.codegen.templating.mustache.CamelCaseLambda;
import org.openapitools.codegen.templating.mustache.LowercaseLambda;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.*;

public class KotlinServerCodegen extends AbstractKotlinCodegen implements BeanValidationFeatures {

    public static final String DEFAULT_LIBRARY = Constants.KTOR;
    private final Logger LOGGER = LoggerFactory.getLogger(KotlinServerCodegen.class);

    @Getter
    @Setter
    private Boolean autoHeadFeatureEnabled = true;
    @Getter
    @Setter
    private Boolean conditionalHeadersFeatureEnabled = false;
    @Getter
    @Setter
    private Boolean hstsFeatureEnabled = true;
    @Getter
    @Setter
    private Boolean corsFeatureEnabled = false;
    @Getter
    @Setter
    private Boolean compressionFeatureEnabled = true;
    @Getter
    @Setter
    private Boolean resourcesFeatureEnabled = true;
    @Getter
    @Setter
    private Boolean metricsFeatureEnabled = true;
    private boolean interfaceOnly = false;
    private boolean useBeanValidation = false;
    private boolean useCoroutines = false;
    private boolean useMutiny = false;
    private boolean returnResponse = false;
    @Setter
    private boolean omitGradleWrapper = false;
    @Getter
    @Setter
    private boolean fixJacksonJsonTypeInfoInheritance = true;

    // This is here to potentially warn the user when an option is not supported by the target framework.
    private Map<String, List<String>> optionsSupportedPerFramework = new ImmutableMap.Builder<String, List<String>>()
            .put(Constants.KTOR, Arrays.asList(
                    Constants.AUTOMATIC_HEAD_REQUESTS,
                    Constants.CONDITIONAL_HEADERS,
                    Constants.HSTS,
                    Constants.CORS,
                    Constants.COMPRESSION,
                    Constants.RESOURCES,
                    Constants.METRICS,
                    Constants.OMIT_GRADLE_WRAPPER
            ))
            .put(Constants.KTOR2, Arrays.asList(
                    Constants.AUTOMATIC_HEAD_REQUESTS,
                    Constants.CONDITIONAL_HEADERS,
                    Constants.HSTS,
                    Constants.CORS,
                    Constants.COMPRESSION,
                    Constants.RESOURCES,
                    Constants.METRICS,
                    Constants.OMIT_GRADLE_WRAPPER
            ))
            .put(Constants.JAXRS_SPEC, Arrays.asList(
                    USE_BEANVALIDATION,
                    Constants.USE_COROUTINES,
                    Constants.USE_MUTINY,
                    Constants.RETURN_RESPONSE,
                    Constants.INTERFACE_ONLY
            ))
            .build();

    /**
     * Constructs an instance of `KotlinServerCodegen`.
     */
    public KotlinServerCodegen() {
        super();

        // Enable proper oneOf/anyOf discriminator handling for polymorphism
        legacyDiscriminatorBehavior = false;

        modifyFeatureSet(features -> features
                .includeDocumentationFeatures(DocumentationFeature.Readme)
                .wireFormatFeatures(EnumSet.of(WireFormatFeature.JSON, WireFormatFeature.XML))
                .securityFeatures(EnumSet.of(
                        SecurityFeature.BasicAuth,
                        SecurityFeature.ApiKey,
                        SecurityFeature.OAuth2_Implicit
                ))
                .excludeGlobalFeatures(
                        GlobalFeature.XMLStructureDefinitions,
                        GlobalFeature.Callbacks,
                        GlobalFeature.LinkObjects,
                        GlobalFeature.ParameterStyling
                )
                .includeSchemaSupportFeatures(
                        SchemaSupportFeature.allOf,
                        SchemaSupportFeature.oneOf,
                        SchemaSupportFeature.Polymorphism
                )
                .excludeParameterFeatures(
                        ParameterFeature.Cookie
                )
        );

        artifactId = "kotlin-server";
        packageName = "org.openapitools.server";

        typeMapping.put("array", "kotlin.collections.List");

        // cliOptions default redefinition need to be updated
        updateOption(CodegenConstants.ARTIFACT_ID, this.artifactId);
        updateOption(CodegenConstants.PACKAGE_NAME, this.packageName);

        outputFolder = "generated-code" + File.separator + "kotlin-server";
        modelTemplateFiles.put("model.mustache", ".kt");
        apiTemplateFiles.put("api.mustache", ".kt");
        embeddedTemplateDir = templateDir = "kotlin-server";
        apiPackage = packageName + ".apis";
        modelPackage = packageName + ".models";

        supportedLibraries.put(Constants.KTOR, "ktor framework");
        supportedLibraries.put(Constants.KTOR2, "ktor (2.x) framework");
        supportedLibraries.put(Constants.JAXRS_SPEC, "JAX-RS spec only");
        supportedLibraries.put(Constants.JAVALIN5, "Javalin 5");
        supportedLibraries.put(Constants.JAVALIN6, "Javalin 6");

        // TODO: Configurable server engine. Defaults to netty in build.gradle.
        addOption(CodegenConstants.LIBRARY, CodegenConstants.LIBRARY_DESC, DEFAULT_LIBRARY, supportedLibraries);
        addSwitch(Constants.AUTOMATIC_HEAD_REQUESTS, Constants.AUTOMATIC_HEAD_REQUESTS_DESC, getAutoHeadFeatureEnabled());
        addSwitch(Constants.CONDITIONAL_HEADERS, Constants.CONDITIONAL_HEADERS_DESC, getConditionalHeadersFeatureEnabled());
        addSwitch(Constants.HSTS, Constants.HSTS_DESC, getHstsFeatureEnabled());
        addSwitch(Constants.CORS, Constants.CORS_DESC, getCorsFeatureEnabled());
        addSwitch(Constants.COMPRESSION, Constants.COMPRESSION_DESC, getCompressionFeatureEnabled());
        addSwitch(Constants.RESOURCES, Constants.RESOURCES_DESC, getResourcesFeatureEnabled());
        addSwitch(Constants.METRICS, Constants.METRICS_DESC, getMetricsFeatureEnabled());
        addSwitch(Constants.INTERFACE_ONLY, Constants.INTERFACE_ONLY_DESC, interfaceOnly);
        addSwitch(USE_BEANVALIDATION, Constants.USE_BEANVALIDATION_DESC, useBeanValidation);
        addSwitch(Constants.USE_COROUTINES, Constants.USE_COROUTINES_DESC, useCoroutines);
        addSwitch(Constants.USE_MUTINY, Constants.USE_MUTINY_DESC, useMutiny);
        addSwitch(Constants.RETURN_RESPONSE, Constants.RETURN_RESPONSE_DESC, returnResponse);
        addSwitch(Constants.OMIT_GRADLE_WRAPPER, Constants.OMIT_GRADLE_WRAPPER_DESC, omitGradleWrapper);
        addSwitch(USE_JAKARTA_EE, Constants.USE_JAKARTA_EE_DESC, useJakartaEe);
        addSwitch(Constants.FIX_JACKSON_JSON_TYPE_INFO_INHERITANCE, Constants.FIX_JACKSON_JSON_TYPE_INFO_INHERITANCE_DESC, fixJacksonJsonTypeInfoInheritance);
    }

    @Override
    public String getHelp() {
        return "Generates a Kotlin server.";
    }

    public boolean getOmitGradleWrapper() {
        return omitGradleWrapper;
    }

    @Override
    public String getName() {
        return "kotlin-server";
    }

    @Override
    public CodegenType getTag() {
        return CodegenType.SERVER;
    }

    @Override
    public void processOpts() {
        super.processOpts();

        if (isModelMutable()) {
            typeMapping.put("array", "kotlin.collections.MutableList");
        }

        if (additionalProperties.containsKey(CodegenConstants.LIBRARY)) {
            this.setLibrary((String) additionalProperties.get(CodegenConstants.LIBRARY));
        }

        if (additionalProperties.containsKey(Constants.INTERFACE_ONLY)) {
            interfaceOnly = Boolean.parseBoolean(additionalProperties.get(Constants.INTERFACE_ONLY).toString());
            if (!interfaceOnly) {
                additionalProperties.remove(Constants.INTERFACE_ONLY);
            }
        }

        if (additionalProperties.containsKey(Constants.USE_COROUTINES)) {
            useCoroutines = Boolean.parseBoolean(additionalProperties.get(Constants.USE_COROUTINES).toString());
            if (!useCoroutines) {
                additionalProperties.remove(Constants.USE_COROUTINES);
            }
        }

        if (additionalProperties.containsKey(Constants.USE_MUTINY)) {
            useMutiny = Boolean.parseBoolean(additionalProperties.get(Constants.USE_MUTINY).toString());
            if (!useMutiny) {
                additionalProperties.remove(Constants.USE_MUTINY);
            }
        }

        if (additionalProperties.containsKey(Constants.RETURN_RESPONSE)) {
            returnResponse = Boolean.parseBoolean(additionalProperties.get(Constants.RETURN_RESPONSE).toString());
            if (!returnResponse) {
                additionalProperties.remove(Constants.RETURN_RESPONSE);
            }
        }

        if (additionalProperties.containsKey(USE_BEANVALIDATION)) {
            setUseBeanValidation(convertPropertyToBoolean(USE_BEANVALIDATION));
        }

        if (additionalProperties.containsKey(Constants.OMIT_GRADLE_WRAPPER)) {
            setOmitGradleWrapper(Boolean.parseBoolean(additionalProperties.get(Constants.OMIT_GRADLE_WRAPPER).toString()));
        }

        if (additionalProperties.containsKey(Constants.FIX_JACKSON_JSON_TYPE_INFO_INHERITANCE)) {
            setFixJacksonJsonTypeInfoInheritance(Boolean.parseBoolean(additionalProperties.get(Constants.FIX_JACKSON_JSON_TYPE_INFO_INHERITANCE).toString()));
        }
        additionalProperties.put(Constants.FIX_JACKSON_JSON_TYPE_INFO_INHERITANCE, fixJacksonJsonTypeInfoInheritance);

        writePropertyBack(USE_BEANVALIDATION, useBeanValidation);

        // set default library to "ktor"
        if (StringUtils.isEmpty(library)) {
            this.setLibrary(DEFAULT_LIBRARY);
            additionalProperties.put(CodegenConstants.LIBRARY, DEFAULT_LIBRARY);
            LOGGER.info("`library` option is empty. Default to {}", DEFAULT_LIBRARY);
        }

        if (isKtor()) {
            typeMapping.put("date-time", "kotlin.String");
            typeMapping.put("DateTime", "kotlin.String");
        }

        if (additionalProperties.containsKey(Constants.AUTOMATIC_HEAD_REQUESTS)) {
            setAutoHeadFeatureEnabled(convertPropertyToBooleanAndWriteBack(Constants.AUTOMATIC_HEAD_REQUESTS));
        } else {
            additionalProperties.put(Constants.AUTOMATIC_HEAD_REQUESTS, getAutoHeadFeatureEnabled());
        }

        if (additionalProperties.containsKey(Constants.CONDITIONAL_HEADERS)) {
            setConditionalHeadersFeatureEnabled(convertPropertyToBooleanAndWriteBack(Constants.CONDITIONAL_HEADERS));
        } else {
            additionalProperties.put(Constants.CONDITIONAL_HEADERS, getConditionalHeadersFeatureEnabled());
        }

        if (additionalProperties.containsKey(Constants.HSTS)) {
            setHstsFeatureEnabled(convertPropertyToBooleanAndWriteBack(Constants.HSTS));
        } else {
            additionalProperties.put(Constants.HSTS, getHstsFeatureEnabled());
        }

        if (additionalProperties.containsKey(Constants.CORS)) {
            setCorsFeatureEnabled(convertPropertyToBooleanAndWriteBack(Constants.CORS));
        } else {
            additionalProperties.put(Constants.CORS, getCorsFeatureEnabled());
        }

        if (additionalProperties.containsKey(Constants.COMPRESSION)) {
            setCompressionFeatureEnabled(convertPropertyToBooleanAndWriteBack(Constants.COMPRESSION));
        } else {
            additionalProperties.put(Constants.COMPRESSION, getCompressionFeatureEnabled());
        }

        if (additionalProperties.containsKey(Constants.RESOURCES)) {
            setResourcesFeatureEnabled(convertPropertyToBooleanAndWriteBack(Constants.RESOURCES));
        } else {
            additionalProperties.put(Constants.RESOURCES, getResourcesFeatureEnabled());
        }

        if (additionalProperties.containsKey(Constants.METRICS)) {
            setMetricsFeatureEnabled(convertPropertyToBooleanAndWriteBack(Constants.METRICS));
        } else {
            additionalProperties.put(Constants.METRICS, getMetricsFeatureEnabled());
        }

        boolean generateApis = additionalProperties.containsKey(CodegenConstants.GENERATE_APIS) && (Boolean) additionalProperties.get(CodegenConstants.GENERATE_APIS);
        String packageFolder = (sourceFolder + File.separator + packageName).replace(".", File.separator);

        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));

        if (isKtor2Or3()) {
            supportingFiles.add(new SupportingFile("Dockerfile.mustache", "", "Dockerfile"));
        }

        String gradleBuildFile = "build.gradle";

        if (isJavalin() || isKtor2Or3()) {
            gradleBuildFile = "build.gradle.kts";
        }

        supportingFiles.add(new SupportingFile(gradleBuildFile + ".mustache", "", gradleBuildFile));
        supportingFiles.add(new SupportingFile("settings.gradle.mustache", "", "settings.gradle"));
        supportingFiles.add(new SupportingFile("gradle.properties", "", "gradle.properties"));

        if (isKtor2Or3()) {
            additionalProperties.put(Constants.IS_KTOR, true);

            supportingFiles.add(new SupportingFile("AppMain.kt.mustache", packageFolder, "AppMain.kt"));
            supportingFiles.add(new SupportingFile("Configuration.kt.mustache", packageFolder, "Configuration.kt"));

            if (generateApis && resourcesFeatureEnabled) {
                supportingFiles.add(new SupportingFile("Paths.kt.mustache", packageFolder, "Paths.kt"));
            }

            supportingFiles.add(new SupportingFile("application.conf.mustache", resourcesFolder, "application.conf"));
            supportingFiles.add(new SupportingFile("logback.xml", resourcesFolder, "logback.xml"));

            final String infrastructureFolder = (sourceFolder + File.separator + packageName + File.separator + "infrastructure").replace(".", File.separator);

            supportingFiles.add(new SupportingFile("ApiKeyAuth.kt.mustache", infrastructureFolder, "ApiKeyAuth.kt"));

            if (!getOmitGradleWrapper()) {
                supportingFiles.add(new SupportingFile("gradle-wrapper.properties", "gradle" + File.separator + "wrapper", "gradle-wrapper.properties"));
            }

        } else if (isJavalin()) {
            supportingFiles.add(new SupportingFile("Main.kt.mustache", packageFolder, "Main.kt"));
            apiTemplateFiles.put("service.mustache", "Service.kt");
            apiTemplateFiles.put("serviceImpl.mustache", "ServiceImpl.kt");
            additionalProperties.put("lowercase", new LowercaseLambda());
            additionalProperties.put("camelcase", new CamelCaseLambda());
            typeMapping.put("file", "io.javalin.http.UploadedFile");
            importMapping.put("io.javalin.http.UploadedFile", "io.javalin.http.UploadedFile");
        }
    }

    @Override
    public void setUseBeanValidation(boolean useBeanValidation) {
        this.useBeanValidation = useBeanValidation;
    }

    public static class Constants {
        public final static String KTOR = "ktor";
        public final static String KTOR2 = "ktor2";
        public final static String JAXRS_SPEC = "jaxrs-spec";

        public final static String JAVALIN5 = "javalin5";
        public final static String JAVALIN6 = "javalin6";
        public final static String AUTOMATIC_HEAD_REQUESTS = "featureAutoHead";
        public final static String AUTOMATIC_HEAD_REQUESTS_DESC = "Automatically provide responses to HEAD requests for existing routes that have the GET verb defined.";
        public final static String CONDITIONAL_HEADERS = "featureConditionalHeaders";
        public final static String CONDITIONAL_HEADERS_DESC = "Avoid sending content if client already has same content, by checking ETag or LastModified properties.";
        public final static String HSTS = "featureHSTS";
        public final static String HSTS_DESC = "Avoid sending content if client already has same content, by checking ETag or LastModified properties.";
        public final static String CORS = "featureCORS";
        public final static String CORS_DESC = "Ktor by default provides an interceptor for implementing proper support for Cross-Origin Resource Sharing (CORS). See enable-cors.org.";
        public final static String COMPRESSION = "featureCompression";
        public final static String COMPRESSION_DESC = "Adds ability to compress outgoing content using gzip, deflate or custom encoder and thus reduce size of the response.";
        public final static String RESOURCES = "featureResources";
        public final static String RESOURCES_DESC = "Generates routes in a typed way, for both: constructing URLs and reading the parameters.";
        public final static String METRICS = "featureMetrics";
        public final static String METRICS_DESC = "Enables metrics feature.";
        public static final String INTERFACE_ONLY = "interfaceOnly";
        public static final String INTERFACE_ONLY_DESC = "Whether to generate only API interface stubs without the server files. This option is currently supported only when using jaxrs-spec library.";
        public static final String USE_BEANVALIDATION_DESC = "Use BeanValidation API annotations. This option is currently supported only when using jaxrs-spec library.";
        public static final String USE_COROUTINES = "useCoroutines";
        public static final String USE_COROUTINES_DESC = "Whether to use the Coroutines. This option is currently supported only when using jaxrs-spec library.";
        public static final String RETURN_RESPONSE = "returnResponse";
        public static final String RETURN_RESPONSE_DESC = "Whether generate API interface should return javax.ws.rs.core.Response instead of a deserialized entity. Only useful if interfaceOnly is true. This option is currently supported only when using jaxrs-spec library.";
        public static final String USE_JAKARTA_EE_DESC = "whether to use Jakarta EE namespace instead of javax";
        public static final String USE_MUTINY = "useMutiny";
        public static final String USE_MUTINY_DESC = "Whether to use Mutiny (should not be used with useCoroutines). This option is currently supported only when using jaxrs-spec library.";
        public static final String OMIT_GRADLE_WRAPPER = "omitGradleWrapper";
        public static final String OMIT_GRADLE_WRAPPER_DESC = "Whether to omit Gradle wrapper for creating a sub project.";
        public static final String IS_KTOR = "isKtor";
        public static final String FIX_JACKSON_JSON_TYPE_INFO_INHERITANCE = "fixJacksonJsonTypeInfoInheritance";
        public static final String FIX_JACKSON_JSON_TYPE_INFO_INHERITANCE_DESC = "When true (default), ensures Jackson polymorphism works correctly by: (1) always setting visible=true on @JsonTypeInfo, and (2) adding the discriminator property to child models with appropriate default values. When false, visible is only set to true if all children already define the discriminator property.";
    }

    @Override
    public Map<String, ModelsMap> postProcessAllModels(Map<String, ModelsMap> objs) {
        objs = super.postProcessAllModels(objs);

        // For libraries that use Jackson, set up parent-child relationships for discriminator children
        // This enables proper polymorphism support with @JsonTypeInfo and @JsonSubTypes annotations
        if (usesJacksonSerialization()) {
            // Build a map of model name -> model for easy lookup
            Map<String, CodegenModel> allModelsMap = new HashMap<>();
            for (ModelsMap modelsMap : objs.values()) {
                for (ModelMap modelMap : modelsMap.getModels()) {
                    CodegenModel model = modelMap.getModel();
                    allModelsMap.put(model.getClassname(), model);
                }
            }

            // First pass: collect all discriminator parent -> children mappings
            // Also identify the "true" discriminator owners (not inherited via allOf)
            Map<String, String> childToParentMap = new HashMap<>();
            Set<String> trueDiscriminatorOwners = new HashSet<>();

            for (ModelsMap modelsMap : objs.values()) {
                for (ModelMap modelMap : modelsMap.getModels()) {
                    CodegenModel model = modelMap.getModel();
                    if (model.getDiscriminator() != null && model.getDiscriminator().getMappedModels() != null
                            && !model.getDiscriminator().getMappedModels().isEmpty()) {
                        String discriminatorPropBaseName = model.getDiscriminator().getPropertyBaseName();

                        for (CodegenDiscriminator.MappedModel mappedModel : model.getDiscriminator().getMappedModels()) {
                            childToParentMap.put(mappedModel.getModelName(), model.getClassname());

                            // If the mapping name equals the model name, check if we can derive
                            // a better mapping name from the child's discriminator property enum value
                            if (mappedModel.getMappingName().equals(mappedModel.getModelName())) {
                                CodegenModel childModel = allModelsMap.get(mappedModel.getModelName());
                                if (childModel != null) {
                                    // Find the discriminator property in the child model
                                    for (CodegenProperty prop : childModel.getAllVars()) {
                                        if (prop.getBaseName().equals(discriminatorPropBaseName) && prop.isEnum) {
                                            // If it's an enum with exactly one value, use that as the mapping name
                                            Map<String, Object> allowableValues = prop.getAllowableValues();
                                            if (allowableValues != null && allowableValues.containsKey("values")) {
                                                @SuppressWarnings("unchecked")
                                                List<Object> values = (List<Object>) allowableValues.get("values");
                                                if (values != null && values.size() == 1) {
                                                    mappedModel.setMappingName(String.valueOf(values.get(0)));
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                        // This model owns its discriminator (has mapped models)
                        trueDiscriminatorOwners.add(model.getClassname());
                    }
                }
            }

            // Second pass: process child models
            for (ModelsMap modelsMap : objs.values()) {
                for (ModelMap modelMap : modelsMap.getModels()) {
                    CodegenModel model = modelMap.getModel();
                    String parentName = childToParentMap.get(model.getClassname());

                    if (parentName != null) {
                        // This model is a child of a discriminator parent
                        CodegenModel parentModel = allModelsMap.get(parentName);

                        // Set parent if not already set
                        if (model.getParent() == null) {
                            model.setParent(parentName);
                        }

                        // If this child has a discriminator but it's inherited (not a true owner),
                        // remove it - only the parent should have the discriminator annotations
                        if (model.getDiscriminator() != null && !trueDiscriminatorOwners.contains(model.getClassname())) {
                            model.setDiscriminator(null);
                        }

                        // For allOf pattern: if parent has properties, mark child's inherited properties
                        // Skip this for oneOf/anyOf patterns where parent properties are merged from children
                        boolean parentIsOneOfOrAnyOf = parentModel != null
                                && ((parentModel.oneOf != null && !parentModel.oneOf.isEmpty())
                                || (parentModel.anyOf != null && !parentModel.anyOf.isEmpty()));

                        if (parentModel != null && parentModel.getHasVars() && !parentIsOneOfOrAnyOf) {
                            Set<String> parentPropNames = new HashSet<>();
                            List<String> inheritedPropNamesList = new ArrayList<>();
                            for (CodegenProperty parentProp : parentModel.getAllVars()) {
                                parentPropNames.add(parentProp.getBaseName());
                                inheritedPropNamesList.add(parentProp.getName());
                            }

                            // Mark properties inherited from parent
                            for (CodegenProperty prop : model.getAllVars()) {
                                if (parentPropNames.contains(prop.getBaseName())) {
                                    prop.isInherited = true;
                                }
                            }
                            for (CodegenProperty prop : model.getVars()) {
                                if (parentPropNames.contains(prop.getBaseName())) {
                                    prop.isInherited = true;
                                }
                            }
                            for (CodegenProperty prop : model.getRequiredVars()) {
                                if (parentPropNames.contains(prop.getBaseName())) {
                                    prop.isInherited = true;
                                }
                            }
                            for (CodegenProperty prop : model.getOptionalVars()) {
                                if (parentPropNames.contains(prop.getBaseName())) {
                                    prop.isInherited = true;
                                }
                            }

                            // Set vendor extension for parent constructor call with inherited properties
                            if (!inheritedPropNamesList.isEmpty()) {
                                String parentCtorArgs = String.join(", ", inheritedPropNamesList.stream()
                                        .map(name -> name + " = " + name)
                                        .toArray(String[]::new));
                                model.getVendorExtensions().put("x-parent-ctor-args", parentCtorArgs);
                            }
                        }
                    }
                }
            }

            // Third pass: set vendor extension for discriminator style and handle fixJacksonJsonTypeInfoInheritance
            for (String ownerName : trueDiscriminatorOwners) {
                CodegenModel owner = allModelsMap.get(ownerName);
                if (owner != null && owner.getDiscriminator() != null) {
                    String discriminatorPropBaseName = owner.getDiscriminator().getPropertyBaseName();
                    boolean isOneOfOrAnyOfPattern = (owner.oneOf != null && !owner.oneOf.isEmpty())
                            || (owner.anyOf != null && !owner.anyOf.isEmpty());

                    // hasParentProperties controls whether the sealed class has properties in its constructor
                    // This should be false for oneOf/anyOf patterns (parent is a type union, no direct properties)
                    // and true for allOf patterns (parent has properties that children inherit)
                    boolean hasParentProperties = !isOneOfOrAnyOfPattern;

                    // visibleTrue controls whether visible=true is set on @JsonTypeInfo
                    // When fixJacksonJsonTypeInfoInheritance is true, we always set visible=true
                    // When false, we only set visible=true if the parent has properties (allOf pattern)
                    boolean visibleTrue;

                    if (fixJacksonJsonTypeInfoInheritance) {
                        // When fixJacksonJsonTypeInfoInheritance is true:
                        // 1. Always set visible=true so Jackson can read the discriminator
                        // 2. For oneOf/anyOf patterns: add discriminator property to parent and children
                        visibleTrue = true;

                        // For oneOf/anyOf patterns, add the discriminator property to the parent sealed class
                        // This allows accessing the discriminator value from the parent type directly
                        if (isOneOfOrAnyOfPattern) {
                            String discriminatorVarName = toVarName(discriminatorPropBaseName);

                            // Clear all merged properties from the oneOf parent - they belong to children only
                            // We'll add back just the discriminator property
                            owner.getVars().clear();
                            owner.getRequiredVars().clear();
                            owner.getOptionalVars().clear();
                            owner.getAllVars().clear();

                            // Add discriminator property to parent
                            CodegenProperty parentDiscriminatorProp = new CodegenProperty();
                            parentDiscriminatorProp.baseName = discriminatorPropBaseName;
                            parentDiscriminatorProp.name = discriminatorVarName;
                            parentDiscriminatorProp.dataType = "kotlin.String";
                            parentDiscriminatorProp.datatypeWithEnum = "kotlin.String";
                            parentDiscriminatorProp.required = true;
                            parentDiscriminatorProp.isNullable = false;
                            parentDiscriminatorProp.isReadOnly = false;

                            owner.getVars().add(parentDiscriminatorProp);
                            owner.getRequiredVars().add(parentDiscriminatorProp);
                            owner.getAllVars().add(parentDiscriminatorProp);

                            // Parent now has properties (just the discriminator)
                            hasParentProperties = true;

                            // Process children: mark discriminator as inherited and set default values
                            for (CodegenDiscriminator.MappedModel mappedModel : owner.getDiscriminator().getMappedModels()) {
                                CodegenModel childModel = allModelsMap.get(mappedModel.getModelName());
                                if (childModel != null) {
                                    boolean hasDiscriminatorProp = false;
                                    String discriminatorDefault = "\"" + mappedModel.getMappingName() + "\"";

                                    // Update existing discriminator property in all lists - mark as inherited
                                    for (CodegenProperty prop : childModel.getVars()) {
                                        if (prop.getBaseName().equals(discriminatorPropBaseName)) {
                                            hasDiscriminatorProp = true;
                                            prop.defaultValue = discriminatorDefault;
                                            prop.dataType = "kotlin.String";
                                            prop.datatypeWithEnum = "kotlin.String";
                                            prop.required = true;
                                            prop.isNullable = false;
                                            prop.isInherited = true;
                                        }
                                    }
                                    for (CodegenProperty prop : childModel.getAllVars()) {
                                        if (prop.getBaseName().equals(discriminatorPropBaseName)) {
                                            prop.defaultValue = discriminatorDefault;
                                            prop.dataType = "kotlin.String";
                                            prop.datatypeWithEnum = "kotlin.String";
                                            prop.required = true;
                                            prop.isNullable = false;
                                            prop.isInherited = true;
                                        }
                                    }

                                    // Move discriminator from optionalVars to requiredVars if needed
                                    CodegenProperty propToMove = null;
                                    for (CodegenProperty prop : childModel.getOptionalVars()) {
                                        if (prop.getBaseName().equals(discriminatorPropBaseName)) {
                                            prop.defaultValue = discriminatorDefault;
                                            prop.dataType = "kotlin.String";
                                            prop.datatypeWithEnum = "kotlin.String";
                                            prop.required = true;
                                            prop.isNullable = false;
                                            prop.isInherited = true;
                                            propToMove = prop;
                                            break;
                                        }
                                    }
                                    if (propToMove != null) {
                                        childModel.getOptionalVars().remove(propToMove);
                                        childModel.getRequiredVars().add(propToMove);
                                    }

                                    // Also update if it's already in requiredVars
                                    for (CodegenProperty prop : childModel.getRequiredVars()) {
                                        if (prop.getBaseName().equals(discriminatorPropBaseName)) {
                                            prop.defaultValue = discriminatorDefault;
                                            prop.dataType = "kotlin.String";
                                            prop.datatypeWithEnum = "kotlin.String";
                                            prop.isNullable = false;
                                            prop.isInherited = true;
                                        }
                                    }

                                    // If child doesn't have the discriminator property, add it as required and inherited
                                    if (!hasDiscriminatorProp) {
                                        CodegenProperty discriminatorProp = new CodegenProperty();
                                        discriminatorProp.baseName = discriminatorPropBaseName;
                                        discriminatorProp.name = discriminatorVarName;
                                        discriminatorProp.dataType = "kotlin.String";
                                        discriminatorProp.datatypeWithEnum = "kotlin.String";
                                        discriminatorProp.defaultValue = discriminatorDefault;
                                        discriminatorProp.required = true;
                                        discriminatorProp.isNullable = false;
                                        discriminatorProp.isReadOnly = false;
                                        discriminatorProp.isInherited = true;

                                        childModel.getVars().add(discriminatorProp);
                                        childModel.getRequiredVars().add(discriminatorProp);
                                        childModel.getAllVars().add(discriminatorProp);
                                    }

                                    // Set parent constructor args for the discriminator property
                                    childModel.getVendorExtensions().put("x-parent-ctor-args",
                                            discriminatorVarName + " = " + discriminatorVarName);
                                }
                            }
                        }
                    } else {
                        // When fixJacksonJsonTypeInfoInheritance is false:
                        // visible=true only for allOf pattern (parent has properties)
                        visibleTrue = hasParentProperties;
                    }

                    // Set on both model and discriminator so it's accessible in different template contexts
                    owner.getVendorExtensions().put("x-discriminator-has-parent-properties", hasParentProperties);
                    owner.getDiscriminator().getVendorExtensions().put("x-discriminator-has-parent-properties", hasParentProperties);
                    owner.getVendorExtensions().put("x-discriminator-visible-true", visibleTrue);
                    owner.getDiscriminator().getVendorExtensions().put("x-discriminator-visible-true", visibleTrue);
                }
            }
        }

        return objs;
    }

    @Override
    public void postProcess() {
        System.out.println("################################################################################");
        System.out.println("# Thanks for using OpenAPI Generator.                                          #");
        System.out.println("# Please consider donation to help us maintain this project \uD83D\uDE4F                #");
        System.out.println("# https://opencollective.com/openapi_generator/donate                          #");
        System.out.println("#                                                                              #");
        System.out.println("# This generator's contributed by Jim Schubert (https://github.com/jimschubert)#");
        System.out.println("# Please support his work directly via https://patreon.com/jimschubert \uD83D\uDE4F     #");
        System.out.println("################################################################################");
    }

    @Override
    public OperationsMap postProcessOperationsWithModels(OperationsMap objs, List<ModelMap> allModels) {
        OperationMap operations = objs.getOperations();
        // The following processing breaks the JAX-RS spec, so we only do this for the other libs.
        if (operations != null && !Objects.equals(library, Constants.JAXRS_SPEC)) {
            List<CodegenOperation> ops = operations.getOperation();
            ops.forEach(operation -> {
                if (isKtor()) {
                    ArrayList<CodegenParameter> params = new ArrayList<>();
                    params.addAll(operation.pathParams);
                    params.addAll(operation.queryParams);
                    operation.vendorExtensions.put("ktor-params", params);
                }

                List<CodegenResponse> responses = operation.responses;
                if (responses != null) {
                    responses.forEach(resp -> {

                        if ("0".equals(resp.code)) {
                            resp.code = "200";
                        }

                        doDataTypeAssignment(resp.dataType, new DataTypeAssigner() {
                            @Override
                            public void setIsVoid(Boolean isVoid) {
                                resp.isVoid = isVoid;
                            }

                            @Override
                            public void setReturnType(final String returnType) {
                                resp.dataType = returnType;
                            }

                            @Override
                            public void setReturnContainer(final String returnContainer) {
                                resp.containerType = returnContainer;
                            }
                        });
                    });
                }

                doDataTypeAssignment(operation.returnType, new DataTypeAssigner() {
                    @Override
                    public void setIsVoid(Boolean isVoid) {
                        operation.isVoid = isVoid;
                    }

                    @Override
                    public void setReturnType(final String returnType) {
                        operation.returnType = returnType;
                    }

                    @Override
                    public void setReturnContainer(final String returnContainer) {
                        operation.returnContainer = returnContainer;
                    }
                });
            });
        }

        return objs;
    }

    private boolean isJavalin() {
        return Constants.JAVALIN5.equals(library) || Constants.JAVALIN6.equals(library);
    }

    /**
     * Returns true if the current library uses Jackson for JSON serialization.
     * This is used to determine if Jackson-specific features like polymorphism annotations should be enabled.
     */
    private boolean usesJacksonSerialization() {
        return Constants.JAVALIN5.equals(library) ||
               Constants.JAVALIN6.equals(library) ||
               Constants.JAXRS_SPEC.equals(library);
    }

    private boolean isKtor2Or3() {
        return Constants.KTOR.equals(library) || Constants.KTOR2.equals(library);
    }

    /**
     * Returns true if latest version of ktor is used.
     *
     * @return true if latest version of ktor is used.
     */
    private boolean isKtor() {
        return Constants.KTOR.equals(library);
    }

    private boolean isKtor2() {
        return Constants.KTOR2.equals(library);
    }
}
