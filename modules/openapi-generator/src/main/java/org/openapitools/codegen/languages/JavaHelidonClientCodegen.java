/*
 * Copyright 2022 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 * Copyright (c) 2022 Oracle and/or its affiliates
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

import java.io.File;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.TreeSet;

import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.servers.Server;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.CliOption;
import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.CodegenModel;
import org.openapitools.codegen.CodegenOperation;
import org.openapitools.codegen.CodegenParameter;
import org.openapitools.codegen.CodegenProperty;
import org.openapitools.codegen.CodegenResponse;
import org.openapitools.codegen.CodegenType;
import org.openapitools.codegen.SupportingFile;
import org.openapitools.codegen.VendorExtension;
import org.openapitools.codegen.meta.GeneratorMetadata;
import org.openapitools.codegen.meta.Stability;
import org.openapitools.codegen.meta.features.DocumentationFeature;
import org.openapitools.codegen.meta.features.GlobalFeature;
import org.openapitools.codegen.model.ModelMap;
import org.openapitools.codegen.model.ModelsMap;
import org.openapitools.codegen.model.OperationsMap;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import static org.openapitools.codegen.CodegenConstants.SERIALIZATION_LIBRARY;

public class JavaHelidonClientCodegen extends JavaHelidonCommonCodegen {

    private final Logger LOGGER = LoggerFactory.getLogger(JavaHelidonClientCodegen.class);

    private static final String X_HELIDON_REQUIRED_IMPL_IMPORTS = "x-helidon-requiredImplImports";
    private static final String X_HELIDON_IMPL_IMPORTS = "x-helidon-implImports";
    public static final String CONFIG_KEY = "configKey";

    protected String configKey = null;
    protected boolean useBeanValidation = false;
    protected boolean performBeanValidation = false;
    protected boolean useGzipFeature = false;
    protected boolean caseInsensitiveResponseHeaders = false;
    protected Path invokerFolder;
    protected Path apiFolder;
    protected String serializationLibrary = null;

    /**
     * Constructor for this generator. Uses the embedded template dir to find common templates
     * shared between Helidon client and server generators.
     *
     * @see org.openapitools.codegen.templating.GeneratorTemplateContentLocator#getFullTemplatePath
     */
    public JavaHelidonClientCodegen() {
        // beta for now
        generatorMetadata = GeneratorMetadata.newBuilder(generatorMetadata)
                .stability(Stability.BETA)
                .build();
        // TODO: Move GlobalFeature.ParameterizedServer to library: jersey after moving featureSet to generatorMetadata
        modifyFeatureSet(features -> features
                .includeDocumentationFeatures(DocumentationFeature.Readme)
                .includeGlobalFeatures(GlobalFeature.ParameterizedServer)
        );

        outputFolder = "generated-code" + File.separator + "java";
        embeddedTemplateDir = "java-helidon" + File.separator + "common";        // use embedded for common
        templateDir = "java-helidon" + File.separator + "client";
        invokerPackage = "org.openapitools.client";
        artifactId = "openapi-java-client";
        apiPackage = invokerPackage + ".api";
        modelPackage = invokerPackage + ".model";

        updateOption(CodegenConstants.INVOKER_PACKAGE, getInvokerPackage());
        updateOption(CodegenConstants.ARTIFACT_ID, getArtifactId());
        updateOption(CodegenConstants.API_PACKAGE, apiPackage);
        updateOption(CodegenConstants.MODEL_PACKAGE, modelPackage);

        modelTestTemplateFiles.put("model_test.mustache", ".java");

        cliOptions.add(CliOption.newString(CONFIG_KEY, "Config key in @RegisterRestClient. Default to none."));

        supportedLibraries.put(HELIDON_MP, "Helidon MP Client");
        supportedLibraries.put(HELIDON_SE, "Helidon SE Client");

        CliOption libraryOption = new CliOption(CodegenConstants.LIBRARY,
                "library template (sub-template) to use");
        libraryOption.setEnum(supportedLibraries);
        libraryOption.setDefault(HELIDON_MP);
        cliOptions.add(libraryOption);
        setLibrary(HELIDON_MP);     // default

        CliOption serializationLibrary = new CliOption(SERIALIZATION_LIBRARY,
                "Serialization library, defaults to Jackson");
        Map<String, String> serializationOptions = new HashMap<>();
        serializationOptions.put(SERIALIZATION_LIBRARY_JACKSON, "Use Jackson as serialization library");
        serializationOptions.put(SERIALIZATION_LIBRARY_JSONB, "Use JSON-B as serialization library");
        serializationLibrary.setEnum(serializationOptions);
        cliOptions.add(serializationLibrary);
        setSerializationLibrary(SERIALIZATION_LIBRARY_JACKSON);     // default

        removeUnusedOptions();

        // Ensure the OAS 3.x discriminator mappings include any descendent schemas that allOf
        // inherit from self, any oneOf schemas, any anyOf schemas, any x-discriminator-values,
        // and the discriminator mapping schemas in the OAS document.
        this.setLegacyDiscriminatorBehavior(false);
    }

    @Override
    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    @Override
    public String getName() {
        return "java-helidon-client";
    }

    @Override
    public String getHelp() {
        return "Generates a Helidon MP or SE client";
    }

    @Override
    public void addOperationToGroup(String tag, String resourcePath, Operation operation, CodegenOperation co,
                                    Map<String, List<CodegenOperation>> operations) {
        super.addOperationToGroup(tag, resourcePath, operation, co, operations);
        if (HELIDON_MP.equals(getLibrary())) {
            co.subresourceOperation = !co.path.isEmpty();
        }
    }

    @Override
    public String apiFilename(String templateName, String tag) {
        if (templateName.contains("_impl")) {
            String suffix = apiTemplateFiles().get(templateName);
            return apiFileFolder() + File.separator + toApiFilename(tag) + "Impl" + suffix;
        }
        return super.apiFilename(templateName, tag);
    }

    @Override
    public void processOpts() {
        super.processOpts();

        if (additionalProperties.containsKey(SERIALIZATION_LIBRARY)) {
            setSerializationLibrary(additionalProperties.get(SERIALIZATION_LIBRARY).toString());
        }

        if (additionalProperties.containsKey(CONFIG_KEY)) {
            setConfigKey(additionalProperties.get(CONFIG_KEY).toString());
        }

        String invokerPath = invokerPackage.replace('.', File.separatorChar);
        invokerFolder = Paths.get(sourceFolder, invokerPath);
        apiFolder = Paths.get(outputFolder).relativize(Paths.get(apiFileFolder()));

        if (isLibrary(HELIDON_MP)) {
            String apiExceptionFolder = Paths.get(sourceFolder,
                    apiPackage().replace('.', File.separatorChar)).toString();

            List<SupportingFile> modifiable = new ArrayList<>();
            modifiable.add(new SupportingFile("pom.mustache", "", "pom.xml"));
            modifiable.add(new SupportingFile("README.mustache", "", "README.md"));
            List<SupportingFile> unmodifiable = new ArrayList<>();
            unmodifiable.add(new SupportingFile("api_exception.mustache", apiExceptionFolder, "ApiException.java"));
            unmodifiable.add(new SupportingFile("api_exception_mapper.mustache", apiExceptionFolder, "ApiExceptionMapper.java"));
            if (additionalProperties.containsKey("jsr310")) {
                unmodifiable.add(new SupportingFile("JavaTimeFormatter.mustache",
                        invokerFolder.toString(), "JavaTimeFormatter.java"));
            }
            processSupportingFiles(modifiable, unmodifiable);
        } else if (isLibrary(HELIDON_SE)) {
            apiTemplateFiles.put("api_impl.mustache", ".java");
            importMapping.put("StringJoiner", "java.util.StringJoiner");
            importMapping.put("WebClientRequestHeaders", "io.helidon.webclient.WebClientRequestHeaders");
            importMapping.put("Pair", invokerPackage + ".Pair");


            List<SupportingFile> modifiable = new ArrayList<>();
            modifiable.add(new SupportingFile("pom.mustache", "", "pom.xml"));
            modifiable.add(new SupportingFile("README.mustache", "", "README.md"));

            List<SupportingFile> unmodifiable = new ArrayList<>();
            unmodifiable.add(new SupportingFile("ApiResponse.mustache", invokerFolder.toString(), "ApiResponse.java"));
            unmodifiable.add(new SupportingFile("ApiResponseBase.mustache", invokerFolder.toString(), "ApiResponseBase.java"));
            unmodifiable.add(new SupportingFile("ApiClient.mustache", invokerFolder.toString(), "ApiClient.java"));
            unmodifiable.add(new SupportingFile("Pair.mustache", invokerFolder.toString(), "Pair.java"));
            unmodifiable.add(new SupportingFile("ResponseType.mustache", apiFolder.toString(), "ResponseType.java"));

            processSupportingFiles(modifiable, unmodifiable);
        }
        else {
            LOGGER.error("Unknown library option (-l/--library): {}", getLibrary());
        }

        if (getSerializationLibrary() == null) {
            LOGGER.info("No serializationLibrary configured, using '{}' as fallback", SERIALIZATION_LIBRARY_JACKSON);
            setSerializationLibrary(SERIALIZATION_LIBRARY_JACKSON);
        }
        switch (getSerializationLibrary()) {
            case SERIALIZATION_LIBRARY_JACKSON:
                additionalProperties.put(SERIALIZATION_LIBRARY_JACKSON, "true");
                additionalProperties.remove(SERIALIZATION_LIBRARY_JSONB);
                supportingFiles.add(new SupportingFile("RFC3339DateFormat.mustache", invokerFolder.toString(), "RFC3339DateFormat.java"));
                break;
            case SERIALIZATION_LIBRARY_JSONB:
                openApiNullable = false;
                additionalProperties.put(OPENAPI_NULLABLE, false);
                additionalProperties.put(SERIALIZATION_LIBRARY_JSONB, "true");
                additionalProperties.remove(SERIALIZATION_LIBRARY_JACKSON);
                break;
            default:
                additionalProperties.remove(SERIALIZATION_LIBRARY_JACKSON);
                additionalProperties.remove(SERIALIZATION_LIBRARY_JSONB);
                LOGGER.error("Unknown serialization library option");
                break;
        }
    }

    /**
     * Check if pom file and src directory already exist.
     *
     * @return outcome of test
     */
    @Override
    protected boolean projectFilesExist() {
        Path projectFolder = Paths.get(getOutputTestFolder());
        Path pom = projectFolder.resolve("pom.xml");
        Path src = projectFolder.resolve(invokerFolder);
        return pom.toFile().exists() && src.toFile().exists();
    }

    @Override
    public OperationsMap postProcessOperationsWithModels(OperationsMap objs, List<ModelMap> allModels) {
        if (isLibrary(HELIDON_MP)) {
            super.postProcessOperationsWithModels(objs, allModels);
            return AbstractJavaJAXRSServerCodegen.jaxrsPostProcessOperations(objs);
        } else {
            // Compute the imports to declare in the generated API impl class.
            List<Map<String, String>> imports = objs.getImports();
            List<Map<String, String>> implImports = new ArrayList<>(imports);

            Set<String> requiredImplImportClassNames = new HashSet<>();
            for (CodegenOperation op : objs.getOperations().getOperation()) {
                requiredImplImportClassNames.addAll((Set) op.vendorExtensions.get(X_HELIDON_REQUIRED_IMPL_IMPORTS));
            }

            Set<String> missingImportClassNames = new TreeSet<>(requiredImplImportClassNames);
            imports.stream()
                .map(m -> m.get("classname"))
                .forEach(missingImportClassNames::remove);

            missingImportClassNames.forEach(c -> {
                    Map<String, String> singleImportMap = new HashMap<>();
                    singleImportMap.put("classname", c);
                    singleImportMap.put("import", Objects.requireNonNull(importMapping.get(c), "no mapping for " + c));
                    implImports.add(singleImportMap);
                });

            objs.put(X_HELIDON_IMPL_IMPORTS, implImports);
            return objs;
        }
    }

    @Override
    public CodegenOperation fromOperation(String path, String httpMethod, Operation operation, List<Server> servers) {
        CodegenOperation op = super.fromOperation(path, httpMethod, operation, servers);
        // We use two templates, one for the API interface and one for the impl class.
        // Add to the normal imports for this operation only those imports used in both
        // the API and the impl. Create a vendor extension on the operation to record the
        // additional imports needed for the implementation class.
        Set<String> requiredImplImports = new TreeSet<>();
        if (op.isArray) {
            op.imports.add("List");
        }
        if (op.isMap) {
            op.imports.add("Map");
        }
        if (op.getHasQueryParams()) {
            requiredImplImports.add("List");
            requiredImplImports.add("ArrayList");
            requiredImplImports.add("Pair");
        }
        if (op.getHasHeaderParams()) {
            requiredImplImports.add("WebClientRequestHeaders");
        }
        if (op.getHasFormParams()) {
            requiredImplImports.add("StringJoiner");
        }
        if (op.getHasCookieParams()) {
            requiredImplImports.add("StringJoiner");
        }
        if (op.bodyParams.stream().anyMatch(JavaHelidonClientCodegen::checkIsArray)
            || op.allParams.stream().anyMatch(JavaHelidonClientCodegen::checkIsArray)
            || op.responses.stream().anyMatch(CodegenResponse::getIsArray)) {
            requiredImplImports.add("List");
            op.imports.add("List");
        }
        if (op.bodyParams.stream().anyMatch(JavaHelidonClientCodegen::checkIsMap)
            || op.allParams.stream().anyMatch(JavaHelidonClientCodegen::checkIsMap)
            || op.responses.stream().anyMatch(CodegenResponse::getIsMap)) {
            requiredImplImports.add("Map");
            op.imports.add("Map");
        }

        op.vendorExtensions.put(X_HELIDON_REQUIRED_IMPL_IMPORTS, requiredImplImports);
        return op;
    }

    private static boolean checkIsArray(CodegenParameter p) {
        return p.isArray || !(p.getSchema() != null && p.getSchema().getIsArray());
    }

    private static boolean checkIsMap(CodegenParameter p) {
        return p.isMap || !(p.getSchema() != null && p.getSchema().getIsMap());
    }

    @Override
    public void postProcessModelProperty(CodegenModel model, CodegenProperty property) {
        super.postProcessModelProperty(model, property);

        if (!BooleanUtils.toBoolean(model.isEnum)
                && additionalProperties.containsKey(SERIALIZATION_LIBRARY_JACKSON)) {
            model.imports.add("JsonProperty");
            model.imports.add("JsonValue");
            model.imports.add("JsonInclude");
            model.imports.add("JsonTypeName");
        } else if (additionalProperties.containsKey(SERIALIZATION_LIBRARY_JACKSON)) {
            model.imports.add("JsonValue");
            model.imports.add("JsonCreator");
        }

        if (HELIDON_MP.equals(getLibrary())) {
            model.imports.remove("ApiModelProperty");
            model.imports.remove("ApiModel");
            model.imports.remove("JsonSerialize");
            model.imports.remove("ToStringSerializer");
        } else if (HELIDON_SE.equals(getLibrary())) {
            // TODO check for SE-specifics
            model.imports.remove("ApiModelProperty");
            model.imports.remove("ApiModel");
            model.imports.remove("JsonSerialize");
            model.imports.remove("ToStringSerializer");
        }

        if ("set".equals(property.containerType) && !JACKSON.equals(serializationLibrary)) {
            // clean-up
            model.imports.remove("JsonDeserialize");
            property.vendorExtensions.remove("x-setter-extra-annotation");
        }
    }

    @Override
    public CodegenModel fromModel(String name, Schema model) {
        CodegenModel codegenModel = super.fromModel(name, model);
        if (isLibrary(HELIDON_MP)) {
            if (codegenModel.imports.contains("ApiModel")) {
                // Remove io.swagger.annotations.ApiModel import
                codegenModel.imports.remove("ApiModel");
            }
        } else if (isLibrary(HELIDON_SE)) {
            if (codegenModel.imports.contains("ApiModel")) {
                // Remove io.swagger.annotations.ApiModel import
                codegenModel.imports.remove("ApiModel");
            }
        }
        return codegenModel;
    }

    @SuppressWarnings("unchecked")
    @Override
    public ModelsMap postProcessModels(ModelsMap objs) {
        objs = super.postProcessModels(objs);
        List<ModelMap> models = objs.getModels();

        if (additionalProperties.containsKey(SERIALIZATION_LIBRARY_JACKSON)) {
            List<Map<String, String>> imports = objs.getImports();
            for (ModelMap mo : models) {
                CodegenModel cm = mo.getModel();
                boolean addImports = false;

                for (CodegenProperty var : cm.vars) {
                    if (this.openApiNullable) {
                        boolean isOptionalNullable = Boolean.FALSE.equals(var.required) && Boolean.TRUE.equals(var.isNullable);
                        // only add JsonNullable and related imports to optional and nullable values
                        addImports |= isOptionalNullable;
                        var.getVendorExtensions().put("x-is-jackson-optional-nullable", isOptionalNullable);
                    }

                    if (Boolean.TRUE.equals(var.getVendorExtensions().get("x-enum-as-string"))) {
                        // treat enum string as just string
                        var.datatypeWithEnum = var.dataType;

                        if (StringUtils.isNotEmpty(var.defaultValue)) { // has default value
                            String defaultValue = var.defaultValue.substring(var.defaultValue.lastIndexOf('.') + 1);
                            for (Map<String, Object> enumVars : (List<Map<String, Object>>) var.getAllowableValues().get("enumVars")) {
                                if (defaultValue.equals(enumVars.get("name"))) {
                                    // update default to use the string directly instead of enum string
                                    var.defaultValue = (String) enumVars.get("value");
                                }
                            }
                        }

                        // add import for Set, HashSet
                        cm.imports.add("Set");
                        Map<String, String> importsSet = new HashMap<>();
                        importsSet.put("import", "java.util.Set");
                        imports.add(importsSet);
                        Map<String, String> importsHashSet = new HashMap<>();
                        importsHashSet.put("import", "java.util.HashSet");
                        imports.add(importsHashSet);
                    }

                }

                if (addImports) {
                    Map<String, String> imports2Classnames = new HashMap<>();
                    imports2Classnames.put("NoSuchElementException", "java.util.NoSuchElementException");
                    imports2Classnames.put("JsonIgnore", "com.fasterxml.jackson.annotation.JsonIgnore");
                    for (Map.Entry<String, String> entry : imports2Classnames.entrySet()) {
                        cm.imports.add(entry.getKey());
                        Map<String, String> importsItem = new HashMap<>();
                        importsItem.put("import", entry.getValue());
                        imports.add(importsItem);
                    }
                }
            }
        }

        return objs;
    }

    public void setConfigKey(String configKey) {
        this.configKey = configKey;
    }

    public void setUseBeanValidation(boolean useBeanValidation) {
        this.useBeanValidation = useBeanValidation;
    }

    public void setPerformBeanValidation(boolean performBeanValidation) {
        this.performBeanValidation = performBeanValidation;
    }

    public void setUseGzipFeature(boolean useGzipFeature) {
        this.useGzipFeature = useGzipFeature;
    }

    public void setCaseInsensitiveResponseHeaders(final Boolean caseInsensitiveResponseHeaders) {
        this.caseInsensitiveResponseHeaders = caseInsensitiveResponseHeaders;
    }

    public String getSerializationLibrary() {
        return serializationLibrary;
    }

    public void setSerializationLibrary(String serializationLibrary) {
        if (SERIALIZATION_LIBRARY_JACKSON.equalsIgnoreCase(serializationLibrary)) {
            this.serializationLibrary = SERIALIZATION_LIBRARY_JACKSON;
        } else if (SERIALIZATION_LIBRARY_JSONB.equalsIgnoreCase(serializationLibrary)) {
            this.serializationLibrary = SERIALIZATION_LIBRARY_JSONB;
        } else {
            throw new IllegalArgumentException("Unexpected serializationLibrary value: " + serializationLibrary);
        }
    }

    @Override
    public Map<String, Object> postProcessSupportingFileData(Map<String, Object> objs) {
        generateYAMLSpecFile(objs);
        return super.postProcessSupportingFileData(objs);
    }

    @Override
    public String toApiVarName(String name) {
        String apiVarName = super.toApiVarName(name);
        if (reservedWords.contains(apiVarName)) {
            apiVarName = escapeReservedWord(apiVarName);
        }
        return apiVarName;
    }

    @Override
    public void addImportsToOneOfInterface(List<Map<String, String>> imports) {
        for (String i : Arrays.asList("JsonSubTypes", "JsonTypeInfo", "JsonIgnoreProperties")) {
            Map<String, String> oneImport = new HashMap<>();
            oneImport.put("import", importMapping.get(i));
            if (!imports.contains(oneImport)) {
                imports.add(oneImport);
            }
        }
    }

    @Override
    public List<VendorExtension> getSupportedVendorExtensions() {
        List<VendorExtension> extensions = super.getSupportedVendorExtensions();
        extensions.add(VendorExtension.X_WEBCLIENT_BLOCKING);
        return extensions;
    }
}
