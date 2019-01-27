/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 * Copyright 2018 SmartBear Software
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.openapitools.codegen;

import com.samskivert.mustache.Mustache;
import com.samskivert.mustache.Template;

import io.swagger.v3.core.util.Json;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.PathItem;
import io.swagger.v3.oas.models.Paths;
import io.swagger.v3.oas.models.info.Contact;
import io.swagger.v3.oas.models.info.Info;
import io.swagger.v3.oas.models.info.License;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.parameters.Parameter;
import io.swagger.v3.oas.models.security.SecurityRequirement;
import io.swagger.v3.oas.models.security.SecurityScheme;
import io.swagger.v3.oas.models.tags.Tag;

import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.config.GeneratorProperties;
import org.openapitools.codegen.ignore.CodegenIgnoreProcessor;
import org.openapitools.codegen.utils.ImplementationVersion;
import org.openapitools.codegen.utils.ModelUtils;
import org.openapitools.codegen.utils.URLPathUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.*;
import java.util.*;
import java.net.*;
import java.time.ZonedDateTime;

public class DefaultGenerator extends AbstractGenerator implements Generator {
    protected final Logger LOGGER = LoggerFactory.getLogger(DefaultGenerator.class);
    protected CodegenConfig config;
    protected ClientOptInput opts;
    protected OpenAPI openAPI;
    protected CodegenIgnoreProcessor ignoreProcessor;
    private Boolean generateApis = null;
    private Boolean generateModels = null;
    private Boolean generateSupportingFiles = null;
    private Boolean generateApiTests = null;
    private Boolean generateApiDocumentation = null;
    private Boolean generateModelTests = null;
    private Boolean generateModelDocumentation = null;
    private Boolean generateMetadata = true;
    private String basePath;
    private String basePathWithoutHost;
    private String contextPath;
    private Map<String, String> generatorPropertyDefaults = new HashMap<>();

    @Override
    public Generator opts(ClientOptInput opts) {
        this.opts = opts;
        this.openAPI = opts.getOpenAPI();
        this.config = opts.getConfig();
        this.config.additionalProperties().putAll(opts.getOpts().getProperties());

        String ignoreFileLocation = this.config.getIgnoreFilePathOverride();
        if (ignoreFileLocation != null) {
            final File ignoreFile = new File(ignoreFileLocation);
            if (ignoreFile.exists() && ignoreFile.canRead()) {
                this.ignoreProcessor = new CodegenIgnoreProcessor(ignoreFile);
            } else {
                LOGGER.warn("Ignore file specified at {} is not valid. This will fall back to an existing ignore file if present in the output directory.", ignoreFileLocation);
            }
        }

        if (this.ignoreProcessor == null) {
            this.ignoreProcessor = new CodegenIgnoreProcessor(this.config.getOutputDir());
        }

        return this;
    }

    /**
     * Programmatically disable the output of .openapi-generator/VERSION, .openapi-generator-ignore,
     * or other metadata files used by OpenAPI Generator.
     *
     * @param generateMetadata true: enable outputs, false: disable outputs
     */
    @SuppressWarnings("WeakerAccess")
    public void setGenerateMetadata(Boolean generateMetadata) {
        this.generateMetadata = generateMetadata;
    }

    /**
     * Set generator properties otherwise pulled from system properties.
     * Useful for running tests in parallel without relying on System.properties.
     *
     * @param key   The system property key
     * @param value The system property value
     */
    @SuppressWarnings("WeakerAccess")
    public void setGeneratorPropertyDefault(final String key, final String value) {
        this.generatorPropertyDefaults.put(key, value);
    }

    private Boolean getGeneratorPropertyDefaultSwitch(final String key, final Boolean defaultValue) {
        String result = null;
        if (this.generatorPropertyDefaults.containsKey(key)) {
            result = this.generatorPropertyDefaults.get(key);
        }
        if (result != null) {
            return Boolean.valueOf(result);
        }
        return defaultValue;
    }

    private void configureGeneratorProperties() {
        // allows generating only models by specifying a CSV of models to generate, or empty for all
        // NOTE: Boolean.TRUE is required below rather than `true` because of JVM boxing constraints and type inference.
        generateApis = GeneratorProperties.getProperty(CodegenConstants.APIS) != null ? Boolean.TRUE : getGeneratorPropertyDefaultSwitch(CodegenConstants.APIS, null);
        generateModels = GeneratorProperties.getProperty(CodegenConstants.MODELS) != null ? Boolean.TRUE : getGeneratorPropertyDefaultSwitch(CodegenConstants.MODELS, null);
        generateSupportingFiles = GeneratorProperties.getProperty(CodegenConstants.SUPPORTING_FILES) != null ? Boolean.TRUE : getGeneratorPropertyDefaultSwitch(CodegenConstants.SUPPORTING_FILES, null);

        if (generateApis == null && generateModels == null && generateSupportingFiles == null) {
            // no specifics are set, generate everything
            generateApis = generateModels = generateSupportingFiles = true;
        } else {
            if (generateApis == null) {
                generateApis = false;
            }
            if (generateModels == null) {
                generateModels = false;
            }
            if (generateSupportingFiles == null) {
                generateSupportingFiles = false;
            }
        }
        // model/api tests and documentation options rely on parent generate options (api or model) and no other options.
        // They default to true in all scenarios and can only be marked false explicitly
        generateModelTests = GeneratorProperties.getProperty(CodegenConstants.MODEL_TESTS) != null ? Boolean.valueOf(GeneratorProperties.getProperty(CodegenConstants.MODEL_TESTS)) : getGeneratorPropertyDefaultSwitch(CodegenConstants.MODEL_TESTS, true);
        generateModelDocumentation = GeneratorProperties.getProperty(CodegenConstants.MODEL_DOCS) != null ? Boolean.valueOf(GeneratorProperties.getProperty(CodegenConstants.MODEL_DOCS)) : getGeneratorPropertyDefaultSwitch(CodegenConstants.MODEL_DOCS, true);
        generateApiTests = GeneratorProperties.getProperty(CodegenConstants.API_TESTS) != null ? Boolean.valueOf(GeneratorProperties.getProperty(CodegenConstants.API_TESTS)) : getGeneratorPropertyDefaultSwitch(CodegenConstants.API_TESTS, true);
        generateApiDocumentation = GeneratorProperties.getProperty(CodegenConstants.API_DOCS) != null ? Boolean.valueOf(GeneratorProperties.getProperty(CodegenConstants.API_DOCS)) : getGeneratorPropertyDefaultSwitch(CodegenConstants.API_DOCS, true);


        // Additional properties added for tests to exclude references in project related files
        config.additionalProperties().put(CodegenConstants.GENERATE_API_TESTS, generateApiTests);
        config.additionalProperties().put(CodegenConstants.GENERATE_MODEL_TESTS, generateModelTests);

        config.additionalProperties().put(CodegenConstants.GENERATE_API_DOCS, generateApiDocumentation);
        config.additionalProperties().put(CodegenConstants.GENERATE_MODEL_DOCS, generateModelDocumentation);

        config.additionalProperties().put(CodegenConstants.GENERATE_APIS, generateApis);
        config.additionalProperties().put(CodegenConstants.GENERATE_MODELS, generateModels);

        if (!generateApiTests && !generateModelTests) {
            config.additionalProperties().put(CodegenConstants.EXCLUDE_TESTS, true);
        }

        if (GeneratorProperties.getProperty("debugOpenAPI") != null) {
            Json.prettyPrint(openAPI);
        } else if (GeneratorProperties.getProperty("debugSwagger") != null) {
            // This exists for backward compatibility
            // We fall to this block only if debugOpenAPI is null. No need to dump this twice.
            LOGGER.info("Please use system property 'debugOpenAPI' instead of 'debugSwagger'.");
            Json.prettyPrint(openAPI);
        }

        config.processOpts();
        config.preprocessOpenAPI(openAPI);

        // set OpenAPI and schemas to make these available to all methods
        config.setGlobalOpenAPI(openAPI);
        config.setGlobalSchemas(openAPI);

        config.additionalProperties().put("generatorVersion", ImplementationVersion.read());
        config.additionalProperties().put("generatedDate", ZonedDateTime.now().toString());
        config.additionalProperties().put("generatedYear", String.valueOf(ZonedDateTime.now().getYear()));
        config.additionalProperties().put("generatorClass", config.getClass().getName());
        config.additionalProperties().put("inputSpec", config.getInputSpec());

        if (openAPI.getExtensions() != null) {
            config.vendorExtensions().putAll(openAPI.getExtensions());
        }

        URL url = URLPathUtils.getServerURL(openAPI);
        contextPath = config.escapeText(url.getPath()).replaceAll("/$", ""); // for backward compatibility
        basePathWithoutHost = contextPath;
        basePath = config.escapeText(URLPathUtils.getHost(openAPI)).replaceAll("/$", "");
    }

    private void configureOpenAPIInfo() {
        Info info = this.openAPI.getInfo();
        if (info == null) {
            return;
        }
        if (info.getTitle() != null) {
            config.additionalProperties().put("appName", config.escapeText(info.getTitle()));
        }
        if (info.getVersion() != null) {
            config.additionalProperties().put("appVersion", config.escapeText(info.getVersion()));
        } else {
            LOGGER.error("Missing required field info version. Default appVersion set to 1.0.0");
            config.additionalProperties().put("appVersion", "1.0.0");
        }

        if (StringUtils.isEmpty(info.getDescription())) {
            // set a default description if none if provided
            config.additionalProperties().put("appDescription",
                    "No description provided (generated by Openapi Generator https://github.com/openapitools/openapi-generator)");
            config.additionalProperties().put("appDescriptionWithNewLines", config.additionalProperties().get("appDescription"));
            config.additionalProperties().put("unescapedAppDescription", "No description provided (generated by Openapi Generator https://github.com/openapitools/openapi-generator)");
        } else {
            config.additionalProperties().put("appDescription", config.escapeText(info.getDescription()));
            config.additionalProperties().put("appDescriptionWithNewLines", config.escapeTextWhileAllowingNewLines(info.getDescription()));
            config.additionalProperties().put("unescapedAppDescription", info.getDescription());
        }

        if (info.getContact() != null) {
            Contact contact = info.getContact();
            if (contact.getEmail() != null) {
                config.additionalProperties().put("infoEmail", config.escapeText(contact.getEmail()));
            }
            if (contact.getName() != null) {
                config.additionalProperties().put("infoName", config.escapeText(contact.getName()));
            }
            if (contact.getUrl() != null) {
                config.additionalProperties().put("infoUrl", config.escapeText(contact.getUrl()));
            }
        }

        if (info.getLicense() != null) {
            License license = info.getLicense();
            if (license.getName() != null) {
                config.additionalProperties().put("licenseInfo", config.escapeText(license.getName()));
            }
            if (license.getUrl() != null) {
                config.additionalProperties().put("licenseUrl", config.escapeText(license.getUrl()));
            }
        }

        if (info.getVersion() != null) {
            config.additionalProperties().put("version", config.escapeText(info.getVersion()));
        } else {
            LOGGER.error("Missing required field info version. Default version set to 1.0.0");
            config.additionalProperties().put("version", "1.0.0");
        }

        if (info.getTermsOfService() != null) {
            config.additionalProperties().put("termsOfService", config.escapeText(info.getTermsOfService()));
        }
    }

    private void generateModelTests(List<File> files, Map<String, Object> models, String modelName) throws IOException {
        // to generate model test files
        for (String templateName : config.modelTestTemplateFiles().keySet()) {
            String suffix = config.modelTestTemplateFiles().get(templateName);
            String filename = config.modelTestFileFolder() + File.separator + config.toModelTestFilename(modelName) + suffix;
            // do not overwrite test file that already exists
            if (new File(filename).exists()) {
                LOGGER.info("File exists. Skipped overwriting " + filename);
                continue;
            }
            File written = processTemplateToFile(models, templateName, filename);
            if (written != null) {
                files.add(written);
                if (config.isEnablePostProcessFile()) {
                    config.postProcessFile(written, "model-test");
                }
            }
        }
    }

    private void generateModelDocumentation(List<File> files, Map<String, Object> models, String modelName) throws IOException {
        for (String templateName : config.modelDocTemplateFiles().keySet()) {
            String docExtension = config.getDocExtension();
            String suffix = docExtension != null ? docExtension : config.modelDocTemplateFiles().get(templateName);
            String filename = config.modelDocFileFolder() + File.separator + config.toModelDocFilename(modelName) + suffix;
            if (!config.shouldOverwrite(filename)) {
                LOGGER.info("Skipped overwriting " + filename);
                continue;
            }
            File written = processTemplateToFile(models, templateName, filename);
            if (written != null) {
                files.add(written);
                if (config.isEnablePostProcessFile()) {
                    config.postProcessFile(written, "model-doc");
                }
            }
        }
    }

    private void generateModel(List<File> files, Map<String, Object> models, String modelName) throws IOException {
        for (String templateName : config.modelTemplateFiles().keySet()) {
            String suffix = config.modelTemplateFiles().get(templateName);
            String filename = config.modelFileFolder() + File.separator + config.toModelFilename(modelName) + suffix;
            if (!config.shouldOverwrite(filename)) {
                LOGGER.info("Skipped overwriting " + filename);
                continue;
            }
            File written = processTemplateToFile(models, templateName, filename);
            if (written != null) {
                files.add(written);
                if (config.isEnablePostProcessFile()) {
                    config.postProcessFile(written, "model");
                }
            }
        }
    }

    private void generateModels(List<File> files, List<Object> allModels, List<String> unusedModels) {
        if (!generateModels) {
            return;
        }

        final Map<String, Schema> schemas = ModelUtils.getSchemas(this.openAPI);
        if (schemas == null) {
            return;
        }

        String modelNames = GeneratorProperties.getProperty("models");
        Set<String> modelsToGenerate = null;
        if (modelNames != null && !modelNames.isEmpty()) {
            modelsToGenerate = new HashSet<String>(Arrays.asList(modelNames.split(",")));
        }

        Set<String> modelKeys = schemas.keySet();
        if (modelsToGenerate != null && !modelsToGenerate.isEmpty()) {
            Set<String> updatedKeys = new HashSet<String>();
            for (String m : modelKeys) {
                if (modelsToGenerate.contains(m)) {
                    updatedKeys.add(m);
                }
            }

            modelKeys = updatedKeys;
        }

        // store all processed models
        Map<String, Object> allProcessedModels = new TreeMap<String, Object>(new Comparator<String>() {
            @Override
            public int compare(String o1, String o2) {
                return ObjectUtils.compare(config.toModelName(o1), config.toModelName(o2));
            }
                /* TODO need to revise the logic below

                Model model1 = definitions.get(o1);
                Model model2 = definitions.get(o2);

                int model1InheritanceDepth = getInheritanceDepth(model1);
                int model2InheritanceDepth = getInheritanceDepth(model2);

                if (model1InheritanceDepth == model2InheritanceDepth) {
                    return ObjectUtils.compare(config.toModelName(o1), config.toModelName(o2));
                } else if (model1InheritanceDepth > model2InheritanceDepth) {
                    return 1;
                } else {
                    return -1;
                }
            }

            private int getInheritanceDepth(Model model) {
                int inheritanceDepth = 0;
                Model parent = getParent(model);

                while (parent != null) {
                    inheritanceDepth++;
                    parent = getParent(parent);
                }

                return inheritanceDepth;
            }

            private Model getParent(Model model) {
                if (model instanceof ComposedModel) {
                    Model parent = ((ComposedModel) model).getParent();
                    if (parent == null) {
                        // check for interfaces
                        List<RefModel> interfaces = ((ComposedModel) model).getInterfaces();
                        if (interfaces.size() > 0) {
                            RefModel interf = interfaces.get(0);
                            return definitions.get(interf.getSimpleRef());
                        }
                    }
                    if (parent != null) {
                        return definitions.get(parent.getReference());
                    }
                }

                return null;
            } */
        });

        Boolean skipFormModel = GeneratorProperties.getProperty(CodegenConstants.SKIP_FORM_MODEL) != null ?
                Boolean.valueOf(GeneratorProperties.getProperty(CodegenConstants.SKIP_FORM_MODEL)) :
                getGeneratorPropertyDefaultSwitch(CodegenConstants.SKIP_FORM_MODEL, false);

        // process models only
        for (String name : modelKeys) {
            try {
                //don't generate models that have an import mapping
                if (config.importMapping().containsKey(name)) {
                    LOGGER.debug("Model " + name + " not imported due to import mapping");
                    continue;
                }

                // don't generate models that are not used as object (e.g. form parameters)
                if (unusedModels.contains(name)) {
                    if (Boolean.FALSE.equals(skipFormModel)) {
                        // if skipFormModel sets to true, still generate the model and log the result
                        LOGGER.info("Model " + name + " (marked as unused due to form parameters) is generated due to skipFormModel=false (default)");
                    } else {
                        LOGGER.info("Model " + name + " not generated since it's marked as unused (due to form parameters) and skipFormModel set to true");
                        continue;
                    }
                }

                Schema schema = schemas.get(name);

                if (ModelUtils.isFreeFormObject(schema)) { // check to see if it'a a free-form object
                    LOGGER.info("Model " + name + " not generated since it's a free-form object");
                    continue;
                } else if (ModelUtils.isMapSchema(schema)) { // check to see if it's a "map" model
                    if (!ModelUtils.isGenerateAliasAsModel() && (schema.getProperties() == null || schema.getProperties().isEmpty())) {
                        // schema without property, i.e. alias to map
                        LOGGER.info("Model " + name + " not generated since it's an alias to map (without property)");
                        continue;
                    }
                } else if (ModelUtils.isArraySchema(schema)) { // check to see if it's an "array" model
                    if (!ModelUtils.isGenerateAliasAsModel() && (schema.getProperties() == null || schema.getProperties().isEmpty())) {
                        // schema without property, i.e. alias to array
                        LOGGER.info("Model " + name + " not generated since it's an alias to array (without property)");
                        continue;
                    }
                }

                Map<String, Schema> schemaMap = new HashMap<>();
                schemaMap.put(name, schema);
                Map<String, Object> models = processModels(config, schemaMap, schemas);
                models.put("classname", config.toModelName(name));
                models.putAll(config.additionalProperties());
                allProcessedModels.put(name, models);
            } catch (Exception e) {
                throw new RuntimeException("Could not process model '" + name + "'" + ".Please make sure that your schema is correct!", e);
            }
        }

        // loop through all models to update children models, isSelfReference, isCircularReference, etc
        allProcessedModels = config.updateAllModels(allProcessedModels);

        // post process all processed models
        allProcessedModels = config.postProcessAllModels(allProcessedModels);

        // generate files based on processed models
        for (String modelName : allProcessedModels.keySet()) {
            Map<String, Object> models = (Map<String, Object>) allProcessedModels.get(modelName);
            models.put("modelPackage", config.modelPackage());
            try {
                //don't generate models that have an import mapping
                if (config.importMapping().containsKey(modelName)) {
                    continue;
                }

                // TODO revise below as we've already performed unaliasing so that the isAlias check may be removed
                Map<String, Object> modelTemplate = (Map<String, Object>) ((List<Object>) models.get("models")).get(0);
                // Special handling of aliases only applies to Java
                if (modelTemplate != null && modelTemplate.containsKey("model")) {
                    CodegenModel m = (CodegenModel) modelTemplate.get("model");
                    if (m.isAlias) {
                        continue;  // Don't create user-defined classes for aliases
                    }
                }

                allModels.add(modelTemplate);

                // to generate model files
                generateModel(files, models, modelName);

                if (generateModelTests) {
                    // to generate model test files
                    generateModelTests(files, models, modelName);
                }
                if (generateModelDocumentation) {
                    // to generate model documentation files
                    generateModelDocumentation(files, models, modelName);
                }
            } catch (Exception e) {
                throw new RuntimeException("Could not generate model '" + modelName + "'", e);
            }
        }
        if (GeneratorProperties.getProperty("debugModels") != null) {
            LOGGER.info("############ Model info ############");
            Json.prettyPrint(allModels);
        }

    }

    private void generateApis(List<File> files, List<Object> allOperations, List<Object> allModels) {
        if (!generateApis) {
            return;
        }
        Map<String, List<CodegenOperation>> paths = processPaths(this.openAPI.getPaths());
        Set<String> apisToGenerate = null;
        String apiNames = GeneratorProperties.getProperty("apis");
        if (apiNames != null && !apiNames.isEmpty()) {
            apisToGenerate = new HashSet<String>(Arrays.asList(apiNames.split(",")));
        }
        if (apisToGenerate != null && !apisToGenerate.isEmpty()) {
            Map<String, List<CodegenOperation>> updatedPaths = new TreeMap<String, List<CodegenOperation>>();
            for (String m : paths.keySet()) {
                if (apisToGenerate.contains(m)) {
                    updatedPaths.put(m, paths.get(m));
                }
            }
            paths = updatedPaths;
        }
        for (String tag : paths.keySet()) {
            try {
                List<CodegenOperation> ops = paths.get(tag);
                Collections.sort(ops, new Comparator<CodegenOperation>() {
                    @Override
                    public int compare(CodegenOperation one, CodegenOperation another) {
                        return ObjectUtils.compare(one.operationId, another.operationId);
                    }
                });
                Map<String, Object> operation = processOperations(config, tag, ops, allModels);
                URL url = URLPathUtils.getServerURL(openAPI);
                operation.put("basePath", basePath);
                operation.put("basePathWithoutHost", config.encodePath(url.getPath()).replaceAll("/$", ""));
                operation.put("contextPath", contextPath);
                operation.put("baseName", tag);
                operation.put("apiPackage", config.apiPackage());
                operation.put("modelPackage", config.modelPackage());
                operation.putAll(config.additionalProperties());
                operation.put("classname", config.toApiName(tag));
                operation.put("classVarName", config.toApiVarName(tag));
                operation.put("importPath", config.toApiImport(tag));
                operation.put("classFilename", config.toApiFilename(tag));

                if (allModels == null || allModels.isEmpty()) {
                    operation.put("hasModel", false);
                } else {
                    operation.put("hasModel", true);
                }

                if (!config.vendorExtensions().isEmpty()) {
                    operation.put("vendorExtensions", config.vendorExtensions());
                }

                // process top-level x-group-parameters
                if (config.vendorExtensions().containsKey("x-group-parameters")) {
                    Boolean isGroupParameters = Boolean.valueOf(config.vendorExtensions().get("x-group-parameters").toString());

                    Map<String, Object> objectMap = (Map<String, Object>) operation.get("operations");
                    @SuppressWarnings("unchecked")
                    List<CodegenOperation> operations = (List<CodegenOperation>) objectMap.get("operation");
                    for (CodegenOperation op : operations) {
                        if (isGroupParameters && !op.vendorExtensions.containsKey("x-group-parameters")) {
                            op.vendorExtensions.put("x-group-parameters", Boolean.TRUE);
                        }
                    }
                }

                // Pass sortParamsByRequiredFlag through to the Mustache template...
                boolean sortParamsByRequiredFlag = true;
                if (this.config.additionalProperties().containsKey(CodegenConstants.SORT_PARAMS_BY_REQUIRED_FLAG)) {
                    sortParamsByRequiredFlag = Boolean.valueOf(this.config.additionalProperties().get(CodegenConstants.SORT_PARAMS_BY_REQUIRED_FLAG).toString());
                }
                operation.put("sortParamsByRequiredFlag", sortParamsByRequiredFlag);

                /* consumes, produces are no longer defined in OAS3.0
                processMimeTypes(swagger.getConsumes(), operation, "consumes");
                processMimeTypes(swagger.getProduces(), operation, "produces");
                */

                allOperations.add(new HashMap<String, Object>(operation));
                for (int i = 0; i < allOperations.size(); i++) {
                    Map<String, Object> oo = (Map<String, Object>) allOperations.get(i);
                    if (i < (allOperations.size() - 1)) {
                        oo.put("hasMore", "true");
                    }
                }

                for (String templateName : config.apiTemplateFiles().keySet()) {
                    String filename = config.apiFilename(templateName, tag);
                    if (!config.shouldOverwrite(filename) && new File(filename).exists()) {
                        LOGGER.info("Skipped overwriting " + filename);
                        continue;
                    }

                    File written = processTemplateToFile(operation, templateName, filename);
                    if (written != null) {
                        files.add(written);
                        if (config.isEnablePostProcessFile()) {
                            config.postProcessFile(written, "api");
                        }
                    }
                }

                if (generateApiTests) {
                    // to generate api test files
                    for (String templateName : config.apiTestTemplateFiles().keySet()) {
                        String filename = config.apiTestFilename(templateName, tag);
                        // do not overwrite test file that already exists
                        if (new File(filename).exists()) {
                            LOGGER.info("File exists. Skipped overwriting " + filename);
                            continue;
                        }

                        File written = processTemplateToFile(operation, templateName, filename);
                        if (written != null) {
                            files.add(written);
                            if (config.isEnablePostProcessFile()) {
                                config.postProcessFile(written, "api-test");
                            }
                        }
                    }
                }


                if (generateApiDocumentation) {
                    // to generate api documentation files
                    for (String templateName : config.apiDocTemplateFiles().keySet()) {
                        String filename = config.apiDocFilename(templateName, tag);
                        if (!config.shouldOverwrite(filename) && new File(filename).exists()) {
                            LOGGER.info("Skipped overwriting " + filename);
                            continue;
                        }

                        File written = processTemplateToFile(operation, templateName, filename);
                        if (written != null) {
                            files.add(written);
                            if (config.isEnablePostProcessFile()) {
                                config.postProcessFile(written, "api-doc");
                            }
                        }
                    }
                }

            } catch (Exception e) {
                throw new RuntimeException("Could not generate api file for '" + tag + "'", e);
            }
        }
        if (GeneratorProperties.getProperty("debugOperations") != null) {
            LOGGER.info("############ Operation info ############");
            Json.prettyPrint(allOperations);
        }

    }

    private void generateSupportingFiles(List<File> files, Map<String, Object> bundle) {
        if (!generateSupportingFiles) {
            return;
        }
        Set<String> supportingFilesToGenerate = null;
        String supportingFiles = GeneratorProperties.getProperty(CodegenConstants.SUPPORTING_FILES);
        if (supportingFiles != null && !supportingFiles.isEmpty()) {
            supportingFilesToGenerate = new HashSet<String>(Arrays.asList(supportingFiles.split(",")));
        }

        for (SupportingFile support : config.supportingFiles()) {
            try {
                String outputFolder = config.outputFolder();
                if (StringUtils.isNotEmpty(support.folder)) {
                    outputFolder += File.separator + support.folder;
                }
                File of = new File(outputFolder);
                if (!of.isDirectory()) {
                    of.mkdirs();
                }
                String outputFilename = outputFolder + File.separator + support.destinationFilename.replace('/', File.separatorChar);
                if (!config.shouldOverwrite(outputFilename)) {
                    LOGGER.info("Skipped overwriting " + outputFilename);
                    continue;
                }
                String templateFile;
                if (support instanceof GlobalSupportingFile) {
                    templateFile = config.getCommonTemplateDir() + File.separator + support.templateFile;
                } else {
                    templateFile = getFullTemplateFile(config, support.templateFile);
                }
                boolean shouldGenerate = true;
                if (supportingFilesToGenerate != null && !supportingFilesToGenerate.isEmpty()) {
                    shouldGenerate = supportingFilesToGenerate.contains(support.destinationFilename);
                }
                if (!shouldGenerate) {
                    continue;
                }

                if (ignoreProcessor.allowsFile(new File(outputFilename))) {
                    if (templateFile.endsWith("mustache")) {
                        String template = readTemplate(templateFile);
                        Mustache.Compiler compiler = Mustache.compiler();
                        compiler = config.processCompiler(compiler);
                        Template tmpl = compiler
                                .withLoader(new Mustache.TemplateLoader() {
                                    @Override
                                    public Reader getTemplate(String name) {
                                        return getTemplateReader(getFullTemplateFile(config, name + ".mustache"));
                                    }
                                })
                                .defaultValue("")
                                .compile(template);

                        writeToFile(outputFilename, tmpl.execute(bundle));
                        File written = new File(outputFilename);
                        files.add(written);
                        if (config.isEnablePostProcessFile()) {
                            config.postProcessFile(written, "supporting-mustache");
                        }
                    } else {
                        InputStream in = null;

                        try {
                            in = new FileInputStream(templateFile);
                        } catch (Exception e) {
                            // continue
                        }
                        if (in == null) {
                            in = this.getClass().getClassLoader().getResourceAsStream(getCPResourcePath(templateFile));
                        }
                        File outputFile = writeInputStreamToFile(outputFilename, in, templateFile);
                        files.add(outputFile);
                        if (config.isEnablePostProcessFile()) {
                            config.postProcessFile(outputFile, "supporting-common");
                        }
                    }
                } else {
                    LOGGER.info("Skipped generation of " + outputFilename + " due to rule in .openapi-generator-ignore");
                }
            } catch (Exception e) {
                throw new RuntimeException("Could not generate supporting file '" + support + "'", e);
            }
        }

        // Consider .openapi-generator-ignore a supporting file
        // Output .openapi-generator-ignore if it doesn't exist and wasn't explicitly created by a generator
        final String openapiGeneratorIgnore = ".openapi-generator-ignore";
        String ignoreFileNameTarget = config.outputFolder() + File.separator + openapiGeneratorIgnore;
        File ignoreFile = new File(ignoreFileNameTarget);
        if (generateMetadata && !ignoreFile.exists()) {
            String ignoreFileNameSource = File.separator + config.getCommonTemplateDir() + File.separator + openapiGeneratorIgnore;
            String ignoreFileContents = readResourceContents(ignoreFileNameSource);
            try {
                writeToFile(ignoreFileNameTarget, ignoreFileContents);
            } catch (IOException e) {
                throw new RuntimeException("Could not generate supporting file '" + openapiGeneratorIgnore + "'", e);
            }
            files.add(ignoreFile);
            if (config.isEnablePostProcessFile()) {
                config.postProcessFile(ignoreFile, "openapi-generator-ignore");
            }
        }

        if (generateMetadata) {
            final String versionMetadata = config.outputFolder() + File.separator + ".openapi-generator" + File.separator + "VERSION";
            File versionMetadataFile = new File(versionMetadata);
            try {
                writeToFile(versionMetadata, ImplementationVersion.read());
                files.add(versionMetadataFile);
                if (config.isEnablePostProcessFile()) {
                    config.postProcessFile(ignoreFile, "openapi-generator-version");
                }
            } catch (IOException e) {
                throw new RuntimeException("Could not generate supporting file '" + versionMetadata + "'", e);
            }
        }

        /*
         * The following code adds default LICENSE (Apache-2.0) for all generators
         * To use license other than Apache2.0, update the following file:
         *   modules/openapi-generator/src/main/resources/_common/LICENSE
         *
        final String apache2License = "LICENSE";
        String licenseFileNameTarget = config.outputFolder() + File.separator + apache2License;
        File licenseFile = new File(licenseFileNameTarget);
        String licenseFileNameSource = File.separator + config.getCommonTemplateDir() + File.separator + apache2License;
        String licenseFileContents = readResourceContents(licenseFileNameSource);
        try {
            writeToFile(licenseFileNameTarget, licenseFileContents);
        } catch (IOException e) {
            throw new RuntimeException("Could not generate LICENSE file '" + apache2License + "'", e);
        }
        files.add(licenseFile);
         */

    }

    protected File writeInputStreamToFile(String filename, InputStream in, String templateFile) throws FileNotFoundException, IOException {
        File outputFile = java.nio.file.Paths.get(filename).toFile();
        if (in != null) {
            OutputStream out = new FileOutputStream(outputFile, false);
            LOGGER.info("writing file " + outputFile);
            IOUtils.copy(in, out);
            out.close();
        } else {
            LOGGER.error("can't open '" + templateFile + "' for input, can not write '" + filename + "'");
        }
        return outputFile;
    }

    private Map<String, Object> buildSupportFileBundle(List<Object> allOperations, List<Object> allModels) {

        Map<String, Object> bundle = new HashMap<String, Object>();
        bundle.putAll(config.additionalProperties());
        bundle.put("apiPackage", config.apiPackage());

        Map<String, Object> apis = new HashMap<String, Object>();
        apis.put("apis", allOperations);

        URL url = URLPathUtils.getServerURL(openAPI);

        bundle.put("openAPI", openAPI);
        bundle.put("basePath", basePath);
        bundle.put("basePathWithoutHost", basePathWithoutHost);
        bundle.put("scheme", URLPathUtils.getScheme(url, config));
        bundle.put("host", url.getHost());
        bundle.put("contextPath", contextPath);
        bundle.put("apiInfo", apis);
        bundle.put("models", allModels);
        bundle.put("apiFolder", config.apiPackage().replace('.', File.separatorChar));
        bundle.put("modelPackage", config.modelPackage());

        Map<String, SecurityScheme> securitySchemeMap = openAPI.getComponents() != null ? openAPI.getComponents().getSecuritySchemes() : null;
        List<CodegenSecurity> authMethods = config.fromSecurity(securitySchemeMap);
        if (authMethods != null && !authMethods.isEmpty()) {
            bundle.put("authMethods", authMethods);
            bundle.put("hasAuthMethods", true);

            if (hasOAuthMethods(authMethods)) {
                bundle.put("hasOAuthMethods", true);
                bundle.put("oauthMethods", getOAuthMethods(authMethods));
            }
        }

        List<CodegenServer> servers = config.fromServers(openAPI.getServers());
        if (servers != null && !servers.isEmpty()) {
            bundle.put("servers", servers);
            bundle.put("hasServers", true);
        }

        if (openAPI.getExternalDocs() != null) {
            bundle.put("externalDocs", openAPI.getExternalDocs());
        }

        for (int i = 0; i < allModels.size() - 1; i++) {
            HashMap<String, CodegenModel> cm = (HashMap<String, CodegenModel>) allModels.get(i);
            CodegenModel m = cm.get("model");
            m.hasMoreModels = true;
        }

        config.postProcessSupportingFileData(bundle);

        if (GeneratorProperties.getProperty("debugSupportingFiles") != null) {
            LOGGER.info("############ Supporting file info ############");
            Json.prettyPrint(bundle);
        }
        return bundle;
    }

    @Override
    public List<File> generate() {

        if (openAPI == null) {
            throw new RuntimeException("missing OpenAPI input!");
        }

        if (config == null) {
            throw new RuntimeException("missing config!");
        }

        // resolve inline models
        InlineModelResolver inlineModelResolver = new InlineModelResolver();
        inlineModelResolver.flatten(openAPI);

        configureGeneratorProperties();
        configureOpenAPIInfo();

        List<File> files = new ArrayList<File>();
        // models
        List<String> filteredSchemas = ModelUtils.getSchemasUsedOnlyInFormParam(openAPI);
        List<Object> allModels = new ArrayList<Object>();
        generateModels(files, allModels, filteredSchemas);
        // apis
        List<Object> allOperations = new ArrayList<Object>();
        generateApis(files, allOperations, allModels);

        // supporting files
        Map<String, Object> bundle = buildSupportFileBundle(allOperations, allModels);
        generateSupportingFiles(files, bundle);
        config.processOpenAPI(openAPI);

        // reset GeneratorProperties, so that the running thread can be reused for another generator-run
        GeneratorProperties.reset();

        return files;
    }


    protected File processTemplateToFile(Map<String, Object> templateData, String templateName, String outputFilename) throws IOException {
        String adjustedOutputFilename = outputFilename.replaceAll("//", "/").replace('/', File.separatorChar);
        if (ignoreProcessor.allowsFile(new File(adjustedOutputFilename))) {
            String templateFile = getFullTemplateFile(config, templateName);
            String template = readTemplate(templateFile);
            Mustache.Compiler compiler = Mustache.compiler();
            compiler = config.processCompiler(compiler);
            Template tmpl = compiler
                    .withLoader(new Mustache.TemplateLoader() {
                        @Override
                        public Reader getTemplate(String name) {
                            return getTemplateReader(getFullTemplateFile(config, name + ".mustache"));
                        }
                    })
                    .defaultValue("")
                    .compile(template);

            writeToFile(adjustedOutputFilename, tmpl.execute(templateData));
            return new File(adjustedOutputFilename);
        }

        LOGGER.info("Skipped generation of " + adjustedOutputFilename + " due to rule in .openapi-generator-ignore");
        return null;
    }

    public Map<String, List<CodegenOperation>> processPaths(Paths paths) {
        Map<String, List<CodegenOperation>> ops = new TreeMap<String, List<CodegenOperation>>();
        for (String resourcePath : paths.keySet()) {
            PathItem path = paths.get(resourcePath);
            processOperation(resourcePath, "get", path.getGet(), ops, path);
            processOperation(resourcePath, "head", path.getHead(), ops, path);
            processOperation(resourcePath, "put", path.getPut(), ops, path);
            processOperation(resourcePath, "post", path.getPost(), ops, path);
            processOperation(resourcePath, "delete", path.getDelete(), ops, path);
            processOperation(resourcePath, "patch", path.getPatch(), ops, path);
            processOperation(resourcePath, "options", path.getOptions(), ops, path);
        }
        return ops;
    }

    private void processOperation(String resourcePath, String httpMethod, Operation operation, Map<String, List<CodegenOperation>> operations, PathItem path) {
        if (operation == null) {
            return;
        }

        if (GeneratorProperties.getProperty("debugOperations") != null) {
            LOGGER.info("processOperation: resourcePath= " + resourcePath + "\t;" + httpMethod + " " + operation + "\n");
        }

        List<Tag> tags = new ArrayList<Tag>();
        List<String> tagNames = operation.getTags();
        List<Tag> swaggerTags = openAPI.getTags();
        if (tagNames != null) {
            if (swaggerTags == null) {
                for (String tagName : tagNames) {
                    tags.add(new Tag().name(tagName));
                }
            } else {
                for (String tagName : tagNames) {
                    boolean foundTag = false;
                    for (Tag tag : swaggerTags) {
                        if (tag.getName().equals(tagName)) {
                            tags.add(tag);
                            foundTag = true;
                            break;
                        }
                    }

                    if (!foundTag) {
                        tags.add(new Tag().name(tagName));
                    }
                }
            }
        }

        if (tags.isEmpty()) {
            tags.add(new Tag().name("default"));
        }

        /*
         build up a set of parameter "ids" defined at the operation level
         per the swagger 2.0 spec "A unique parameter is defined by a combination of a name and location"
          i'm assuming "location" == "in"
        */
        Set<String> operationParameters = new HashSet<String>();
        if (operation.getParameters() != null) {
            for (Parameter parameter : operation.getParameters()) {
                operationParameters.add(generateParameterId(parameter));
            }
        }

        //need to propagate path level down to the operation
        if (path.getParameters() != null) {
            for (Parameter parameter : path.getParameters()) {
                //skip propagation if a parameter with the same name is already defined at the operation level
                if (!operationParameters.contains(generateParameterId(parameter))) {
                    operation.addParametersItem(parameter);
                }
            }
        }

        final Map<String, Schema> schemas = openAPI.getComponents() != null ? openAPI.getComponents().getSchemas() : null;
        final Map<String, SecurityScheme> securitySchemes = openAPI.getComponents() != null ? openAPI.getComponents().getSecuritySchemes() : null;
        final List<SecurityRequirement> globalSecurities = openAPI.getSecurity();
        for (Tag tag : tags) {
            try {
                CodegenOperation codegenOperation = config.fromOperation(resourcePath, httpMethod, operation, schemas, openAPI);
                codegenOperation.tags = new ArrayList<>(tags);
                config.addOperationToGroup(config.sanitizeTag(tag.getName()), resourcePath, operation, codegenOperation, operations);

                List<SecurityRequirement> securities = operation.getSecurity();
                if (securities != null && securities.isEmpty()) {
                    continue;
                }

                Map<String, SecurityScheme> authMethods = getAuthMethods(securities, securitySchemes);
                if (authMethods == null || authMethods.isEmpty()) {
                    authMethods = getAuthMethods(globalSecurities, securitySchemes);
                }

                if (authMethods != null && !authMethods.isEmpty()) {
                    codegenOperation.authMethods = config.fromSecurity(authMethods);
                    List<Map<String, Object>> scopes = new ArrayList<Map<String, Object>>();
                    for (CodegenSecurity security : codegenOperation.authMethods){
                        if (security.isBasicBearer){
                            for (SecurityRequirement req : securities){
                                for (String key : req.keySet()){
                                    if (key.equals(security.name)){
                                        int count = 0;
                                        for (String sc : req.get(key)){
                                            Map<String, Object> scope = new HashMap<String, Object>();
                                            scope.put("scope", sc);
                                            scope.put("description", "");
                                            count++;
                                            if (count < req.get(key).size()){
                                                scope.put("hasMore", "true");
                                            } else {
                                                scope.put("hasMore", null);
                                            }
                                            scopes.add(scope);
                                        }
                                        //end this inner for 
                                        break;
                                    }
                                }
                                
                            }
                            security.scopes = scopes;
                        }
                    }
                    
                    codegenOperation.hasAuthMethods = true;
                }

                /* TODO need to revise the logic below
                Map<String, SecurityScheme> securitySchemeMap = openAPI.getComponents().getSecuritySchemes();
                if (securitySchemeMap != null && !securitySchemeMap.isEmpty()) {
                    codegenOperation.authMethods = config.fromSecurity(securitySchemeMap);
                    codegenOperation.hasAuthMethods = true;
                }
                */
            } catch (Exception ex) {
                String msg = "Could not process operation:\n" //
                        + "  Tag: " + tag + "\n"//
                        + "  Operation: " + operation.getOperationId() + "\n" //
                        + "  Resource: " + httpMethod + " " + resourcePath + "\n"//
                        + "  Schemas: " + openAPI.getComponents().getSchemas() + "\n"  //
                        + "  Exception: " + ex.getMessage();
                throw new RuntimeException(msg, ex);
            }
        }

    }

    private static String generateParameterId(Parameter parameter) {
        return parameter.getName() + ":" + parameter.getIn();
    }


    private Map<String, Object> processOperations(CodegenConfig config, String tag, List<CodegenOperation> ops, List<Object> allModels) {
        Map<String, Object> operations = new HashMap<String, Object>();
        Map<String, Object> objs = new HashMap<String, Object>();
        objs.put("classname", config.toApiName(tag));
        objs.put("pathPrefix", config.toApiVarName(tag));

        // check for operationId uniqueness
        Set<String> opIds = new HashSet<String>();
        int counter = 0;
        for (CodegenOperation op : ops) {
            String opId = op.nickname;
            if (opIds.contains(opId)) {
                counter++;
                op.nickname += "_" + counter;
            }
            opIds.add(opId);
        }
        objs.put("operation", ops);

        operations.put("operations", objs);
        operations.put("package", config.apiPackage());

        Set<String> allImports = new TreeSet<String>();
        for (CodegenOperation op : ops) {
            allImports.addAll(op.imports);
        }

        List<Map<String, String>> imports = new ArrayList<Map<String, String>>();
        Set<String> mappingSet = new TreeSet<>();
        for (String nextImport : allImports) {
            Map<String, String> im = new LinkedHashMap<String, String>();
            String mapping = config.importMapping().get(nextImport);
            if (mapping == null) {
                mapping = config.toModelImport(nextImport);
            }

            if (mapping != null && !mappingSet.contains(mapping)) { // ensure import (mapping) is unique
                mappingSet.add(mapping);
                im.put("import", mapping);
                im.put("classname", nextImport);
                if (!imports.contains(im)) { // avoid duplicates
                    imports.add(im);
                }
            }
        }

        operations.put("imports", imports);

        // add a flag to indicate whether there's any {{import}}
        if (imports.size() > 0) {
            operations.put("hasImport", true);
        }

        config.postProcessOperations(operations);
        config.postProcessOperationsWithModels(operations, allModels);
        if (objs.size() > 0) {
            List<CodegenOperation> os = (List<CodegenOperation>) objs.get("operation");

            if (os != null && os.size() > 0) {
                CodegenOperation op = os.get(os.size() - 1);
                op.hasMore = false;
            }
        }
        return operations;
    }


    private Map<String, Object> processModels(CodegenConfig config, Map<String, Schema> definitions, Map<String, Schema> allDefinitions) {
        Map<String, Object> objs = new HashMap<String, Object>();
        objs.put("package", config.modelPackage());
        List<Object> models = new ArrayList<Object>();
        Set<String> allImports = new LinkedHashSet<String>();
        for (String key : definitions.keySet()) {
            Schema schema = definitions.get(key);
            if (schema == null)
                throw new RuntimeException("schema cannot be null in processMoels");
            CodegenModel cm = config.fromModel(key, schema, allDefinitions);
            Map<String, Object> mo = new HashMap<String, Object>();
            mo.put("model", cm);
            mo.put("importPath", config.toModelImport(cm.classname));
            models.add(mo);

            cm.removeSelfReferenceImport();

            allImports.addAll(cm.imports);
        }
        objs.put("models", models);
        Set<String> importSet = new TreeSet<String>();
        for (String nextImport : allImports) {
            String mapping = config.importMapping().get(nextImport);
            if (mapping == null) {
                mapping = config.toModelImport(nextImport);
            }
            if (mapping != null && !config.defaultIncludes().contains(mapping)) {
                importSet.add(mapping);
            }
            // add instantiation types
            mapping = config.instantiationTypes().get(nextImport);
            if (mapping != null && !config.defaultIncludes().contains(mapping)) {
                importSet.add(mapping);
            }
        }
        List<Map<String, String>> imports = new ArrayList<Map<String, String>>();
        for (String s : importSet) {
            Map<String, String> item = new HashMap<String, String>();
            item.put("import", s);
            imports.add(item);
        }
        objs.put("imports", imports);
        config.postProcessModels(objs);
        return objs;
    }

    private Map<String, SecurityScheme> getAuthMethods(List<SecurityRequirement> securities, Map<String, SecurityScheme> securitySchemes) {
        if (securities == null || (securitySchemes == null || securitySchemes.isEmpty())) {
            return null;
        }
        final Map<String, SecurityScheme> authMethods = new HashMap<>();
        for (SecurityRequirement requirement : securities) {
            for (String key : requirement.keySet()) {
                SecurityScheme securityScheme = securitySchemes.get(key);
                if (securityScheme != null) {
                    authMethods.put(key, securityScheme);
                }
            }
        }
        return authMethods;
    }

    private boolean hasOAuthMethods(List<CodegenSecurity> authMethods) {
        for (CodegenSecurity cs : authMethods) {
            if (cs.isOAuth) {
                return true;
            }
        }

        return false;
    }

    private List<CodegenSecurity> getOAuthMethods(List<CodegenSecurity> authMethods) {
        List<CodegenSecurity> oauthMethods = new ArrayList<>();

        for (CodegenSecurity cs : authMethods) {
            if (cs.isOAuth) {
                oauthMethods.add(cs);
            }
        }

        return oauthMethods;
    }
}
