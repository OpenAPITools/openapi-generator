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
import io.swagger.v3.oas.models.security.*;
import io.swagger.v3.oas.models.tags.Tag;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.config.GlobalSettings;
import org.openapitools.codegen.api.TemplatingEngineAdapter;
import org.openapitools.codegen.ignore.CodegenIgnoreProcessor;
import org.openapitools.codegen.languages.PythonClientExperimentalCodegen;
import org.openapitools.codegen.meta.GeneratorMetadata;
import org.openapitools.codegen.meta.Stability;
import org.openapitools.codegen.serializer.SerializerUtils;
import org.openapitools.codegen.templating.MustacheEngineAdapter;
import org.openapitools.codegen.utils.ImplementationVersion;
import org.openapitools.codegen.utils.ModelUtils;
import org.openapitools.codegen.utils.ProcessUtils;
import org.openapitools.codegen.utils.URLPathUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.*;
import java.net.URL;
import java.nio.file.Path;
import java.time.ZonedDateTime;
import java.util.*;

import static org.openapitools.codegen.utils.OnceLogger.once;

@SuppressWarnings("rawtypes")
public class DefaultGenerator extends AbstractGenerator implements Generator {
    protected final Logger LOGGER = LoggerFactory.getLogger(DefaultGenerator.class);
    protected CodegenConfig config;
    protected ClientOptInput opts;
    protected OpenAPI openAPI;
    protected CodegenIgnoreProcessor ignoreProcessor;
    protected TemplatingEngineAdapter templatingEngine;
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


    public DefaultGenerator() {
    }

    public DefaultGenerator(Boolean dryRun) {
        this.dryRun = Boolean.TRUE.equals(dryRun);
        LOGGER.info("Generating with dryRun={}", this.dryRun);
    }

    @Override
    public boolean getEnableMinimalUpdate() {
        return config.isEnableMinimalUpdate();
    }

    @SuppressWarnings("deprecation")
    @Override
    public Generator opts(ClientOptInput opts) {
        this.opts = opts;
        this.openAPI = opts.getOpenAPI();
        this.config = opts.getConfig();
        this.templatingEngine = this.config.getTemplatingEngine();

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

    private void configPostProcessMustacheCompiler() {
        if (this.templatingEngine instanceof MustacheEngineAdapter) {
            MustacheEngineAdapter mustacheEngineAdapter = (MustacheEngineAdapter) this.templatingEngine;
            mustacheEngineAdapter.setCompiler(this.config.processCompiler(mustacheEngineAdapter.getCompiler()));
        }
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

    void configureGeneratorProperties() {
        // allows generating only models by specifying a CSV of models to generate, or empty for all
        // NOTE: Boolean.TRUE is required below rather than `true` because of JVM boxing constraints and type inference.
        generateApis = GlobalSettings.getProperty(CodegenConstants.APIS) != null ? Boolean.TRUE : getGeneratorPropertyDefaultSwitch(CodegenConstants.APIS, null);
        generateModels = GlobalSettings.getProperty(CodegenConstants.MODELS) != null ? Boolean.TRUE : getGeneratorPropertyDefaultSwitch(CodegenConstants.MODELS, null);
        generateSupportingFiles = GlobalSettings.getProperty(CodegenConstants.SUPPORTING_FILES) != null ? Boolean.TRUE : getGeneratorPropertyDefaultSwitch(CodegenConstants.SUPPORTING_FILES, null);

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
        generateModelTests = GlobalSettings.getProperty(CodegenConstants.MODEL_TESTS) != null ? Boolean.valueOf(GlobalSettings.getProperty(CodegenConstants.MODEL_TESTS)) : getGeneratorPropertyDefaultSwitch(CodegenConstants.MODEL_TESTS, true);
        generateModelDocumentation = GlobalSettings.getProperty(CodegenConstants.MODEL_DOCS) != null ? Boolean.valueOf(GlobalSettings.getProperty(CodegenConstants.MODEL_DOCS)) : getGeneratorPropertyDefaultSwitch(CodegenConstants.MODEL_DOCS, true);
        generateApiTests = GlobalSettings.getProperty(CodegenConstants.API_TESTS) != null ? Boolean.valueOf(GlobalSettings.getProperty(CodegenConstants.API_TESTS)) : getGeneratorPropertyDefaultSwitch(CodegenConstants.API_TESTS, true);
        generateApiDocumentation = GlobalSettings.getProperty(CodegenConstants.API_DOCS) != null ? Boolean.valueOf(GlobalSettings.getProperty(CodegenConstants.API_DOCS)) : getGeneratorPropertyDefaultSwitch(CodegenConstants.API_DOCS, true);

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

        if (GlobalSettings.getProperty("debugOpenAPI") != null) {
            System.out.println(SerializerUtils.toJsonString(openAPI));
        } else if (GlobalSettings.getProperty("debugSwagger") != null) {
            // This exists for backward compatibility
            // We fall to this block only if debugOpenAPI is null. No need to dump this twice.
            LOGGER.info("Please use system property 'debugOpenAPI' instead of 'debugSwagger'.");
            System.out.println(SerializerUtils.toJsonString(openAPI));
        }

        config.processOpts();
        config.preprocessOpenAPI(openAPI);

        // set OpenAPI to make these available to all methods
        config.setOpenAPI(openAPI);

        config.additionalProperties().put("generatorVersion", ImplementationVersion.read());
        config.additionalProperties().put("generatedDate", ZonedDateTime.now().toString());
        config.additionalProperties().put("generatedYear", String.valueOf(ZonedDateTime.now().getYear()));
        config.additionalProperties().put("generatorClass", config.getClass().getName());
        config.additionalProperties().put("inputSpec", config.getInputSpec());

        if (openAPI.getExtensions() != null) {
            config.vendorExtensions().putAll(openAPI.getExtensions());
        }

        // TODO: Allow user to define _which_ servers object in the array to target.
        // Configures contextPath/basePath according to api document's servers
        URL url = URLPathUtils.getServerURL(openAPI, config.serverVariableOverrides());
        contextPath = config.escapeText(url.getPath()).replaceAll("/$", ""); // for backward compatibility
        basePathWithoutHost = contextPath;
        basePath = config.escapeText(URLPathUtils.getHost(openAPI, config.serverVariableOverrides())).replaceAll("/$", "");
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

            if (generateModelTests) {
                // do not overwrite test file that already exists (regardless of config's skipOverwrite setting)
                if (new File(filename).exists()) {
                    LOGGER.info("File exists. Skipped overwriting {}", filename);
                    if (dryRun) {
                        dryRunStatusMap.put(filename,
                                new DryRunStatus(
                                        java.nio.file.Paths.get(filename),
                                        DryRunStatus.State.SkippedOverwrite,
                                        "Test files never overwrite an existing file of the same name."
                                ));
                    }
                    continue;
                }
                File written = processTemplateToFile(models, templateName, filename);
                if (written != null) {
                    files.add(written);
                    if (config.isEnablePostProcessFile() && !dryRun) {
                        config.postProcessFile(written, "model-test");
                    }
                }
            } else if (dryRun) {
                dryRunStatusMap.put(filename,
                        new DryRunStatus(
                                java.nio.file.Paths.get(filename),
                                DryRunStatus.State.Skipped,
                                "Skipped by modelTests option supplied by user."
                        ));
            }
        }
    }

    private void generateModelDocumentation(List<File> files, Map<String, Object> models, String modelName) throws IOException {
        for (String templateName : config.modelDocTemplateFiles().keySet()) {
            String docExtension = config.getDocExtension();
            String suffix = docExtension != null ? docExtension : config.modelDocTemplateFiles().get(templateName);
            String filename = config.modelDocFileFolder() + File.separator + config.toModelDocFilename(modelName) + suffix;

            if (generateModelDocumentation) {
                if (!config.shouldOverwrite(filename)) {
                    LOGGER.info("Skipped overwriting {}", filename);
                    if (dryRun) {
                        dryRunStatusMap.put(filename, new DryRunStatus(java.nio.file.Paths.get(filename), DryRunStatus.State.SkippedOverwrite));
                    }
                    continue;
                }
                File written = processTemplateToFile(models, templateName, filename);
                if (written != null) {
                    files.add(written);
                    if (config.isEnablePostProcessFile() && !dryRun) {
                        config.postProcessFile(written, "model-doc");
                    }
                }
            } else if (dryRun) {
                dryRunStatusMap.put(filename,
                        new DryRunStatus(
                                java.nio.file.Paths.get(filename),
                                DryRunStatus.State.Skipped,
                                "Skipped by modelDocs option supplied by user."
                        ));
            }
        }
    }

    private String getModelFilenameByTemplate(String modelName, String templateName){
        String suffix = config.modelTemplateFiles().get(templateName);
        return config.modelFileFolder() + File.separator + config.toModelFilename(modelName) + suffix;
    }

    private void generateModel(List<File> files, Map<String, Object> models, String modelName) throws IOException {
        for (String templateName : config.modelTemplateFiles().keySet()) {
            String filename = getModelFilenameByTemplate(modelName, templateName);
            if (!config.shouldOverwrite(filename)) {
                LOGGER.info("Skipped overwriting {}", filename);
                if (dryRun) {
                    dryRunStatusMap.put(filename, new DryRunStatus(
                            java.nio.file.Paths.get(filename),
                            DryRunStatus.State.SkippedOverwrite
                    ));
                }
                continue;
            }
            File written = processTemplateToFile(models, templateName, filename);
            if (written != null) {
                files.add(written);
                if (config.isEnablePostProcessFile() && !dryRun) {
                    config.postProcessFile(written, "model");
                }
            } else {
                LOGGER.warn("Unknown issue writing {}", filename);
            }
        }
    }

    @SuppressWarnings("unchecked")
    void generateModels(List<File> files, List<Object> allModels, List<String> unusedModels) {
        if (!generateModels) {
            // TODO: Process these anyway and add to dryRun info
            LOGGER.info("Skipping generation of models.");
            return;
        }

        final Map<String, Schema> schemas = ModelUtils.getSchemas(this.openAPI);
        if (schemas == null) {
            LOGGER.warn("Skipping generation of models because specification document has no schemas.");
            return;
        }

        String modelNames = GlobalSettings.getProperty("models");
        Set<String> modelsToGenerate = null;
        if (modelNames != null && !modelNames.isEmpty()) {
            modelsToGenerate = new HashSet<>(Arrays.asList(modelNames.split(",")));
        }

        Set<String> modelKeys = schemas.keySet();
        if (modelsToGenerate != null && !modelsToGenerate.isEmpty()) {
            Set<String> updatedKeys = new HashSet<>();
            for (String m : modelKeys) {
                if (modelsToGenerate.contains(m)) {
                    updatedKeys.add(m);
                }
            }

            modelKeys = updatedKeys;
        }

        // store all processed models
        Map<String, Object> allProcessedModels = new TreeMap<>((o1, o2) -> ObjectUtils.compare(config.toModelName(o1), config.toModelName(o2)));

        Boolean skipFormModel = GlobalSettings.getProperty(CodegenConstants.SKIP_FORM_MODEL) != null ?
                Boolean.valueOf(GlobalSettings.getProperty(CodegenConstants.SKIP_FORM_MODEL)) :
                getGeneratorPropertyDefaultSwitch(CodegenConstants.SKIP_FORM_MODEL, false);

        // process models only
        for (String name : modelKeys) {
            try {
                //don't generate models that have an import mapping
                if (config.importMapping().containsKey(name)) {
                    LOGGER.debug("Model {} not imported due to import mapping", name);
                    if (dryRun) {
                        // HACK: Because this returns early, could lead to some invalid model reporting.
                        for (String templateName : config.modelTemplateFiles().keySet()) {
                            String filename = getModelFilenameByTemplate(name, templateName);
                            dryRunStatusMap.put(filename, new DryRunStatus(
                                    java.nio.file.Paths.get(filename),
                                    DryRunStatus.State.Skipped,
                                    "Skipped prior to model processing due to import mapping conflict (either by user or by generator)."
                            ));
                        }
                    }
                    continue;
                }

                // don't generate models that are not used as object (e.g. form parameters)
                if (unusedModels.contains(name)) {
                    if (Boolean.FALSE.equals(skipFormModel)) {
                        // if skipFormModel sets to true, still generate the model and log the result
                        LOGGER.info("Model {} (marked as unused due to form parameters) is generated due to the system property skipFormModel=false (default)", name);
                    } else {
                        LOGGER.info("Model {} not generated since it's marked as unused (due to form parameters) and skipFormModel (system property) set to true", name);
                        // TODO: Should this be added to dryRun? If not, this seems like a weird place to return early from processing.
                        continue;
                    }
                }

                Schema schema = schemas.get(name);

                if (ModelUtils.isFreeFormObject(schema)) { // check to see if it'a a free-form object
                    LOGGER.info("Model {} not generated since it's a free-form object", name);
                    continue;
                } else if (ModelUtils.isMapSchema(schema)) { // check to see if it's a "map" model
                    // A composed schema (allOf, oneOf, anyOf) is considered a Map schema if the additionalproperties attribute is set
                    // for that composed schema. However, in the case of a composed schema, the properties are defined or referenced
                    // in the inner schemas, and the outer schema does not have properties.
                    if (!ModelUtils.isGenerateAliasAsModel() && !ModelUtils.isComposedSchema(schema) && (schema.getProperties() == null || schema.getProperties().isEmpty())) {
                        // schema without property, i.e. alias to map
                        LOGGER.info("Model {} not generated since it's an alias to map (without property) and `generateAliasAsModel` is set to false (default)", name);
                        continue;
                    }
                } else if (ModelUtils.isArraySchema(schema)) { // check to see if it's an "array" model
                    if (!ModelUtils.isGenerateAliasAsModel() && (schema.getProperties() == null || schema.getProperties().isEmpty())) {
                        // schema without property, i.e. alias to array
                        LOGGER.info("Model {} not generated since it's an alias to array (without property) and `generateAliasAsModel` is set to false (default)", name);
                        continue;
                    }
                }

                Map<String, Schema> schemaMap = new HashMap<>();
                schemaMap.put(name, schema);
                Map<String, Object> models = processModels(config, schemaMap);
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
                if (modelTemplate != null && modelTemplate.containsKey("model")) {
                    CodegenModel m = (CodegenModel) modelTemplate.get("model");
                    if (m.isAlias && !(config instanceof PythonClientExperimentalCodegen))  {
                        // alias to number, string, enum, etc, which should not be generated as model
                        // for PythonClientExperimentalCodegen, all aliases are generated as models
                        continue;  // Don't create user-defined classes for aliases
                    }
                }

                allModels.add(modelTemplate);

                // to generate model files
                generateModel(files, models, modelName);

                // to generate model test files
                generateModelTests(files, models, modelName);

                // to generate model documentation files
                generateModelDocumentation(files, models, modelName);

            } catch (Exception e) {
                throw new RuntimeException("Could not generate model '" + modelName + "'", e);
            }
        }
        if (GlobalSettings.getProperty("debugModels") != null) {
            LOGGER.info("############ Model info ############");
            Json.prettyPrint(allModels);
        }

    }

    @SuppressWarnings("unchecked")
    private void generateApis(List<File> files, List<Object> allOperations, List<Object> allModels) {
        if (!generateApis) {
            // TODO: Process these anyway and present info via dryRun?
            LOGGER.info("Skipping generation of APIs.");
            return;
        }
        Map<String, List<CodegenOperation>> paths = processPaths(this.openAPI.getPaths());
        Set<String> apisToGenerate = null;
        String apiNames = GlobalSettings.getProperty("apis");
        if (apiNames != null && !apiNames.isEmpty()) {
            apisToGenerate = new HashSet<>(Arrays.asList(apiNames.split(",")));
        }
        if (apisToGenerate != null && !apisToGenerate.isEmpty()) {
            Map<String, List<CodegenOperation>> updatedPaths = new TreeMap<>();
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
                ops.sort((one, another) -> ObjectUtils.compare(one.operationId, another.operationId));
                Map<String, Object> operation = processOperations(config, tag, ops, allModels);
                URL url = URLPathUtils.getServerURL(openAPI, config.serverVariableOverrides());
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
                operation.put("strictSpecBehavior", config.isStrictSpecBehavior());

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
                    boolean isGroupParameters = Boolean.parseBoolean(config.vendorExtensions().get("x-group-parameters").toString());

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
                    sortParamsByRequiredFlag = Boolean.parseBoolean(this.config.additionalProperties().get(CodegenConstants.SORT_PARAMS_BY_REQUIRED_FLAG).toString());
                }
                operation.put("sortParamsByRequiredFlag", sortParamsByRequiredFlag);

                /* consumes, produces are no longer defined in OAS3.0
                processMimeTypes(swagger.getConsumes(), operation, "consumes");
                processMimeTypes(swagger.getProduces(), operation, "produces");
                */

                allOperations.add(new HashMap<>(operation));
                for (int i = 0; i < allOperations.size(); i++) {
                    Map<String, Object> oo = (Map<String, Object>) allOperations.get(i);
                    if (i < (allOperations.size() - 1)) {
                        oo.put("hasMore", "true");
                    }
                }

                for (String templateName : config.apiTemplateFiles().keySet()) {
                    String filename = config.apiFilename(templateName, tag);
                    File apiFile = new File(filename);
                    if (!config.shouldOverwrite(filename) && apiFile.exists()) {
                        LOGGER.info("Skipped overwriting {}", filename);
                        if (dryRun) {
                            DryRunStatus status = new DryRunStatus(apiFile.toPath(), DryRunStatus.State.SkippedOverwrite);
                            dryRunStatusMap.put(filename, status);
                        }
                        continue;
                    }

                    File written = processTemplateToFile(operation, templateName, filename);
                    if (written != null) {
                        files.add(written);
                        if (config.isEnablePostProcessFile() && !dryRun) {
                            config.postProcessFile(written, "api");
                        }
                    }
                }

                // to generate api test files
                for (String templateName : config.apiTestTemplateFiles().keySet()) {
                    String filename = config.apiTestFilename(templateName, tag);
                    File apiTestFile = new File(filename);
                    if (generateApiTests) {
                        // do not overwrite test file that already exists
                        if (apiTestFile.exists()) {
                            LOGGER.info("File exists. Skipped overwriting {}", filename);
                            if (dryRun) {
                                dryRunStatusMap.put(filename, new DryRunStatus(apiTestFile.toPath(), DryRunStatus.State.SkippedOverwrite));
                            }
                            continue;
                        }

                        File written = processTemplateToFile(operation, templateName, filename);
                        if (written != null) {
                            files.add(written);
                            if (config.isEnablePostProcessFile() && !dryRun) {
                                config.postProcessFile(written, "api-test");
                            }
                        }
                    } else if (dryRun) {
                        dryRunStatusMap.put(filename, new DryRunStatus(
                                apiTestFile.toPath(),
                                DryRunStatus.State.Skipped,
                                "Skipped by apiTests option supplied by user."
                        ));
                    }
                }

                // to generate api documentation files
                for (String templateName : config.apiDocTemplateFiles().keySet()) {
                    String filename = config.apiDocFilename(templateName, tag);
                    File apiDocFile = new File(filename);
                    if (generateApiDocumentation) {
                        if (!config.shouldOverwrite(filename) && apiDocFile.exists()) {
                            LOGGER.info("Skipped overwriting {}", filename);
                            if (dryRun) {
                                dryRunStatusMap.put(filename, new DryRunStatus(apiDocFile.toPath(), DryRunStatus.State.SkippedOverwrite));
                            }
                            continue;
                        }

                        File written = processTemplateToFile(operation, templateName, filename);
                        if (written != null) {
                            files.add(written);
                            if (config.isEnablePostProcessFile() && !dryRun) {
                                config.postProcessFile(written, "api-doc");
                            }
                        }
                    } else if (dryRun) {
                        dryRunStatusMap.put(filename, new DryRunStatus(
                                apiDocFile.toPath(),
                                DryRunStatus.State.Skipped,
                                "Skipped by apiDocs option supplied by user."
                        ));
                    }
                }

            } catch (Exception e) {
                throw new RuntimeException("Could not generate api file for '" + tag + "'", e);
            }
        }
        if (GlobalSettings.getProperty("debugOperations") != null) {
            LOGGER.info("############ Operation info ############");
            Json.prettyPrint(allOperations);
        }

    }

    private void generateSupportingFiles(List<File> files, Map<String, Object> bundle) {
        if (!generateSupportingFiles) {
            // TODO: process these anyway and report via dryRun?
            LOGGER.info("Skipping generation of supporting files.");
            return;
        }
        Set<String> supportingFilesToGenerate = null;
        String supportingFiles = GlobalSettings.getProperty(CodegenConstants.SUPPORTING_FILES);
        if (supportingFiles != null && !supportingFiles.isEmpty()) {
            supportingFilesToGenerate = new HashSet<>(Arrays.asList(supportingFiles.split(",")));
        }

        for (SupportingFile support : config.supportingFiles()) {
            try {
                String outputFolder = config.outputFolder();
                if (StringUtils.isNotEmpty(support.folder)) {
                    outputFolder += File.separator + support.folder;
                }
                File of = new File(outputFolder);
                if (!of.isDirectory()) {
                    if(!dryRun && !of.mkdirs()) {
                        once(LOGGER).debug("Output directory {} not created. It {}.", outputFolder, of.exists() ? "already exists." : "may not have appropriate permissions.");
                    }
                }
                String outputFilename = new File(support.destinationFilename).isAbsolute() // split
                        ? support.destinationFilename
                        : outputFolder + File.separator + support.destinationFilename.replace('/', File.separatorChar);
                if (!config.shouldOverwrite(outputFilename)) {
                    LOGGER.info("Skipped overwriting {}", outputFilename);
                    if (dryRun) {
                        Path skippedSupportingFile = java.nio.file.Paths.get(outputFilename);
                        DryRunStatus status = new DryRunStatus(
                                skippedSupportingFile,
                                DryRunStatus.State.SkippedOverwrite
                        );
                    }
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
                    if (dryRun) {
                        Path skippedSupportingFile = java.nio.file.Paths.get(outputFilename);
                        DryRunStatus status = new DryRunStatus(
                                skippedSupportingFile,
                                DryRunStatus.State.Skipped,
                                "Skipped by supportingFiles option supplied by user."
                        );
                        dryRunStatusMap.put(outputFilename, status);
                    }
                    continue;
                }

                if (ignoreProcessor.allowsFile(new File(outputFilename))) {
                    // support.templateFile is the unmodified/original supporting file name (e.g. build.sh.mustache)
                    // templatingEngine.templateExists dispatches resolution to this, performing template-engine specific inspect of support file extensions.
                    if (templatingEngine.templateExists(this, support.templateFile)) {
                        String templateContent = templatingEngine.compileTemplate(this, bundle, support.templateFile);
                        writeToFile(outputFilename, templateContent);
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
                        if (config.isEnablePostProcessFile() && !dryRun) {
                            config.postProcessFile(outputFile, "supporting-common");
                        }
                    }

                } else {
                    if (dryRun) {
                        dryRunStatusMap.put(outputFilename, new DryRunStatus(java.nio.file.Paths.get(outputFilename), DryRunStatus.State.Ignored));
                    }
                    LOGGER.info("Skipped generation of {} due to rule in .openapi-generator-ignore", outputFilename);
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
            if (config.isEnablePostProcessFile() && !dryRun) {
                config.postProcessFile(ignoreFile, "openapi-generator-ignore");
            }
        } else if (generateMetadata && dryRun && ignoreFile.exists()) {
            dryRunStatusMap.put(ignoreFileNameTarget, new DryRunStatus(ignoreFile.toPath(), DryRunStatus.State.SkippedOverwrite));
        } else if (!generateMetadata && dryRun) {
            dryRunStatusMap.put(ignoreFileNameTarget, new DryRunStatus(
                    ignoreFile.toPath(),
                    DryRunStatus.State.Skipped,
                    "Skipped by generateMetadata option supplied by user"
            ));
        }

        String versionMetadata = config.outputFolder() + File.separator + ".openapi-generator" + File.separator + "VERSION";
        if (generateMetadata) {
            File versionMetadataFile = new File(versionMetadata);
            try {
                writeToFile(versionMetadata, ImplementationVersion.read());
                files.add(versionMetadataFile);
                if (config.isEnablePostProcessFile() && !dryRun) {
                    config.postProcessFile(ignoreFile, "openapi-generator-version");
                }
            } catch (IOException e) {
                throw new RuntimeException("Could not generate supporting file '" + versionMetadata + "'", e);
            }
        } else if(!generateMetadata && dryRun) {
            Path metadata = java.nio.file.Paths.get(versionMetadata);
            DryRunStatus status = new DryRunStatus(metadata, DryRunStatus.State.Skipped, "Skipped by generateMetadata option supplied by user.");
            dryRunStatusMap.put(versionMetadata, status);
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

    @SuppressWarnings("unchecked")
    private Map<String, Object> buildSupportFileBundle(List<Object> allOperations, List<Object> allModels) {

        Map<String, Object> bundle = new HashMap<>(config.additionalProperties());
        bundle.put("apiPackage", config.apiPackage());

        Map<String, Object> apis = new HashMap<>();
        apis.put("apis", allOperations);

        URL url = URLPathUtils.getServerURL(openAPI, config.serverVariableOverrides());

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

            if (hasBearerMethods(authMethods)) {
                bundle.put("hasBearerMethods", true);
            }
            if (ProcessUtils.hasHttpSignatureMethods(authMethods)) {
                bundle.put("hasHttpSignatureMethods", true);
            }
            if (ProcessUtils.hasHttpBasicMethods(authMethods)) {
                bundle.put("hasHttpBasicMethods", true);
            }
            if (ProcessUtils.hasApiKeyMethods(authMethods)) {
                bundle.put("hasApiKeyMethods", true);
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

        if (GlobalSettings.getProperty("debugSupportingFiles") != null) {
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

        if (config.getGeneratorMetadata() == null) {
            LOGGER.warn("Generator '{}' is missing generator metadata!", config.getName());
        } else {
            GeneratorMetadata generatorMetadata = config.getGeneratorMetadata();
            if (StringUtils.isNotEmpty(generatorMetadata.getGenerationMessage())) {
                LOGGER.info(generatorMetadata.getGenerationMessage());
            }

            Stability stability = generatorMetadata.getStability();
            String stabilityMessage = String.format(Locale.ROOT, "Generator '%s' is considered %s.", config.getName(), stability.value());
            if (stability == Stability.DEPRECATED) {
                LOGGER.warn(stabilityMessage);
            } else {
                LOGGER.info(stabilityMessage);
            }
        }

        // resolve inline models
        InlineModelResolver inlineModelResolver = new InlineModelResolver();
        inlineModelResolver.flatten(openAPI);

        configureGeneratorProperties();
        configureOpenAPIInfo();

        // If the template adapter is mustache, we'll set the config-modified Compiler.
        configPostProcessMustacheCompiler();

        List<File> files = new ArrayList<>();
        // models
        List<String> filteredSchemas = ModelUtils.getSchemasUsedOnlyInFormParam(openAPI);
        List<Object> allModels = new ArrayList<>();
        generateModels(files, allModels, filteredSchemas);
        // apis
        List<Object> allOperations = new ArrayList<>();
        generateApis(files, allOperations, allModels);

        // supporting files
        Map<String, Object> bundle = buildSupportFileBundle(allOperations, allModels);
        generateSupportingFiles(files, bundle);
        config.processOpenAPI(openAPI);

        if(dryRun) {
            boolean verbose = Boolean.parseBoolean(GlobalSettings.getProperty("verbose"));
            StringBuilder sb = new StringBuilder();

            sb.append(System.lineSeparator()).append(System.lineSeparator());
            sb.append("Dry Run Results:");
            sb.append(System.lineSeparator()).append(System.lineSeparator());

            dryRunStatusMap.entrySet().stream().sorted(Map.Entry.comparingByKey()).forEach(entry -> {
                DryRunStatus status = entry.getValue();
                try {
                    status.appendTo(sb);
                    sb.append(System.lineSeparator());
                    if (verbose) {
                        sb.append("  ")
                            .append(StringUtils.rightPad(status.getState().getDescription(), 20, "."))
                            .append(" ").append(status.getReason())
                            .append(System.lineSeparator());
                    }
                } catch (IOException e) {
                    LOGGER.debug("Unable to document dry run status for {}.", entry.getKey());
                }
            });

            sb.append(System.lineSeparator()).append(System.lineSeparator());
            sb.append("States:");
            sb.append(System.lineSeparator()).append(System.lineSeparator());

            for (DryRunStatus.State state : DryRunStatus.State.values()) {
                sb.append("  - ").append(state.getShortDisplay()).append(" ").append(state.getDescription()).append(System.lineSeparator());
            }

            sb.append(System.lineSeparator());

            System.err.println(sb.toString());
        }

        // reset GlobalSettings, so that the running thread can be reused for another generator-run
        GlobalSettings.reset();

        return files;
    }

    @Override
    public String getFullTemplateContents(String templateName) {
        return readTemplate(getFullTemplateFile(config, templateName));
    }

    /**
     * Returns the path of a template, allowing access to the template where consuming literal contents aren't desirable or possible.
     *
     * @param name the template name (e.g. model.mustache)
     * @return The {@link Path} to the template
     */
    @Override
    public Path getFullTemplatePath(String name) {
        String fullPath = getFullTemplateFile(config, name);
        return java.nio.file.Paths.get(fullPath);
    }

    protected File processTemplateToFile(Map<String, Object> templateData, String templateName, String outputFilename) throws IOException {
        String adjustedOutputFilename = outputFilename.replaceAll("//", "/").replace('/', File.separatorChar);
        File target = new File(adjustedOutputFilename);
        if (ignoreProcessor.allowsFile(target)) {
            String templateContent = templatingEngine.compileTemplate(this, templateData, templateName);
            writeToFile(adjustedOutputFilename, templateContent);
            return target;
        } else if (this.dryRun) {
            dryRunStatusMap.put(adjustedOutputFilename, new DryRunStatus(target.toPath(), DryRunStatus.State.Ignored));
            return target;
        }

        LOGGER.info("Skipped generation of {} due to rule in .openapi-generator-ignore", adjustedOutputFilename);
        return null;
    }

    public Map<String, List<CodegenOperation>> processPaths(Paths paths) {
        Map<String, List<CodegenOperation>> ops = new TreeMap<>();
        for (String resourcePath : paths.keySet()) {
            PathItem path = paths.get(resourcePath);
            processOperation(resourcePath, "get", path.getGet(), ops, path);
            processOperation(resourcePath, "head", path.getHead(), ops, path);
            processOperation(resourcePath, "put", path.getPut(), ops, path);
            processOperation(resourcePath, "post", path.getPost(), ops, path);
            processOperation(resourcePath, "delete", path.getDelete(), ops, path);
            processOperation(resourcePath, "patch", path.getPatch(), ops, path);
            processOperation(resourcePath, "options", path.getOptions(), ops, path);
            processOperation(resourcePath, "trace", path.getTrace(), ops, path);
        }
        return ops;
    }

    private void processOperation(String resourcePath, String httpMethod, Operation operation, Map<String, List<CodegenOperation>> operations, PathItem path) {
        if (operation == null) {
            return;
        }

        if (GlobalSettings.getProperty("debugOperations") != null) {
            LOGGER.info("processOperation: resourcePath=  {}\t;{} {}\n", resourcePath, httpMethod, operation);
        }

        List<Tag> tags = new ArrayList<>();
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
        Set<String> operationParameters = new HashSet<>();
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

        final Map<String, SecurityScheme> securitySchemes = openAPI.getComponents() != null ? openAPI.getComponents().getSecuritySchemes() : null;
        final List<SecurityRequirement> globalSecurities = openAPI.getSecurity();
        for (Tag tag : tags) {
            try {
                CodegenOperation codegenOperation = config.fromOperation(resourcePath, httpMethod, operation, path.getServers());
                codegenOperation.tags = new ArrayList<>(tags);
                config.addOperationToGroup(config.sanitizeTag(tag.getName()), resourcePath, operation, codegenOperation, operations);

                List<SecurityRequirement> securities = operation.getSecurity();
                if (securities != null && securities.isEmpty()) {
                    continue;
                }

                Map<String, SecurityScheme> authMethods = getAuthMethods(securities, securitySchemes);

                if (authMethods != null && !authMethods.isEmpty()) {
                    List<CodegenSecurity> fullAuthMethods = config.fromSecurity(authMethods);
                    codegenOperation.authMethods = filterAuthMethods(fullAuthMethods, securities);
                    codegenOperation.hasAuthMethods = true;
                } else {
                    authMethods = getAuthMethods(globalSecurities, securitySchemes);

                    if (authMethods != null && !authMethods.isEmpty()) {
                        List<CodegenSecurity> fullAuthMethods = config.fromSecurity(authMethods);
                        codegenOperation.authMethods = filterAuthMethods(fullAuthMethods, globalSecurities);
                        codegenOperation.hasAuthMethods = true;
                    }
                }

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

    @SuppressWarnings("unchecked")
    private Map<String, Object> processOperations(CodegenConfig config, String tag, List<CodegenOperation> ops, List<Object> allModels) {
        Map<String, Object> operations = new HashMap<>();
        Map<String, Object> objs = new HashMap<>();
        objs.put("classname", config.toApiName(tag));
        objs.put("pathPrefix", config.toApiVarName(tag));

        // check for operationId uniqueness
        Set<String> opIds = new HashSet<>();
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

        Set<String> allImports = new TreeSet<>();
        for (CodegenOperation op : ops) {
            allImports.addAll(op.imports);
        }

        List<Map<String, String>> imports = new ArrayList<>();
        Set<String> mappingSet = new TreeSet<>();
        for (String nextImport : allImports) {
            Map<String, String> im = new LinkedHashMap<>();
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

    private Map<String, Object> processModels(CodegenConfig config, Map<String, Schema> definitions) {
        Map<String, Object> objs = new HashMap<>();
        objs.put("package", config.modelPackage());
        List<Object> models = new ArrayList<>();
        Set<String> allImports = new LinkedHashSet<>();
        for (String key : definitions.keySet()) {
            Schema schema = definitions.get(key);
            if (schema == null)
                throw new RuntimeException("schema cannot be null in processModels");
            CodegenModel cm = config.fromModel(key, schema);
            Map<String, Object> mo = new HashMap<>();
            mo.put("model", cm);
            mo.put("importPath", config.toModelImport(cm.classname));
            models.add(mo);

            cm.removeSelfReferenceImport();

            allImports.addAll(cm.imports);
        }
        objs.put("models", models);
        Set<String> importSet = new TreeSet<>();
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
        List<Map<String, String>> imports = new ArrayList<>();
        for (String s : importSet) {
            Map<String, String> item = new HashMap<>();
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
            for (Map.Entry<String, List<String>> entry : requirement.entrySet()) {
                final String key = entry.getKey();
                SecurityScheme securityScheme = securitySchemes.get(key);
                if (securityScheme != null) {

                    if (securityScheme.getType().equals(SecurityScheme.Type.OAUTH2)) {
                        OAuthFlows oautUpdatedFlows = new OAuthFlows();
                        oautUpdatedFlows.extensions(securityScheme.getFlows().getExtensions());

                        SecurityScheme oauthUpdatedScheme = new SecurityScheme()
                                .type(securityScheme.getType())
                                .description(securityScheme.getDescription())
                                .name(securityScheme.getName())
                                .$ref(securityScheme.get$ref())
                                .in(securityScheme.getIn())
                                .scheme(securityScheme.getScheme())
                                .bearerFormat(securityScheme.getBearerFormat())
                                .openIdConnectUrl(securityScheme.getOpenIdConnectUrl())
                                .extensions(securityScheme.getExtensions())
                                .flows(oautUpdatedFlows);

                        // Ensure inserted AuthMethod only contains scopes of actual operation, and not all of them defined in the Security Component
                        // have to iterate through and create new SecurityScheme objects with the scopes 'fixed/updated'
                        final OAuthFlows securitySchemeFlows = securityScheme.getFlows();


                        if (securitySchemeFlows.getAuthorizationCode() != null) {
                            OAuthFlow updatedFlow = cloneOAuthFlow(securitySchemeFlows.getAuthorizationCode(), entry.getValue());

                            oautUpdatedFlows.setAuthorizationCode(updatedFlow);
                        }
                        if (securitySchemeFlows.getImplicit() != null) {
                            OAuthFlow updatedFlow = cloneOAuthFlow(securitySchemeFlows.getImplicit(), entry.getValue());

                            oautUpdatedFlows.setImplicit(updatedFlow);
                        }
                        if (securitySchemeFlows.getPassword() != null) {
                            OAuthFlow updatedFlow = cloneOAuthFlow(securitySchemeFlows.getPassword(), entry.getValue());

                            oautUpdatedFlows.setPassword(updatedFlow);
                        }
                        if (securitySchemeFlows.getClientCredentials() != null) {
                            OAuthFlow updatedFlow = cloneOAuthFlow(securitySchemeFlows.getClientCredentials(), entry.getValue());

                            oautUpdatedFlows.setClientCredentials(updatedFlow);
                        }

                        authMethods.put(key, oauthUpdatedScheme);
                    } else {
                        authMethods.put(key, securityScheme);
                    }
                }
            }
        }
        return authMethods;
    }

    private static OAuthFlow cloneOAuthFlow(OAuthFlow originFlow, List<String> operationScopes) {
        Scopes newScopes = new Scopes();
        for (String operationScope : operationScopes) {
            newScopes.put(operationScope, originFlow.getScopes().get(operationScope));
        }

        return new OAuthFlow()
                .authorizationUrl(originFlow.getAuthorizationUrl())
                .tokenUrl(originFlow.getTokenUrl())
                .refreshUrl(originFlow.getRefreshUrl())
                .extensions(originFlow.getExtensions())
                .scopes(newScopes);
    }

    private List<CodegenSecurity> filterAuthMethods(List<CodegenSecurity> authMethods, List<SecurityRequirement> securities) {
        if (securities == null || securities.isEmpty() || authMethods == null) {
            return authMethods;
        }

        List<CodegenSecurity> result = new ArrayList<>();

        for (CodegenSecurity security : authMethods) {
            boolean filtered = false;
            if (security != null && security.scopes != null) {
                for (SecurityRequirement requirement : securities) {
                    List<String> opScopes = requirement.get(security.name);
                    if (opScopes != null) {
                        // We have operation-level scopes for this method, so filter the auth method to
                        // describe the operation auth method with only the scopes that it requires.
                        // We have to create a new auth method instance because the original object must
                        // not be modified.
                        CodegenSecurity opSecurity = security.filterByScopeNames(opScopes);
                        opSecurity.hasMore = security.hasMore;
                        result.add(opSecurity);
                        filtered = true;
                        break;
                    }
                }
            }

            // If we didn't get a filtered version, then we can keep the original auth method.
            if (!filtered) {
                result.add(security);
            }
        }

        return result;
    }

    private boolean hasOAuthMethods(List<CodegenSecurity> authMethods) {
        for (CodegenSecurity cs : authMethods) {
            if (Boolean.TRUE.equals(cs.isOAuth)) {
                return true;
            }
        }

        return false;
    }

    private boolean hasBearerMethods(List<CodegenSecurity> authMethods) {
        for (CodegenSecurity cs : authMethods) {
            if (Boolean.TRUE.equals(cs.isBasicBearer)) {
                return true;
            }
        }

        return false;
    }

    private List<CodegenSecurity> getOAuthMethods(List<CodegenSecurity> authMethods) {
        List<CodegenSecurity> oauthMethods = new ArrayList<>();

        for (CodegenSecurity cs : authMethods) {
            if (Boolean.TRUE.equals(cs.isOAuth)) {
                oauthMethods.add(cs);
            }
        }

        return oauthMethods;
    }

    protected File writeInputStreamToFile(String filename, InputStream in, String templateFile) throws IOException {
        if (in != null) {
            byte[] bytes = IOUtils.toByteArray(in);
            if (dryRun) {
                Path path = java.nio.file.Paths.get(filename);
                dryRunStatusMap.put(filename, new DryRunStatus(path));
                return path.toFile();
            }

            return writeToFile(filename, bytes);
        } else {
            LOGGER.error("can't open '{}' for input; cannot write '{}'", templateFile, filename);
            if (dryRun) {
                Path path = java.nio.file.Paths.get(filename);
                dryRunStatusMap.put(filename, new DryRunStatus(path, DryRunStatus.State.Error));
            }

            return null;
        }
    }

    /**
     * Write bytes to a file
     *
     * @param filename The name of file to write
     * @param contents The contents bytes.  Typically this is a UTF-8 formatted string.
     * @return File representing the written file.
     * @throws IOException If file cannot be written.
     */
    @Override
    public File writeToFile(String filename, byte[] contents) throws IOException {
        if (dryRun) {
            Path path = java.nio.file.Paths.get(filename);
            DryRunStatus status = new DryRunStatus(path);
            if (getEnableMinimalUpdate()) {
                status.setState(DryRunStatus.State.WriteIfNewer);
            } else {
                status.setState(DryRunStatus.State.Write);
            }

            dryRunStatusMap.put(filename, status);
            return path.toFile();
        } else {
            return super.writeToFile(filename, contents);
        }
    }
}
