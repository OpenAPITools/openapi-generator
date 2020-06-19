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

package org.openapitools.codegen.config;

import com.fasterxml.jackson.databind.Module;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.datatype.guava.GuavaModule;
import io.swagger.parser.OpenAPIParser;
import io.swagger.v3.core.util.Json;
import io.swagger.v3.core.util.Yaml;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.parser.core.models.AuthorizationValue;
import io.swagger.v3.parser.core.models.ParseOptions;
import io.swagger.v3.parser.core.models.SwaggerParseResult;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.Validate;
import org.openapitools.codegen.*;
import org.openapitools.codegen.api.TemplatingEngineAdapter;
import org.openapitools.codegen.auth.AuthParser;
import org.openapitools.codegen.utils.ModelUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.IOException;
import java.util.*;

import static org.apache.commons.lang3.StringUtils.isEmpty;
import static org.apache.commons.lang3.StringUtils.isNotEmpty;

/**
 * A class which manages the contextual configuration for code generation.
 * This includes configuring the generator, templating, and the workflow which orchestrates these.
 *
 * This helper also enables the deserialization of {@link GeneratorSettings} via application-specific Jackson JSON usage
 * (see {@link DynamicSettings}.
 */
@SuppressWarnings("UnusedReturnValue")
public class CodegenConfigurator {

    public static final Logger LOGGER = LoggerFactory.getLogger(CodegenConfigurator.class);

    private GeneratorSettings.Builder generatorSettingsBuilder = GeneratorSettings.newBuilder();
    private WorkflowSettings.Builder workflowSettingsBuilder = WorkflowSettings.newBuilder();

    private String generatorName;
    private String inputSpec;
    private String templatingEngineName;
    private Map<String, String> globalProperties = new HashMap<>();
    private Map<String, String> instantiationTypes = new HashMap<>();
    private Map<String, String> typeMappings = new HashMap<>();
    private Map<String, Object> additionalProperties = new HashMap<>();
    private Map<String, String> importMappings = new HashMap<>();
    private Set<String> languageSpecificPrimitives = new HashSet<>();
    private Map<String, String> reservedWordMappings = new HashMap<>();
    private Map<String, String> serverVariables = new HashMap<>();
    private String auth;

    public CodegenConfigurator() {

    }

    @SuppressWarnings("DuplicatedCode")
    public static CodegenConfigurator fromFile(String configFile, Module... modules) {

        if (isNotEmpty(configFile)) {
            DynamicSettings settings = readDynamicSettings(configFile, modules);

            CodegenConfigurator configurator = new CodegenConfigurator();

            GeneratorSettings generatorSettings = settings.getGeneratorSettings();
            WorkflowSettings workflowSettings = settings.getWorkflowSettings();

            // We copy "cached" properties into configurator so it is appropriately configured with all settings in external files.
            // FIXME: target is to eventually move away from CodegenConfigurator properties except gen/workflow settings.
            configurator.generatorName = generatorSettings.getGeneratorName();
            configurator.inputSpec = workflowSettings.getInputSpec();
            configurator.templatingEngineName = workflowSettings.getTemplatingEngineName();
            if (workflowSettings.getGlobalProperties() != null) {
                configurator.globalProperties.putAll(workflowSettings.getGlobalProperties());
            }
            if(generatorSettings.getInstantiationTypes() != null) {
                configurator.instantiationTypes.putAll(generatorSettings.getInstantiationTypes());
            }
            if(generatorSettings.getTypeMappings() != null) {
                configurator.typeMappings.putAll(generatorSettings.getTypeMappings());
            }
            if(generatorSettings.getAdditionalProperties() != null) {
                configurator.additionalProperties.putAll(generatorSettings.getAdditionalProperties());
            }
            if(generatorSettings.getImportMappings() != null) {
                configurator.importMappings.putAll(generatorSettings.getImportMappings());
            }
            if(generatorSettings.getLanguageSpecificPrimitives() != null) {
                configurator.languageSpecificPrimitives.addAll(generatorSettings.getLanguageSpecificPrimitives());
            }
            if(generatorSettings.getReservedWordMappings() != null) {
                configurator.reservedWordMappings.putAll(generatorSettings.getReservedWordMappings());
            }
            if(generatorSettings.getServerVariables() != null) {
                configurator.serverVariables.putAll(generatorSettings.getServerVariables());
            }

            configurator.generatorSettingsBuilder = GeneratorSettings.newBuilder(generatorSettings);
            configurator.workflowSettingsBuilder = WorkflowSettings.newBuilder(workflowSettings);

            return configurator;
        }
        return null;
    }

    private static DynamicSettings readDynamicSettings(String configFile, Module... modules) {
        ObjectMapper mapper;

        if (FilenameUtils.isExtension(configFile.toLowerCase(Locale.ROOT), new String[]{"yml", "yaml"})) {
            mapper = Yaml.mapper().copy();
        } else {
            mapper = Json.mapper().copy();
        }

        if (modules != null && modules.length > 0) {
            mapper.registerModules(modules);
        }

        mapper.registerModule(new GuavaModule());

        try {
            return mapper.readValue(new File(configFile), DynamicSettings.class);
        } catch (IOException ex) {
            LOGGER.error(ex.getMessage());
            throw new RuntimeException("Unable to deserialize config file: " + configFile);
        }
    }

    public CodegenConfigurator addServerVariable(String key, String value) {
        this.serverVariables.put(key, value);
        generatorSettingsBuilder.withServerVariable(key, value);
        return this;
    }

    public CodegenConfigurator addAdditionalProperty(String key, Object value) {
        this.additionalProperties.put(key, value);
        generatorSettingsBuilder.withAdditionalProperty(key, value);
        return this;
    }

    public CodegenConfigurator addAdditionalReservedWordMapping(String key, String value) {
        this.reservedWordMappings.put(key, value);
        generatorSettingsBuilder.withReservedWordMapping(key, value);
        return this;
    }

    public CodegenConfigurator addImportMapping(String key, String value) {
        this.importMappings.put(key, value);
        generatorSettingsBuilder.withImportMapping(key, value);
        return this;
    }

    public CodegenConfigurator addInstantiationType(String key, String value) {
        this.instantiationTypes.put(key, value);
        generatorSettingsBuilder.withInstantiationType(key, value);
        return this;
    }

    public CodegenConfigurator addLanguageSpecificPrimitive(String value) {
        this.languageSpecificPrimitives.add(value);
        generatorSettingsBuilder.withLanguageSpecificPrimitive(value);
        return this;
    }

    public CodegenConfigurator addGlobalProperty(String key, String value) {
        this.globalProperties.put(key, value);
        workflowSettingsBuilder.withGlobalProperty(key, value);
        return this;
    }

    public CodegenConfigurator addTypeMapping(String key, String value) {
        this.typeMappings.put(key, value);
        generatorSettingsBuilder.withTypeMappings(this.typeMappings);
        return this;
    }

    public CodegenConfigurator setAdditionalProperties(Map<String, Object> additionalProperties) {
        this.additionalProperties = additionalProperties;
        generatorSettingsBuilder.withAdditionalProperties(additionalProperties);
        return this;
    }

    public CodegenConfigurator setServerVariables(Map<String, String> serverVariables) {
        this.serverVariables = serverVariables;
        generatorSettingsBuilder.withServerVariables(serverVariables);
        return this;
    }

    public CodegenConfigurator setReservedWordsMappings(Map<String, String> reservedWordMappings) {
        this.reservedWordMappings = reservedWordMappings;
        generatorSettingsBuilder.withReservedWordMappings(reservedWordMappings);
        return this;
    }

    public CodegenConfigurator setApiPackage(String apiPackage) {
        if (StringUtils.isNotEmpty(apiPackage)) {
            addAdditionalProperty(CodegenConstants.API_PACKAGE, apiPackage);
        }
        generatorSettingsBuilder.withApiPackage(apiPackage);
        return this;
    }

    public CodegenConfigurator setArtifactId(String artifactId) {
        if (StringUtils.isNotEmpty(artifactId)) {
            addAdditionalProperty(CodegenConstants.ARTIFACT_ID, artifactId);
        }
        generatorSettingsBuilder.withArtifactId(artifactId);
        return this;
    }

    public CodegenConfigurator setArtifactVersion(String artifactVersion) {
        if (StringUtils.isNotEmpty(artifactVersion)) {
            addAdditionalProperty(CodegenConstants.ARTIFACT_VERSION, artifactVersion);
        }
        generatorSettingsBuilder.withArtifactVersion(artifactVersion);
        return this;
    }

    public CodegenConfigurator setAuth(String auth) {
        // do not cache this in additional properties.
        this.auth = auth;
        return this;
    }

    public CodegenConfigurator setEnableMinimalUpdate(boolean enableMinimalUpdate) {
        workflowSettingsBuilder.withEnableMinimalUpdate(enableMinimalUpdate);
        return this;
    }

    public CodegenConfigurator setEnablePostProcessFile(boolean enablePostProcessFile) {
        workflowSettingsBuilder.withEnablePostProcessFile(enablePostProcessFile);
        return this;
    }

    public CodegenConfigurator setGenerateAliasAsModel(boolean generateAliasAsModel) {
        workflowSettingsBuilder.withGenerateAliasAsModel(generateAliasAsModel);
        ModelUtils.setGenerateAliasAsModel(generateAliasAsModel);
        return this;
    }

    /**
     * Sets the name of the target generator.
     * <p>
     * The generator's name is used to uniquely identify the generator as a mechanism to lookup the
     * desired implementation at runtime.
     *
     * @param generatorName The name of the generator.
     * @return The fluent instance of {@link CodegenConfigurator}
     */
    public CodegenConfigurator setGeneratorName(final String generatorName) {
        this.generatorName = generatorName;
        generatorSettingsBuilder.withGeneratorName(generatorName);
        return this;
    }

    public CodegenConfigurator setGitRepoId(String gitRepoId) {
        if (StringUtils.isNotEmpty(gitRepoId)) {
            addAdditionalProperty(CodegenConstants.GIT_REPO_ID, gitRepoId);
        }
        generatorSettingsBuilder.withGitRepoId(gitRepoId);
        return this;
    }

    public CodegenConfigurator setGitHost(String gitHost) {
        if (StringUtils.isNotEmpty(gitHost)) {
            addAdditionalProperty(CodegenConstants.GIT_HOST, gitHost);
        }
        generatorSettingsBuilder.withGitHost(gitHost);
        return this;
    }

    public CodegenConfigurator setGitUserId(String gitUserId) {
        if (StringUtils.isNotEmpty(gitUserId)) {
            addAdditionalProperty(CodegenConstants.GIT_HOST, gitUserId);
        }
        generatorSettingsBuilder.withGitUserId(gitUserId);
        return this;
    }

    public CodegenConfigurator setGroupId(String groupId) {
        if (StringUtils.isNotEmpty(groupId)) {
            addAdditionalProperty(CodegenConstants.GROUP_ID, groupId);
        }
        generatorSettingsBuilder.withGroupId(groupId);
        return this;
    }

    public CodegenConfigurator setHttpUserAgent(String httpUserAgent) {
        if (StringUtils.isNotEmpty(httpUserAgent)) {
            addAdditionalProperty(CodegenConstants.HTTP_USER_AGENT, httpUserAgent);
        }
        generatorSettingsBuilder.withHttpUserAgent(httpUserAgent);
        return this;
    }

    public CodegenConfigurator setIgnoreFileOverride(final String ignoreFileOverride) {
        workflowSettingsBuilder.withIgnoreFileOverride(ignoreFileOverride);
        return this;
    }

    public CodegenConfigurator setImportMappings(Map<String, String> importMappings) {
        this.importMappings = importMappings;
        generatorSettingsBuilder.withImportMappings(importMappings);
        return this;
    }

    public CodegenConfigurator setInputSpec(String inputSpec) {
        this.inputSpec = inputSpec;
        workflowSettingsBuilder.withInputSpec(inputSpec);
        return this;
    }

    public CodegenConfigurator setInstantiationTypes(Map<String, String> instantiationTypes) {
        this.instantiationTypes = instantiationTypes;
        generatorSettingsBuilder.withInstantiationTypes(instantiationTypes);
        return this;
    }

    public CodegenConfigurator setInvokerPackage(String invokerPackage) {
        if (StringUtils.isNotEmpty(invokerPackage)) {
            addAdditionalProperty(CodegenConstants.INVOKER_PACKAGE, invokerPackage);
        }
        generatorSettingsBuilder.withInvokerPackage(invokerPackage);
        return this;
    }

    public CodegenConfigurator setLanguageSpecificPrimitives(
            Set<String> languageSpecificPrimitives) {
        this.languageSpecificPrimitives = languageSpecificPrimitives;
        generatorSettingsBuilder.withLanguageSpecificPrimitives(languageSpecificPrimitives);
        return this;
    }

    public CodegenConfigurator setLibrary(String library) {
        generatorSettingsBuilder.withLibrary(library);
        return this;
    }

    public CodegenConfigurator setLogToStderr(boolean logToStderr) {
        workflowSettingsBuilder.withLogToStderr(logToStderr);
        return this;
    }

    public CodegenConfigurator setApiNameSuffix(String suffix) {
        if (StringUtils.isNotEmpty(suffix)) {
            addAdditionalProperty(CodegenConstants.API_NAME_SUFFIX, suffix);
        }
        generatorSettingsBuilder.withApiNameSuffix(suffix);
        return this;
    }

    public CodegenConfigurator setModelNamePrefix(String prefix) {
        if (StringUtils.isNotEmpty(prefix)) {
            addAdditionalProperty(CodegenConstants.MODEL_NAME_PREFIX, prefix);
        }
        generatorSettingsBuilder.withModelNamePrefix(prefix);
        return this;
    }

    public CodegenConfigurator setModelNameSuffix(String suffix) {
        if (StringUtils.isNotEmpty(suffix)) {
            addAdditionalProperty(CodegenConstants.MODEL_NAME_SUFFIX, suffix);
        }
        generatorSettingsBuilder.withModelNameSuffix(suffix);
        return this;
    }

    public CodegenConfigurator setModelPackage(String modelPackage) {
        if (StringUtils.isNotEmpty(modelPackage)) {
            addAdditionalProperty(CodegenConstants.MODEL_PACKAGE, modelPackage);
        }
        generatorSettingsBuilder.withModelPackage(modelPackage);
        return this;
    }

    public CodegenConfigurator setOutputDir(String outputDir) {
        workflowSettingsBuilder.withOutputDir(outputDir);
        return this;
    }

    public CodegenConfigurator setPackageName(String packageName) {
        if (StringUtils.isNotEmpty(packageName)) {
            addAdditionalProperty(CodegenConstants.PACKAGE_NAME, packageName);
        }
        generatorSettingsBuilder.withPackageName(packageName);
        return this;
    }

    public CodegenConfigurator setReleaseNote(String releaseNote) {
        if (StringUtils.isNotEmpty(releaseNote)) {
            addAdditionalProperty(CodegenConstants.RELEASE_NOTE, releaseNote);
        }
        generatorSettingsBuilder.withReleaseNote(releaseNote);
        return this;
    }

    public CodegenConfigurator setRemoveOperationIdPrefix(boolean removeOperationIdPrefix) {
        workflowSettingsBuilder.withRemoveOperationIdPrefix(removeOperationIdPrefix);
        return this;
    }

    public CodegenConfigurator setSkipOverwrite(boolean skipOverwrite) {
        workflowSettingsBuilder.withSkipOverwrite(skipOverwrite);
        return this;
    }

    public CodegenConfigurator setStrictSpecBehavior(boolean strictSpecBehavior) {
        workflowSettingsBuilder.withStrictSpecBehavior(strictSpecBehavior);
        return this;
    }

    public CodegenConfigurator setGlobalProperties(Map<String, String> globalProperties) {
        this.globalProperties = globalProperties;
        workflowSettingsBuilder.withGlobalProperties(globalProperties);
        return this;
    }

    public CodegenConfigurator setTemplateDir(String templateDir) {
        workflowSettingsBuilder.withTemplateDir(templateDir);
        return this;
    }

    public CodegenConfigurator setTemplatingEngineName(String templatingEngineName) {
        this.templatingEngineName = templatingEngineName;
        workflowSettingsBuilder.withTemplatingEngineName(templatingEngineName);
        return this;
    }

    public CodegenConfigurator setTypeMappings(Map<String, String> typeMappings) {
        this.typeMappings = typeMappings;
        generatorSettingsBuilder.withTypeMappings(typeMappings);
        return this;
    }

    public CodegenConfigurator setValidateSpec(final boolean validateSpec) {
        workflowSettingsBuilder.withValidateSpec(validateSpec);
        return this;
    }

    public CodegenConfigurator setVerbose(boolean verbose) {
        workflowSettingsBuilder.withVerbose(verbose);
        return this;
    }

    @SuppressWarnings("WeakerAccess")
    public Context<?> toContext() {
        Validate.notEmpty(generatorName, "generator name must be specified");
        Validate.notEmpty(inputSpec, "input spec must be specified");

        if (isEmpty(templatingEngineName)) {
            // Built-in templates are mustache, but allow users to use a simplified handlebars engine for their custom templates.
            workflowSettingsBuilder.withTemplatingEngineName("mustache");
        } else {
            workflowSettingsBuilder.withTemplatingEngineName(templatingEngineName);
        }

        // at this point, all "additionalProperties" are set, and are now immutable per GeneratorSettings instance.
        GeneratorSettings generatorSettings = generatorSettingsBuilder.build();
        WorkflowSettings workflowSettings = workflowSettingsBuilder.build();

        if (workflowSettings.isVerbose()) {
            LOGGER.info("\nVERBOSE MODE: ON. Additional debug options are injected"
                    + "\n - [debugOpenAPI] prints the OpenAPI specification as interpreted by the codegen"
                    + "\n - [debugModels] prints models passed to the template engine"
                    + "\n - [debugOperations] prints operations passed to the template engine"
                    + "\n - [debugSupportingFiles] prints additional data passed to the template engine");

            GlobalSettings.setProperty("debugOpenAPI", "");
            GlobalSettings.setProperty("debugModels", "");
            GlobalSettings.setProperty("debugOperations", "");
            GlobalSettings.setProperty("debugSupportingFiles", "");
            GlobalSettings.setProperty("verbose", "true");
        } else {
            GlobalSettings.setProperty("verbose", "false");
        }

        for (Map.Entry<String, String> entry : workflowSettings.getGlobalProperties().entrySet()) {
            GlobalSettings.setProperty(entry.getKey(), entry.getValue());
        }

        // if caller resets GlobalSettings, we'll need to reset generateAliasAsModel. As noted in this method, this should be moved.
        ModelUtils.setGenerateAliasAsModel(workflowSettings.isGenerateAliasAsModel());

        // TODO: Support custom spec loader implementations (https://github.com/OpenAPITools/openapi-generator/issues/844)
        final List<AuthorizationValue> authorizationValues = AuthParser.parse(this.auth);
        ParseOptions options = new ParseOptions();
        options.setResolve(true);
        SwaggerParseResult result = new OpenAPIParser().readLocation(inputSpec, authorizationValues, options);

        // TODO: Move custom validations to a separate type as part of a "Workflow"
        Set<String> validationMessages = new HashSet<>(result.getMessages());
        OpenAPI specification = result.getOpenAPI();
        // TODO: The line below could be removed when at least one of the issue below has been resolved.
        // https://github.com/swagger-api/swagger-parser/issues/1369
        // https://github.com/swagger-api/swagger-parser/pull/1374
        //ModelUtils.getOpenApiVersion(specification, inputSpec, authorizationValues);

        // NOTE: We will only expose errors+warnings if there are already errors in the spec.
        if (validationMessages.size() > 0) {
            Set<String> warnings = new HashSet<>();
            if (specification != null) {
                List<String> unusedModels = ModelUtils.getUnusedSchemas(specification);
                if (unusedModels != null) {
                    unusedModels.forEach(name -> warnings.add("Unused model: " + name));
                }
            }

            if (workflowSettings.isValidateSpec()) {
                String sb = "There were issues with the specification. The option can be disabled via validateSpec (Maven/Gradle) or --skip-validate-spec (CLI)." +
                        System.lineSeparator();
                SpecValidationException ex = new SpecValidationException(sb);
                ex.setErrors(validationMessages);
                ex.setWarnings(warnings);
                throw ex;
            } else {
                StringBuilder sb = new StringBuilder();
                sb.append(
                        "There were issues with the specification, but validation has been explicitly disabled.");
                sb.append(System.lineSeparator());

                sb.append("Errors: ").append(System.lineSeparator());
                validationMessages.forEach(
                        msg -> sb.append("\t-").append(msg).append(System.lineSeparator()));

                if (!warnings.isEmpty()) {
                    sb.append("Warnings: ").append(System.lineSeparator());
                    warnings.forEach(
                            msg -> sb.append("\t-").append(msg).append(System.lineSeparator()));
                }
                LOGGER.warn(sb.toString());
            }
        }

        return new Context<>(specification, generatorSettings, workflowSettings);
    }

    public ClientOptInput toClientOptInput() {
        Context<?> context = toContext();
        WorkflowSettings workflowSettings = context.getWorkflowSettings();
        GeneratorSettings generatorSettings = context.getGeneratorSettings();

        // We load the config via generatorSettings.getGeneratorName() because this is guaranteed to be set
        // regardless of entrypoint (CLI sets properties on this type, config deserialization sets on generatorSettings).
        CodegenConfig config = CodegenConfigLoader.forName(generatorSettings.getGeneratorName());

        if (isNotEmpty(generatorSettings.getLibrary())) {
            config.setLibrary(generatorSettings.getLibrary());
        }

        // TODO: Work toward CodegenConfig having a "WorkflowSettings" property, or better a "Workflow" object which itself has a "WorkflowSettings" property.
        config.setInputSpec(workflowSettings.getInputSpec());
        config.setOutputDir(workflowSettings.getOutputDir());
        config.setSkipOverwrite(workflowSettings.isSkipOverwrite());
        config.setIgnoreFilePathOverride(workflowSettings.getIgnoreFileOverride());
        config.setRemoveOperationIdPrefix(workflowSettings.isRemoveOperationIdPrefix());
        config.setEnablePostProcessFile(workflowSettings.isEnablePostProcessFile());
        config.setEnableMinimalUpdate(workflowSettings.isEnableMinimalUpdate());
        config.setStrictSpecBehavior(workflowSettings.isStrictSpecBehavior());

        TemplatingEngineAdapter templatingEngine = TemplatingEngineLoader.byIdentifier(workflowSettings.getTemplatingEngineName());
        config.setTemplatingEngine(templatingEngine);

        // TODO: Work toward CodegenConfig having a "GeneratorSettings" property.
        config.instantiationTypes().putAll(generatorSettings.getInstantiationTypes());
        config.typeMapping().putAll(generatorSettings.getTypeMappings());
        config.importMapping().putAll(generatorSettings.getImportMappings());
        config.languageSpecificPrimitives().addAll(generatorSettings.getLanguageSpecificPrimitives());
        config.reservedWordsMappings().putAll(generatorSettings.getReservedWordMappings());
        config.additionalProperties().putAll(generatorSettings.getAdditionalProperties());

        Map<String, String> serverVariables = generatorSettings.getServerVariables();
        if (!serverVariables.isEmpty()) {
            // This is currently experimental due to vagueness in the specification
            LOGGER.warn("user-defined server variable support is experimental.");
            config.serverVariableOverrides().putAll(serverVariables);
        }

        // any other additional properties?
        String templateDir = workflowSettings.getTemplateDir();
        if (templateDir != null) {
            config.additionalProperties().put(CodegenConstants.TEMPLATE_DIR, workflowSettings.getTemplateDir());
        }

        ClientOptInput input = new ClientOptInput()
                .config(config);

        return input.openAPI((OpenAPI)context.getSpecDocument());
    }
}
