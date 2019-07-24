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

package org.openapitools.codegen.config;

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
    private Map<String, String> systemProperties = new HashMap<>();
    private Map<String, String> instantiationTypes = new HashMap<>();
    private Map<String, String> typeMappings = new HashMap<>();
    private Map<String, Object> additionalProperties = new HashMap<>();
    private Map<String, String> importMappings = new HashMap<>();
    private Set<String> languageSpecificPrimitives = new HashSet<>();
    private Map<String, String> reservedWordMappings = new HashMap<>();
    private String auth;

    public CodegenConfigurator() {

    }

    public static CodegenConfigurator fromFile(String configFile) {

        if (isNotEmpty(configFile)) {
            ObjectMapper mapper;

            if (FilenameUtils.isExtension(configFile, new String[]{"yml", "yaml"})) {
                mapper = Yaml.mapper();
            } else {
                mapper = Json.mapper();
            }

            mapper.registerModule(new GuavaModule());

            try {
                DynamicSettings settings = mapper.readValue(new File(configFile), DynamicSettings.class);
                CodegenConfigurator configurator = new CodegenConfigurator();
                configurator.generatorSettingsBuilder = GeneratorSettings.newBuilder(settings.getGeneratorSettings());
                return configurator;
            } catch (IOException ex) {
                LOGGER.error("Unable to deserialize config file: " + configFile, ex);
            }
        }
        return null;
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

    public CodegenConfigurator addSystemProperty(String key, String value) {
        this.systemProperties.put(key, value);
        workflowSettingsBuilder.withSystemProperty(key, value);
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

    public CodegenConfigurator setApiPackage(String apiPackage) {
        generatorSettingsBuilder.withApiPackage(apiPackage);
        return this;
    }

    public CodegenConfigurator setArtifactId(String artifactId) {
        generatorSettingsBuilder.withArtifactId(artifactId);
        return this;
    }

    public CodegenConfigurator setArtifactVersion(String artifactVersion) {
        generatorSettingsBuilder.withArtifactVersion(artifactVersion);
        return this;
    }

    public CodegenConfigurator setAuth(String auth) {
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
        // TODO: Move to GlobalSettings?
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
        generatorSettingsBuilder.withGitRepoId(gitRepoId);
        return this;
    }

    public CodegenConfigurator setGitUserId(String gitUserId) {
        generatorSettingsBuilder.withGitUserId(gitUserId);
        return this;
    }

    public CodegenConfigurator setGroupId(String groupId) {
        generatorSettingsBuilder.withGroupId(groupId);
        return this;
    }

    public CodegenConfigurator setHttpUserAgent(String httpUserAgent) {
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
        generatorSettingsBuilder.withInvokerPackage(invokerPackage);
        return this;
    }

    public CodegenConfigurator setLanguageSpecificPrimitives(
            Set<String> languageSpecificPrimitives) {
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

    public CodegenConfigurator setModelNamePrefix(String prefix) {
        generatorSettingsBuilder.withModelNamePrefix(prefix);
        return this;
    }

    public CodegenConfigurator setModelNameSuffix(String suffix) {
        generatorSettingsBuilder.withModelNameSuffix(suffix);
        return this;
    }

    public CodegenConfigurator setModelPackage(String modelPackage) {
        generatorSettingsBuilder.withModelPackage(modelPackage);
        return this;
    }

    public CodegenConfigurator setOutputDir(String outputDir) {
        workflowSettingsBuilder.withOutputDir(outputDir);
        return this;
    }

    public CodegenConfigurator setPackageName(String packageName) {
        generatorSettingsBuilder.withPackageName(packageName);
        return this;
    }

    public CodegenConfigurator setReleaseNote(String releaseNote) {
        generatorSettingsBuilder.withReleaseNote(releaseNote);
        return this;
    }

    public CodegenConfigurator setRemoveOperationIdPrefix(boolean removeOperationIdPrefix) {
        workflowSettingsBuilder.withRemoveOperationIdPrefix(removeOperationIdPrefix);
        return this;
    }

    public CodegenConfigurator setReservedWordsMappings(Map<String, String> reservedWordMappings) {
        this.reservedWordMappings = reservedWordMappings;
        generatorSettingsBuilder.withReservedWordMappings(reservedWordMappings);
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

    public CodegenConfigurator setSystemProperties(Map<String, String> systemProperties) {
        this.systemProperties = systemProperties;
        workflowSettingsBuilder.withSystemProperties(systemProperties);
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
        Validate.notEmpty(generatorName, "language/generatorName must be specified");
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
        }

        for (Map.Entry<String, String> entry : workflowSettings.getSystemProperties().entrySet()) {
            GlobalSettings.setProperty(entry.getKey(), entry.getValue());
        }

        // TODO: Support custom spec loader implementations (https://github.com/OpenAPITools/openapi-generator/issues/844)
        final List<AuthorizationValue> authorizationValues = AuthParser.parse(this.auth);
        ParseOptions options = new ParseOptions();
        options.setResolve(true);
        SwaggerParseResult result = new OpenAPIParser().readLocation(inputSpec, authorizationValues, options);

        // TODO: Move custom validations to a separate type as part of a "Workflow"
        Set<String> validationMessages = new HashSet<>(result.getMessages());
        OpenAPI specification = result.getOpenAPI();

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
        CodegenConfig config = CodegenConfigLoader.forName(generatorName);

        Context<?> context = toContext();
        WorkflowSettings workflowSettings = context.getWorkflowSettings();
        GeneratorSettings generatorSettings = context.getGeneratorSettings();

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
