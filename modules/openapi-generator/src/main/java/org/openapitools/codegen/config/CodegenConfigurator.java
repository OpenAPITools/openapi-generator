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

import com.fasterxml.jackson.annotation.JsonAnyGetter;
import com.fasterxml.jackson.annotation.JsonAnySetter;
import com.fasterxml.jackson.databind.ObjectMapper;
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
import org.openapitools.codegen.languages.*;
import org.openapitools.codegen.templating.HandlebarsEngineAdapter;
import org.openapitools.codegen.templating.MustacheEngineAdapter;
import org.openapitools.codegen.utils.ModelUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.IOException;
import java.io.Serializable;
import java.nio.file.Paths;
import java.util.*;

import static org.apache.commons.lang3.StringUtils.isEmpty;
import static org.apache.commons.lang3.StringUtils.isNotEmpty;

/**
 * A class that contains all codegen configuration properties a user would want to manipulate. An
 * instance could be created by deserializing a JSON file or being populated from CLI or Maven
 * plugin parameters. It also has a convenience method for creating a ClientOptInput class which is
 * THE object DefaultGenerator.java needs to generate code.
 */
public class CodegenConfigurator implements Serializable {

    public static final Logger LOGGER = LoggerFactory.getLogger(CodegenConfigurator.class);

    private String generatorName;
    private String inputSpec;
    private String outputDir;
    private boolean verbose;
    private boolean skipOverwrite;
    private boolean removeOperationIdPrefix;
    private boolean logToStderr;
    private boolean validateSpec;
    private boolean enablePostProcessFile;
    private boolean enableMinimalUpdate;
    private boolean strictSpecBehavior;
    private String templateDir;
    private String templatingEngineName;
    private String auth;
    private String apiPackage;
    private String modelPackage;
    private String invokerPackage;
    private String packageName;
    private String modelNamePrefix;
    private String modelNameSuffix;
    private String groupId;
    private String artifactId;
    private String artifactVersion;
    private String library;
    private String ignoreFileOverride;
    private Map<String, String> systemProperties = new HashMap<String, String>();
    private Map<String, String> instantiationTypes = new HashMap<String, String>();
    private Map<String, String> typeMappings = new HashMap<String, String>();
    private Map<String, Object> additionalProperties = new HashMap<String, Object>();
    private Map<String, String> importMappings = new HashMap<String, String>();
    private Set<String> languageSpecificPrimitives = new HashSet<String>();
    private Map<String, String> reservedWordMappings = new HashMap<String, String>();

    private String gitUserId = "GIT_USER_ID";
    private String gitRepoId = "GIT_REPO_ID";
    private String releaseNote = "Minor update";
    private String httpUserAgent;

    private final Map<String, Object> dynamicProperties = new HashMap<String, Object>();
    //the map that holds the JsonAnySetter/JsonAnyGetter values

    public CodegenConfigurator() {
        this.validateSpec = true;
        this.strictSpecBehavior = true;
        this.setOutputDir(".");
    }

    /**
     * Set the "language". This has drifted away from language-only to include framework and
     * hyphenated generator types as well as language.
     * <p>
     * NOTE: This will eventually become language only again. It is deprecated in its current
     * state.
     * </p>
     *
     * @param lang The generator name. Previously, language name only.
     * @return The fluent instance of {@link CodegenConfigurator}
     * @deprecated Please use {@link #setGeneratorName(String)}, as generators are no longer
     * identified only by language. We may reuse language in the future.
     */
    @Deprecated public CodegenConfigurator setLang(String lang) {
        this.setGeneratorName(lang);
        return this;
    }

    /**
     * Sets the name of the target generator.
     *
     * The generator's name is used to uniquely identify the generator as a mechanism to lookup the
     * desired implementation at runtime.
     *
     * @param generatorName The name of the generator.
     * @return The fluent instance of {@link CodegenConfigurator}
     */
    public CodegenConfigurator setGeneratorName(final String generatorName) {
        this.generatorName = generatorName;
        return this;
    }

    public CodegenConfigurator setInputSpec(String inputSpec) {
        this.inputSpec = inputSpec;
        return this;
    }

    public String getInputSpec() {
        return inputSpec;
    }

    public String getOutputDir() {
        return outputDir;
    }

    public CodegenConfigurator setOutputDir(String outputDir) {
        this.outputDir = toAbsolutePathStr(outputDir);
        return this;
    }

    public String getModelPackage() {
        return modelPackage;
    }

    public CodegenConfigurator setModelPackage(String modelPackage) {
        this.modelPackage = modelPackage;
        return this;
    }

    public String getModelNamePrefix() {
        return modelNamePrefix;
    }

    public CodegenConfigurator setModelNamePrefix(String prefix) {
        this.modelNamePrefix = prefix;
        return this;
    }

    public boolean getRemoveOperationIdPrefix() {
        return removeOperationIdPrefix;
    }

    public CodegenConfigurator setRemoveOperationIdPrefix(boolean removeOperationIdPrefix) {
        this.removeOperationIdPrefix = removeOperationIdPrefix;
        return this;
    }

    public boolean getEnablePostProcessFile() {
        return enablePostProcessFile;
    }

    public CodegenConfigurator setEnablePostProcessFile(boolean enablePostProcessFile) {
        this.enablePostProcessFile = enablePostProcessFile;
        return this;
    }

    public boolean getLogToStderr() {
        return logToStderr;
    }

    public CodegenConfigurator setLogToStderr(boolean logToStderrte) {
        this.logToStderr = logToStderr;
        return this;
    }

    public boolean getEnableMinimalUpdate() {
        return enableMinimalUpdate;
    }

    public CodegenConfigurator setEnableMinimalUpdate(boolean enableMinimalUpdate) {
        this.enableMinimalUpdate = enableMinimalUpdate;
        return this;
    }

    public boolean isGenerateAliasAsModel() {
        return ModelUtils.isGenerateAliasAsModel();
    }

    public CodegenConfigurator setGenerateAliasAsModel(boolean generateAliasAsModel) {
        ModelUtils.setGenerateAliasAsModel(generateAliasAsModel);
        return this;
    }

    public String getModelNameSuffix() {
        return modelNameSuffix;
    }

    public CodegenConfigurator setModelNameSuffix(String suffix) {
        this.modelNameSuffix = suffix;
        return this;
    }

    public boolean isStrictSpecBehavior() {
        return strictSpecBehavior;
    }

    public CodegenConfigurator setStrictSpecBehavior(boolean strictSpecBehavior) {
        this.strictSpecBehavior = strictSpecBehavior;
        return this;
    }

    public boolean isVerbose() {
        return verbose;
    }

    public CodegenConfigurator setVerbose(boolean verbose) {
        this.verbose = verbose;
        return this;
    }

    public boolean isValidateSpec() {
        return validateSpec;
    }

    public CodegenConfigurator setValidateSpec(final boolean validateSpec) {
        this.validateSpec = validateSpec;
        return this;
    }

    public boolean isSkipOverwrite() {
        return skipOverwrite;
    }

    public CodegenConfigurator setSkipOverwrite(boolean skipOverwrite) {
        this.skipOverwrite = skipOverwrite;
        return this;
    }


    /**
     * Gets the "language". This has drifted away from language-only to include framework and
     * hyphenated generator types as well as language.
     * <p>
     * NOTE: This will eventually become language only again. It is deprecated in its current
     * state.
     * </p>
     *
     * @return A string which defines the generator.
     * @deprecated Please use {@link #getGeneratorName()}, as generators are no longer identified
     * only by language. We may reuse language in the future.
     */
    @Deprecated public String getLang() {
        return getGeneratorName();
    }

    public String getGeneratorName() {
        return generatorName;
    }

    public String getTemplateDir() {
        return templateDir;
    }

    public CodegenConfigurator setTemplateDir(String templateDir) {
        File f = new File(templateDir);

        // check to see if the folder exists
        if (!(f.exists() && f.isDirectory())) {
            throw new IllegalArgumentException(
                    "Template directory " + templateDir + " does not exist.");
        }

        this.templateDir = f.getAbsolutePath();
        return this;
    }

    public String getAuth() {
        return auth;
    }

    public CodegenConfigurator setAuth(String auth) {
        this.auth = auth;
        return this;
    }

    public String getApiPackage() {
        return apiPackage;
    }

    public CodegenConfigurator setApiPackage(String apiPackage) {
        this.apiPackage = apiPackage;
        return this;
    }

    public String getInvokerPackage() {
        return invokerPackage;
    }

    public CodegenConfigurator setInvokerPackage(String invokerPackage) {
        this.invokerPackage = invokerPackage;
        return this;
    }

    public String getPackageName() {
        return packageName;
    }

    public CodegenConfigurator setPackageName(String packageName) {
        this.packageName = packageName;
        return this;
    }

    public String getGroupId() {
        return groupId;
    }

    public CodegenConfigurator setGroupId(String groupId) {
        this.groupId = groupId;
        return this;
    }

    public String getArtifactId() {
        return artifactId;
    }

    public CodegenConfigurator setArtifactId(String artifactId) {
        this.artifactId = artifactId;
        return this;
    }

    public String getArtifactVersion() {
        return artifactVersion;
    }

    public CodegenConfigurator setArtifactVersion(String artifactVersion) {
        this.artifactVersion = artifactVersion;
        return this;
    }

    public Map<String, String> getSystemProperties() {
        return systemProperties;
    }

    public CodegenConfigurator setSystemProperties(Map<String, String> systemProperties) {
        this.systemProperties = systemProperties;
        return this;
    }

    public CodegenConfigurator addSystemProperty(String key, String value) {
        this.systemProperties.put(key, value);
        return this;
    }

    public Map<String, String> getInstantiationTypes() {
        return instantiationTypes;
    }

    public CodegenConfigurator setInstantiationTypes(Map<String, String> instantiationTypes) {
        this.instantiationTypes = instantiationTypes;
        return this;
    }

    public CodegenConfigurator addInstantiationType(String key, String value) {
        this.instantiationTypes.put(key, value);
        return this;
    }

    public Map<String, String> getTypeMappings() {
        return typeMappings;
    }

    public CodegenConfigurator setTypeMappings(Map<String, String> typeMappings) {
        this.typeMappings = typeMappings;
        return this;
    }

    public CodegenConfigurator addTypeMapping(String key, String value) {
        this.typeMappings.put(key, value);
        return this;
    }

    public Map<String, Object> getAdditionalProperties() {
        return additionalProperties;
    }

    public CodegenConfigurator setAdditionalProperties(Map<String, Object> additionalProperties) {
        this.additionalProperties = additionalProperties;
        return this;
    }

    public CodegenConfigurator addAdditionalProperty(String key, Object value) {
        this.additionalProperties.put(key, value);
        return this;
    }

    public Map<String, String> getImportMappings() {
        return importMappings;
    }

    public CodegenConfigurator setImportMappings(Map<String, String> importMappings) {
        this.importMappings = importMappings;
        return this;
    }

    public CodegenConfigurator addImportMapping(String key, String value) {
        this.importMappings.put(key, value);
        return this;
    }

    public Set<String> getLanguageSpecificPrimitives() {
        return languageSpecificPrimitives;
    }

    public CodegenConfigurator setLanguageSpecificPrimitives(
            Set<String> languageSpecificPrimitives) {
        this.languageSpecificPrimitives = languageSpecificPrimitives;
        return this;
    }

    public CodegenConfigurator addLanguageSpecificPrimitive(String value) {
        this.languageSpecificPrimitives.add(value);
        return this;
    }

    public String getLibrary() {
        return library;
    }

    public CodegenConfigurator setLibrary(String library) {
        this.library = library;
        return this;
    }

    public String getGitUserId() {
        return gitUserId;
    }

    public CodegenConfigurator setGitUserId(String gitUserId) {
        this.gitUserId = gitUserId;
        return this;
    }

    public String getGitRepoId() {
        return gitRepoId;
    }

    public CodegenConfigurator setGitRepoId(String gitRepoId) {
        this.gitRepoId = gitRepoId;
        return this;
    }

    public String getReleaseNote() {
        return releaseNote;
    }

    public CodegenConfigurator setReleaseNote(String releaseNote) {
        this.releaseNote = releaseNote;
        return this;
    }

    public String getHttpUserAgent() {
        return httpUserAgent;
    }

    public CodegenConfigurator setHttpUserAgent(String httpUserAgent) {
        this.httpUserAgent = httpUserAgent;
        return this;
    }

    public Map<String, String> getReservedWordsMappings() {
        return reservedWordMappings;
    }

    public CodegenConfigurator setReservedWordsMappings(Map<String, String> reservedWordsMappings) {
        this.reservedWordMappings = reservedWordsMappings;
        return this;
    }

    public CodegenConfigurator addAdditionalReservedWordMapping(String key, String value) {
        this.reservedWordMappings.put(key, value);
        return this;
    }

    public String getIgnoreFileOverride() {
        return ignoreFileOverride;
    }

    public CodegenConfigurator setIgnoreFileOverride(final String ignoreFileOverride) {
        this.ignoreFileOverride = ignoreFileOverride;
        return this;
    }

    public ClientOptInput toClientOptInput() {

        Validate.notEmpty(generatorName, "language/generatorName must be specified");
        Validate.notEmpty(inputSpec, "input spec must be specified");

        setVerboseFlags();
        setSystemProperties();

        CodegenConfig config = CodegenConfigLoader.forName(generatorName);

        config.setInputSpec(inputSpec);
        config.setOutputDir(outputDir);
        config.setSkipOverwrite(skipOverwrite);
        config.setIgnoreFilePathOverride(ignoreFileOverride);
        config.setRemoveOperationIdPrefix(removeOperationIdPrefix);
        config.setEnablePostProcessFile(enablePostProcessFile);
        config.setEnableMinimalUpdate(enableMinimalUpdate);

        config.instantiationTypes().putAll(instantiationTypes);
        config.typeMapping().putAll(typeMappings);
        config.importMapping().putAll(importMappings);
        config.languageSpecificPrimitives().addAll(languageSpecificPrimitives);
        config.reservedWordsMappings().putAll(reservedWordMappings);

        config.setStrictSpecBehavior(isStrictSpecBehavior());

        checkAndSetAdditionalProperty(apiPackage, CodegenConstants.API_PACKAGE);
        checkAndSetAdditionalProperty(modelPackage, CodegenConstants.MODEL_PACKAGE);
        checkAndSetAdditionalProperty(invokerPackage, CodegenConstants.INVOKER_PACKAGE);
        checkAndSetAdditionalProperty(packageName, CodegenConstants.PACKAGE_NAME);
        checkAndSetAdditionalProperty(groupId, CodegenConstants.GROUP_ID);
        checkAndSetAdditionalProperty(artifactId, CodegenConstants.ARTIFACT_ID);
        checkAndSetAdditionalProperty(artifactVersion, CodegenConstants.ARTIFACT_VERSION);
        checkAndSetAdditionalProperty(templateDir, toAbsolutePathStr(templateDir),
                CodegenConstants.TEMPLATE_DIR);
        checkAndSetAdditionalProperty(templatingEngineName, CodegenConstants.TEMPLATING_ENGINE);
        checkAndSetAdditionalProperty(modelNamePrefix, CodegenConstants.MODEL_NAME_PREFIX);
        checkAndSetAdditionalProperty(modelNameSuffix, CodegenConstants.MODEL_NAME_SUFFIX);
        checkAndSetAdditionalProperty(gitUserId, CodegenConstants.GIT_USER_ID);
        checkAndSetAdditionalProperty(gitRepoId, CodegenConstants.GIT_REPO_ID);
        checkAndSetAdditionalProperty(releaseNote, CodegenConstants.RELEASE_NOTE);
        checkAndSetAdditionalProperty(httpUserAgent, CodegenConstants.HTTP_USER_AGENT);

        handleDynamicProperties(config);

        if (isNotEmpty(library)) {
            config.setLibrary(library);
        }

        String templatingEngineId;
        if (isEmpty(templatingEngineName)) {
            // Built-in templates are mustache, but allow users to use a simplified handlebars engine for their custom templates.
            templatingEngineId = "mustache";
        }
        else { templatingEngineId = templatingEngineName; }

        TemplatingEngineAdapter templatingEngine = TemplatingEngineLoader.byIdentifier(templatingEngineId);
        config.setTemplatingEngine(templatingEngine);

        config.additionalProperties().putAll(additionalProperties);

        ClientOptInput input = new ClientOptInput().config(config);

        final List<AuthorizationValue> authorizationValues = AuthParser.parse(auth);
        ParseOptions options = new ParseOptions();
        options.setResolve(true);
        SwaggerParseResult result = new OpenAPIParser().readLocation(inputSpec, authorizationValues, options);

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

            if (this.isValidateSpec()) {
                StringBuilder sb = new StringBuilder();
                sb.append(
                        "There were issues with the specification. The option can be disabled via validateSpec (Maven/Gradle) or --skip-validate-spec (CLI).");
                sb.append(System.lineSeparator());
                SpecValidationException ex = new SpecValidationException(sb.toString());
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

        input.opts(new ClientOpts()).openAPI(specification);

        return input;
    }

    @JsonAnySetter public CodegenConfigurator addDynamicProperty(String name, Object value) {
        dynamicProperties.put(name, value);
        return this;
    }

    @JsonAnyGetter public Map<String, Object> getDynamicProperties() {
        return dynamicProperties;
    }

    private void handleDynamicProperties(CodegenConfig codegenConfig) {
        for (CliOption langCliOption : codegenConfig.cliOptions()) {
            String opt = langCliOption.getOpt();
            if (dynamicProperties.containsKey(opt)) {
                codegenConfig.additionalProperties().put(opt, dynamicProperties.get(opt));
            } else if (systemProperties.containsKey(opt)) {
                codegenConfig.additionalProperties().put(opt, systemProperties.get(opt));
            }
        }
    }

    private void setVerboseFlags() {
        if (!verbose) {
            return;
        }
        LOGGER.info("\nVERBOSE MODE: ON. Additional debug options are injected"
                + "\n - [debugOpenAPI] prints the OpenAPI specification as interpreted by the codegen"
                + "\n - [debugModels] prints models passed to the template engine"
                + "\n - [debugOperations] prints operations passed to the template engine"
                + "\n - [debugSupportingFiles] prints additional data passed to the template engine");

        GeneratorProperties.setProperty("debugOpenAPI", "");
        GeneratorProperties.setProperty("debugModels", "");
        GeneratorProperties.setProperty("debugOperations", "");
        GeneratorProperties.setProperty("debugSupportingFiles", "");
    }

    private void setSystemProperties() {
        for (Map.Entry<String, String> entry : systemProperties.entrySet()) {
            GeneratorProperties.setProperty(entry.getKey(), entry.getValue());
        }
    }

    private static String toAbsolutePathStr(String path) {
        if (isNotEmpty(path)) {
            return Paths.get(path).toAbsolutePath().toString();
        }

        return path;

    }

    private void checkAndSetAdditionalProperty(String property, String propertyKey) {
        checkAndSetAdditionalProperty(property, property, propertyKey);
    }

    private void checkAndSetAdditionalProperty(String property, String valueToSet,
            String propertyKey) {
        if (isNotEmpty(property)) {
            additionalProperties.put(propertyKey, valueToSet);
        }
    }

    public static CodegenConfigurator fromFile(String configFile) {

        if (isNotEmpty(configFile)) {
            ObjectMapper mapper;

            if (FilenameUtils.isExtension(configFile, new String[] {"yml", "yaml"})) {
                mapper = Yaml.mapper();
            } else {
                mapper = Json.mapper();
            }

            try {
                return mapper.readValue(new File(configFile), CodegenConfigurator.class);
            } catch (IOException ex) {
                LOGGER.error("Unable to deserialize config file: " + configFile, ex);
            }
        }
        return null;
    }

    public CodegenConfigurator setTemplatingEngineName(String templatingEngineName) {
        this.templatingEngineName = templatingEngineName;
        return this;
    }
}
