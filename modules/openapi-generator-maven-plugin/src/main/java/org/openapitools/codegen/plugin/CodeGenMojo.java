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

package org.openapitools.codegen.plugin;

import static org.openapitools.codegen.config.CodegenConfiguratorUtils.applyAdditionalPropertiesKvp;
import static org.openapitools.codegen.config.CodegenConfiguratorUtils.applyImportMappingsKvp;
import static org.openapitools.codegen.config.CodegenConfiguratorUtils.applyInstantiationTypesKvp;
import static org.openapitools.codegen.config.CodegenConfiguratorUtils.applyLanguageSpecificPrimitivesCsv;
import static org.openapitools.codegen.config.CodegenConfiguratorUtils.applyTypeMappingsKvp;
import static org.openapitools.codegen.config.CodegenConfiguratorUtils.applyReservedWordsMappingsKvp;
import static org.openapitools.codegen.config.CodegenConfiguratorUtils.applyAdditionalPropertiesKvpList;
import static org.openapitools.codegen.config.CodegenConfiguratorUtils.applyImportMappingsKvpList;
import static org.openapitools.codegen.config.CodegenConfiguratorUtils.applyInstantiationTypesKvpList;
import static org.openapitools.codegen.config.CodegenConfiguratorUtils.applyLanguageSpecificPrimitivesCsvList;
import static org.openapitools.codegen.config.CodegenConfiguratorUtils.applyTypeMappingsKvpList;
import static org.openapitools.codegen.config.CodegenConfiguratorUtils.applyReservedWordsMappingsKvpList;
import static org.apache.commons.lang3.StringUtils.isNotEmpty;

import java.io.File;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.google.common.io.ByteSource;
import com.google.common.io.CharSource;
import io.swagger.v3.parser.util.ClasspathHelper;
import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.plugins.annotations.Component;
import org.apache.maven.project.MavenProject;

import org.openapitools.codegen.CliOption;
import org.openapitools.codegen.ClientOptInput;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.openapitools.codegen.config.GlobalSettings;
import org.sonatype.plexus.build.incremental.BuildContext;
import org.sonatype.plexus.build.incremental.DefaultBuildContext;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.base.Charsets;
import com.google.common.hash.Hashing;
import com.google.common.io.Files;

/**
 * Goal which generates client/server code from a OpenAPI json/yaml definition.
 */
@Mojo(name = "generate", defaultPhase = LifecyclePhase.GENERATE_SOURCES)
public class CodeGenMojo extends AbstractMojo {

    private static final Logger LOGGER = LoggerFactory.getLogger(CodeGenMojo.class);

    /**
     * The build context is only avail when running from within eclipse.
     * It is used to update the eclipse-m2e-layer when the plugin is executed inside the IDE.
     */
    @Component
    private BuildContext buildContext = new DefaultBuildContext();

    @Parameter(name = "verbose", required = false, defaultValue = "false")
    private boolean verbose;

    /**
     * Client language to generate.
     */
    @Parameter(name = "language")
    private String language;


    /**
     * The name of the generator to use.
     */
    @Parameter(name = "generatorName")
    private String generatorName;

    /**
     * Location of the output directory.
     */
    @Parameter(name = "output", property = "openapi.generator.maven.plugin.output",
            defaultValue = "${project.build.directory}/generated-sources/openapi")
    private File output;

    /**
     * Location of the OpenAPI spec, as URL or file.
     */
    @Parameter(name = "inputSpec", property = "openapi.generator.maven.plugin.inputSpec", required = true)
    private String inputSpec;

    /**
     * Git user ID, e.g. swagger-api.
     */
    @Parameter(name = "gitUserId", property = "openapi.generator.maven.plugin.gitUserId", required = false)
    private String gitUserId;

    /**
     * Git repo ID, e.g. openapi-generator.
     */
    @Parameter(name = "gitRepoId", property = "openapi.generator.maven.plugin.gitRepoId", required = false)
    private String gitRepoId;

    /**
     * Folder containing the template files.
     */
    @Parameter(name = "templateDirectory", property = "openapi.generator.maven.plugin.templateDirectory")
    private File templateDirectory;

    /**
     * The name of templating engine to use, "mustache" (default) or "handlebars" (beta)
     */
    @Parameter(name = "engine", defaultValue = "mustache", property="openapi.generator.maven.plugin.engine")
    private String engine;

    /**
     * Adds authorization headers when fetching the swagger definitions remotely. " Pass in a
     * URL-encoded string of name:header with a comma separating multiple values
     */
    @Parameter(name = "auth", property = "openapi.generator.maven.plugin.auth")
    private String auth;

    /**
     * Path to separate json configuration file.
     */
    @Parameter(name = "configurationFile", property = "openapi.generator.maven.plugin.configurationFile", required = false)
    private String configurationFile;

    /**
     * Specifies if the existing files should be overwritten during the generation.
     */
    @Parameter(name = "skipOverwrite", property = "openapi.generator.maven.plugin.skipOverwrite", required = false)
    private Boolean skipOverwrite;

    /**
     * The package to use for generated api objects/classes
     */
    @Parameter(name = "apiPackage", property = "openapi.generator.maven.plugin.apiPackage")
    private String apiPackage;

    /**
     * The package to use for generated model objects/classes
     */
    @Parameter(name = "modelPackage", property = "openapi.generator.maven.plugin.modelPackage")
    private String modelPackage;

    /**
     * The package to use for the generated invoker objects
     */
    @Parameter(name = "invokerPackage", property = "openapi.generator.maven.plugin.invokerPackage")
    private String invokerPackage;

    /**
     * The default package to use for the generated objects
     */
    @Parameter(name = "packageName", property = "openapi.generator.maven.plugin.packageName")
    private String packageName;

    /**
     * groupId in generated pom.xml
     */
    @Parameter(name = "groupId", property = "openapi.generator.maven.plugin.groupId")
    private String groupId;

    /**
     * artifactId in generated pom.xml
     */
    @Parameter(name = "artifactId", property = "openapi.generator.maven.plugin.artifactId")
    private String artifactId;

    /**
     * artifact version in generated pom.xml
     */
    @Parameter(name = "artifactVersion", property = "openapi.generator.maven.plugin.artifactVersion")
    private String artifactVersion;

    /**
     * Sets the library
     */
    @Parameter(name = "library", property = "openapi.generator.maven.plugin.library", required = false)
    private String library;

    /**
     * Sets the prefix for model enums and classes
     */
    @Parameter(name = "modelNamePrefix", property = "openapi.generator.maven.plugin.modelNamePrefix", required = false)
    private String modelNamePrefix;

    /**
     * Sets the suffix for model enums and classes
     */
    @Parameter(name = "modelNameSuffix", property = "openapi.generator.maven.plugin.modelNameSuffix", required = false)
    private String modelNameSuffix;

    /**
     * Sets an optional ignoreFileOverride path
     */
    @Parameter(name = "ignoreFileOverride", property = "openapi.generator.maven.plugin.ignoreFileOverride", required = false)
    private String ignoreFileOverride;

    /**
     * To remove operationId prefix (e.g. user_getName => getName)
     */
    @Parameter(name = "removeOperationIdPrefix", property = "openapi.generator.maven.plugin.removeOperationIdPrefix", required = false)
    private Boolean removeOperationIdPrefix;

    /**
     * To write all log messages (not just errors) to STDOUT
     */
    @Parameter(name = "logToStderr", property = "openapi.generator.maven.plugin.logToStderr", required = false)
    private Boolean logToStderr;

    /**
     * To file post-processing hook
     */
    @Parameter(name = "enablePostProcessFile", property = "openapi.generator.maven.plugin.enablePostProcessFile", required = false)
    private Boolean enablePostProcessFile;

    /**
     * To skip spec validation
     */
    @Parameter(name = "skipValidateSpec", property = "openapi.generator.maven.plugin.skipValidateSpec", required = false)
    private Boolean skipValidateSpec;

    /**
     * To treat a document strictly against the spec.
     */
    @Parameter(name = "strictSpec", property = "openapi.generator.maven.plugin.strictSpec", required = false)
    private Boolean strictSpec;

    /**
     * To generate alias (array, map) as model
     */
    @Parameter(name = "generateAliasAsModel", property = "openapi.generator.maven.plugin.generateAliasAsModel", required = false)
    private Boolean generateAliasAsModel;

    /**
     * A map of language-specific parameters as passed with the -c option to the command line
     */
    @Parameter(name = "configOptions")
    private Map<?, ?> configOptions;

    /**
     * A map of types and the types they should be instantiated as
     */
    @Parameter(name = "instantiationTypes", property = "openapi.generator.maven.plugin.instantiationTypes")
    private List<String> instantiationTypes;

    /**
     * A map of classes and the import that should be used for that class
     */
    @Parameter(name = "importMappings", property = "openapi.generator.maven.plugin.importMappings")
    private List<String> importMappings;

    /**
     * A map of swagger spec types and the generated code types to use for them
     */
    @Parameter(name = "typeMappings", property = "openapi.generator.maven.plugin.typeMappings")
    private List<String> typeMappings;

    /**
     * A map of additional language specific primitive types
     */
    @Parameter(name = "languageSpecificPrimitives", property = "openapi.generator.maven.plugin.languageSpecificPrimitives")
    private List<String> languageSpecificPrimitives;

    /**
     * A map of additional properties that can be referenced by the mustache templates
     */
    @Parameter(name = "additionalProperties", property = "openapi.generator.maven.plugin.additionalProperties")
    private List<String> additionalProperties;

    /**
     * A map of reserved names and how they should be escaped
     */
    @Parameter(name = "reservedWordsMappings", property = "openapi.generator.maven.plugin.reservedWordMappings")
    private List<String> reservedWordsMappings;

    /**
     * Generate the apis
     */
    @Parameter(name = "generateApis", property = "openapi.generator.maven.plugin.generateApis", required = false)
    private Boolean generateApis = true;

    /**
     * Generate the models
     */
    @Parameter(name = "generateModels", property = "openapi.generator.maven.plugin.generateModels", required = false)
    private Boolean generateModels = true;

    /**
     * A comma separated list of models to generate. All models is the default.
     */
    @Parameter(name = "modelsToGenerate", property = "openapi.generator.maven.plugin.modelsToGenerate", required = false)
    private String modelsToGenerate = "";

    /**
     * Generate the supporting files
     */
    @Parameter(name = "generateSupportingFiles", property = "openapi.generator.maven.plugin.generateSupportingFiles", required = false)
    private Boolean generateSupportingFiles = true;

    /**
     * A comma separated list of models to generate. All models is the default.
     */
    @Parameter(name = "supportingFilesToGenerate", property = "openapi.generator.maven.plugin.supportingFilesToGenerate", required = false)
    private String supportingFilesToGenerate = "";

    /**
     * Generate the model tests
     */
    @Parameter(name = "generateModelTests", property = "openapi.generator.maven.plugin.generateModelTests", required = false)
    private Boolean generateModelTests = true;

    /**
     * Generate the model documentation
     */
    @Parameter(name = "generateModelDocumentation", property = "openapi.generator.maven.plugin.generateModelDocumentation", required = false)
    private Boolean generateModelDocumentation = true;

    /**
     * Generate the api tests
     */
    @Parameter(name = "generateApiTests", property = "openapi.generator.maven.plugin.generateApiTests", required = false)
    private Boolean generateApiTests = true;

    /**
     * Generate the api documentation
     */
    @Parameter(name = "generateApiDocumentation", property = "openapi.generator.maven.plugin.generateApiDocumentation", required = false)
    private Boolean generateApiDocumentation = true;

    /**
     * Generate the api documentation
     */
    @Parameter(name = "withXml", property = "openapi.generator.maven.plugin.withXml", required = false)
    private Boolean withXml = false;

    /**
     * Skip the execution.
     */
    @Parameter(name = "skip", property = "codegen.skip", required = false, defaultValue = "false")
    private Boolean skip;

    /**
     * Skip the execution if the source file is older than the output folder.
     */
    @Parameter(name = "skipIfSpecIsUnchanged", property = "codegen.skipIfSpecIsUnchanged", required = false, defaultValue = "false")
    private Boolean skipIfSpecIsUnchanged;

    /**
     * Add the output directory to the project as a source root, so that the generated java types
     * are compiled and included in the project artifact.
     */
    @Parameter(defaultValue = "true")
    private boolean addCompileSourceRoot = true;

    @Parameter
    protected Map<String, String> environmentVariables = new HashMap<String, String>();

    @Parameter
    protected Map<String, String> originalEnvironmentVariables = new HashMap<String, String>();

    @Parameter(property = "codegen.configHelp")
    private boolean configHelp = false;

    /**
     * The project being built.
     */
    @Parameter(readonly = true, required = true, defaultValue = "${project}")
    private MavenProject project;

    public void setBuildContext(BuildContext buildContext) {
        this.buildContext = buildContext;
    }

    @Override
    public void execute() throws MojoExecutionException {
        File inputSpecFile = new File(inputSpec);
        addCompileSourceRootIfConfigured();

        try {
            if (skip) {
                getLog().info("Code generation is skipped.");
                return;
            }

            if (buildContext != null) {
                if (buildContext.isIncremental()) {
                    if (inputSpec != null) {
                        if (inputSpecFile.exists()) {
                            if (!buildContext.hasDelta(inputSpecFile)) {
                                getLog().info(
                                        "Code generation is skipped in delta-build because source-json was not modified.");
                                return;
                            }
                        }
                    }
                }
            }

            if (skipIfSpecIsUnchanged) {
                if (inputSpecFile.exists()) {
                    File storedInputSpecHashFile = getHashFile(inputSpecFile);
                    if(storedInputSpecHashFile.exists()) {
                        String inputSpecHash = Files.asByteSource(inputSpecFile).hash(Hashing.sha256()).toString();
                        String storedInputSpecHash = Files.asCharSource(storedInputSpecHashFile, Charsets.UTF_8).read();
                        if (inputSpecHash.equals(storedInputSpecHash)) {
                            getLog().info(
                                    "Code generation is skipped because input was unchanged");
                            return;
                        }
                    }
                }
            }

            // attempt to read from config file
            CodegenConfigurator configurator = CodegenConfigurator.fromFile(configurationFile);

            // if a config file wasn't specified or we were unable to read it
            if (configurator == null) {
                configurator = new CodegenConfigurator();
            }

            configurator.setVerbose(verbose);

            if (skipOverwrite != null) {
                configurator.setSkipOverwrite(skipOverwrite);
            }

            if (removeOperationIdPrefix != null) {
                configurator.setRemoveOperationIdPrefix(removeOperationIdPrefix);
            }

            if (isNotEmpty(inputSpec)) {
                configurator.setInputSpec(inputSpec);
            }

            if (isNotEmpty(gitUserId)) {
                configurator.setGitUserId(gitUserId);
            }

            if (isNotEmpty(gitRepoId)) {
                configurator.setGitRepoId(gitRepoId);
            }

            if (isNotEmpty(ignoreFileOverride)) {
                configurator.setIgnoreFileOverride(ignoreFileOverride);
            }

            if (skipValidateSpec != null) {
                configurator.setValidateSpec(!skipValidateSpec);
            }

            if (strictSpec != null) {
                configurator.setStrictSpecBehavior(strictSpec);
            }

            if (logToStderr != null) {
                configurator.setLogToStderr(logToStderr);
            }

            if (enablePostProcessFile != null) {
                configurator.setEnablePostProcessFile(enablePostProcessFile);
            }

            if (generateAliasAsModel  != null) {
                configurator.setGenerateAliasAsModel(generateAliasAsModel);
            }

            // TODO: After 3.0.0 release (maybe for 3.1.0): Fully deprecate lang.
            if (isNotEmpty(generatorName)) {
                configurator.setGeneratorName(generatorName);

                // check if generatorName & language are set together, inform user this needs to be updated to prevent future issues.
                if (isNotEmpty(language)) {
                    LOGGER.warn("The 'language' option is deprecated and was replaced by 'generatorName'. Both can not be set together");
                    throw new MojoExecutionException(
                            "Illegal configuration: 'language' and  'generatorName' can not be set both, remove 'language' from your configuration");
                }
            } else if (isNotEmpty(language)) {
                LOGGER.warn(
                        "The 'language' option is deprecated and may reference language names only in the next major release (4.0). Please use 'generatorName' instead.");
                configurator.setGeneratorName(language);
            } else {
                LOGGER.error("A generator name (generatorName) is required.");
                throw new MojoExecutionException("The generator requires 'generatorName'. Refer to documentation for a list of options.");
            }

            configurator.setOutputDir(output.getAbsolutePath());

            if (isNotEmpty(auth)) {
                configurator.setAuth(auth);
            }

            if (isNotEmpty(apiPackage)) {
                configurator.setApiPackage(apiPackage);
            }

            if (isNotEmpty(modelPackage)) {
                configurator.setModelPackage(modelPackage);
            }

            if (isNotEmpty(invokerPackage)) {
                configurator.setInvokerPackage(invokerPackage);
            }

            if (isNotEmpty(packageName)) {
                configurator.setPackageName(packageName);
            }

            if (isNotEmpty(groupId)) {
                configurator.setGroupId(groupId);
            }

            if (isNotEmpty(artifactId)) {
                configurator.setArtifactId(artifactId);
            }

            if (isNotEmpty(artifactVersion)) {
                configurator.setArtifactVersion(artifactVersion);
            }

            if (isNotEmpty(library)) {
                configurator.setLibrary(library);
            }

            if (isNotEmpty(modelNamePrefix)) {
                configurator.setModelNamePrefix(modelNamePrefix);
            }

            if (isNotEmpty(modelNameSuffix)) {
                configurator.setModelNameSuffix(modelNameSuffix);
            }

            if (null != templateDirectory) {
                configurator.setTemplateDir(templateDirectory.getAbsolutePath());
            }

            if (null != engine) {
                configurator.setTemplatingEngineName(engine);
            }

            // Set generation options
            if (null != generateApis && generateApis) {
                GlobalSettings.setProperty(CodegenConstants.APIS, "");
            } else {
                GlobalSettings.clearProperty(CodegenConstants.APIS);
            }

            if (null != generateModels && generateModels) {
                GlobalSettings.setProperty(CodegenConstants.MODELS, modelsToGenerate);
            } else {
                GlobalSettings.clearProperty(CodegenConstants.MODELS);
            }

            if (null != generateSupportingFiles && generateSupportingFiles) {
                GlobalSettings.setProperty(CodegenConstants.SUPPORTING_FILES, supportingFilesToGenerate);
            } else {
                GlobalSettings.clearProperty(CodegenConstants.SUPPORTING_FILES);
            }

            GlobalSettings.setProperty(CodegenConstants.MODEL_TESTS, generateModelTests.toString());
            GlobalSettings.setProperty(CodegenConstants.MODEL_DOCS, generateModelDocumentation.toString());
            GlobalSettings.setProperty(CodegenConstants.API_TESTS, generateApiTests.toString());
            GlobalSettings.setProperty(CodegenConstants.API_DOCS, generateApiDocumentation.toString());
            GlobalSettings.setProperty(CodegenConstants.WITH_XML, withXml.toString());

            if (configOptions != null) {
                // Retained for backwards-compataibility with configOptions -> instantiation-types
                if (instantiationTypes == null && configOptions.containsKey("instantiation-types")) {
                    applyInstantiationTypesKvp(configOptions.get("instantiation-types").toString(),
                            configurator);
                }

                // Retained for backwards-compataibility with configOptions -> import-mappings
                if (importMappings == null && configOptions.containsKey("import-mappings")) {
                    applyImportMappingsKvp(configOptions.get("import-mappings").toString(),
                            configurator);
                }

                // Retained for backwards-compataibility with configOptions -> type-mappings
                if (typeMappings == null && configOptions.containsKey("type-mappings")) {
                    applyTypeMappingsKvp(configOptions.get("type-mappings").toString(), configurator);
                }

                // Retained for backwards-compataibility with configOptions -> language-specific-primitives
                if (languageSpecificPrimitives == null && configOptions.containsKey("language-specific-primitives")) {
                    applyLanguageSpecificPrimitivesCsv(configOptions
                            .get("language-specific-primitives").toString(), configurator);
                }

                // Retained for backwards-compataibility with configOptions -> additional-properties
                if (additionalProperties == null && configOptions.containsKey("additional-properties")) {
                    applyAdditionalPropertiesKvp(configOptions.get("additional-properties").toString(),
                            configurator);
                }

                // Retained for backwards-compataibility with configOptions -> reserved-words-mappings
                if (reservedWordsMappings == null && configOptions.containsKey("reserved-words-mappings")) {
                    applyReservedWordsMappingsKvp(configOptions.get("reserved-words-mappings")
                            .toString(), configurator);
                }
            }

            // Apply Instantiation Types
            if (instantiationTypes != null && (configOptions == null || !configOptions.containsKey("instantiation-types"))) {
                applyInstantiationTypesKvpList(instantiationTypes, configurator);
            }

            // Apply Import Mappings
            if (importMappings != null && (configOptions == null || !configOptions.containsKey("import-mappings"))) {
                applyImportMappingsKvpList(importMappings, configurator);
            }

            // Apply Type Mappings
            if (typeMappings != null && (configOptions == null || !configOptions.containsKey("type-mappings"))) {
                applyTypeMappingsKvpList(typeMappings, configurator);
            }

            // Apply Language Specific Primitives
            if (languageSpecificPrimitives != null
                    && (configOptions == null || !configOptions.containsKey("language-specific-primitives"))) {
                applyLanguageSpecificPrimitivesCsvList(languageSpecificPrimitives, configurator);
            }

            // Apply Additional Properties
            if (additionalProperties != null && (configOptions == null || !configOptions.containsKey("additional-properties"))) {
                applyAdditionalPropertiesKvpList(additionalProperties, configurator);
            }

            // Apply Reserved Words Mappings
            if (reservedWordsMappings != null && (configOptions == null || !configOptions.containsKey("reserved-words-mappings"))) {
                applyReservedWordsMappingsKvpList(reservedWordsMappings, configurator);
            }

            if (environmentVariables != null) {

                for (String key : environmentVariables.keySet()) {
                    originalEnvironmentVariables.put(key, GlobalSettings.getProperty(key));
                    String value = environmentVariables.get(key);
                    if (value == null) {
                        // don't put null values
                        value = "";
                    }
                    GlobalSettings.setProperty(key, value);
                    configurator.addSystemProperty(key, value);
                }
            }

            final ClientOptInput input = configurator.toClientOptInput();
            final CodegenConfig config = input.getConfig();

            if (configOptions != null) {
                for (CliOption langCliOption : config.cliOptions()) {
                    if (configOptions.containsKey(langCliOption.getOpt())) {
                        input.getConfig().additionalProperties()
                                .put(langCliOption.getOpt(), configOptions.get(langCliOption.getOpt()));
                    }
                }
            }

            if (configHelp) {
                for (CliOption langCliOption : config.cliOptions()) {
                    System.out.println("\t" + langCliOption.getOpt());
                    System.out.println("\t    "
                            + langCliOption.getOptionHelp().replaceAll("\n", "\n\t    "));
                    System.out.println();
                }
                return;
            }
            adjustAdditionalProperties(config);
            new DefaultGenerator().opts(input).generate();

            if (buildContext != null) {
                buildContext.refresh(new File(getCompileSourceRoot()));
            }

            // Store a checksum of the input spec
            File storedInputSpecHashFile = getHashFile(inputSpecFile);
            ByteSource inputSpecByteSource =
                inputSpecFile.exists()
                    ? Files.asByteSource(inputSpecFile)
                    : CharSource.wrap(ClasspathHelper.loadFileFromClasspath(inputSpecFile.toString().replaceAll("\\\\","/")))
                        .asByteSource(Charsets.UTF_8);
            String  inputSpecHash =inputSpecByteSource.hash(Hashing.sha256()).toString();

            if (storedInputSpecHashFile.getParent() != null && !new File(storedInputSpecHashFile.getParent()).exists()) {
                File parent = new File(storedInputSpecHashFile.getParent());
                parent.mkdirs();
            }
            Files.asCharSink(storedInputSpecHashFile, Charsets.UTF_8).write(inputSpecHash);

        } catch (Exception e) {
            // Maven logs exceptions thrown by plugins only if invoked with -e
            // I find it annoying to jump through hoops to get basic diagnostic information,
            // so let's log it in any case:
            if (buildContext != null) {
                buildContext.addError(inputSpecFile, 0, 0, "unexpected error in Open-API generation", e);
            }
            getLog().error(e);
            throw new MojoExecutionException(
                    "Code generation failed. See above for the full exception.");
        }
    }

    private File getHashFile(File inputSpecFile) {
        return new File(output.getPath() + File.separator + ".openapi-generator" + File.separator + inputSpecFile.getName() + ".sha256");
    }

    private String getCompileSourceRoot() {
        final Object sourceFolderObject =
                configOptions == null ? null : configOptions
                        .get(CodegenConstants.SOURCE_FOLDER);
        final String sourceFolder =
                sourceFolderObject == null ? "src/main/java" : sourceFolderObject.toString();

        String sourceJavaFolder = output.toString() + "/" + sourceFolder;
        return sourceJavaFolder;
    }

    private void addCompileSourceRootIfConfigured() {
        if (addCompileSourceRoot) {
            project.addCompileSourceRoot(getCompileSourceRoot());
        }

        // Reset all environment variables to their original value. This prevents unexpected
        // behaviour
        // when running the plugin multiple consecutive times with different configurations.
        for (Map.Entry<String, String> entry : originalEnvironmentVariables.entrySet()) {
            if (entry.getValue() == null) {
                GlobalSettings.clearProperty(entry.getKey());
            } else {
                GlobalSettings.setProperty(entry.getKey(), entry.getValue());
            }
        }
    }
    /**
     * This method enables conversion of true/false strings in
     * config.additionalProperties (configuration/configOptions) to proper booleans.
     * This enables mustache files to handle the properties better.
     *
     * @param config
     */
    private void adjustAdditionalProperties(final CodegenConfig config) {
        Map<String, Object> configAdditionalProperties = config.additionalProperties();
        Set<String> keySet = configAdditionalProperties.keySet();
        for (String key : keySet) {
            Object value = configAdditionalProperties.get(key);
            if (value != null) {
                if (value instanceof String) {
                    String stringValue = (String) value;
                    if (stringValue.equalsIgnoreCase("true")) {
                        configAdditionalProperties.put(key, Boolean.TRUE);
                    } else if (stringValue.equalsIgnoreCase("false")) {
                        configAdditionalProperties.put(key, Boolean.FALSE);
                    }
                }
            } else {
                configAdditionalProperties.put(key, Boolean.FALSE);
            }
        }
    }
}
