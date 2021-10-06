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

package org.openapitools.codegen.plugin;

import static org.apache.commons.lang3.StringUtils.isNotEmpty;
import static org.openapitools.codegen.config.CodegenConfiguratorUtils.*;

import io.swagger.v3.parser.core.models.AuthorizationValue;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.net.URLConnection;
import java.nio.channels.Channels;
import java.nio.channels.FileChannel;
import java.nio.channels.ReadableByteChannel;
import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.google.common.io.ByteSource;
import com.google.common.io.CharSource;
import io.swagger.v3.parser.util.ClasspathHelper;
import org.apache.commons.lang3.StringUtils;
import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoExecution;
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
import org.openapitools.codegen.auth.AuthParser;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.openapitools.codegen.config.GlobalSettings;
import org.sonatype.plexus.build.incremental.BuildContext;
import org.sonatype.plexus.build.incremental.DefaultBuildContext;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.hash.Hashing;
import com.google.common.io.Files;

/**
 * Goal which generates client/server code from a OpenAPI json/yaml definition.
 */
@SuppressWarnings({"unused", "MismatchedQueryAndUpdateOfCollection"})
@Mojo(name = "generate", defaultPhase = LifecyclePhase.GENERATE_SOURCES, threadSafe = true)
public class CodeGenMojo extends AbstractMojo {

    private final Logger LOGGER = LoggerFactory.getLogger(CodeGenMojo.class);

    /**
     * The build context is only avail when running from within eclipse.
     * It is used to update the eclipse-m2e-layer when the plugin is executed inside the IDE.
     */
    @Component
    private BuildContext buildContext = new DefaultBuildContext();

    @Parameter(name = "verbose", defaultValue = "false")
    private boolean verbose;

    /**
     * The name of the generator to use.
     */
    @Parameter(name = "generatorName", property = "openapi.generator.maven.plugin.generatorName")
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
     * Git host, e.g. gitlab.com.
     */
    @Parameter(name = "gitHost", property = "openapi.generator.maven.plugin.gitHost")
    private String gitHost;

    /**
     * Git user ID, e.g. swagger-api.
     */
    @Parameter(name = "gitUserId", property = "openapi.generator.maven.plugin.gitUserId")
    private String gitUserId;

    /**
     * Git repo ID, e.g. openapi-generator.
     */
    @Parameter(name = "gitRepoId", property = "openapi.generator.maven.plugin.gitRepoId")
    private String gitRepoId;

    /**
     * Folder containing the template files.
     */
    @Parameter(name = "templateDirectory", property = "openapi.generator.maven.plugin.templateDirectory")
    private File templateDirectory;

    /**
     * Resource path containing template files.
     */
    @Parameter(name = "templateResourcePath", property = "openapi.generator.maven.plugin.templateResourcePath")
    private String templateResourcePath;

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
    @Parameter(name = "configurationFile", property = "openapi.generator.maven.plugin.configurationFile")
    private String configurationFile;

    /**
     * Specifies if the existing files should be overwritten during the generation.
     */
    @Parameter(name = "skipOverwrite", property = "openapi.generator.maven.plugin.skipOverwrite")
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
    @Parameter(name = "library", property = "openapi.generator.maven.plugin.library")
    private String library;

    /**
     * Sets the prefix for model enums and classes
     */
    @Parameter(name = "modelNamePrefix", property = "openapi.generator.maven.plugin.modelNamePrefix")
    private String modelNamePrefix;

    /**
     * Sets the suffix for model enums and classes
     */
    @Parameter(name = "modelNameSuffix", property = "openapi.generator.maven.plugin.modelNameSuffix")
    private String modelNameSuffix;

    /**
     * Sets an optional ignoreFileOverride path
     */
    @Parameter(name = "ignoreFileOverride", property = "openapi.generator.maven.plugin.ignoreFileOverride")
    private String ignoreFileOverride;

    /**
     * Sets custom User-Agent header value
     */
    @Parameter(name = "httpUserAgent", property = "openapi.generator.maven.plugin.httpUserAgent")
    private String httpUserAgent;

    /**
     * To remove operationId prefix (e.g. user_getName => getName)
     */
    @Parameter(name = "removeOperationIdPrefix", property = "openapi.generator.maven.plugin.removeOperationIdPrefix")
    private Boolean removeOperationIdPrefix;

    /**
     * To skip examples defined in the operation
     */
    @Parameter(name = "skipOperationExample", property = "openapi.generator.maven.plugin.skipOperationExample")
    private Boolean skipOperationExample;

    /**
     * To write all log messages (not just errors) to STDOUT
     */
    @Parameter(name = "logToStderr", property = "openapi.generator.maven.plugin.logToStderr")
    private Boolean logToStderr;

    /**
     * To file post-processing hook
     */
    @Parameter(name = "enablePostProcessFile", property = "openapi.generator.maven.plugin.enablePostProcessFile")
    private Boolean enablePostProcessFile;

    /**
     * To skip spec validation
     */
    @Parameter(name = "skipValidateSpec", property = "openapi.generator.maven.plugin.skipValidateSpec")
    private Boolean skipValidateSpec;

    /**
     * To treat a document strictly against the spec.
     */
    @Parameter(name = "strictSpec", property = "openapi.generator.maven.plugin.strictSpec")
    private Boolean strictSpec;

    /**
     * To generate alias (array, map) as model
     */
    @Parameter(name = "generateAliasAsModel", property = "openapi.generator.maven.plugin.generateAliasAsModel")
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
     * A map of server variable overrides for specs that support server URL templating
     */
    @Parameter(name = "serverVariableOverrides", property = "openapi.generator.maven.plugin.serverVariableOverrides")
    private List<String> serverVariableOverrides;

    /**
     * A map of reserved names and how they should be escaped
     */
    @Parameter(name = "reservedWordsMappings", property = "openapi.generator.maven.plugin.reservedWordMappings")
    private List<String> reservedWordsMappings;

    /**
     * Generate the apis
     */
    @Parameter(name = "generateApis", property = "openapi.generator.maven.plugin.generateApis")
    private Boolean generateApis = true;

    /**
     * A comma separated list of apis to generate. All apis is the default.
     */
    @Parameter(name = "apisToGenerate", property = "openapi.generator.maven.plugin.apisToGenerate")
    private String apisToGenerate = "";

    /**
     * Generate the models
     */
    @Parameter(name = "generateModels", property = "openapi.generator.maven.plugin.generateModels")
    private Boolean generateModels = true;

    /**
     * A comma separated list of models to generate. All models is the default.
     */
    @Parameter(name = "modelsToGenerate", property = "openapi.generator.maven.plugin.modelsToGenerate")
    private String modelsToGenerate = "";

    /**
     * Generate the supporting files
     */
    @Parameter(name = "generateSupportingFiles", property = "openapi.generator.maven.plugin.generateSupportingFiles")
    private Boolean generateSupportingFiles = true;

    /**
     * A comma separated list of models to generate. All models is the default.
     */
    @Parameter(name = "supportingFilesToGenerate", property = "openapi.generator.maven.plugin.supportingFilesToGenerate")
    private String supportingFilesToGenerate = "";

    /**
     * Generate the model tests
     */
    @Parameter(name = "generateModelTests", property = "openapi.generator.maven.plugin.generateModelTests")
    private Boolean generateModelTests = true;

    /**
     * Generate the model documentation
     */
    @Parameter(name = "generateModelDocumentation", property = "openapi.generator.maven.plugin.generateModelDocumentation")
    private Boolean generateModelDocumentation = true;

    /**
     * Generate the api tests
     */
    @Parameter(name = "generateApiTests", property = "openapi.generator.maven.plugin.generateApiTests")
    private Boolean generateApiTests = true;

    /**
     * Generate the api documentation
     */
    @Parameter(name = "generateApiDocumentation", property = "openapi.generator.maven.plugin.generateApiDocumentation")
    private Boolean generateApiDocumentation = true;

    /**
     * Generate the api documentation
     */
    @Parameter(name = "withXml", property = "openapi.generator.maven.plugin.withXml")
    private Boolean withXml = false;

    /**
     * Skip the execution.
     */
    @Parameter(name = "skip", property = "codegen.skip", defaultValue = "false")
    private Boolean skip;

    /**
     * Skip the execution if the source file is older than the output folder.
     */
    @Parameter(name = "skipIfSpecIsUnchanged", property = "codegen.skipIfSpecIsUnchanged", defaultValue = "false")
    private Boolean skipIfSpecIsUnchanged;

    /**
     * Add the output directory to the project as a source root, so that the generated java types
     * are compiled and included in the project artifact. Mutually exclusive with {@link #addTestCompileSourceRoot}.
     */
    @Parameter(defaultValue = "true", property = "openapi.generator.maven.plugin.addCompileSourceRoot")
    private boolean addCompileSourceRoot = true;

    /**
     * Add the output directory to the project as a test source root, so that the generated java types
     * are compiled only for the test classpath of the project. Mutually exclusive with {@link #addCompileSourceRoot}.
     */
    @Parameter(defaultValue = "false", property = "openapi.generator.maven.plugin.addTestCompileSourceRoot")
    private boolean addTestCompileSourceRoot = false;

    // TODO: Rename to global properties in version 5.1
    @Parameter
    protected Map<String, String> environmentVariables = new HashMap<>();

    @Parameter
    protected Map<String, String> globalProperties = new HashMap<>();

    @Parameter(property = "codegen.configHelp")
    private boolean configHelp = false;

    @Parameter(defaultValue = "${mojoExecution}", readonly = true)
    private MojoExecution mojo;

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
            if (Boolean.TRUE.equals(skip)) {
                getLog().info("Code generation is skipped.");
                return;
            }

            if (buildContext != null && inputSpec != null ) {
                if (buildContext.isIncremental() &&
                        inputSpecFile.exists() &&
                        !buildContext.hasDelta(inputSpecFile)) {
                    getLog().info(
                            "Code generation is skipped in delta-build because source-json was not modified.");
                    return;
                }
            }

            if (Boolean.TRUE.equals(skipIfSpecIsUnchanged)) {
                File storedInputSpecHashFile = getHashFile(inputSpecFile);
                if (storedInputSpecHashFile.exists()) {
                    String inputSpecHash = null;
                    try {
                        inputSpecHash = calculateInputSpecHash(inputSpecFile);
                    } catch (IOException ex) {
                        ex.printStackTrace();
                    }
                    @SuppressWarnings("UnstableApiUsage")
                    String storedInputSpecHash = Files.asCharSource(storedInputSpecHashFile, StandardCharsets.UTF_8).read();
                    if (storedInputSpecHash.equals(inputSpecHash)) {
                        getLog().info(
                                "Code generation is skipped because input was unchanged");
                        return;
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

            if (skipOperationExample != null) {
                configurator.setSkipOperationExample(skipOperationExample);
            }

            if (isNotEmpty(inputSpec)) {
                configurator.setInputSpec(inputSpec);
            }

            if (isNotEmpty(gitHost)) {
                configurator.setGitHost(gitHost);
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

            if (isNotEmpty(httpUserAgent)) {
                configurator.setHttpUserAgent(httpUserAgent);
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

            if (isNotEmpty(generatorName)) {
                configurator.setGeneratorName(generatorName);
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

            if (StringUtils.isNotEmpty(templateResourcePath)) {
                if (null != templateDirectory) {
                    LOGGER.warn("Both templateDirectory and templateResourcePath were configured. templateResourcePath overwrites templateDirectory.");
                }
                configurator.setTemplateDir(templateResourcePath);
            }

            if (null != engine) {
                configurator.setTemplatingEngineName(engine);
            }

            // Set generation options
            if (null != generateApis && generateApis) {
                GlobalSettings.setProperty(CodegenConstants.APIS, apisToGenerate);
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
                // Retained for backwards-compatibility with configOptions -> instantiation-types
                if (instantiationTypes == null && configOptions.containsKey("instantiation-types")) {
                    applyInstantiationTypesKvp(configOptions.get("instantiation-types").toString(),
                            configurator);
                }

                // Retained for backwards-compatibility with configOptions -> import-mappings
                if (importMappings == null && configOptions.containsKey("import-mappings")) {
                    applyImportMappingsKvp(configOptions.get("import-mappings").toString(),
                            configurator);
                }

                // Retained for backwards-compatibility with configOptions -> type-mappings
                if (typeMappings == null && configOptions.containsKey("type-mappings")) {
                    applyTypeMappingsKvp(configOptions.get("type-mappings").toString(), configurator);
                }

                // Retained for backwards-compatibility with configOptions -> language-specific-primitives
                if (languageSpecificPrimitives == null && configOptions.containsKey("language-specific-primitives")) {
                    applyLanguageSpecificPrimitivesCsv(configOptions
                            .get("language-specific-primitives").toString(), configurator);
                }

                // Retained for backwards-compatibility with configOptions -> additional-properties
                if (additionalProperties == null && configOptions.containsKey("additional-properties")) {
                    applyAdditionalPropertiesKvp(configOptions.get("additional-properties").toString(),
                            configurator);
                }

                if (serverVariableOverrides == null && configOptions.containsKey("server-variables")) {
                    applyServerVariablesKvp(configOptions.get("server-variables").toString(), configurator);
                }

                // Retained for backwards-compatibility with configOptions -> reserved-words-mappings
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

            if (serverVariableOverrides != null && (configOptions == null || !configOptions.containsKey("server-variables"))) {
                applyServerVariablesKvpList(serverVariableOverrides, configurator);
            }

            // Apply Reserved Words Mappings
            if (reservedWordsMappings != null && (configOptions == null || !configOptions.containsKey("reserved-words-mappings"))) {
                applyReservedWordsMappingsKvpList(reservedWordsMappings, configurator);
            }

            if (globalProperties == null) {
                globalProperties = new HashMap<>();
            }

            if (environmentVariables != null && environmentVariables.size() > 0) {
                globalProperties.putAll(environmentVariables);
                getLog().warn("environmentVariables is deprecated and will be removed in version 5.1. Use globalProperties instead.");
            }

            for (Map.Entry<String, String> globalPropertiesEntry : globalProperties.entrySet()) {
                String key = globalPropertiesEntry.getKey();
                String value = globalPropertiesEntry.getValue();
                if (value != null) {
                    configurator.addGlobalProperty(key, value);
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
            String inputSpecHash = calculateInputSpecHash(inputSpecFile);

            if (storedInputSpecHashFile.getParent() != null && !new File(storedInputSpecHashFile.getParent()).exists()) {
                File parent = new File(storedInputSpecHashFile.getParent());
                if (!parent.mkdirs()) {
                    throw new RuntimeException("Failed to create the folder " + parent.getAbsolutePath() +
                                               " to store the checksum of the input spec.");
                }
            }
            Files.asCharSink(storedInputSpecHashFile, StandardCharsets.UTF_8).write(inputSpecHash);

        } catch (Exception e) {
            // Maven logs exceptions thrown by plugins only if invoked with -e
            // I find it annoying to jump through hoops to get basic diagnostic information,
            // so let's log it in any case:
            if (buildContext != null) {
                buildContext.addMessage(inputSpecFile, 0, 0, "unexpected error in Open-API generation", BuildContext.SEVERITY_WARNING, e);
            }
            getLog().error(e);
            throw new MojoExecutionException(
                    "Code generation failed. See above for the full exception.");
        }
    }

    /**
     * Calculate openapi specification file hash. If specification is hosted on remote resource it is downloaded first
     *
     * @param inputSpecFile - Openapi specification input file to calculate it's hash.
     *                        Does not taken into account if input spec is hosted on remote resource
     * @return openapi specification file hash
     * @throws IOException
     */
    private String calculateInputSpecHash(File inputSpecFile) throws IOException {

        URL inputSpecRemoteUrl = inputSpecRemoteUrl();

        File inputSpecTempFile = inputSpecFile;

        if (inputSpecRemoteUrl != null) {
            inputSpecTempFile = java.nio.file.Files.createTempFile("openapi-spec", ".tmp").toFile();

            URLConnection conn = inputSpecRemoteUrl.openConnection();
            if (isNotEmpty(auth)) {
                List<AuthorizationValue> authList = AuthParser.parse(auth);
                for (AuthorizationValue a : authList) {
                    conn.setRequestProperty(a.getKeyName(), a.getValue());
                }
            }
            try (ReadableByteChannel readableByteChannel = Channels.newChannel(conn.getInputStream())) {
                FileChannel fileChannel;
                try (FileOutputStream fileOutputStream = new FileOutputStream(inputSpecTempFile)) {
                    fileChannel = fileOutputStream.getChannel();
                    fileChannel.transferFrom(readableByteChannel, 0, Long.MAX_VALUE);
                }
            }
        }

        ByteSource inputSpecByteSource =
                inputSpecTempFile.exists()
                        ? Files.asByteSource(inputSpecTempFile)
                        : CharSource.wrap(ClasspathHelper.loadFileFromClasspath(inputSpecTempFile.toString().replaceAll("\\\\","/")))
                        .asByteSource(StandardCharsets.UTF_8);

        return inputSpecByteSource.hash(Hashing.sha256()).toString();
    }

    /**
     * Try to parse inputSpec setting string into URL
     * @return A valid URL or null if inputSpec is not a valid URL
     */
    private URL inputSpecRemoteUrl(){
        try {
            return new URI(inputSpec).toURL();
        } catch (URISyntaxException | MalformedURLException | IllegalArgumentException e) {
            return null;
        }
    }

    /**
     * Get specification hash file
     * @param inputSpecFile - Openapi specification input file to calculate it's hash.
     *                        Does not taken into account if input spec is hosted on remote resource
     * @return a file with previously calculated hash
     */
    private File getHashFile(File inputSpecFile) {
        String name = inputSpecFile.getName();

        URL url = inputSpecRemoteUrl();
        if (url != null) {
            String[] segments = url.getPath().split("/");
            name = Files.getNameWithoutExtension(segments[segments.length - 1]);
        }

        return new File(output.getPath() + File.separator + ".openapi-generator" + File.separator + name + "-" + mojo.getExecutionId() + ".sha256");
    }

    private String getCompileSourceRoot() {
        final Object sourceFolderObject =
                configOptions == null ? null : configOptions
                        .get(CodegenConstants.SOURCE_FOLDER);
        final String sourceFolder;
        if (sourceFolderObject != null) {
            sourceFolder = sourceFolderObject.toString();
        } else {
            sourceFolder = addTestCompileSourceRoot ? "src/test/java" : "src/main/java";
        }

        return output.toString() + "/" + sourceFolder;
    }

    private void addCompileSourceRootIfConfigured() throws MojoExecutionException {
        if (addCompileSourceRoot) {
            if (addTestCompileSourceRoot) {
                throw new MojoExecutionException("Either 'addCompileSourceRoot' or 'addTestCompileSourceRoot' may be active, not both.");
            }
            project.addCompileSourceRoot(getCompileSourceRoot());
        } else if (addTestCompileSourceRoot) {
            project.addTestCompileSourceRoot(getCompileSourceRoot());
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
