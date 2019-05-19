/*
 * Copyright 2019 OpenAPI-Generator Contributors (https://openapi-generator.tech)
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

import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSet;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.Serializable;
import java.nio.file.Paths;
import java.util.*;

/**
 * Represents those settings applied to a generator.
 */
@SuppressWarnings("WeakerAccess")
public final class GeneratorSettings implements Serializable {

    private static final Logger LOGGER = LoggerFactory.getLogger(GeneratorSettings.class);

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

    // TODO: Evaluate need for system properties to be exposed here.
    private ImmutableMap<String, String> systemProperties;
    private ImmutableMap<String, String> instantiationTypes;
    private ImmutableMap<String, String> typeMappings;
    private ImmutableMap<String, Object> additionalProperties;
    private ImmutableMap<String, String> importMappings;
    private ImmutableSet<String> languageSpecificPrimitives;
    private ImmutableMap<String, String> reservedWordMappings;

    private String gitUserId;
    private String gitRepoId;
    private String releaseNote;
    private String httpUserAgent;

    /**
     * Gets the name of the generator to use.
     *
     * @return the generator name
     */
    public String getGeneratorName() {
        return generatorName;
    }

    /**
     * Gets input spec's location, as URL or file
     *
     * @return the input spec
     */
    public String getInputSpec() {
        return inputSpec;
    }

    /**
     * Gets the output dir (where we write generated files). Defaults to the current directory.
     *
     * @return the output dir
     */
    public String getOutputDir() {
        return outputDir;
    }

    /**
     * Configures verbosity of generation. When <code>true</code>, more messages will be printed during generation.
     *
     * @return <code>true</code> if verbose mode, <code>false</code> otherwise.
     */
    public boolean isVerbose() {
        return verbose;
    }

    /**
     * Indicates whether or not the existing files should be overwritten during the generation. This option is more rudimentary than
     * defining patterns in .openapi-generator-ignore or {@link GeneratorSettings#isEnableMinimalUpdate()}.
     *
     * @return <code>true</code> if defaulting to overwriting files, false otherwise.
     * @see <a href="https://openapi-generator.tech/docs/customization#ignore-file-format">Ignore File Format</a>
     */
    public boolean isSkipOverwrite() {
        return skipOverwrite;
    }

    /**
     * Indicates whether or not to remove the prefix of operationId, e.g. <code>config_getId</code> to <code>getId</code>.
     *
     * @return <code>true</code> if the operation id prefix should be removed during generation, <code>false</code> otherwise.
     */
    public boolean isRemoveOperationIdPrefix() {
        return removeOperationIdPrefix;
    }

    /**
     * Indicates whether or not the generator's executor will write all log messages (not just errors) to STDOUT. Useful for
     * piping the JSON output of debug options (e.g. <code>-DdebugOperations</code>) to an external parser directly while testing a generator.
     *
     * @return <code>true</code> if the executor should attempt to write all messages to stderr,
     * false otherwise (which defaults to logging configuration, not necessarily stdout).
     */
    public boolean isLogToStderr() {
        return logToStderr;
    }

    /**
     * Indicates whether or not generation should also validate an input specification.
     * <p>
     * NOTE: Invalid specs may result in a generation error, disabling may cause unexpected results.
     *
     * @return <code>true</code> if spec document validation is enabled, otherwise <code>false</code>. Default: <code>true</code>.
     */
    public boolean isValidateSpec() {
        return validateSpec;
    }

    /**
     * Indicates whether or not file post-processing is enabled for generators which support it. Refer to individual generator documentation for details.
     * <p>
     * In general, once enabled, a generator will evaluate a command stored in <code>LANG_POST_PROCESS_FILE</code>.
     * <p>
     * For example:
     *
     * <code>export SCALA_POST_PROCESS_FILE=/usr/local/bin/scalafmt</code>
     * <p>
     * Here, a Scala generator which supports file post-processing will run scalafmt for each generated file.
     *
     * @return <code>true</code> if file post-processing is enabled, otherwise <code>false</code>.
     */
    public boolean isEnablePostProcessFile() {
        return enablePostProcessFile;
    }

    /**
     * Indicates whether or not the generation should update only those files which have changed.
     *
     * @return <code>true</code> if minimal updates are enabled, otherwise <code>false</code>.
     */
    public boolean isEnableMinimalUpdate() {
        return enableMinimalUpdate;
    }

    /**
     * Indicates whether or not 'MUST' and 'SHALL' wording in the api specification is strictly adhered to.
     * For example, when <code>false</code>, no automatic 'fixes' will be applied to documents which pass validation but don't follow the spec.
     *
     * @return <code>true</code> if the generator should attempt to strictly follow rules in the specification, otherwise <code>false</code>.
     */
    public boolean isStrictSpecBehavior() {
        return strictSpecBehavior;
    }

    /**
     * Gets the directory holding templates used in generation. This option allows users to extend or modify built-in templates, or to write their own.
     *
     * @return the template dir
     */
    public String getTemplateDir() {
        return templateDir;
    }

    /**
     * Gets the name of the templating engine to target. This option allows a user to target an engine which differs from the generator default, or to
     * refer to their own fully qualified type name of a custom template engine adapter.
     *
     * @return the templating engine name
     * @see <a href="https://openapi-generator.tech/docs/templating#custom-engines">Custom Engines</a>
     */
    public String getTemplatingEngineName() {
        return templatingEngineName;
    }

    /**
     * Gets the api package name for generated sources.
     *
     * @return the api package
     */
    public String getApiPackage() {
        return apiPackage;
    }

    /**
     * Gets the model package name for generated sources
     *
     * @return the model package
     */
    public String getModelPackage() {
        return modelPackage;
    }

    /**
     * Gets the invoker package name for generated sources.
     *
     * @return the invoker package
     */
    public String getInvokerPackage() {
        return invokerPackage;
    }

    /**
     * Gets the overall package name for generated sources.
     *
     * @return the package name
     */
    public String getPackageName() {
        return packageName;
    }

    /**
     * Gets a model name prefix for generated models. This name will be prefixed to a model name.
     * <p>
     * This option is often used to circumvent compilation issues where models match keywords.
     * <p>
     * Example:
     * <p>
     * Prefix <code>My</code> applied to <code>Object</code> results in a generated class named <code>MyObject</code>.
     *
     * @return the model name prefix
     */
    public String getModelNamePrefix() {
        return modelNamePrefix;
    }

    /**
     * Gets a model name suffix for generated models. This name will be appended to a model name.
     * <p>
     * This option is often used to circumvent compilation issues where models match keywords.
     * <p>
     * Example:
     * <p>
     * Suffix <code>Gen</code> applied to <code>Object</code> results in a generated class named <code>ObjectGen</code>.
     *
     * @return the model name suffix
     */
    public String getModelNameSuffix() {
        return modelNameSuffix;
    }

    /**
     * Gets the group id for generated sources which support this concept (e.g. Java and pom.xml, Scala and SBT/Gradle/pom).
     *
     * @return the group id
     */
    public String getGroupId() {
        return groupId;
    }

    /**
     * Gets artifact id for generated sources which support this concept (e.g. Java and pom.xml, Scala and SBT/Gradle/pom).
     *
     * @return the artifact id
     */
    public String getArtifactId() {
        return artifactId;
    }

    /**
     * Gets artifact version for generated sources which support this concept (e.g. Java and pom.xml, Scala and SBT/Gradle/pom).
     *
     * @return the artifact version
     */
    public String getArtifactVersion() {
        return artifactVersion;
    }

    /**
     * Gets library (sub-template) for the target generated.
     *
     * @return the library
     */
    public String getLibrary() {
        return library;
    }

    /**
     * Gets the override location for the .openapi-generator-ignore file. Most useful on initial generation.
     *
     * @return the ignore file override
     */
    public String getIgnoreFileOverride() {
        return ignoreFileOverride;
    }

    /**
     * Gets system properties applied to the generator.
     *
     * @return the system properties
     */
    public Map<String, String> getSystemProperties() {
        return systemProperties;
    }

    /**
     * Gets instantiation types mappings. These allow for customizing the defaults provided by a built-in generator.
     * <p>
     * For example, "array" to "ArrayList" applied to the Java generator will cause all array properties to be instantiated as ArrayList.
     * <p>
     * This option differs from {@link GeneratorSettings#getTypeMappings()} in that values provided here are generally used for type construction (what is applied to "new").
     *
     * @return the instantiation types
     */
    public Map<String, String> getInstantiationTypes() {
        return instantiationTypes;
    }

    /**
     * Gets type mappings. These allow for customizing type definitions.
     * <p>
     * For example, "array" to "List" applied to the Java generator will cause all variable assignments for array properties to be of type <code>List</code>.
     * <p>
     * This option differs from {@link GeneratorSettings#getInstantiationTypes()} in that values provided here are variable reference types rather than concrete instantiation types.
     *
     * @return the type mappings
     */
    public Map<String, String> getTypeMappings() {
        return typeMappings;
    }

    /**
     * Gets additional properties which will be passed to template as dynamic properties.
     *
     * @return the additional properties
     */
    public Map<String, Object> getAdditionalProperties() {
        return additionalProperties;
    }

    /**
     * Gets import mappings between a given class and the import that should be used for that class.
     * <p>
     * Use import mappings, for example, when you want to "bring your own models" from another location.
     *
     * @return the import mappings
     * @see <a href="https://openapi-generator.tech/docs/customization#bringing-your-own-models">Bringing your own models</a>
     */
    public Map<String, String> getImportMappings() {
        return importMappings;
    }

    /**
     * Gets language specific primitives. These are in addition to the "base" primitives defined in a generator.
     * <p>
     * In general, a primitive defined here will indicate to the generator:
     * <p>
     * - models with these types don't require an import
     * - model names not included here require imports and likely indicate a model reference
     * <p>
     * There may be generator-specific implementation details which differ slightly.
     *
     * @return the language specific primitives
     */
    public Set<String> getLanguageSpecificPrimitives() {
        return languageSpecificPrimitives;
    }

    /**
     * Gets reserved word mappings. Values defined here define how a reserved word should be escaped.
     * <p>
     * If no mapping is present, the mapping is generally automatically applied to a default with prefixed underscore (<code>_name</code>). Note that
     * some languages don't support identifiers beginning with a prefix, in which case the generator applies a more appropriate prefix.
     *
     * @return the reserved word mappings
     */
    public Map<String, String> getReservedWordMappings() {
        return reservedWordMappings;
    }

    /**
     * Gets git user id. e.g. <strong>openapitools</strong>.
     * <p>
     * Generally used by git_push.sh in generated sources which support it.
     * This value may also be used by templates in maven style references, READMEs, or other documentation.
     *
     * @return the git user id
     */
    public String getGitUserId() {
        return gitUserId;
    }

    /**
     * Gets git repo id. e.g. <strong>openapi-generator</strong>.
     * <p>
     * Generally used by git_push.sh in generated sources which support it.
     * This value may also be used by templates in maven style references, READMEs, or other documentation.
     *
     * @return the git repo id
     */
    public String getGitRepoId() {
        return gitRepoId;
    }

    /**
     * Gets release note for the generated instance.
     * <p>
     * Generally used by git_push.sh in generated sources which support it.
     * This value may also be used by templates in maven style references, READMEs, or other documentation.
     *
     * @return the release note
     */
    public String getReleaseNote() {
        return releaseNote;
    }

    /**
     * Gets the http user agent to be used by client generators which support setting this value.
     * <p>
     * e.g. codegen_csharp_api_client, defaults to 'OpenAPI-Generator/{packageVersion}}/{language}'
     *
     * @return the http user agent
     */
    public String getHttpUserAgent() {
        return httpUserAgent;
    }

    private GeneratorSettings(Builder builder) {
        generatorName = builder.generatorName;
        inputSpec = builder.inputSpec;
        outputDir = builder.outputDir;
        verbose = builder.verbose;
        skipOverwrite = builder.skipOverwrite;
        removeOperationIdPrefix = builder.removeOperationIdPrefix;
        logToStderr = builder.logToStderr;
        validateSpec = builder.validateSpec;
        enablePostProcessFile = builder.enablePostProcessFile;
        enableMinimalUpdate = builder.enableMinimalUpdate;
        strictSpecBehavior = builder.strictSpecBehavior;
        templateDir = builder.templateDir;
        templatingEngineName = builder.templatingEngineName;
        apiPackage = builder.apiPackage;
        modelPackage = builder.modelPackage;
        invokerPackage = builder.invokerPackage;
        packageName = builder.packageName;
        modelNamePrefix = builder.modelNamePrefix;
        modelNameSuffix = builder.modelNameSuffix;
        groupId = builder.groupId;
        artifactId = builder.artifactId;
        artifactVersion = builder.artifactVersion;
        library = builder.library;
        ignoreFileOverride = builder.ignoreFileOverride;
        systemProperties = ImmutableMap.copyOf(builder.systemProperties);
        instantiationTypes = ImmutableMap.copyOf(builder.instantiationTypes);
        typeMappings = ImmutableMap.copyOf(builder.typeMappings);
        additionalProperties = ImmutableMap.copyOf(builder.additionalProperties);
        importMappings = ImmutableMap.copyOf(builder.importMappings);
        languageSpecificPrimitives = ImmutableSet.copyOf(builder.languageSpecificPrimitives);
        reservedWordMappings = ImmutableMap.copyOf(builder.reservedWordMappings);
        gitUserId = builder.gitUserId;
        gitRepoId = builder.gitRepoId;
        releaseNote = builder.releaseNote;
        httpUserAgent = builder.httpUserAgent;
    }

    /**
     * Instantiates a new Generator settings.
     */
    @SuppressWarnings("unused")
    public GeneratorSettings() {
        systemProperties = ImmutableMap.of();
        instantiationTypes = ImmutableMap.of();
        typeMappings = ImmutableMap.of();
        additionalProperties = ImmutableMap.of();
        importMappings = ImmutableMap.of();
        languageSpecificPrimitives = ImmutableSet.of();
        reservedWordMappings = ImmutableMap.of();
    }

    /**
     * New builder builder.
     *
     * @return the builder
     */
    public static Builder newBuilder() {
        return new Builder();
    }

    /**
     * New builder builder.
     *
     * @param copy the copy
     * @return the builder
     */
    public static Builder newBuilder(GeneratorSettings copy) {
        Builder builder = new Builder();
        builder.generatorName = copy.getGeneratorName();
        builder.inputSpec = copy.getInputSpec();
        builder.outputDir = copy.getOutputDir();
        builder.verbose = copy.isVerbose();
        builder.skipOverwrite = copy.isSkipOverwrite();
        builder.removeOperationIdPrefix = copy.isRemoveOperationIdPrefix();
        builder.logToStderr = copy.isLogToStderr();
        builder.validateSpec = copy.isValidateSpec();
        builder.enablePostProcessFile = copy.isEnablePostProcessFile();
        builder.enableMinimalUpdate = copy.isEnableMinimalUpdate();
        builder.strictSpecBehavior = copy.isStrictSpecBehavior();
        builder.templatingEngineName = copy.getTemplatingEngineName();
        builder.apiPackage = copy.getApiPackage();
        builder.modelPackage = copy.getModelPackage();
        builder.invokerPackage = copy.getInvokerPackage();
        builder.packageName = copy.getPackageName();
        builder.modelNamePrefix = copy.getModelNamePrefix();
        builder.modelNameSuffix = copy.getModelNameSuffix();
        builder.groupId = copy.getGroupId();
        builder.artifactId = copy.getArtifactId();
        builder.artifactVersion = copy.getArtifactVersion();
        builder.library = copy.getLibrary();
        builder.ignoreFileOverride = copy.getIgnoreFileOverride();
        builder.systemProperties = copy.getSystemProperties();
        builder.instantiationTypes = copy.getInstantiationTypes();
        builder.typeMappings = copy.getTypeMappings();
        builder.additionalProperties = copy.getAdditionalProperties();
        builder.importMappings = copy.getImportMappings();
        builder.languageSpecificPrimitives = copy.getLanguageSpecificPrimitives();
        builder.reservedWordMappings = copy.getReservedWordMappings();
        builder.gitUserId = copy.getGitUserId();
        builder.gitRepoId = copy.getGitRepoId();
        builder.releaseNote = copy.getReleaseNote();
        builder.httpUserAgent = copy.getHttpUserAgent();

        // force builder "with" methods to invoke side effects
        builder.withTemplateDir(copy.getTemplateDir());

        return builder;
    }

    /**
     * {@code GeneratorSettings} builder static inner class.
     */
    @SuppressWarnings("UnusedReturnValue")
    public static final class Builder {
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
        private Map<String, String> systemProperties;
        private Map<String, String> instantiationTypes;
        private Map<String, String> typeMappings;
        private Map<String, Object> additionalProperties;
        private Map<String, String> importMappings;
        private Set<String> languageSpecificPrimitives;
        private Map<String, String> reservedWordMappings;
        private String gitUserId;
        private String gitRepoId;
        private String releaseNote;
        private String httpUserAgent;

        /**
         * Instantiates a new Builder.
         */
        public Builder() {
            gitUserId = "GIT_USER_ID";
            gitRepoId = "GIT_REPO_ID";
            releaseNote = "Minor update";

            validateSpec = true;
            strictSpecBehavior = true;
            outputDir = ".";

            systemProperties = new HashMap<>();
            instantiationTypes = new HashMap<>();
            typeMappings = new HashMap<>();
            additionalProperties = new HashMap<>();
            importMappings = new HashMap<>();
            languageSpecificPrimitives = new HashSet<>();
            reservedWordMappings = new HashMap<>();
        }

        /**
         * Sets the {@code generatorName} and returns a reference to this Builder so that the methods can be chained together.
         *
         * @param generatorName the {@code generatorName} to set
         * @return a reference to this Builder
         */
        public Builder withGeneratorName(String generatorName) {
            this.generatorName = generatorName;
            return this;
        }


        /**
         * Sets the {@code inputSpec} and returns a reference to this Builder so that the methods can be chained together.
         *
         * @param inputSpec the {@code inputSpec} to set
         * @return a reference to this Builder
         */
        public Builder withInputSpec(String inputSpec) {
            this.inputSpec = inputSpec;
            return this;
        }

        /**
         * Sets the {@code outputDir} and returns a reference to this Builder so that the methods can be chained together.
         *
         * @param outputDir the {@code outputDir} to set
         * @return a reference to this Builder
         */
        public Builder withOutputDir(String outputDir) {
            this.outputDir = Paths.get(outputDir).toAbsolutePath().toString();
            return this;
        }

        /**
         * Sets the {@code verbose} and returns a reference to this Builder so that the methods can be chained together.
         *
         * @param verbose the {@code verbose} to set
         * @return a reference to this Builder
         */
        public Builder withVerbose(boolean verbose) {
            this.verbose = verbose;
            return this;
        }

        /**
         * Sets the {@code skipOverwrite} and returns a reference to this Builder so that the methods can be chained together.
         *
         * @param skipOverwrite the {@code skipOverwrite} to set
         * @return a reference to this Builder
         */
        public Builder withSkipOverwrite(boolean skipOverwrite) {
            this.skipOverwrite = skipOverwrite;
            return this;
        }

        /**
         * Sets the {@code removeOperationIdPrefix} and returns a reference to this Builder so that the methods can be chained together.
         *
         * @param removeOperationIdPrefix the {@code removeOperationIdPrefix} to set
         * @return a reference to this Builder
         */
        public Builder withRemoveOperationIdPrefix(boolean removeOperationIdPrefix) {
            this.removeOperationIdPrefix = removeOperationIdPrefix;
            return this;
        }

        /**
         * Sets the {@code logToStderr} and returns a reference to this Builder so that the methods can be chained together.
         *
         * @param logToStderr the {@code logToStderr} to set
         * @return a reference to this Builder
         */
        public Builder withLogToStderr(boolean logToStderr) {
            this.logToStderr = logToStderr;
            return this;
        }

        /**
         * Sets the {@code validateSpec} and returns a reference to this Builder so that the methods can be chained together.
         *
         * @param validateSpec the {@code validateSpec} to set
         * @return a reference to this Builder
         */
        public Builder withValidateSpec(boolean validateSpec) {
            this.validateSpec = validateSpec;
            return this;
        }

        /**
         * Sets the {@code enablePostProcessFile} and returns a reference to this Builder so that the methods can be chained together.
         *
         * @param enablePostProcessFile the {@code enablePostProcessFile} to set
         * @return a reference to this Builder
         */
        public Builder withEnablePostProcessFile(boolean enablePostProcessFile) {
            this.enablePostProcessFile = enablePostProcessFile;
            return this;
        }

        /**
         * Sets the {@code enableMinimalUpdate} and returns a reference to this Builder so that the methods can be chained together.
         *
         * @param enableMinimalUpdate the {@code enableMinimalUpdate} to set
         * @return a reference to this Builder
         */
        public Builder withEnableMinimalUpdate(boolean enableMinimalUpdate) {
            this.enableMinimalUpdate = enableMinimalUpdate;
            return this;
        }

        /**
         * Sets the {@code strictSpecBehavior} and returns a reference to this Builder so that the methods can be chained together.
         *
         * @param strictSpecBehavior the {@code strictSpecBehavior} to set
         * @return a reference to this Builder
         */
        public Builder withStrictSpecBehavior(boolean strictSpecBehavior) {
            this.strictSpecBehavior = strictSpecBehavior;
            return this;
        }

        /**
         * Sets the {@code templateDir} and returns a reference to this Builder so that the methods can be chained together.
         *
         * @param templateDir the {@code templateDir} to set
         * @return a reference to this Builder
         */
        public Builder withTemplateDir(String templateDir) {
            if (templateDir == null) {
                this.templateDir = null;
            } else {
                File f = new File(templateDir);

                // check to see if the folder exists
                if (!(f.exists() && f.isDirectory())) {
                    throw new IllegalArgumentException(
                            "Template directory " + templateDir + " does not exist.");
                }

                this.templateDir = f.getAbsolutePath();
            }

            return this;
        }

        /**
         * Sets the {@code templatingEngineName} and returns a reference to this Builder so that the methods can be chained together.
         *
         * @param templatingEngineName the {@code templatingEngineName} to set
         * @return a reference to this Builder
         */
        public Builder withTemplatingEngineName(String templatingEngineName) {
            this.templatingEngineName = templatingEngineName;
            return this;
        }

        /**
         * Sets the {@code apiPackage} and returns a reference to this Builder so that the methods can be chained together.
         *
         * @param apiPackage the {@code apiPackage} to set
         * @return a reference to this Builder
         */
        public Builder withApiPackage(String apiPackage) {
            this.apiPackage = apiPackage;
            return this;
        }

        /**
         * Sets the {@code modelPackage} and returns a reference to this Builder so that the methods can be chained together.
         *
         * @param modelPackage the {@code modelPackage} to set
         * @return a reference to this Builder
         */
        public Builder withModelPackage(String modelPackage) {
            this.modelPackage = modelPackage;
            return this;
        }

        /**
         * Sets the {@code invokerPackage} and returns a reference to this Builder so that the methods can be chained together.
         *
         * @param invokerPackage the {@code invokerPackage} to set
         * @return a reference to this Builder
         */
        public Builder withInvokerPackage(String invokerPackage) {
            this.invokerPackage = invokerPackage;
            return this;
        }

        /**
         * Sets the {@code packageName} and returns a reference to this Builder so that the methods can be chained together.
         *
         * @param packageName the {@code packageName} to set
         * @return a reference to this Builder
         */
        public Builder withPackageName(String packageName) {
            this.packageName = packageName;
            return this;
        }

        /**
         * Sets the {@code modelNamePrefix} and returns a reference to this Builder so that the methods can be chained together.
         *
         * @param modelNamePrefix the {@code modelNamePrefix} to set
         * @return a reference to this Builder
         */
        public Builder withModelNamePrefix(String modelNamePrefix) {
            this.modelNamePrefix = modelNamePrefix;
            return this;
        }

        /**
         * Sets the {@code modelNameSuffix} and returns a reference to this Builder so that the methods can be chained together.
         *
         * @param modelNameSuffix the {@code modelNameSuffix} to set
         * @return a reference to this Builder
         */
        public Builder withModelNameSuffix(String modelNameSuffix) {
            this.modelNameSuffix = modelNameSuffix;
            return this;
        }

        /**
         * Sets the {@code groupId} and returns a reference to this Builder so that the methods can be chained together.
         *
         * @param groupId the {@code groupId} to set
         * @return a reference to this Builder
         */
        public Builder withGroupId(String groupId) {
            this.groupId = groupId;
            return this;
        }

        /**
         * Sets the {@code artifactId} and returns a reference to this Builder so that the methods can be chained together.
         *
         * @param artifactId the {@code artifactId} to set
         * @return a reference to this Builder
         */
        public Builder withArtifactId(String artifactId) {
            this.artifactId = artifactId;
            return this;
        }

        /**
         * Sets the {@code artifactVersion} and returns a reference to this Builder so that the methods can be chained together.
         *
         * @param artifactVersion the {@code artifactVersion} to set
         * @return a reference to this Builder
         */
        public Builder withArtifactVersion(String artifactVersion) {
            this.artifactVersion = artifactVersion;
            return this;
        }

        /**
         * Sets the {@code library} and returns a reference to this Builder so that the methods can be chained together.
         *
         * @param library the {@code library} to set
         * @return a reference to this Builder
         */
        public Builder withLibrary(String library) {
            this.library = library;
            return this;
        }

        /**
         * Sets the {@code ignoreFileOverride} and returns a reference to this Builder so that the methods can be chained together.
         *
         * @param ignoreFileOverride the {@code ignoreFileOverride} to set
         * @return a reference to this Builder
         */
        public Builder withIgnoreFileOverride(String ignoreFileOverride) {
            this.ignoreFileOverride = ignoreFileOverride;
            return this;
        }

        /**
         * Sets the {@code systemProperties} and returns a reference to this Builder so that the methods can be chained together.
         *
         * @param systemProperties the {@code systemProperties} to set
         * @return a reference to this Builder
         */
        public Builder withSystemProperties(Map<String, String> systemProperties) {
            this.systemProperties = systemProperties;
            return this;
        }

        /**
         * Sets the {@code instantiationTypes} and returns a reference to this Builder so that the methods can be chained together.
         *
         * @param instantiationTypes the {@code instantiationTypes} to set
         * @return a reference to this Builder
         */
        public Builder withInstantiationTypes(Map<String, String> instantiationTypes) {
            this.instantiationTypes = instantiationTypes;
            return this;
        }

        /**
         * Sets the {@code typeMappings} and returns a reference to this Builder so that the methods can be chained together.
         *
         * @param typeMappings the {@code typeMappings} to set
         * @return a reference to this Builder
         */
        public Builder withTypeMappings(Map<String, String> typeMappings) {
            this.typeMappings = typeMappings;
            return this;
        }

        /**
         * Sets the {@code additionalProperties} and returns a reference to this Builder so that the methods can be chained together.
         *
         * @param additionalProperties the {@code additionalProperties} to set
         * @return a reference to this Builder
         */
        public Builder withAdditionalProperties(Map<String, Object> additionalProperties) {
            this.additionalProperties = additionalProperties;
            return this;
        }

        /**
         * Sets the {@code importMappings} and returns a reference to this Builder so that the methods can be chained together.
         *
         * @param importMappings the {@code importMappings} to set
         * @return a reference to this Builder
         */
        public Builder withImportMappings(Map<String, String> importMappings) {
            this.importMappings = importMappings;
            return this;
        }

        /**
         * Sets the {@code languageSpecificPrimitives} and returns a reference to this Builder so that the methods can be chained together.
         *
         * @param languageSpecificPrimitives the {@code languageSpecificPrimitives} to set
         * @return a reference to this Builder
         */
        public Builder withLanguageSpecificPrimitives(Set<String> languageSpecificPrimitives) {
            this.languageSpecificPrimitives = languageSpecificPrimitives;
            return this;
        }

        /**
         * Sets the {@code reservedWordMappings} and returns a reference to this Builder so that the methods can be chained together.
         *
         * @param reservedWordMappings the {@code reservedWordMappings} to set
         * @return a reference to this Builder
         */
        public Builder withReservedWordMappings(Map<String, String> reservedWordMappings) {
            this.reservedWordMappings = reservedWordMappings;
            return this;
        }

        /**
         * Sets the {@code gitUserId} and returns a reference to this Builder so that the methods can be chained together.
         *
         * @param gitUserId the {@code gitUserId} to set
         * @return a reference to this Builder
         */
        public Builder withGitUserId(String gitUserId) {
            this.gitUserId = gitUserId;
            return this;
        }

        /**
         * Sets the {@code gitRepoId} and returns a reference to this Builder so that the methods can be chained together.
         *
         * @param gitRepoId the {@code gitRepoId} to set
         * @return a reference to this Builder
         */
        public Builder withGitRepoId(String gitRepoId) {
            this.gitRepoId = gitRepoId;
            return this;
        }

        /**
         * Sets the {@code releaseNote} and returns a reference to this Builder so that the methods can be chained together.
         *
         * @param releaseNote the {@code releaseNote} to set
         * @return a reference to this Builder
         */
        public Builder withReleaseNote(String releaseNote) {
            this.releaseNote = releaseNote;
            return this;
        }

        /**
         * Sets the {@code httpUserAgent} and returns a reference to this Builder so that the methods can be chained together.
         *
         * @param httpUserAgent the {@code httpUserAgent} to set
         * @return a reference to this Builder
         */
        public Builder withHttpUserAgent(String httpUserAgent) {
            this.httpUserAgent = httpUserAgent;
            return this;
        }

        /**
         * Returns a {@code GeneratorSettings} built from the parameters previously set.
         *
         * @return a {@code GeneratorSettings} built with parameters of this {@code GeneratorSettings.Builder}
         */
        public GeneratorSettings build() {
            GeneratorSettings instance = new GeneratorSettings(this);
            //noinspection PlaceholderCountMatchesArgumentCount
            LOGGER.debug("GeneratorSettings#build: %s", instance.toString());
            return instance;
        }
    }

    @Override
    public String toString() {
        return "GeneratorSettings{" +
                "generatorName='" + generatorName + '\'' +
                ", inputSpec='" + inputSpec + '\'' +
                ", outputDir='" + outputDir + '\'' +
                ", verbose=" + verbose +
                ", skipOverwrite=" + skipOverwrite +
                ", removeOperationIdPrefix=" + removeOperationIdPrefix +
                ", logToStderr=" + logToStderr +
                ", validateSpec=" + validateSpec +
                ", enablePostProcessFile=" + enablePostProcessFile +
                ", enableMinimalUpdate=" + enableMinimalUpdate +
                ", strictSpecBehavior=" + strictSpecBehavior +
                ", templateDir='" + templateDir + '\'' +
                ", templatingEngineName='" + templatingEngineName + '\'' +
                ", apiPackage='" + apiPackage + '\'' +
                ", modelPackage='" + modelPackage + '\'' +
                ", invokerPackage='" + invokerPackage + '\'' +
                ", packageName='" + packageName + '\'' +
                ", modelNamePrefix='" + modelNamePrefix + '\'' +
                ", modelNameSuffix='" + modelNameSuffix + '\'' +
                ", groupId='" + groupId + '\'' +
                ", artifactId='" + artifactId + '\'' +
                ", artifactVersion='" + artifactVersion + '\'' +
                ", library='" + library + '\'' +
                ", ignoreFileOverride='" + ignoreFileOverride + '\'' +
                ", systemProperties=" + systemProperties +
                ", instantiationTypes=" + instantiationTypes +
                ", typeMappings=" + typeMappings +
                ", additionalProperties=" + additionalProperties +
                ", importMappings=" + importMappings +
                ", languageSpecificPrimitives=" + languageSpecificPrimitives +
                ", reservedWordMappings=" + reservedWordMappings +
                ", gitUserId='" + gitUserId + '\'' +
                ", gitRepoId='" + gitRepoId + '\'' +
                ", releaseNote='" + releaseNote + '\'' +
                ", httpUserAgent='" + httpUserAgent + '\'' +
                '}';
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof GeneratorSettings)) return false;
        GeneratorSettings that = (GeneratorSettings) o;
        return isVerbose() == that.isVerbose() &&
                isSkipOverwrite() == that.isSkipOverwrite() &&
                isRemoveOperationIdPrefix() == that.isRemoveOperationIdPrefix() &&
                isLogToStderr() == that.isLogToStderr() &&
                isValidateSpec() == that.isValidateSpec() &&
                isEnablePostProcessFile() == that.isEnablePostProcessFile() &&
                isEnableMinimalUpdate() == that.isEnableMinimalUpdate() &&
                isStrictSpecBehavior() == that.isStrictSpecBehavior() &&
                Objects.equals(getGeneratorName(), that.getGeneratorName()) &&
                Objects.equals(getInputSpec(), that.getInputSpec()) &&
                Objects.equals(getOutputDir(), that.getOutputDir()) &&
                Objects.equals(getTemplateDir(), that.getTemplateDir()) &&
                Objects.equals(getTemplatingEngineName(), that.getTemplatingEngineName()) &&
                Objects.equals(getApiPackage(), that.getApiPackage()) &&
                Objects.equals(getModelPackage(), that.getModelPackage()) &&
                Objects.equals(getInvokerPackage(), that.getInvokerPackage()) &&
                Objects.equals(getPackageName(), that.getPackageName()) &&
                Objects.equals(getModelNamePrefix(), that.getModelNamePrefix()) &&
                Objects.equals(getModelNameSuffix(), that.getModelNameSuffix()) &&
                Objects.equals(getGroupId(), that.getGroupId()) &&
                Objects.equals(getArtifactId(), that.getArtifactId()) &&
                Objects.equals(getArtifactVersion(), that.getArtifactVersion()) &&
                Objects.equals(getLibrary(), that.getLibrary()) &&
                Objects.equals(getIgnoreFileOverride(), that.getIgnoreFileOverride()) &&
                Objects.equals(getSystemProperties(), that.getSystemProperties()) &&
                Objects.equals(getInstantiationTypes(), that.getInstantiationTypes()) &&
                Objects.equals(getTypeMappings(), that.getTypeMappings()) &&
                Objects.equals(getAdditionalProperties(), that.getAdditionalProperties()) &&
                Objects.equals(getImportMappings(), that.getImportMappings()) &&
                Objects.equals(getLanguageSpecificPrimitives(), that.getLanguageSpecificPrimitives()) &&
                Objects.equals(getReservedWordMappings(), that.getReservedWordMappings()) &&
                Objects.equals(getGitUserId(), that.getGitUserId()) &&
                Objects.equals(getGitRepoId(), that.getGitRepoId()) &&
                Objects.equals(getReleaseNote(), that.getReleaseNote()) &&
                Objects.equals(getHttpUserAgent(), that.getHttpUserAgent());
    }

    @Override
    public int hashCode() {
        return Objects.hash(
                getGeneratorName(),
                getInputSpec(),
                getOutputDir(),
                isVerbose(),
                isSkipOverwrite(),
                isRemoveOperationIdPrefix(),
                isLogToStderr(),
                isValidateSpec(),
                isEnablePostProcessFile(),
                isEnableMinimalUpdate(),
                isStrictSpecBehavior(),
                getTemplateDir(),
                getTemplatingEngineName(),
                getApiPackage(),
                getModelPackage(),
                getInvokerPackage(),
                getPackageName(),
                getModelNamePrefix(),
                getModelNameSuffix(),
                getGroupId(),
                getArtifactId(),
                getArtifactVersion(),
                getLibrary(),
                getIgnoreFileOverride(),
                getSystemProperties(),
                getInstantiationTypes(),
                getTypeMappings(),
                getAdditionalProperties(),
                getImportMappings(),
                getLanguageSpecificPrimitives(),
                getReservedWordMappings(),
                getGitUserId(),
                getGitRepoId(),
                getReleaseNote(),
                getHttpUserAgent()
        );
    }
}
