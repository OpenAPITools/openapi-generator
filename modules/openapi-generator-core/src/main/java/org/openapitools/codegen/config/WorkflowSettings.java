/*
 * Copyright 2019 OpenAPI-Generator Contributors (https://openapi-generator.tech)
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

import com.google.common.collect.ImmutableMap;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicLong;

/**
 * Represents those settings applied to a generation workflow.
 */
@SuppressWarnings("WeakerAccess")
public class WorkflowSettings {
    private static final AtomicLong lastWarning = new AtomicLong(0);
    private static final Logger LOGGER = LoggerFactory.getLogger(WorkflowSettings.class);
    public static final String DEFAULT_OUTPUT_DIR = ".";
    public static final boolean DEFAULT_VERBOSE = false;
    public static final boolean DEFAULT_SKIP_OVERWRITE = false;
    public static final boolean DEFAULT_REMOVE_OPERATION_ID_PREFIX = false;
    public static final boolean DEFAULT_LOG_TO_STDERR = false;
    public static final boolean DEFAULT_VALIDATE_SPEC = true;
    public static final boolean DEFAULT_ENABLE_POST_PROCESS_FILE = false;
    public static final boolean DEFAULT_ENABLE_MINIMAL_UPDATE = false;
    public static final boolean DEFAULT_STRICT_SPEC_BEHAVIOR = true;
    public static final boolean DEFAULT_GENERATE_ALIAS_AS_MODEL = false;
    public static final String DEFAULT_TEMPLATING_ENGINE_NAME = "mustache";
    public static final ImmutableMap<String, String> DEFAULT_GLOBAL_PROPERTIES = ImmutableMap.of();

    private String inputSpec;
    private String outputDir = DEFAULT_OUTPUT_DIR;
    private boolean verbose = DEFAULT_VERBOSE;
    private boolean skipOverwrite = DEFAULT_SKIP_OVERWRITE;
    private boolean removeOperationIdPrefix = DEFAULT_REMOVE_OPERATION_ID_PREFIX;
    private boolean logToStderr = DEFAULT_LOG_TO_STDERR;
    private boolean validateSpec = DEFAULT_VALIDATE_SPEC;
    private boolean enablePostProcessFile = DEFAULT_ENABLE_POST_PROCESS_FILE;
    private boolean enableMinimalUpdate = DEFAULT_ENABLE_MINIMAL_UPDATE;
    private boolean strictSpecBehavior = DEFAULT_STRICT_SPEC_BEHAVIOR;
    private boolean generateAliasAsModel = DEFAULT_GENERATE_ALIAS_AS_MODEL;
    private String templateDir;
    private String templatingEngineName = DEFAULT_TEMPLATING_ENGINE_NAME;
    private String ignoreFileOverride;
    private ImmutableMap<String, String> globalProperties = DEFAULT_GLOBAL_PROPERTIES;

    private WorkflowSettings(Builder builder) {
        this.inputSpec = builder.inputSpec;
        this.outputDir = builder.outputDir;
        this.verbose = builder.verbose;
        this.skipOverwrite = builder.skipOverwrite;
        this.removeOperationIdPrefix = builder.removeOperationIdPrefix;
        this.logToStderr = builder.logToStderr;
        this.validateSpec = builder.validateSpec;
        this.enablePostProcessFile = builder.enablePostProcessFile;
        this.enableMinimalUpdate = builder.enableMinimalUpdate;
        this.strictSpecBehavior = builder.strictSpecBehavior;
        this.templateDir = builder.templateDir;
        this.templatingEngineName = builder.templatingEngineName;
        this.ignoreFileOverride = builder.ignoreFileOverride;
        this.globalProperties = ImmutableMap.copyOf(builder.globalProperties);
        this.generateAliasAsModel = builder.generateAliasAsModel;
    }

    /**
     * Instantiates a new workflow settings.
     */
    @SuppressWarnings("unused")
    public WorkflowSettings() {

    }

    public static Builder newBuilder() {
        return new Builder();
    }

    public static Builder newBuilder(WorkflowSettings copy) {
        Builder builder = newBuilder();
        builder.inputSpec = copy.getInputSpec();
        builder.outputDir = copy.getOutputDir();
        builder.verbose = copy.isVerbose();
        builder.skipOverwrite = copy.isSkipOverwrite();
        builder.removeOperationIdPrefix = copy.isRemoveOperationIdPrefix();
        builder.logToStderr = copy.isLogToStderr();
        builder.validateSpec = copy.isValidateSpec();
        builder.enablePostProcessFile = copy.isEnablePostProcessFile();
        builder.enableMinimalUpdate = copy.isEnableMinimalUpdate();
        builder.generateAliasAsModel = copy.isGenerateAliasAsModel();
        builder.strictSpecBehavior = copy.isStrictSpecBehavior();
        builder.templatingEngineName = copy.getTemplatingEngineName();
        builder.ignoreFileOverride = copy.getIgnoreFileOverride();

        // this, and any other collections, must be mutable in the builder.
        builder.globalProperties = new HashMap<>(copy.getGlobalProperties());

        // force builder "with" methods to invoke side effects
        builder.withTemplateDir(copy.getTemplateDir());

        return builder;
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
     * defining patterns in .openapi-generator-ignore or {@link WorkflowSettings#isEnableMinimalUpdate()}.
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
     * Indicates whether or not the generation should convert aliases (primitives defined as schema for use within documents) as models.
     *
     * @return <code>true</code> if generate-alias-as-model is enabled, otherwise <code>false</code>.
     */
    public boolean isGenerateAliasAsModel() {
        return generateAliasAsModel;
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
    public Map<String, String> getGlobalProperties() {
        return globalProperties;
    }

    /**
     * {@code WorkflowSettings} builder static inner class.
     */
    @SuppressWarnings("unused")
    public static final class Builder {
        private String inputSpec;
        private String outputDir = DEFAULT_OUTPUT_DIR;
        private Boolean verbose = DEFAULT_VERBOSE;
        private Boolean skipOverwrite = DEFAULT_SKIP_OVERWRITE;
        private Boolean removeOperationIdPrefix = DEFAULT_REMOVE_OPERATION_ID_PREFIX;
        private Boolean logToStderr = DEFAULT_LOG_TO_STDERR;
        private Boolean validateSpec = DEFAULT_VALIDATE_SPEC;
        private Boolean enablePostProcessFile = DEFAULT_ENABLE_POST_PROCESS_FILE;
        private Boolean enableMinimalUpdate = DEFAULT_ENABLE_MINIMAL_UPDATE;
        private Boolean strictSpecBehavior = DEFAULT_STRICT_SPEC_BEHAVIOR;
        private Boolean generateAliasAsModel = DEFAULT_GENERATE_ALIAS_AS_MODEL;
        private String templateDir;
        private String templatingEngineName = DEFAULT_TEMPLATING_ENGINE_NAME;
        private String ignoreFileOverride;

        // NOTE: All collections must be mutable in the builder, and copied to a new immutable collection in .build()
        private Map<String, String> globalProperties = new HashMap<>();;

        private Builder() {
        }


        /**
         * Sets the {@code inputSpec} and returns a reference to this Builder so that the methods can be chained together.
         *
         * @param inputSpec the {@code inputSpec} to set
         * @return a reference to this Builder
         */
        public Builder withInputSpec(String inputSpec) {
            if (inputSpec != null) {
                this.inputSpec = inputSpec;
            }
            return this;
        }

        /**
         * Sets the {@code outputDir} and returns a reference to this Builder so that the methods can be chained together.
         *
         * @param outputDir the {@code outputDir} to set
         * @return a reference to this Builder
         */
        public Builder withOutputDir(String outputDir) {
            if (outputDir != null ) {
                this.outputDir = Paths.get(outputDir).toAbsolutePath().toString();
            } else {
                this.outputDir = DEFAULT_OUTPUT_DIR;
            }
            return this;
        }

        /**
         * Sets the {@code verbose} and returns a reference to this Builder so that the methods can be chained together.
         *
         * @param verbose the {@code verbose} to set
         * @return a reference to this Builder
         */
        public Builder withVerbose(Boolean verbose) {
            this.verbose = verbose != null ? verbose : Boolean.valueOf(DEFAULT_VERBOSE);
            return this;
        }

        /**
         * Sets the {@code skipOverwrite} and returns a reference to this Builder so that the methods can be chained together.
         *
         * @param skipOverwrite the {@code skipOverwrite} to set
         * @return a reference to this Builder
         */
        public Builder withSkipOverwrite(Boolean skipOverwrite) {
            this.skipOverwrite = skipOverwrite != null ? skipOverwrite : Boolean.valueOf(DEFAULT_SKIP_OVERWRITE);
            return this;
        }

        /**
         * Sets the {@code removeOperationIdPrefix} and returns a reference to this Builder so that the methods can be chained together.
         *
         * @param removeOperationIdPrefix the {@code removeOperationIdPrefix} to set
         * @return a reference to this Builder
         */
        public Builder withRemoveOperationIdPrefix(Boolean removeOperationIdPrefix) {
            this.removeOperationIdPrefix = removeOperationIdPrefix != null ? removeOperationIdPrefix : Boolean.valueOf(DEFAULT_REMOVE_OPERATION_ID_PREFIX);
            return this;
        }

        /**
         * Sets the {@code logToStderr} and returns a reference to this Builder so that the methods can be chained together.
         *
         * @param logToStderr the {@code logToStderr} to set
         * @return a reference to this Builder
         */
        public Builder withLogToStderr(Boolean logToStderr) {
            this.logToStderr = logToStderr != null ? logToStderr : Boolean.valueOf(DEFAULT_LOG_TO_STDERR);
            return this;
        }

        /**
         * Sets the {@code validateSpec} and returns a reference to this Builder so that the methods can be chained together.
         *
         * @param validateSpec the {@code validateSpec} to set
         * @return a reference to this Builder
         */
        public Builder withValidateSpec(Boolean validateSpec) {
            this.validateSpec = validateSpec != null ? validateSpec : Boolean.valueOf(DEFAULT_VALIDATE_SPEC);
            return this;
        }

        /**
         * Sets the {@code enablePostProcessFile} and returns a reference to this Builder so that the methods can be chained together.
         *
         * @param enablePostProcessFile the {@code enablePostProcessFile} to set
         * @return a reference to this Builder
         */
        public Builder withEnablePostProcessFile(Boolean enablePostProcessFile) {
            this.enablePostProcessFile = enablePostProcessFile != null ? enablePostProcessFile : Boolean.valueOf(DEFAULT_ENABLE_POST_PROCESS_FILE);
            return this;
        }

        /**
         * Sets the {@code enableMinimalUpdate} and returns a reference to this Builder so that the methods can be chained together.
         *
         * @param enableMinimalUpdate the {@code enableMinimalUpdate} to set
         * @return a reference to this Builder
         */
        public Builder withEnableMinimalUpdate(Boolean enableMinimalUpdate) {
            this.enableMinimalUpdate = enableMinimalUpdate != null ? enableMinimalUpdate : Boolean.valueOf(DEFAULT_ENABLE_MINIMAL_UPDATE);
            return this;
        }

        /**
         * Sets the {@code strictSpecBehavior} and returns a reference to this Builder so that the methods can be chained together.
         *
         * @param strictSpecBehavior the {@code strictSpecBehavior} to set
         * @return a reference to this Builder
         */
        public Builder withStrictSpecBehavior(Boolean strictSpecBehavior) {
            this.strictSpecBehavior = strictSpecBehavior != null ? strictSpecBehavior : Boolean.valueOf(DEFAULT_STRICT_SPEC_BEHAVIOR);
            return this;
        }

        /**
         * Sets the {@code generateAliasAsModel} and returns a reference to this Builder so that the methods can be chained together.
         * An 'alias' is a primitive type defined as a schema, and this option will attempt to construct a model for that primitive.
         *
         * @param generateAliasAsModel the {@code generateAliasAsModel} to set
         * @return a reference to this Builder
         */
        public Builder withGenerateAliasAsModel(Boolean generateAliasAsModel) {
            this.generateAliasAsModel = generateAliasAsModel != null ? generateAliasAsModel : Boolean.valueOf(DEFAULT_GENERATE_ALIAS_AS_MODEL);
            return this;
        }

        /**
         * Sets the {@code templateDir} and returns a reference to this Builder so that the methods can be chained together.
         *
         * @param templateDir the {@code templateDir} to set
         * @return a reference to this Builder
         */
        public Builder withTemplateDir(String templateDir) {
            if (templateDir != null) {
                URI uri = null;
                File f = new File(templateDir);

                // check to see if the folder exists
                if (f.exists() && f.isDirectory()) {
                    uri = f.toURI();
                    this.templateDir =  Paths.get(uri).toAbsolutePath().toString();
                } else {
                    URL url = this.getClass().getClassLoader().getResource(templateDir);
                    if (url != null) {
                        try {
                            uri = url.toURI();
                            this.templateDir = templateDir;
                        } catch (URISyntaxException e) {
                            LOGGER.warn("The requested template was found on the classpath, but resulted in a syntax error.");
                        }
                    }
                }

                if (uri == null) {
                    throw new IllegalArgumentException(
                            "Template directory " + templateDir + " does not exist.");
                }
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
            this.templatingEngineName = templatingEngineName != null ? templatingEngineName : DEFAULT_TEMPLATING_ENGINE_NAME;
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
         * Sets the {@code globalProperties} and returns a reference to this Builder so that the methods can be chained together.
         *
         * @param globalProperties the {@code globalProperties} to set
         * @return a reference to this Builder
         */
        public Builder withGlobalProperties(Map<String, String> globalProperties) {
            if (globalProperties != null) {
                this.globalProperties = globalProperties;
            }
            return this;
        }

        /**
         * Sets the {@code globalProperties} and returns a reference to this Builder so that the methods can be chained together.
         *
         * @param key The key of a system (global) property to set
         * @param value The value of a system (global) property to set
         * @return a reference to this Builder
         */
        public Builder withGlobalProperty(String key, String value) {
            if (this.globalProperties == null) {
                this.globalProperties = new HashMap<>();
            }
            this.globalProperties.put(key, value);
            return this;
        }

        /**
         * Returns a {@code WorkflowSettings} built from the parameters previously set.
         *
         * @return a {@code WorkflowSettings} built with parameters of this {@code WorkflowSettings.Builder}
         */
        public WorkflowSettings build() {
            WorkflowSettings instance = new WorkflowSettings(this);
            //noinspection PlaceholderCountMatchesArgumentCount
            LOGGER.debug("WorkflowSettings#build: %s", instance.toString());
            return instance;
        }
    }


    @Override
    public String toString() {
        return "WorkflowSettings{" +
                "inputSpec='" + inputSpec + '\'' +
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
                ", ignoreFileOverride='" + ignoreFileOverride + '\'' +
                ", globalProperties=" + globalProperties +
                ", generateAliasAsModel=" + generateAliasAsModel +
                '}';
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof WorkflowSettings)) return false;
        WorkflowSettings that = (WorkflowSettings) o;
        return isVerbose() == that.isVerbose() &&
                isSkipOverwrite() == that.isSkipOverwrite() &&
                isRemoveOperationIdPrefix() == that.isRemoveOperationIdPrefix() &&
                isLogToStderr() == that.isLogToStderr() &&
                isValidateSpec() == that.isValidateSpec() &&
                isEnablePostProcessFile() == that.isEnablePostProcessFile() &&
                isEnableMinimalUpdate() == that.isEnableMinimalUpdate() &&
                isStrictSpecBehavior() == that.isStrictSpecBehavior() &&
                isGenerateAliasAsModel() == that.isGenerateAliasAsModel() &&
                Objects.equals(getInputSpec(), that.getInputSpec()) &&
                Objects.equals(getOutputDir(), that.getOutputDir()) &&
                Objects.equals(getTemplateDir(), that.getTemplateDir()) &&
                Objects.equals(getTemplatingEngineName(), that.getTemplatingEngineName()) &&
                Objects.equals(getIgnoreFileOverride(), that.getIgnoreFileOverride()) &&
                Objects.equals(getGlobalProperties(), that.getGlobalProperties());
    }

    @Override
    public int hashCode() {
        return Objects.hash(
                getInputSpec(),
                getOutputDir(),
                isVerbose(),
                isSkipOverwrite(),
                isRemoveOperationIdPrefix(),
                isLogToStderr(),
                isValidateSpec(),
                isGenerateAliasAsModel(),
                isEnablePostProcessFile(),
                isEnableMinimalUpdate(),
                isStrictSpecBehavior(),
                getTemplateDir(),
                getTemplatingEngineName(),
                getIgnoreFileOverride(),
                getGlobalProperties()
        );
    }
}
