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
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

/**
 * Represents those settings applied to a generation workflow.
 */
@SuppressWarnings("WeakerAccess")
public class WorkflowSettings {

    private static final Logger LOGGER = LoggerFactory.getLogger(WorkflowSettings.class);
    public static final String DEFAULT_INPUT_SPEC = null;
    public static final String DEFAULT_OUTPUT_DIR = ".";
    public static final boolean DEFAULT_VERBOSE = false;
    public static final boolean DEFAULT_SKIP_OVERWRITE = false;
    public static final boolean DEFAULT_REMOVE_OPERATION_ID_PREFIX = false;
    public static final boolean DEFAULT_LOG_TO_STDERR = false;
    public static final boolean DEFAULT_VALIDATE_SPEC = true;
    public static final boolean DEFAULT_ENABLE_POST_PROCESS_FILE = false;
    public static final boolean DEFAULT_ENABLE_MINIMAL_UPDATE = false;
    public static final boolean DEFAULT_STRICT_SPEC_BEHAVIOR = true;
    public static final String DEFAULT_TEMPLATE_DIR = null;
    public static final String DEFAULT_TEMPLATING_ENGINE_NAME = null;
    public static final String DEFAULT_IGNORE_FILE_OVERRIDE = null;
    public static final ImmutableMap<String, String> DEFAULT_SYSTEM_PROPERTIES = ImmutableMap.copyOf(new HashMap<>());

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
    private String ignoreFileOverride;
    private ImmutableMap<String, String> systemProperties;

    private WorkflowSettings(Builder builder) {
        this();
        if (builder.inputSpec == null) {
            this.inputSpec = DEFAULT_INPUT_SPEC;
        } else {
            this.inputSpec = builder.inputSpec;
        }
        if (builder.outputDir == null) {
            this.outputDir = DEFAULT_OUTPUT_DIR;
        } else {
            this.outputDir = builder.outputDir;
        }
        if (builder.verbose == null) {
            this.verbose = DEFAULT_VERBOSE;
        } else {
            this.verbose = builder.verbose;
        }
        if (builder.skipOverwrite == null) {
            this.skipOverwrite = DEFAULT_SKIP_OVERWRITE;
        } else {
            this.skipOverwrite = builder.skipOverwrite;
        }
        if (builder.removeOperationIdPrefix == null) {
            this.removeOperationIdPrefix = DEFAULT_REMOVE_OPERATION_ID_PREFIX;
        } else {
            this.removeOperationIdPrefix = builder.removeOperationIdPrefix;
        }
        if (builder.logToStderr == null) {
            this.logToStderr = DEFAULT_LOG_TO_STDERR;
        } else {
            this.logToStderr = builder.logToStderr;
        }
        if (builder.validateSpec == null) {
            this.validateSpec = DEFAULT_VALIDATE_SPEC;
        } else {
            this.validateSpec = builder.validateSpec;
        }
        if (builder.enablePostProcessFile == null) {
            this.enablePostProcessFile = DEFAULT_ENABLE_POST_PROCESS_FILE;
        } else {
            this.enablePostProcessFile = builder.enablePostProcessFile;
        }
        if (builder.enableMinimalUpdate == null) {
            this.enableMinimalUpdate = DEFAULT_ENABLE_MINIMAL_UPDATE;
        } else {
            this.enableMinimalUpdate = builder.enableMinimalUpdate;
        }
        if (builder.strictSpecBehavior == null) {
            this.strictSpecBehavior = DEFAULT_STRICT_SPEC_BEHAVIOR;
        } else {
            this.strictSpecBehavior = builder.strictSpecBehavior;
        }
        if (builder.templateDir == null) {
            this.templateDir = DEFAULT_TEMPLATE_DIR;
        } else {
            this.templateDir = builder.templateDir;
        }
        if (builder.templatingEngineName == null) {
            this.templatingEngineName = DEFAULT_TEMPLATING_ENGINE_NAME;
        } else {
            this.templatingEngineName = builder.templatingEngineName;
        }
        if (builder.ignoreFileOverride == null) {
            this.ignoreFileOverride = DEFAULT_IGNORE_FILE_OVERRIDE;
        } else {
            this.ignoreFileOverride = builder.ignoreFileOverride;
        }
        if (builder.systemProperties == null) {
            this.systemProperties = DEFAULT_SYSTEM_PROPERTIES;
        } else {
            this.systemProperties = ImmutableMap.copyOf(builder.systemProperties);
        }

    }

    /**
     * Instantiates a new workflow settings.
     */
    @SuppressWarnings("unused")
    public WorkflowSettings() {
        systemProperties = ImmutableMap.of();
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
        builder.strictSpecBehavior = copy.isStrictSpecBehavior();
        builder.templatingEngineName = copy.getTemplatingEngineName();
        builder.ignoreFileOverride = copy.getIgnoreFileOverride();
        builder.systemProperties = ImmutableMap.copyOf(copy.getSystemProperties());

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
    public Map<String, String> getSystemProperties() {
        return systemProperties;
    }

    /**
     * {@code WorkflowSettings} builder static inner class.
     */
    @SuppressWarnings("unused")
    public static final class Builder {
        private String inputSpec;
        private String outputDir;
        private Boolean verbose;
        private Boolean skipOverwrite;
        private Boolean removeOperationIdPrefix;
        private Boolean logToStderr;
        private Boolean validateSpec;
        private Boolean enablePostProcessFile;
        private Boolean enableMinimalUpdate;
        private Boolean strictSpecBehavior;
        private String templateDir;
        private String templatingEngineName;
        private String ignoreFileOverride;
        private Map<String, String> systemProperties;

        private Builder() {
            setDefaults();
            systemProperties = new HashMap<>();
        }

        private void setDefaults(){
            this.inputSpec = WorkflowSettings.DEFAULT_INPUT_SPEC;
            this.outputDir = WorkflowSettings.DEFAULT_OUTPUT_DIR;
            this.verbose = WorkflowSettings.DEFAULT_VERBOSE;
            this.skipOverwrite = WorkflowSettings.DEFAULT_SKIP_OVERWRITE;
            this.removeOperationIdPrefix = WorkflowSettings.DEFAULT_REMOVE_OPERATION_ID_PREFIX;
            this.logToStderr = WorkflowSettings.DEFAULT_LOG_TO_STDERR;
            this.validateSpec = WorkflowSettings.DEFAULT_VALIDATE_SPEC;
            this.enablePostProcessFile = WorkflowSettings.DEFAULT_ENABLE_POST_PROCESS_FILE;
            this.enableMinimalUpdate = WorkflowSettings.DEFAULT_ENABLE_MINIMAL_UPDATE;
            this.strictSpecBehavior = WorkflowSettings.DEFAULT_STRICT_SPEC_BEHAVIOR;
            this.templateDir = WorkflowSettings.DEFAULT_TEMPLATE_DIR;
            this.templatingEngineName = WorkflowSettings.DEFAULT_TEMPLATING_ENGINE_NAME;
            this.ignoreFileOverride = WorkflowSettings.DEFAULT_IGNORE_FILE_OVERRIDE;
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
            this.outputDir = Paths.get(outputDir).toAbsolutePath().toString();;
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

                this.templateDir =  Paths.get(f.toURI()).toAbsolutePath().toString();
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
         * Sets the {@code systemProperties} and returns a reference to this Builder so that the methods can be chained together.
         *
         * @param key The key of a system (global) property to set
         * @param value The value of a system (global) property to set
         * @return a reference to this Builder
         */
        public Builder withSystemProperty(String key, String value) {
            if (this.systemProperties == null) {
                this.systemProperties = new HashMap<>();
            }
            this.systemProperties.put(key, value);
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
                ", systemProperties=" + systemProperties +
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
                Objects.equals(getInputSpec(), that.getInputSpec()) &&
                Objects.equals(getOutputDir(), that.getOutputDir()) &&
                Objects.equals(getTemplateDir(), that.getTemplateDir()) &&
                Objects.equals(getTemplatingEngineName(), that.getTemplatingEngineName()) &&
                Objects.equals(getIgnoreFileOverride(), that.getIgnoreFileOverride()) &&
                Objects.equals(getSystemProperties(), that.getSystemProperties());
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
                isEnablePostProcessFile(),
                isEnableMinimalUpdate(),
                isStrictSpecBehavior(),
                getTemplateDir(),
                getTemplatingEngineName(),
                getIgnoreFileOverride(),
                getSystemProperties()
        );
    }
}
