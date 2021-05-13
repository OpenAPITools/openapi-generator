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

package org.openapitools.codegen.cmd;

import static org.apache.commons.lang3.StringUtils.isNotEmpty;
import static org.openapitools.codegen.config.CodegenConfiguratorUtils.*;

import ch.qos.logback.classic.LoggerContext;
import ch.qos.logback.core.spi.FilterAttachable;
import io.airlift.airline.Command;
import io.airlift.airline.Option;
import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Stream;

import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.*;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@SuppressWarnings({"java:S106"})
@Command(name = "generate", description = "Generate code with the specified generator.")
public class Generate extends OpenApiGeneratorCommand {

    CodegenConfigurator configurator;
    Generator generator;

    @Option(name = {"-v", "--verbose"}, description = "verbose mode")
    private Boolean verbose;

    @Option(name = {"-g", "--generator-name"}, title = "generator name",
            description = "generator to use (see list command for list)")
    private String generatorName;

    @Option(name = {"-o", "--output"}, title = "output directory",
            description = "where to write the generated files (current dir by default)")
    private String output = "";

    @Option(name = {"-i", "--input-spec"}, title = "spec file",
            description = "location of the OpenAPI spec, as URL or file (required if not loaded via config using -c)")
    private String spec;

    @Option(name = {"-t", "--template-dir"}, title = "template directory",
            description = "folder containing the template files")
    private String templateDir;

    @Option(name = {"-e", "--engine"}, title = "templating engine",
            description = "templating engine: \"mustache\" (default) or \"handlebars\" (beta)")
    private String templatingEngine;

    @Option(
            name = {"-a", "--auth"},
            title = "authorization",
            description = "adds authorization headers when fetching the OpenAPI definitions remotely. "
                    + "Pass in a URL-encoded string of name:header with a comma separating multiple values")
    private String auth;

    @Option(
            name = {"--global-property"},
            title = "global properties",
            description = "sets specified global properties (previously called 'system properties') in "
                    + "the format of name=value,name=value (or multiple options, each with name=value)")
    private List<String> globalProperties = new ArrayList<>();

    @Option(
            name = {"-c", "--config"},
            title = "configuration file",
            description = "Path to configuration file. It can be JSON or YAML. "
                    + "If file is JSON, the content should have the format {\"optionKey\":\"optionValue\", \"optionKey1\":\"optionValue1\"...}. "
                    + "If file is YAML, the content should have the format optionKey: optionValue. "
                    + "Supported options can be different for each language. Run config-help -g {generator name} command for language-specific config options.")
    private String configFile;

    @Option(name = {"-s", "--skip-overwrite"}, title = "skip overwrite",
            description = "specifies if the existing files should be "
                    + "overwritten during the generation.")
    private Boolean skipOverwrite;

    @Option(name = { "--dry-run" }, title = "Dry run",
            description = "Try things out and report on potential changes (without actually making changes).")
    private Boolean isDryRun;

    @Option(name = {"--package-name"}, title = "package name",
            description = CodegenConstants.PACKAGE_NAME_DESC)
    private String packageName;

    @Option(name = {"--api-package"}, title = "api package",
            description = CodegenConstants.API_PACKAGE_DESC)
    private String apiPackage;

    @Option(name = {"--model-package"}, title = "model package",
            description = CodegenConstants.MODEL_PACKAGE_DESC)
    private String modelPackage;

    @Option(name = {"--api-name-suffix"}, title = "api name suffix",
            description = CodegenConstants.API_NAME_SUFFIX_DESC)
    private String apiNameSuffix;

    @Option(name = {"--model-name-prefix"}, title = "model name prefix",
            description = CodegenConstants.MODEL_NAME_PREFIX_DESC)
    private String modelNamePrefix;

    @Option(name = {"--model-name-suffix"}, title = "model name suffix",
            description = CodegenConstants.MODEL_NAME_SUFFIX_DESC)
    private String modelNameSuffix;

    @Option(
            name = {"--instantiation-types"},
            title = "instantiation types",
            description = "sets instantiation type mappings in the format of type=instantiatedType,type=instantiatedType."
                    + "For example (in Java): array=ArrayList,map=HashMap. In other words array types will get instantiated as ArrayList in generated code."
                    + " You can also have multiple occurrences of this option.")
    private List<String> instantiationTypes = new ArrayList<>();

    @Option(
            name = {"--type-mappings"},
            title = "type mappings",
            description = "sets mappings between OpenAPI spec types and generated code types "
                    + "in the format of OpenAPIType=generatedType,OpenAPIType=generatedType. For example: array=List,map=Map,string=String."
                    + " You can also have multiple occurrences of this option."
                    + " To map a specified format, use type+format, e.g. string+password=EncryptedString will map `type: string, format: password` to `EncryptedString`.")
    private List<String> typeMappings = new ArrayList<>();

    @Option(
            name = {"-p", "--additional-properties"},
            title = "additional properties",
            description = "sets additional properties that can be referenced by the mustache templates in the format of name=value,name=value."
                    + " You can also have multiple occurrences of this option.")
    private List<String> additionalProperties = new ArrayList<>();

    @Option(
            name = {"--language-specific-primitives"},
            title = "language specific primitives",
            description = "specifies additional language specific primitive types in the format of type1,type2,type3,type3. For example: String,boolean,Boolean,Double."
                    + " You can also have multiple occurrences of this option.")
    private List<String> languageSpecificPrimitives = new ArrayList<>();

    @Option(
            name = {"--import-mappings"},
            title = "import mappings",
            description = "specifies mappings between a given class and the import that should be used for that class in the format of type=import,type=import."
                    + " You can also have multiple occurrences of this option.")
    private List<String> importMappings = new ArrayList<>();

    @Option(
            name = {"--server-variables"},
            title = "server variables",
            description = "sets server variables overrides for spec documents which support variable templating of servers.")
    private List<String> serverVariableOverrides = new ArrayList<>();

    @Option(name = {"--invoker-package"}, title = "invoker package",
            description = CodegenConstants.INVOKER_PACKAGE_DESC)
    private String invokerPackage;

    @Option(name = {"--group-id"}, title = "group id", description = CodegenConstants.GROUP_ID_DESC)
    private String groupId;

    @Option(name = {"--artifact-id"}, title = "artifact id",
            description = CodegenConstants.ARTIFACT_ID_DESC)
    private String artifactId;

    @Option(name = {"--artifact-version"}, title = "artifact version",
            description = CodegenConstants.ARTIFACT_VERSION_DESC)
    private String artifactVersion;

    @Option(name = {"--library"}, title = "library", description = CodegenConstants.LIBRARY_DESC)
    private String library;

    @Option(name = {"--git-host"}, title = "git host",
            description = CodegenConstants.GIT_HOST_DESC)
    private String gitHost;

    @Option(name = {"--git-user-id"}, title = "git user id",
            description = CodegenConstants.GIT_USER_ID_DESC)
    private String gitUserId;

    @Option(name = {"--git-repo-id"}, title = "git repo id",
            description = CodegenConstants.GIT_REPO_ID_DESC)
    private String gitRepoId;

    @Option(name = {"--release-note"}, title = "release note",
            description = CodegenConstants.RELEASE_NOTE_DESC)
    private String releaseNote;

    @Option(name = {"--http-user-agent"}, title = "http user agent",
            description = CodegenConstants.HTTP_USER_AGENT_DESC)
    private String httpUserAgent;

    @Option(
            name = {"--reserved-words-mappings"},
            title = "reserved word mappings",
            description = "specifies how a reserved name should be escaped to. Otherwise, the default _<name> is used. For example id=identifier."
                    + " You can also have multiple occurrences of this option.")
    private List<String> reservedWordsMappings = new ArrayList<>();

    @Option(name = {"--ignore-file-override"}, title = "ignore file override location",
            description = CodegenConstants.IGNORE_FILE_OVERRIDE_DESC)
    private String ignoreFileOverride;

    @Option(name = {"--remove-operation-id-prefix"}, title = "remove prefix of the operationId",
            description = CodegenConstants.REMOVE_OPERATION_ID_PREFIX_DESC)
    private Boolean removeOperationIdPrefix;

    @Option(name = {"--skip-operation-example"}, title = "skip examples defined in the operation",
            description = CodegenConstants.SKIP_OPERATION_EXAMPLE_DESC)
    private Boolean skipOperationExample;

    @Option(name = {"--skip-validate-spec"},
            title = "skip spec validation",
            description = "Skips the default behavior of validating an input specification.")
    private Boolean skipValidateSpec;

    @Option(name = {"--strict-spec"},
            title = "true/false strict behavior",
            description = "'MUST' and 'SHALL' wording in OpenAPI spec is strictly adhered to. e.g. when false, no fixes will be applied to documents which pass validation but don't follow the spec.",
            arity = 1)
    private Boolean strictSpecBehavior;

    @Option(name = {"--log-to-stderr"},
            title = "Log to STDERR",
            description = "write all log messages (not just errors) to STDOUT."
                    + " Useful for piping the JSON output of debug options (e.g. `--global-property debugOperations`) to an external parser directly while testing a generator.")
    private Boolean logToStderr;

    @Option(name = {"--enable-post-process-file"}, title = "enable post-processing of files (in generators supporting it)", description = CodegenConstants.ENABLE_POST_PROCESS_FILE_DESC)
    private Boolean enablePostProcessFile;

    @Option(name = {"--generate-alias-as-model"}, title = "generate alias (array, map) as model", description = CodegenConstants.GENERATE_ALIAS_AS_MODEL_DESC)
    private Boolean generateAliasAsModel;

    @Option(name = {"--legacy-discriminator-behavior"}, title = "Support legacy logic for evaluating discriminators", description = CodegenConstants.LEGACY_DISCRIMINATOR_BEHAVIOR_DESC)
    private Boolean legacyDiscriminatorBehavior;

    @Option(name = {"--minimal-update"},
        title = "Minimal update",
        description = "Only write output files that have changed.")
    private Boolean minimalUpdate;

    @Override
    public void execute() {
        if (logToStderr != null) {
            LoggerContext lc = (LoggerContext) LoggerFactory.getILoggerFactory();
            Stream.of(Logger.ROOT_LOGGER_NAME, "io.swagger", "org.openapitools")
                    .map(lc::getLogger)
                    .peek(logger -> logger.detachAppender("STDOUT"))
                    .reduce((logger, next) -> logger.getName().equals(Logger.ROOT_LOGGER_NAME) ? logger : next)
                    .map(root -> root.getAppender("STDERR"))
                    .ifPresent(FilterAttachable::clearAllFilters);
        }

        // this initial check allows for field-level package private injection (for unit testing)
        if (configurator == null) {
            if (configFile != null && configFile.length() > 0) {
                // attempt to load from configFile
                configurator = CodegenConfigurator.fromFile(configFile);
            } else if (StringUtils.isEmpty(spec)) {
                // if user doesn't pass configFile and does not pass spec, we can fail immediately because one of these two is required to run.
                System.err.println("[error] Required option '-i' is missing");
                System.exit(1);
            }

            // if a config file wasn't specified, or we were unable to read it
            if (configurator == null) {
                // create a fresh configurator
                configurator = new CodegenConfigurator();
            }
        }

        // now override with any specified parameters
        if (skipValidateSpec != null) {
            configurator.setValidateSpec(false);
        }

        if (verbose != null) {
            configurator.setVerbose(verbose);
        }

        if (skipOverwrite != null) {
            configurator.setSkipOverwrite(skipOverwrite);
        }

        if (isNotEmpty(spec)) {
            if (!spec.matches("^http(s)?://.*") && !new File(spec).exists()) {
                System.err.println("[error] The spec file is not found: " + spec);
                System.err.println("[error] Check the path of the OpenAPI spec and try again.");
                System.exit(1);
            }
            configurator.setInputSpec(spec);
        }

        // Generator name should not be validated here, as it's validated in toClientOptInput
        if (isNotEmpty(generatorName)) {
            configurator.setGeneratorName(generatorName);
        }

        if (isNotEmpty(output)) {
            configurator.setOutputDir(output);
        }

        if (isNotEmpty(auth)) {
            configurator.setAuth(auth);
        }

        if (isNotEmpty(templateDir)) {
            configurator.setTemplateDir(templateDir);
        }

        if (isNotEmpty(packageName)) {
            configurator.setPackageName(packageName);
        }

        if (isNotEmpty(templatingEngine)) {
            configurator.setTemplatingEngineName(templatingEngine);
        }

        if (isNotEmpty(apiPackage)) {
            configurator.setApiPackage(apiPackage);
        }

        if (isNotEmpty(modelPackage)) {
            configurator.setModelPackage(modelPackage);
        }

        if (isNotEmpty(apiNameSuffix)) {
            configurator.setApiNameSuffix(apiNameSuffix);
        }

        if (isNotEmpty(modelNamePrefix)) {
            configurator.setModelNamePrefix(modelNamePrefix);
        }

        if (isNotEmpty(modelNameSuffix)) {
            configurator.setModelNameSuffix(modelNameSuffix);
        }

        if (isNotEmpty(invokerPackage)) {
            configurator.setInvokerPackage(invokerPackage);
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

        if (isNotEmpty(gitHost)) {
            configurator.setGitHost(gitHost);
        }

        if (isNotEmpty(gitUserId)) {
            configurator.setGitUserId(gitUserId);
        }

        if (isNotEmpty(gitRepoId)) {
            configurator.setGitRepoId(gitRepoId);
        }

        if (isNotEmpty(releaseNote)) {
            configurator.setReleaseNote(releaseNote);
        }

        if (isNotEmpty(httpUserAgent)) {
            configurator.setHttpUserAgent(httpUserAgent);
        }

        if (isNotEmpty(ignoreFileOverride)) {
            configurator.setIgnoreFileOverride(ignoreFileOverride);
        }

        if (removeOperationIdPrefix != null) {
            configurator.setRemoveOperationIdPrefix(removeOperationIdPrefix);
        }

        if (skipOperationExample != null) {
            configurator.setSkipOperationExample(skipOperationExample);
        }

        if (enablePostProcessFile != null) {
            configurator.setEnablePostProcessFile(enablePostProcessFile);
        }

        if (generateAliasAsModel != null) {
            configurator.setGenerateAliasAsModel(generateAliasAsModel);
        }

        if (minimalUpdate != null) {
            configurator.setEnableMinimalUpdate(minimalUpdate);
        }

        if (strictSpecBehavior != null) {
            configurator.setStrictSpecBehavior(strictSpecBehavior);
        }

        if (globalProperties != null && !globalProperties.isEmpty()) {
            applyGlobalPropertiesKvpList(globalProperties, configurator);
        }
        applyInstantiationTypesKvpList(instantiationTypes, configurator);
        applyImportMappingsKvpList(importMappings, configurator);
        applyTypeMappingsKvpList(typeMappings, configurator);
        applyAdditionalPropertiesKvpList(additionalProperties, configurator);
        applyLanguageSpecificPrimitivesCsvList(languageSpecificPrimitives, configurator);
        applyReservedWordsMappingsKvpList(reservedWordsMappings, configurator);
        applyServerVariablesKvpList(serverVariableOverrides, configurator);

        try {
            final ClientOptInput clientOptInput = configurator.toClientOptInput();

            // this null check allows us to inject for unit testing.
            if (generator == null) {
                generator = new DefaultGenerator(isDryRun);
            }

            generator.opts(clientOptInput);
            generator.generate();
        } catch (GeneratorNotFoundException e) {
            System.err.println(e.getMessage());
            System.err.println("[error] Check the spelling of the generator's name and try again.");
            System.exit(1);
        }
    }
}
