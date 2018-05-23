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

package org.openapitools.codegen.cmd;

import io.airlift.airline.Command;
import io.airlift.airline.Option;
import org.openapitools.codegen.ClientOptInput;
import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import static org.apache.commons.lang3.StringUtils.isEmpty;
import static org.openapitools.codegen.config.CodegenConfiguratorUtils.*;
import static org.apache.commons.lang3.StringUtils.isNotEmpty;

import java.util.ArrayList;
import java.util.List;

/**
 * User: lanwen Date: 24.03.15 Time: 20:22
 */

@Command(name = "generate", description = "Generate code with the specified generator.")
public class Generate implements Runnable {

    private static final Logger LOGGER = LoggerFactory.getLogger(Generate.class);

    @Option(name = {"-v", "--verbose"}, description = "verbose mode")
    private Boolean verbose;

    @Option(name = {"-l", "--lang"}, title = "language",
            description = "client language to generate (maybe class name in classpath, required)")
    private String lang;

    @Option(name = {"-g", "--generator-name"}, title = "generator name",
            description = "generator to use (see langs command for list)")
    private String generatorName;

    @Option(name = {"-o", "--output"}, title = "output directory",
            description = "where to write the generated files (current dir by default)")
    private String output = "";

    @Option(name = {"-i", "--input-spec"}, title = "spec file", required = true,
            description = "location of the OpenAPI spec, as URL or file (required)")
    private String spec;

    @Option(name = {"-t", "--template-dir"}, title = "template directory",
            description = "folder containing the template files")
    private String templateDir;

    @Option(
            name = {"-a", "--auth"},
            title = "authorization",
            description = "adds authorization headers when fetching the OpenAPI definitions remotely. "
                    + "Pass in a URL-encoded string of name:header with a comma separating multiple values")
    private String auth;

    @Option(
            name = {"-D"},
            title = "system properties",
            description = "sets specified system properties in "
                    + "the format of name=value,name=value (or multiple options, each with name=value)")
    private List<String> systemProperties = new ArrayList<>();

    @Option(
            name = {"-c", "--config"},
            title = "configuration file",
            description = "Path to json configuration file. "
                    + "File content should be in a json format {\"optionKey\":\"optionValue\", \"optionKey1\":\"optionValue1\"...} "
                    + "Supported options can be different for each language. Run config-help -g {generator name} command for language specific config options.")
    private String configFile;

    @Option(name = {"-s", "--skip-overwrite"}, title = "skip overwrite",
            description = "specifies if the existing files should be "
                    + "overwritten during the generation.")
    private Boolean skipOverwrite;

    @Option(name = {"--api-package"}, title = "api package",
            description = CodegenConstants.API_PACKAGE_DESC)
    private String apiPackage;

    @Option(name = {"--model-package"}, title = "model package",
            description = CodegenConstants.MODEL_PACKAGE_DESC)
    private String modelPackage;

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
                    + "in the format of OpenaAPIType=generatedType,OpenAPIType=generatedType. For example: array=List,map=Map,string=String."
                    + " You can also have multiple occurrences of this option.")
    private List<String> typeMappings = new ArrayList<>();

    @Option(
            name = {"--additional-properties"},
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

    @Override
    public void run() {

        // attempt to read from config file
        CodegenConfigurator configurator = CodegenConfigurator.fromFile(configFile);

        // if a config file wasn't specified or we were unable to read it
        if (configurator == null) {
            // createa a fresh configurator
            configurator = new CodegenConfigurator();
        }

        // now override with any specified parameters
        if (verbose != null) {
            configurator.setVerbose(verbose);
        }

        if (skipOverwrite != null) {
            configurator.setSkipOverwrite(skipOverwrite);
        }

        if (isNotEmpty(spec)) {
            configurator.setInputSpec(spec);
        }

        // TODO: After 3.0.0 release (maybe for 3.1.0): Fully deprecate lang.
        if (isNotEmpty(generatorName)) {
            configurator.setGeneratorName(generatorName);
        } else if (isNotEmpty(lang)) {
            LOGGER.warn("The '--lang' and '-l' are deprecated and may reference language names only in the next major release (4.0). Please use --generator-name /-g instead.");
            configurator.setGeneratorName(lang);
        } else {
            LOGGER.error("A generator name (--generator-name / -g) is required. ");
            System.exit(1);
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

        if (isNotEmpty(apiPackage)) {
            configurator.setApiPackage(apiPackage);
        }

        if (isNotEmpty(modelPackage)) {
            configurator.setModelPackage(modelPackage);
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

        applySystemPropertiesKvpList(systemProperties, configurator);
        applyInstantiationTypesKvpList(instantiationTypes, configurator);
        applyImportMappingsKvpList(importMappings, configurator);
        applyTypeMappingsKvpList(typeMappings, configurator);
        applyAdditionalPropertiesKvpList(additionalProperties, configurator);
        applyLanguageSpecificPrimitivesCsvList(languageSpecificPrimitives, configurator);
        applyReservedWordsMappingsKvpList(reservedWordsMappings, configurator);
        final ClientOptInput clientOptInput = configurator.toClientOptInput();

        new DefaultGenerator().opts(clientOptInput).generate();
    }
}
