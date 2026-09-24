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

import ch.qos.logback.classic.spi.ILoggingEvent;
import ch.qos.logback.core.read.ListAppender;
import io.swagger.v3.oas.models.OpenAPI;
import org.junit.jupiter.api.Assertions;
import org.openapitools.codegen.ClientOptInput;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.SpecValidationException;
import org.openapitools.codegen.testutils.ConfigAssert;
import org.slf4j.LoggerFactory;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import static org.testng.Assert.assertFalse;
import static org.testng.Assert.assertTrue;

public class CodegenConfiguratorTest {
    private void want(ConfigAssert configAssert, String key, Object expected) {
        configAssert.assertValue(key, expected);
    }

    @Test
    public void shouldSetConfigProperties() throws IOException {
        // This tests that properties we set on CodegenConfigurator make it down into generator properties,
        // limiting to those managed in DefaultCodegen.
        Map<String, Object> properties = new HashMap<String, Object>() {{
            put("foo", "bar");
            put("baz", "quux");
            put(CodegenConstants.HIDE_GENERATION_TIMESTAMP, true);
            put(CodegenConstants.SORT_PARAMS_BY_REQUIRED_FLAG, true);
            put(CodegenConstants.PREPEND_FORM_OR_BODY_PARAMETERS, false);
            put(CodegenConstants.ENSURE_UNIQUE_PARAMS, true);
            put(CodegenConstants.ALLOW_UNICODE_IDENTIFIERS, true);
            put(CodegenConstants.REMOVE_OPERATION_ID_PREFIX, false);
            put(CodegenConstants.DOCEXTENSION, "D");
            put(CodegenConstants.ENABLE_POST_PROCESS_FILE, false);
            put(CodegenConstants.GENERATE_ALIAS_AS_MODEL, true);
        }};

        File output = Files.createTempDirectory("test").toFile();
        File template = Files.createTempDirectory("test").toFile();
        String outDir = Paths.get(output.toURI()).toAbsolutePath().toString();
        String templateDir = Paths.get(template.toURI()).toAbsolutePath().toString();

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("java")
                .setAdditionalProperties(properties)
                .setInputSpec("src/test/resources/3_0/ping.yaml")
                .addImportMapping("one", "two")
                .addInstantiationType("three", "four")
                .addLanguageSpecificPrimitive("five")
                .addGlobalProperty("six", "seven")
                .addTypeMapping("eight", "nine")
                .setApiPackage("test-api")
                .setArtifactId("test-artifactId")
                .setArtifactVersion("test-artifactVersion")
                .setAuth("test-auth")
                .setGitRepoId("git")
                .setGitUserId("user")
                .setGitHost("test.com")
                .setGroupId("group")
                .setHttpUserAgent("agent")
                .setApiNameSuffix("api-suffix")
                .setModelNamePrefix("model-prefix")
                .setModelNameSuffix("model-suffix")
                .setModelPackage("model-package")
                .setPackageName("package-name")
                .setReleaseNote("release-note")
                .setTemplateDir(templateDir)
                .setOutputDir(outDir);

        final ClientOptInput clientOptInput = configurator.toClientOptInput();

        CodegenConfig config = clientOptInput.getConfig();
        config.processOpts();

        ConfigAssert props = new ConfigAssert(config.additionalProperties());

        // This verifies that things we expect to make it into the template will, as a result of this CodegenConfigurator.
        want(props, CodegenConstants.MODEL_PACKAGE, "model_package"); // * mutated by codegen
        want(props, CodegenConstants.API_PACKAGE, "test_api"); // * mutated by codegen
        want(props, CodegenConstants.HIDE_GENERATION_TIMESTAMP, true);
        want(props, CodegenConstants.SORT_PARAMS_BY_REQUIRED_FLAG, true);
        want(props, CodegenConstants.PREPEND_FORM_OR_BODY_PARAMETERS, false);
        want(props, CodegenConstants.ENSURE_UNIQUE_PARAMS, true);
        want(props, CodegenConstants.ALLOW_UNICODE_IDENTIFIERS, true);
        want(props, CodegenConstants.API_NAME_SUFFIX, "api-suffix");
        want(props, CodegenConstants.MODEL_NAME_PREFIX, "model-prefix");
        want(props, CodegenConstants.MODEL_NAME_SUFFIX, "model-suffix");
        want(props, CodegenConstants.REMOVE_OPERATION_ID_PREFIX, false);
        want(props, CodegenConstants.DOCEXTENSION, "D");
        want(props, CodegenConstants.ENABLE_POST_PROCESS_FILE, false);
        want(props, CodegenConstants.GENERATE_ALIAS_AS_MODEL, true);
        want(props, CodegenConstants.TEMPLATE_DIR, templateDir);
        want(props, CodegenConstants.GIT_REPO_ID, "git");
        want(props, CodegenConstants.GIT_USER_ID, "user");
        want(props, CodegenConstants.GIT_HOST, "test.com");
        want(props, CodegenConstants.GROUP_ID, "group");
        want(props, CodegenConstants.ARTIFACT_ID, "test-artifactId");
        want(props, CodegenConstants.ARTIFACT_VERSION, "test-artifactVersion");
        want(props, CodegenConstants.HTTP_USER_AGENT, "agent");
        want(props, CodegenConstants.RELEASE_NOTE, "release-note");
        want(props, CodegenConstants.PACKAGE_NAME, "package-name");

        // test custom properties
        want(props, "foo", "bar");
        want(props, "baz", "quux");
    }

    @Test
    public void resolvesResponses() {
        @SuppressWarnings("unchecked") Context<OpenAPI> context = (Context<OpenAPI>) new CodegenConfigurator()
                .setInputSpec("src/test/resources/3_0/response-ref.yaml")
                .setGeneratorName("java")
                .toContext();

        Assertions.assertNotNull(context.getSpecDocument().getPaths().get("/hello").getGet().getResponses().get("200").getContent());
    }

    // https://github.com/OpenAPITools/openapi-generator/issues/24212
    @Test
    public void shouldWarnAboutDroppedUnrecognizedPathItemOperation() {
        ch.qos.logback.classic.Logger logger =
                (ch.qos.logback.classic.Logger) LoggerFactory.getLogger(CodegenConfigurator.class);
        ListAppender<ILoggingEvent> listAppender = new ListAppender<>();
        listAppender.start();
        logger.addAppender(listAppender);

        try {
            @SuppressWarnings("unchecked") Context<OpenAPI> context = (Context<OpenAPI>) new CodegenConfigurator()
                    .setInputSpec("src/test/resources/3_0/issue_24212_unknown_path_item_member.yaml")
                    .setGeneratorName("java")
                    .setValidateSpec(false)
                    .toContext();

            // generation still proceeds: the recognized 'get' operation is present
            Assertions.assertNotNull(context.getSpecDocument().getPaths().get("/tasks").getGet());

            List<ILoggingEvent> missingWarnLogs = listAppender.list.stream()
                    .filter(e -> e.getLevel() == ch.qos.logback.classic.Level.WARN)
                    .filter(e -> e.getFormattedMessage().contains("'query' at path '/tasks'"))
                    .filter(e -> e.getFormattedMessage().contains("MISSING"))
                    .collect(Collectors.toList());
            assertFalse(missingWarnLogs.isEmpty(),
                    "A WARN log naming the dropped 'query' operation at path '/tasks' as MISSING must be emitted");
        } finally {
            logger.detachAppender(listAppender);
        }
    }

    // https://github.com/OpenAPITools/openapi-generator/issues/24212
    @Test
    public void shouldNotFalsePositiveOnNestedPathItemMemberTypo() {
        ch.qos.logback.classic.Logger logger =
                (ch.qos.logback.classic.Logger) LoggerFactory.getLogger(CodegenConfigurator.class);
        ListAppender<ILoggingEvent> listAppender = new ListAppender<>();
        listAppender.start();
        logger.addAppender(listAppender);

        try {
            @SuppressWarnings("unchecked") Context<OpenAPI> context = (Context<OpenAPI>) new CodegenConfigurator()
                    .setInputSpec("src/test/resources/3_0/issue_24212_path_item_parameter_typo.yaml")
                    .setGeneratorName("java")
                    .setValidateSpec(false)
                    .toContext();

            // generation still proceeds despite the typo'd nested attribute
            Assertions.assertNotNull(context.getSpecDocument().getPaths().get("/tasks/{id}").getGet());

            // a typo inside a path-level parameter/server object is not itself a dropped operation
            List<ILoggingEvent> missingWarnLogs = listAppender.list.stream()
                    .filter(e -> e.getLevel() == ch.qos.logback.classic.Level.WARN)
                    .filter(e -> e.getFormattedMessage().contains("MISSING"))
                    .collect(Collectors.toList());
            assertTrue(missingWarnLogs.isEmpty(),
                    "A nested parameter/server typo must not be reported as a dropped path-item operation");
        } finally {
            logger.detachAppender(listAppender);
        }
    }

    // https://github.com/OpenAPITools/openapi-generator/issues/24212
    @Test
    public void shouldFailWithClearMessageAndNoMisleadingWarningWhenSpecificationIsNull() {
        ch.qos.logback.classic.Logger logger =
                (ch.qos.logback.classic.Logger) LoggerFactory.getLogger(CodegenConfigurator.class);
        ListAppender<ILoggingEvent> listAppender = new ListAppender<>();
        listAppender.start();
        logger.addAppender(listAppender);

        try {
            CodegenConfigurator configurator = new CodegenConfigurator()
                    .setInputSpec("src/test/resources/3_0/issue_24212_unsupported_version.yaml")
                    .setGeneratorName("java")
                    .setValidateSpec(false);

            RuntimeException ex = Assertions.assertThrows(RuntimeException.class, configurator::toContext);
            assertFalse(ex instanceof SpecValidationException, "expected a plain RuntimeException, not SpecValidationException");
            assertTrue(ex.getMessage().startsWith("Unable to parse an OpenAPI document"), ex.getMessage());

            // nothing will be generated at all, so no operation should be reported as merely "MISSING"
            List<ILoggingEvent> missingWarnLogs = listAppender.list.stream()
                    .filter(e -> e.getFormattedMessage().contains("MISSING"))
                    .collect(Collectors.toList());
            assertTrue(missingWarnLogs.isEmpty(),
                    "Must not claim specific operations are 'MISSING' when generation cannot proceed at all");
        } finally {
            logger.detachAppender(listAppender);
        }
    }

    // https://github.com/OpenAPITools/openapi-generator/issues/24212
    @Test
    public void shouldStillThrowSpecValidationExceptionByDefaultForUnknownPathItemOperation() {
        CodegenConfigurator configurator = new CodegenConfigurator()
                .setInputSpec("src/test/resources/3_0/issue_24212_unknown_path_item_member.yaml")
                .setGeneratorName("java");
        // default validateSpec=true is unchanged by this fix: it still fails fast with the
        // existing, structured SpecValidationException rather than the new generic message.
        Assertions.assertThrows(SpecValidationException.class, configurator::toContext);
    }
}
