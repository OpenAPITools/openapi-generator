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

import org.openapitools.codegen.ClientOptInput;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.CodegenConstants;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.Map;

import static org.testng.Assert.*;

public class CodegenConfiguratorTest {
    private void want(Map<String, Object> additionalProperties, String key, Object expected) {
        assertEquals(additionalProperties.getOrDefault(key, null), expected);
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

        Map<String, Object> props = config.additionalProperties();

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
}