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

import io.airlift.airline.Cli;
import org.apache.commons.lang3.ArrayUtils;
import org.mockito.MockSettings;
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.Generator;
import org.openapitools.codegen.SpecValidationException;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.testng.TestException;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;

import static org.mockito.Answers.CALLS_REAL_METHODS;
import static org.mockito.Mockito.*;
import static org.testng.Assert.fail;

@SuppressWarnings("unused")
public class GenerateTest {

    protected MockSettings mockSettings = withSettings().useConstructor().defaultAnswer(CALLS_REAL_METHODS);
    private Generator generator;
    private CodegenConfigurator configurator;
    private Path outputDirectory;

    @AfterMethod
    public void afterEachTest() {
        outputDirectory.toFile().deleteOnExit();
    }

    @BeforeMethod
    public void beforeEachTest() throws IOException {
        outputDirectory = Files.createTempDirectory("GenerateTest");
        generator = mock(DefaultGenerator.class);
        when(generator.generate()).thenReturn(new ArrayList<>());

        configurator = mock(CodegenConfigurator.class, mockSettings);
    }

    @Test
    public void testAdditionalPropertiesLong() {
        setupAndRunGenericTest("--additional-properties", "hello=world,key=,foo=bar,key2");
        verify(configurator).addAdditionalProperty("hello", "world");
        verify(configurator).addAdditionalProperty("foo", "bar");
        verify(configurator).addAdditionalProperty("key", "");
        verify(configurator).addAdditionalProperty("key2", "");
    }

    @Test
    public void testAdditionalPropertiesLongMultiple() {
        setupAndRunGenericTest("--additional-properties", "hello=world", "--additional-properties",
                "key=", "--additional-properties", "foo=bar", "--additional-properties", "key2");
        verify(configurator).addAdditionalProperty("hello", "world");
        verify(configurator).addAdditionalProperty("foo", "bar");
        verify(configurator).addAdditionalProperty("key", "");
        verify(configurator).addAdditionalProperty("key2", "");
    }

    @Test
    public void testApiPackage() {
        final String value = "io.foo.bar.api";
        setupAndRunGenericTest("--api-package", value);
        verify(configurator).setApiPackage(value);
    }

    @Test
    public void testArtifactId() {
        final String value = "awesome-api";
        setupAndRunGenericTest("--artifact-id", value);

        verify(configurator).setArtifactId(value);
    }

    private void setupAndRunGenericTest(String... additionalParameters) {
        setupAndRunTest("-i", "src/test/resources/swagger.yaml", "-g", "java", "-o", "src/main/java", false, null,
                additionalParameters);
    }

    @SuppressWarnings("SameParameterValue")
    private void setupAndRunTest(String specFlag, final String spec, String langFlag,
                                 final String lang, String outputDirFlag, final String outputDir,
                                 boolean configuratorFromFile, final String configFile, String... additionalParameters) {
        final String[] commonArgs =
                {"generate", langFlag, lang, outputDirFlag, outputDir, specFlag, spec};

        String[] argsToUse = ArrayUtils.addAll(commonArgs, additionalParameters);

        Cli.CliBuilder<Runnable> builder =
                Cli.<Runnable>builder("openapi-generator-cli")
                        .withCommands(Generate.class);

        Generate generate = (Generate) builder.build().parse(argsToUse);

        generate.configurator = configurator;
        generate.generator = generator;

        try {
            generate.run();
        } finally {
            verify(configurator).setInputSpec(spec);
            verify(configurator).setGeneratorName(lang);
            verify(configurator).setOutputDir(outputDir);
        }
    }

    @Test
    public void testArtifactVersion() {
        final String value = "1.2.3";
        setupAndRunGenericTest("--artifact-version", value);

        verify(configurator).setArtifactVersion(value);
    }

    @Test
    public void testAuthLong() {
        final String auth = "hello:world";
        setupAndRunGenericTest("--auth", auth);
        verify(configurator).toClientOptInput();
        verify(configurator).toContext();
        verify(configurator).setAuth(auth);
        verifyNoMoreInteractions(configurator);
    }

    @Test
    public void testAuthShort() {
        final String auth = "hello:world";
        setupAndRunGenericTest("-a", auth);
        verify(configurator).toClientOptInput();
        verify(configurator).toContext();
        verify(configurator).setAuth(auth);
        verifyNoMoreInteractions(configurator);
    }

    @Test
    public void testAuthUnspecified() {
        setupAndRunGenericTest();

        verify(configurator).toClientOptInput();
        verify(configurator).toContext();

        verify(configurator, never()).setAuth(anyString());
        verifyNoMoreInteractions(configurator);
    }

    @Test
    public void testConfigJsonLong() {
        setupAndRunTest("-i", "src/test/resources/swagger.yaml", "-g", "java", "-o", "src/main/java", true,
                "config.json", "--config", "config.json");

        verify(configurator).toClientOptInput();
        verify(configurator).toContext();

        verifyNoMoreInteractions(configurator);
    }

    @Test
    public void testConfigJsonShort() {
        setupAndRunTest("-i", "src/test/resources/swagger.yaml", "-g", "java", "-o", "src/main/java", true,
                "config.json", "-c", "config.json");

        // on top of those in setupAndRunTest
        verify(configurator).toClientOptInput();
        verify(configurator).toContext();

        verifyNoMoreInteractions(configurator);
    }

    @Test
    public void testConfigYamlLong() {
        setupAndRunTest("-i", "src/test/resources/swagger.yaml", "-g", "java", "-o", "src/main/java", true,
                "config.yaml", "--config", "config.yaml");

        verify(configurator).toClientOptInput();
        verify(configurator).toContext();

        verifyNoMoreInteractions(configurator);
    }

    @Test
    public void testConfigYamlShort() {
        setupAndRunTest("-i", "src/test/resources/swagger.yaml", "-g", "java", "-o", "src/main/java", true,
                "config.yaml", "-c", "config.yaml");

        // on top of those in setupAndRunTest
        verify(configurator).toClientOptInput();
        verify(configurator).toContext();

        verifyNoMoreInteractions(configurator);
    }

    @Test
    public void testGroupId() {
        final String value = "io.foo.bar.api";
        setupAndRunGenericTest("--group-id", value);
        verify(configurator).setGroupId(value);
    }

    @Test
    public void testImportMappingsLong() {
        setupAndRunGenericTest("--import-mappings", "hello=world,key=,foo=bar,key2");

        verify(configurator).addImportMapping("hello", "world");
        verify(configurator).addImportMapping("foo", "bar");
        verify(configurator).addImportMapping("key", "");
        verify(configurator).addImportMapping("key2", "");
    }

    @Test
    public void testImportMappingsLongMultiple() {
        setupAndRunGenericTest("--import-mappings", "hello=world", "--import-mappings", "key=",
                "--import-mappings", "foo=bar", "--import-mappings", "key2");

        verify(configurator).addImportMapping("hello", "world");
        verify(configurator).addImportMapping("foo", "bar");
        verify(configurator).addImportMapping("key", "");
        verify(configurator).addImportMapping("key2", "");
    }

    @Test
    public void testInstantiationTypesLong() {
        setupAndRunGenericTest("--instantiation-types", "hello=world,key=,foo=bar,key2");
        verify(configurator).addInstantiationType("hello", "world");
        verify(configurator).addInstantiationType("foo", "bar");
        verify(configurator).addInstantiationType("key", "");
        verify(configurator).addInstantiationType("key2", "");
    }

    @Test
    public void testInstantiationTypesLongMultiple() {
        setupAndRunGenericTest("--instantiation-types", "hello=world", "--instantiation-types",
                "key=", "--instantiation-types", "foo=bar", "--instantiation-types", "key2");
        verify(configurator).addInstantiationType("hello", "world");
        verify(configurator).addInstantiationType("foo", "bar");
        verify(configurator).addInstantiationType("key", "");
        verify(configurator).addInstantiationType("key2", "");
    }

    @Test
    public void testInvokerPackage() {
        final String value = "io.foo.bar.api";
        setupAndRunGenericTest("--invoker-package", value);
        verify(configurator).setInvokerPackage(value);
    }

    @Test
    public void testLanguageSpecificPrimitives() {
        setupAndRunGenericTest("--language-specific-primitives", "foo,,bar",
                "--language-specific-primitives", "hello,world");

        verify(configurator).addLanguageSpecificPrimitive("foo");
        verify(configurator).addLanguageSpecificPrimitive("bar");
        verify(configurator).addLanguageSpecificPrimitive("hello");
        verify(configurator).addLanguageSpecificPrimitive("world");
    }

    @Test
    public void testLibrary() {
        final String value = "feign";
        setupAndRunGenericTest("--library", value);
        verify(configurator).setLibrary(value);
    }

    @Test
    public void testModelPackage() {
        final String value = "io.foo.bar.api";
        setupAndRunGenericTest("--model-package", value);
        verify(configurator).setModelPackage(value);
    }

    @Test
    public void testPackageName() {
        final String value = "io.foo.bar.baz";
        setupAndRunGenericTest("--package-name", value);
        verify(configurator).setPackageName(value);
    }

    @Test
    public void testRequiredArgs_LongArgs() {
        setupAndRunTest("--input-spec", "src/test/resources/swagger.yaml", "--generator-name", "java", "--output",
                "src/main/java", false, null);

        // on top of those in setupAndRunTest:
        verify(configurator).toClientOptInput();
        verify(configurator).toContext();

        verifyNoMoreInteractions(configurator);
    }

    @Test
    public void testRequiredArgs_ShortArgs() {
        setupAndRunTest("-i", "src/test/resources/swagger.yaml", "-g", "java", "-o", "src/main/java", false, null, "-p", "foo=bar");

        verify(configurator).addAdditionalProperty("foo", "bar");
        verify(configurator).toClientOptInput();
        verify(configurator).toContext();

        verifyNoMoreInteractions(configurator);
    }

    @Test
    public void testSkipOverwriteLong() {
        setupAndRunGenericTest("--skip-overwrite");
        verify(configurator).setSkipOverwrite(true);
        verify(configurator).toClientOptInput();
        verify(configurator).toContext();
        verifyNoMoreInteractions(configurator);
    }

    @Test
    public void testSkipOverwriteShort() {
        setupAndRunGenericTest("-s");
        verify(configurator).setSkipOverwrite(true);
        verify(configurator).toClientOptInput();
        verify(configurator).toContext();
        verifyNoMoreInteractions(configurator);
    }

    @Test
    public void testStrictSpecFalse() {
        setupAndRunGenericTest("--strict-spec", "false");
        verify(configurator).setStrictSpecBehavior(false);
    }

    @Test
    public void testStrictSpecTrue() {
        setupAndRunGenericTest("--strict-spec", "true");
        verify(configurator).setStrictSpecBehavior(true);
    }

    @SuppressWarnings("ResultOfMethodCallIgnored")
    @Test
    public void testTemplateDirLong() {
        final String templateDir = "src/main/resources/customTemplates";
        File f = outputDirectory.resolve(templateDir).toFile();
        try {
            f.mkdirs();
            setupAndRunGenericTest("--template-dir", f.getAbsolutePath());
            verify(configurator).setTemplateDir(f.getAbsolutePath());
            verify(configurator).toClientOptInput();
            verify(configurator).toContext();
            verifyNoMoreInteractions(configurator);
        } finally {
            if(!f.delete()) {
                System.out.println("Directory didn't delete. You can ignore this.");
            }
        }
    }

    @Test(expectedExceptions = IllegalArgumentException.class, expectedExceptionsMessageRegExp = "Template directory src/main/resources/customTemplates does not exist.")
    public void testTemplateDirMustExist() {
        final String templateDir = "src/main/resources/customTemplates";
        setupAndRunGenericTest("-t", templateDir);
        fail("Expected exception was not thrown.");
    }

    @SuppressWarnings("ResultOfMethodCallIgnored")
    @Test
    public void testTemplateDirShort() {
        final String templateDir = "src/main/resources/customTemplates";
        File f = outputDirectory.resolve(templateDir).toFile();
        try {
            f.mkdirs();
            setupAndRunGenericTest("-t", f.getAbsolutePath());
        } finally {
            verify(configurator).setTemplateDir(f.getAbsolutePath());
            verify(configurator).toClientOptInput();
            verify(configurator).toContext();
            verifyNoMoreInteractions(configurator);
            if(!f.delete()) {
                System.out.println("Directory didn't delete. You can ignore this.");
            }
        }
    }

    @Test
    public void testTypeMappingsLong() {
        setupAndRunGenericTest("--type-mappings", "hello=world,key=,foo=bar,key2");
        verify(configurator).addTypeMapping("hello", "world");
        verify(configurator).addTypeMapping("foo", "bar");
        verify(configurator).addTypeMapping("key", "");
        verify(configurator).addTypeMapping("key2", "");
    }

    @Test
    public void testTypeMappingsLongMultiple() {
        setupAndRunGenericTest("--type-mappings", "hello=world", "--type-mappings", "key=",
                "--type-mappings", "foo=bar", "--type-mappings", "key2");
        verify(configurator).addTypeMapping("hello", "world");
        verify(configurator).addTypeMapping("foo", "bar");
        verify(configurator).addTypeMapping("key", "");
        verify(configurator).addTypeMapping("key2", "");
    }

    @Test
    public void testVerboseLong() {
        setupAndRunGenericTest("--verbose");
        verify(configurator).setVerbose(true);
        verify(configurator).toClientOptInput();
        verify(configurator).toContext();
        verifyNoMoreInteractions(configurator);
    }

    @Test
    public void testVerboseShort() {
        setupAndRunGenericTest("-v");
        verify(configurator).setVerbose(true);
        verify(configurator).toClientOptInput();
        verify(configurator).toContext();
        verifyNoMoreInteractions(configurator);
    }

    /**
     * This test ensures that when the
     */
    @Test(expectedExceptions = SpecValidationException.class)
    public void testNPEWithInvalidSpecFile() {
        setupAndRunTest("-i", "src/test/resources/npe-test.yaml", "-g", "java", "-o", "src/main/java", false, null);
    }
}
