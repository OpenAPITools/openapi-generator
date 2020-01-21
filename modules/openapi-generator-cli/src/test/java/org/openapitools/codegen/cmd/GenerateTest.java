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

import org.mockito.MockSettings;
import org.openapitools.codegen.ClientOptInput;
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.OpenAPIGenerator;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.apache.commons.lang3.ArrayUtils;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;

import static org.mockito.Answers.CALLS_REAL_METHODS;
import static org.mockito.Mockito.*;

@SuppressWarnings("unused")
public class GenerateTest {

    protected MockSettings mockSettings = withSettings().useConstructor().defaultAnswer(CALLS_REAL_METHODS);

    private DefaultGenerator generator;
    private CodegenConfigurator configurator;
    File outputDirectory;

    // ???
    ClientOptInput clientOptInput;

    @BeforeMethod
    public void beforeEachTest() throws IOException {
        generator = mock(DefaultGenerator.class, mockSettings);
        configurator = mock(CodegenConfigurator.class, mockSettings);
        outputDirectory = Files.createTempDirectory("GenerateTest").toFile();
    }
//
//    @Test
//    public void testVerboseShort() throws Exception {
//        setupAndRunGenericTest("-v");
//        verify(configurator).setVerbose(true);
//    }
//
//    @Test
//    public void testVerboseLong() throws Exception {
//        setupAndRunGenericTest("--verbose");
//        verify(configurator).setVerbose(true);
//    }
//
//    @Test
//    public void testRequiredArgs_ShortArgs() throws Exception {
//        setupAndRunTest("-i", "src/test/resources/swagger.yaml", "-g", "java", "-o", "src/main/java", false, null, "-p", "foo=bar");
//        verify(configurator).addAdditionalProperty("foo", "bar");
//    }
//
//    @Test
//    public void testRequiredArgs_LongArgs() throws Exception {
//        setupAndRunTest("--input-spec", "src/test/resources/swagger.yaml", "--generator-name", "java", "--output",
//                "src/main/java", false, null);
//
//        // TODO: I added these
//        verify(configurator).addAdditionalProperty("generatorName", "java");
//        verify(configurator).addAdditionalProperty("inputSpec", "src/test/resources/swagger.yaml");
//    }
//
//    @Test
//    public void testTemplateDirShort() throws Exception {
//        final String templateDir = "src/main/resources/customTemplates";
//        setupAndRunGenericTest("-t", templateDir);
//        verify(configurator).setTemplateDir(templateDir);
//    }
//
//    @Test
//    public void testTemplateDirLong() throws Exception {
//        final String templateDir = "src/main/resources/customTemplates";
//        setupAndRunGenericTest("--template-dir", templateDir);
//        verify(configurator).setTemplateDir(templateDir);
//    }
//
//    @Test
//    public void testAuthLong() throws Exception {
//        final String auth = "hello:world";
//        setupAndRunGenericTest("--auth", auth);
//        verify(configurator).setAuth(auth);
//    }
//
//    @Test
//    public void testAuthShort() throws Exception {
//        final String auth = "hello:world";
//        setupAndRunGenericTest("-a", auth);
//        verify(configurator).setAuth(auth);
//    }
//
//    @Test
//    public void testAuthUnspecified() throws Exception {
//        setupAndRunGenericTest();
//        verify(configurator, never()).setAuth(anyString());
//    }
//
//    @Test
//    public void testConfigJsonShort() throws Exception {
//        setupAndRunTest("-i", "src/test/resources/swagger.yaml", "-g", "java", "-o", "src/main/java", true,
//                "config.json", "-c", "config.json");
//        // TODO: Verifications
//    }
//
//    @Test
//    public void testConfigJsonLong() throws Exception {
//        setupAndRunTest("-i", "src/test/resources/swagger.yaml", "-g", "java", "-o", "src/main/java", true,
//                "config.json", "--config", "config.json");
//        // TODO: Verifications
//    }
//
//    @Test
//    public void testConfigYamlShort() throws Exception {
//        setupAndRunTest("-i", "src/test/resources/swagger.yaml", "-g", "java", "-o", "src/main/java", true,
//                "config.yaml", "-c", "config.yaml");
//
//        // TODO: Verifications
//    }
//
//    @Test
//    public void testConfigYamlLong() throws Exception {
//
//        setupAndRunTest("-i", "src/test/resources/swagger.yaml", "-g", "java", "-o", "src/main/java", true,
//                "config.yaml", "--config", "config.yaml");
//
//        // TODO: Verifications
//    }
//
//    @Test
//    public void testSkipOverwriteShort() throws Exception {
//        setupAndRunGenericTest("-s");
//        verify(configurator).setSkipOverwrite(true);
//    }
//
//    @Test
//    public void testSkipOverwriteLong() throws Exception {
//        setupAndRunGenericTest("--skip-overwrite");
//        verify(configurator).setSkipOverwrite(true);
//    }
//
//    @Test
//    public void testStrictSpec() throws Exception {
//
//        setupAndRunGenericTest("--strict-spec", "true");
//        new FullVerifications() {
//            {
//                configurator.setStrictSpecBehavior(true);
//                times = 1;
//            }
//        };
//
//        setupAndRunGenericTest("--strict-spec", "false");
//        new FullVerifications() {
//            {
//                configurator.setStrictSpecBehavior(false);
//                times = 1;
//            }
//        };
//    }
//
//    @Test
//    public void testPackageName() throws Exception {
//        final String value = "io.foo.bar.baz";
//        setupAndRunGenericTest("--package-name", value);
//
//        new FullVerifications() {
//            {
//                configurator.setPackageName(value);
//                times = 1;
//            }
//        };
//    }
//
//    @Test
//    public void testApiPackage() throws Exception {
//        final String value = "io.foo.bar.api";
//        setupAndRunGenericTest("--api-package", value);
//
//        new FullVerifications() {
//            {
//                configurator.setApiPackage(value);
//                times = 1;
//            }
//        };
//    }
//
//    @Test
//    public void testModelPackage() throws Exception {
//        final String value = "io.foo.bar.api";
//        setupAndRunGenericTest("--model-package", value);
//
//        new FullVerifications() {
//            {
//                configurator.setModelPackage(value);
//                times = 1;
//            }
//        };
//    }
//
//    @Test
//    public void testInstantiationTypes() throws Exception {
//
//        setupAndRunGenericTest("--instantiation-types", "hello=world,key=,foo=bar,key2");
//
//        new FullVerifications() {
//            {
//                configurator.addInstantiationType("hello", "world");
//                times = 1;
//                configurator.addInstantiationType("foo", "bar");
//                times = 1;
//                configurator.addInstantiationType("key", "");
//                times = 1;
//                configurator.addInstantiationType("key2", "");
//                times = 1;
//            }
//        };
//
//        setupAndRunGenericTest("--instantiation-types", "hello=world", "--instantiation-types",
//                "key=", "--instantiation-types", "foo=bar", "--instantiation-types", "key2");
//
//        new FullVerifications() {
//            {
//                configurator.addInstantiationType("hello", "world");
//                times = 1;
//                configurator.addInstantiationType("foo", "bar");
//                times = 1;
//                configurator.addInstantiationType("key", "");
//                times = 1;
//                configurator.addInstantiationType("key2", "");
//                times = 1;
//            }
//        };
//    }
//
//    @Test
//    public void testTypeMappings() throws Exception {
//        setupAndRunGenericTest("--type-mappings", "hello=world,key=,foo=bar,key2");
//
//        new FullVerifications() {
//            {
//                configurator.addTypeMapping("hello", "world");
//                times = 1;
//                configurator.addTypeMapping("foo", "bar");
//                times = 1;
//                configurator.addTypeMapping("key", "");
//                times = 1;
//                configurator.addTypeMapping("key2", "");
//                times = 1;
//            }
//        };
//
//        setupAndRunGenericTest("--type-mappings", "hello=world", "--type-mappings", "key=",
//                "--type-mappings", "foo=bar", "--type-mappings", "key2");
//
//        new FullVerifications() {
//            {
//                configurator.addTypeMapping("hello", "world");
//                times = 1;
//                configurator.addTypeMapping("foo", "bar");
//                times = 1;
//                configurator.addTypeMapping("key", "");
//                times = 1;
//                configurator.addTypeMapping("key2", "");
//                times = 1;
//            }
//        };
//    }
//
//    @Test
//    public void testAdditionalProperties() throws Exception {
//        setupAndRunGenericTest("--additional-properties", "hello=world,key=,foo=bar,key2");
//
//        new FullVerifications() {
//            {
//                configurator.addAdditionalProperty("hello", "world");
//                times = 1;
//                configurator.addAdditionalProperty("foo", "bar");
//                times = 1;
//                configurator.addAdditionalProperty("key", "");
//                times = 1;
//                configurator.addAdditionalProperty("key2", "");
//                times = 1;
//            }
//        };
//
//        setupAndRunGenericTest("--additional-properties", "hello=world", "--additional-properties",
//                "key=", "--additional-properties", "foo=bar", "--additional-properties", "key2");
//
//        new FullVerifications() {
//            {
//                configurator.addAdditionalProperty("hello", "world");
//                times = 1;
//                configurator.addAdditionalProperty("foo", "bar");
//                times = 1;
//                configurator.addAdditionalProperty("key", "");
//                times = 1;
//                configurator.addAdditionalProperty("key2", "");
//                times = 1;
//            }
//        };
//    }
//
//    @Test
//    public void testLanguageSpecificPrimitives() throws Exception {
//        setupAndRunGenericTest("--language-specific-primitives", "foo,,bar",
//                "--language-specific-primitives", "hello,world");
//
//        new FullVerifications() {
//            {
//                configurator.addLanguageSpecificPrimitive("foo");
//                times = 1;
//                configurator.addLanguageSpecificPrimitive("bar");
//                times = 1;
//                configurator.addLanguageSpecificPrimitive("hello");
//                times = 1;
//                configurator.addLanguageSpecificPrimitive("world");
//                times = 1;
//            }
//        };
//    }
//
//    @Test
//    public void testImportMappings() throws Exception {
//        setupAndRunGenericTest("--import-mappings", "hello=world,key=,foo=bar,key2");
//
//        new FullVerifications() {
//            {
//                configurator.addImportMapping("hello", "world");
//                times = 1;
//                configurator.addImportMapping("foo", "bar");
//                times = 1;
//                configurator.addImportMapping("key", "");
//                times = 1;
//                configurator.addImportMapping("key2", "");
//                times = 1;
//            }
//        };
//
//        setupAndRunGenericTest("--import-mappings", "hello=world", "--import-mappings", "key=",
//                "--import-mappings", "foo=bar", "--import-mappings", "key2");
//
//        new FullVerifications() {
//            {
//                configurator.addImportMapping("hello", "world");
//                times = 1;
//                configurator.addImportMapping("foo", "bar");
//                times = 1;
//                configurator.addImportMapping("key", "");
//                times = 1;
//                configurator.addImportMapping("key2", "");
//                times = 1;
//            }
//        };
//    }
//
//    @Test
//    public void testInvokerPackage() throws Exception {
//        final String value = "io.foo.bar.api";
//        setupAndRunGenericTest("--invoker-package", value);
//
//        new FullVerifications() {
//            {
//                configurator.setInvokerPackage(value);
//                times = 1;
//            }
//        };
//    }
//
//    @Test
//    public void testGroupId() throws Exception {
//        final String value = "io.foo.bar.api";
//        setupAndRunGenericTest("--group-id", value);
//
//        new FullVerifications() {
//            {
//                configurator.setGroupId(value);
//                times = 1;
//            }
//        };
//    }
//
//    @Test
//    public void testArtifactId() throws Exception {
//        final String value = "awesome-api";
//        setupAndRunGenericTest("--artifact-id", value);
//
//        new FullVerifications() {
//            {
//                configurator.setArtifactId(value);
//                times = 1;
//            }
//        };
//    }
//
//    @Test
//    public void testArtifactVersion() throws Exception {
//        final String value = "1.2.3";
//        setupAndRunGenericTest("--artifact-version", value);
//
//        new FullVerifications() {
//            {
//                configurator.setArtifactVersion(value);
//                times = 1;
//            }
//        };
//    }
//
//    @Test
//    public void testLibrary() throws Exception {
//        final String value = "library1";
//        setupAndRunGenericTest("--library", value);
//
//        new FullVerifications() {
//            {
//                configurator.setLibrary(value);
//                times = 1;
//            }
//        };
//    }

    private void setupAndRunTest(String specFlag, final String spec, String langFlag,
            final String lang, String outputDirFlag, final String outputDir,
            boolean configuratorFromFile, final String configFile, String... additionalParameters) {
        File actualOutputDir = new File(outputDirectory, outputDir);
        final String[] commonArgs =
                {"generate", langFlag, lang, outputDirFlag, actualOutputDir.getAbsolutePath(), specFlag, spec};

        String[] argsToUse = ArrayUtils.addAll(commonArgs, additionalParameters);


        OpenAPIGenerator.main(argsToUse);

//        verify(configurator).toClientOptInput();
        verify(configurator).setGeneratorName(lang);
        verify(configurator).setInputSpec(spec);
        verify(configurator).setOutputDir(outputDir);
    }

    private void setupAndRunGenericTest(String... additionalParameters) {
        setupAndRunTest("-i", "src/test/resources/swagger.yaml", "-g", "java", "-o", "src/main/java", false, null,
                additionalParameters);
    }
}
