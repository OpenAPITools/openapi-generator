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

import org.openapitools.codegen.ClientOptInput;
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.OpenAPIGenerator;
import org.openapitools.codegen.config.CodegenConfigurator;
import mockit.Expectations;
import mockit.FullVerifications;
import mockit.Injectable;
import mockit.Mocked;
import mockit.Verifications;
import org.apache.commons.lang3.ArrayUtils;
import org.testng.annotations.Test;

@SuppressWarnings("unused")
public class GenerateTest {

    @Mocked
    CodegenConfigurator configurator;

    @Injectable
    ClientOptInput clientOptInput;

    @Mocked
    DefaultGenerator generator;

    @Test
    public void testVerbose() throws Exception {
        setupAndRunGenericTest("-v");

        new FullVerifications() {
            {
                configurator.setVerbose(true);
                times = 1;
            }
        };

        setupAndRunGenericTest("--verbose");

        new FullVerifications() {
            {
                configurator.setVerbose(true);
                times = 1;
            }
        };
    }

    @Test
    public void testRequiredArgs_ShortArgs() throws Exception {
        setupAndRunTest("-i", "src/test/resources/swagger.yaml", "-g", "java", "-o", "src/main/java", false, null, "-p", "foo=bar");
        new FullVerifications() {
            {
                configurator.addAdditionalProperty("foo", "bar");
                times = 1;
            }
        };
    }

    @Test
    public void testRequiredArgs_LongArgs() throws Exception {
        setupAndRunTest("--input-spec", "src/test/resources/swagger.yaml", "--generator-name", "java", "--output",
                "src/main/java", false, null);
        new FullVerifications() {
            {
            }
        };
    }

    @Test
    public void testTemplateDir() throws Exception {

        final String templateDir = "src/main/resources/customTemplates";

        setupAndRunGenericTest("--template-dir", templateDir);

        new FullVerifications() {
            {
                configurator.setTemplateDir(templateDir);
                times = 1;
            }
        };

        setupAndRunGenericTest("-t", templateDir);

        new FullVerifications() {
            {
                configurator.setTemplateDir(templateDir);
                times = 1;
            }
        };
    }

    @Test
    public void testAuth() throws Exception {

        final String auth = "hello:world";

        setupAndRunGenericTest("--auth", auth);

        new FullVerifications() {
            {
                configurator.setAuth(auth);
                times = 1;
            }
        };

        setupAndRunGenericTest("-a", auth);

        new FullVerifications() {
            {
                configurator.setAuth(auth);
                times = 1;
            }
        };

        setupAndRunGenericTest();

        new FullVerifications() {
            {
                configurator.setAuth(anyString);
                times = 0;
            }
        };
    }

    @Test
    public void testConfigJson() throws Exception {

        setupAndRunTest("-i", "src/test/resources/swagger.yaml", "-g", "java", "-o", "src/main/java", true,
                "config.json", "-c", "config.json");

        new FullVerifications() {
            {
            }
        };

        setupAndRunTest("-i", "src/test/resources/swagger.yaml", "-g", "java", "-o", "src/main/java", true,
                "config.json", "--config", "config.json");

        new FullVerifications() {
            {
            }
        };
    }

    @Test
    public void testConfigYaml() throws Exception {

        setupAndRunTest("-i", "src/test/resources/swagger.yaml", "-g", "java", "-o", "src/main/java", true,
                "config.yaml", "-c", "config.yaml");

        new FullVerifications() {
            {
            }
        };

        setupAndRunTest("-i", "src/test/resources/swagger.yaml", "-g", "java", "-o", "src/main/java", true,
                "config.yaml", "--config", "config.yaml");

        new FullVerifications() {
            {
            }
        };
    }

    @Test
    public void testSkipOverwrite() throws Exception {

        setupAndRunGenericTest("-s");
        new FullVerifications() {
            {
                configurator.setSkipOverwrite(true);
                times = 1;
            }
        };

        setupAndRunGenericTest("--skip-overwrite");
        new FullVerifications() {
            {
                configurator.setSkipOverwrite(true);
                times = 1;
            }
        };
    }

    @Test
    public void testStrictSpec() throws Exception {

        setupAndRunGenericTest("--strict-spec", "true");
        new FullVerifications() {
            {
                configurator.setStrictSpecBehavior(true);
                times = 1;
            }
        };

        setupAndRunGenericTest("--strict-spec", "false");
        new FullVerifications() {
            {
                configurator.setStrictSpecBehavior(false);
                times = 1;
            }
        };
    }

    @Test
    public void testPackageName() throws Exception {
        final String value = "io.foo.bar.baz";
        setupAndRunGenericTest("--package-name", value);

        new FullVerifications() {
            {
                configurator.setPackageName(value);
                times = 1;
            }
        };
    }

    @Test
    public void testApiPackage() throws Exception {
        final String value = "io.foo.bar.api";
        setupAndRunGenericTest("--api-package", value);

        new FullVerifications() {
            {
                configurator.setApiPackage(value);
                times = 1;
            }
        };
    }

    @Test
    public void testModelPackage() throws Exception {
        final String value = "io.foo.bar.api";
        setupAndRunGenericTest("--model-package", value);

        new FullVerifications() {
            {
                configurator.setModelPackage(value);
                times = 1;
            }
        };
    }

    @Test
    public void testInstantiationTypes() throws Exception {

        setupAndRunGenericTest("--instantiation-types", "hello=world,key=,foo=bar,key2");

        new FullVerifications() {
            {
                configurator.addInstantiationType("hello", "world");
                times = 1;
                configurator.addInstantiationType("foo", "bar");
                times = 1;
                configurator.addInstantiationType("key", "");
                times = 1;
                configurator.addInstantiationType("key2", "");
                times = 1;
            }
        };

        setupAndRunGenericTest("--instantiation-types", "hello=world", "--instantiation-types",
                "key=", "--instantiation-types", "foo=bar", "--instantiation-types", "key2");

        new FullVerifications() {
            {
                configurator.addInstantiationType("hello", "world");
                times = 1;
                configurator.addInstantiationType("foo", "bar");
                times = 1;
                configurator.addInstantiationType("key", "");
                times = 1;
                configurator.addInstantiationType("key2", "");
                times = 1;
            }
        };
    }

    @Test
    public void testTypeMappings() throws Exception {
        setupAndRunGenericTest("--type-mappings", "hello=world,key=,foo=bar,key2");

        new FullVerifications() {
            {
                configurator.addTypeMapping("hello", "world");
                times = 1;
                configurator.addTypeMapping("foo", "bar");
                times = 1;
                configurator.addTypeMapping("key", "");
                times = 1;
                configurator.addTypeMapping("key2", "");
                times = 1;
            }
        };

        setupAndRunGenericTest("--type-mappings", "hello=world", "--type-mappings", "key=",
                "--type-mappings", "foo=bar", "--type-mappings", "key2");

        new FullVerifications() {
            {
                configurator.addTypeMapping("hello", "world");
                times = 1;
                configurator.addTypeMapping("foo", "bar");
                times = 1;
                configurator.addTypeMapping("key", "");
                times = 1;
                configurator.addTypeMapping("key2", "");
                times = 1;
            }
        };
    }

    @Test
    public void testAdditionalProperties() throws Exception {
        setupAndRunGenericTest("--additional-properties", "hello=world,key=,foo=bar,key2");

        new FullVerifications() {
            {
                configurator.addAdditionalProperty("hello", "world");
                times = 1;
                configurator.addAdditionalProperty("foo", "bar");
                times = 1;
                configurator.addAdditionalProperty("key", "");
                times = 1;
                configurator.addAdditionalProperty("key2", "");
                times = 1;
            }
        };

        setupAndRunGenericTest("--additional-properties", "hello=world", "--additional-properties",
                "key=", "--additional-properties", "foo=bar", "--additional-properties", "key2");

        new FullVerifications() {
            {
                configurator.addAdditionalProperty("hello", "world");
                times = 1;
                configurator.addAdditionalProperty("foo", "bar");
                times = 1;
                configurator.addAdditionalProperty("key", "");
                times = 1;
                configurator.addAdditionalProperty("key2", "");
                times = 1;
            }
        };
    }

    @Test
    public void testLanguageSpecificPrimitives() throws Exception {
        setupAndRunGenericTest("--language-specific-primitives", "foo,,bar",
                "--language-specific-primitives", "hello,world");

        new FullVerifications() {
            {
                configurator.addLanguageSpecificPrimitive("foo");
                times = 1;
                configurator.addLanguageSpecificPrimitive("bar");
                times = 1;
                configurator.addLanguageSpecificPrimitive("hello");
                times = 1;
                configurator.addLanguageSpecificPrimitive("world");
                times = 1;
            }
        };
    }

    @Test
    public void testImportMappings() throws Exception {
        setupAndRunGenericTest("--import-mappings", "hello=world,key=,foo=bar,key2");

        new FullVerifications() {
            {
                configurator.addImportMapping("hello", "world");
                times = 1;
                configurator.addImportMapping("foo", "bar");
                times = 1;
                configurator.addImportMapping("key", "");
                times = 1;
                configurator.addImportMapping("key2", "");
                times = 1;
            }
        };

        setupAndRunGenericTest("--import-mappings", "hello=world", "--import-mappings", "key=",
                "--import-mappings", "foo=bar", "--import-mappings", "key2");

        new FullVerifications() {
            {
                configurator.addImportMapping("hello", "world");
                times = 1;
                configurator.addImportMapping("foo", "bar");
                times = 1;
                configurator.addImportMapping("key", "");
                times = 1;
                configurator.addImportMapping("key2", "");
                times = 1;
            }
        };
    }

    @Test
    public void testInvokerPackage() throws Exception {
        final String value = "io.foo.bar.api";
        setupAndRunGenericTest("--invoker-package", value);

        new FullVerifications() {
            {
                configurator.setInvokerPackage(value);
                times = 1;
            }
        };
    }

    @Test
    public void testGroupId() throws Exception {
        final String value = "io.foo.bar.api";
        setupAndRunGenericTest("--group-id", value);

        new FullVerifications() {
            {
                configurator.setGroupId(value);
                times = 1;
            }
        };
    }

    @Test
    public void testArtifactId() throws Exception {
        final String value = "awesome-api";
        setupAndRunGenericTest("--artifact-id", value);

        new FullVerifications() {
            {
                configurator.setArtifactId(value);
                times = 1;
            }
        };
    }

    @Test
    public void testArtifactVersion() throws Exception {
        final String value = "1.2.3";
        setupAndRunGenericTest("--artifact-version", value);

        new FullVerifications() {
            {
                configurator.setArtifactVersion(value);
                times = 1;
            }
        };
    }

    @Test
    public void testLibrary() throws Exception {
        final String value = "library1";
        setupAndRunGenericTest("--library", value);

        new FullVerifications() {
            {
                configurator.setLibrary(value);
                times = 1;
            }
        };
    }

    private void setupAndRunTest(String specFlag, final String spec, String langFlag,
            final String lang, String outputDirFlag, final String outputDir,
            boolean configuratorFromFile, final String configFile, String... additionalParameters) {
        final String[] commonArgs =
                {"generate", langFlag, lang, outputDirFlag, outputDir, specFlag, spec};

        String[] argsToUse = ArrayUtils.addAll(commonArgs, additionalParameters);

        if (configuratorFromFile) {

            new Expectations() {
                {
                    CodegenConfigurator.fromFile(configFile);
                    times = 1;
                    result = configurator;
                }
            };

        } else {
            new Expectations() {
                {
                    CodegenConfigurator.fromFile(anyString);
                    result = null;

                    new CodegenConfigurator();
                    times = 1;
                    result = configurator;
                }
            };
        }

        new Expectations() {
            {

                configurator.toClientOptInput();
                times = 1;
                result = clientOptInput;

                new DefaultGenerator();
                times = 1;
                result = generator;

                generator.opts(clientOptInput);
                times = 1;
                result = generator;

                generator.generate();
                times = 1;

            }
        };

        OpenAPIGenerator.main(argsToUse);

        new Verifications() {
            {
                configurator.setGeneratorName(lang);
                times = 1;
                configurator.setInputSpec(spec);
                times = 1;
                configurator.setOutputDir(outputDir);
            }
        };
    }

    private void setupAndRunGenericTest(String... additionalParameters) {
        setupAndRunTest("-i", "src/test/resources/swagger.yaml", "-g", "java", "-o", "src/main/java", false, null,
                additionalParameters);
    }
}
