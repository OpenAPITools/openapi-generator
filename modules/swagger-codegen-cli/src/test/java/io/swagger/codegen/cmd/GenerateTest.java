package io.swagger.codegen.cmd;

import com.fasterxml.jackson.databind.JsonNode;
import io.swagger.codegen.CLIHelper;
import io.swagger.codegen.ClientOptInput;
import io.swagger.codegen.DefaultGenerator;
import io.swagger.codegen.SwaggerCodegen;
import io.swagger.codegen.config.CodegenConfigurator;
import io.swagger.v3.core.util.Json;
import io.swagger.v3.core.util.Yaml;
import mockit.Expectations;
import mockit.FullVerifications;
import mockit.Injectable;
import mockit.Mocked;
import mockit.Verifications;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.ArrayUtils;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

@SuppressWarnings("unused")
public class GenerateTest {

    @Mocked
    CodegenConfigurator configurator;

    @Injectable
    ClientOptInput clientOptInput;

    @Mocked
    DefaultGenerator generator;

    @Test(enabled=false)
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

    @Test(enabled=false)
    public void testRequiredArgs_ShortArgs() throws Exception {
        setupAndRunTest("-i", "swagger.yaml", "-l", "java", "-o", "src/main/java", false, null);
        new FullVerifications() {
            {
            }
        };
    }

    @Test(enabled=false)
    public void testRequiredArgs_LongArgs() throws Exception {
        setupAndRunTest("--input-spec", "swagger.yaml", "--lang", "java", "--output",
                "src/main/java", false, null);
        new FullVerifications() {
            {
            }
        };
    }

    @Test(enabled=false)
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

    @Test(enabled=false)
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

    @Test(enabled=false)
    public void testSystemProperties() throws Exception {

        setupAndRunGenericTest("-D", "hello=world,foo=bar");

        new FullVerifications() {
            {
                configurator.addSystemProperty("hello", "world");
                times = 1;
                configurator.addSystemProperty("foo", "bar");
                times = 1;
            }
        };

        setupAndRunGenericTest("-Dhello=world,foo=bar");

        new FullVerifications() {
            {
                configurator.addSystemProperty("hello", "world");
                times = 1;
                configurator.addSystemProperty("foo", "bar");
                times = 1;
            }
        };

        setupAndRunGenericTest("-D", "hello=world,key=,foo=bar");

        new FullVerifications() {
            {
                configurator.addSystemProperty("hello", "world");
                times = 1;
                configurator.addSystemProperty("foo", "bar");
                times = 1;
                configurator.addSystemProperty("key", "");
                times = 1;
            }
        };

        setupAndRunGenericTest("-D", "hello=world,key,foo=bar");

        new FullVerifications() {
            {
                configurator.addSystemProperty("hello", "world");
                times = 1;
                configurator.addSystemProperty("foo", "bar");
                times = 1;
                configurator.addSystemProperty("key", "");
                times = 1;
            }
        };

        setupAndRunGenericTest("-D", "hello=world", "-D", "key", "-D", "foo=bar");

        new FullVerifications() {
            {
                configurator.addSystemProperty("hello", "world");
                times = 1;
                configurator.addSystemProperty("foo", "bar");
                times = 1;
                configurator.addSystemProperty("key", "");
                times = 1;
            }
        };

        setupAndRunGenericTest("-Dhello=world", "-Dkey", "-Dfoo=bar");

        new FullVerifications() {
            {
                configurator.addSystemProperty("hello", "world");
                times = 1;
                configurator.addSystemProperty("foo", "bar");
                times = 1;
                configurator.addSystemProperty("key", "");
                times = 1;
            }
        };
    }


    @Test(enabled=false)
    public void testConfig() throws Exception {

        setupAndRunTest("-i", "swagger.yaml", "-l", "java", "-o", "src/main/java", true,
                "config.json", "-c", "config.json");

        new FullVerifications() {
            {
            }
        };

        setupAndRunTest("-i", "swagger.yaml", "-l", "java", "-o", "src/main/java", true,
                "config.json", "--config", "config.json");

        new FullVerifications() {
            {
            }
        };
    }

    @Test(enabled=false)
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

    @Test(enabled=false)
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

    @Test(enabled=false)
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

    @Test(enabled=false)
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

    @Test(enabled=false)
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

    @Test(enabled=false)
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

    @Test(enabled=false)
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

    @Test(enabled=false)
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

    @Test(enabled=false)
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

    @Test(enabled=false)
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

    @Test(enabled=false)
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

    @Test(enabled=false)
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

    @Test(enabled=false)
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

        SwaggerCodegen.main(argsToUse);

        new Verifications() {
            {
                configurator.setLang(lang);
                times = 1;
                configurator.setInputSpec(spec);
                times = 1;
                configurator.setOutputDir(outputDir);
            }
        };
    }

    @Test
    public void testExternalArguments() throws Exception {
        String content = IOUtils.toString(getClass().getClassLoader().getResourceAsStream("args.json"));
        JsonNode node = Json.mapper().readTree(content.getBytes());

        Map<String, Object> valueMap = CLIHelper.createOptionValueMap(node);
        Assert.assertNotNull(valueMap);

        Assert.assertTrue(valueMap.containsKey("lang"));
        Assert.assertTrue(valueMap.containsKey("library"));
        Assert.assertTrue(valueMap.containsKey("additionalProperties"));
        Assert.assertTrue(valueMap.containsKey("spec"));
        Assert.assertTrue(valueMap.containsKey("output"));


        Assert.assertEquals(valueMap.get("lang"), "java");
        Assert.assertEquals(valueMap.get("library"), "jersey2");
        Assert.assertNotNull(valueMap.get("additionalProperties"));
        Assert.assertTrue(valueMap.get("additionalProperties") instanceof ArrayList);

        List<String> properties = (List<String>) valueMap.get("additionalProperties");
        Assert.assertEquals(properties.size(), 2);
        Assert.assertEquals(properties.get(0), "serializableModel=true");
        Assert.assertEquals(properties.get(1), "withXml=true");


        content = IOUtils.toString(getClass().getClassLoader().getResourceAsStream("args.yaml"));
        node = Yaml.mapper().readTree(content.getBytes());

        valueMap = CLIHelper.createOptionValueMap(node);
        Assert.assertNotNull(valueMap);

        Assert.assertTrue(valueMap.containsKey("lang"));
        Assert.assertTrue(valueMap.containsKey("library"));
        Assert.assertTrue(valueMap.containsKey("additionalProperties"));
        Assert.assertTrue(valueMap.containsKey("spec"));
        Assert.assertTrue(valueMap.containsKey("output"));


        Assert.assertEquals(valueMap.get("lang"), "java");
        Assert.assertEquals(valueMap.get("library"), "jersey2");
        Assert.assertNotNull(valueMap.get("additionalProperties"));
        Assert.assertTrue(valueMap.get("additionalProperties") instanceof ArrayList);

        properties = (List<String>) valueMap.get("additionalProperties");
        Assert.assertEquals(properties.size(), 2);
        Assert.assertEquals(properties.get(0), "serializableModel=true");
        Assert.assertEquals(properties.get(1), "withXml=true");
    }

    @Test
    public void testExternalArgumentsFromFile() throws Exception {
        String location = "src/test/resources/args.json";

        Generate generate = new Generate();
        generate.setUrl(location);

        generate.run();

        Assert.assertEquals(generate.lang, "java");
        Assert.assertEquals(generate.library, "jersey2");
        Assert.assertEquals(generate.additionalProperties.size(), 2);
        Assert.assertEquals(generate.additionalProperties.get(0), "serializableModel=true");
        Assert.assertEquals(generate.additionalProperties.get(1), "withXml=true");

        location = "src/test/resources/args.yaml";

        generate = new Generate();
        generate.setUrl(location);

        generate.run();

        Assert.assertEquals(generate.lang, "java");
        Assert.assertEquals(generate.library, "jersey2");
        Assert.assertEquals(generate.additionalProperties.size(), 2);
        Assert.assertEquals(generate.additionalProperties.get(0), "serializableModel=true");
        Assert.assertEquals(generate.additionalProperties.get(1), "withXml=true");
    }

    private void setupAndRunGenericTest(String... additionalParameters) {
        setupAndRunTest("-i", "swagger.yaml", "-l", "java", "-o", "src/main/java", false, null,
                additionalParameters);
    }
}
