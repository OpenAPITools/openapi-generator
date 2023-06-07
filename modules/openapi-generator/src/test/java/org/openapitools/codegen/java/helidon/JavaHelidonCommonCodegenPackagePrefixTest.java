/*
 * Copyright 2022 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 * Copyright (c) 2022 Oracle and/or its affiliates.
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
package org.openapitools.codegen.java.helidon;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.junit.Assert;
import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

public class JavaHelidonCommonCodegenPackagePrefixTest {

  private static final String INPUT_FILE = "src/test/resources/3_0/helidon/petstore-for-testing.yaml";
  private static final String PACKAGE_PREFIX_KEY = "rootJavaEEPackage";
  private static final String HELIDON_VERSION_KEY = "helidonVersion";

  private static final String EXCEPTION_MESSAGE_FRAGMENT = "namespace but options specified";

  // The generated SE client does not depend on the jakarta/javax imports, so no need to test it.
  private static final List<List<String>> GENERATOR_LIBRARY_PAIRS = new ArrayList<List<String>>() {
    {
      add(listOf("java-helidon-client", "mp"));
      add(listOf("java-helidon-server", "se"));
      add(listOf("java-helidon-server", "mp"));
    }
  };

  private String outputDir;

  @BeforeMethod
  public void setup() throws IOException {
    File output = Files.createTempDirectory("test").toFile();
    output.deleteOnExit();
    outputDir = output.getAbsolutePath().replace('\\', '/');
  }

  @Test(dataProvider = "valid")
  public void checkValidCombinations(String explicitHelidonVersion,
                                     String explicitPrefix,
                                     String expectedPrefix,
                                     String generatorName,
                                     String libraryName) {
    List<File> files = runTest(explicitHelidonVersion, explicitPrefix, generatorName, libraryName);
    checkFileForPackagePrefix(files, generatorName, libraryName, expectedPrefix);
  }

  @Test(dataProvider = "invalid")
  public void checkInvalidCombinations(String explicitHelidonVersion,
                                       String explicitPrefix,
                                       String generatorName,
                                       String libraryName) {
    IllegalArgumentException e = Assert.assertThrows(IllegalArgumentException.class,
        () -> runTest(explicitHelidonVersion, explicitPrefix, generatorName, libraryName));
    Assert.assertTrue("Exception message '" + e.getMessage() + "' contains '" + EXCEPTION_MESSAGE_FRAGMENT + "'",
        e.getMessage().contains(EXCEPTION_MESSAGE_FRAGMENT));
  }

  @DataProvider(name = "valid")
  public Object [][] createValidData() {
    Object [][] settingsForEachRun =  new Object[][] {
        {null, null, "jakarta"},
        {"3.0.1", null, "jakarta"},
        {"2.5.3", null, "javax"},
        {null, "jakarta", "jakarta"},
        {"3.0.1", "jakarta", "jakarta"},
        {"2.5.3", "javax", "javax"}
    };

    return prepareTestData(settingsForEachRun);
  }

  @DataProvider(name = "invalid")
  public Object [][] createInvalidData() {
    Object [][] settingsForEachRun = new Object[][] {
        {"2.5.3", "jakarta"},
        {null, "javax"},
        {"3.0.1", "javax"}
    };

    return prepareTestData(settingsForEachRun);
  }

  /**
   * Creates test data for each tested generator/library pair for all the version/prefix settings.
   *
   * @param settingsForEachRun version/prefix settings to test
   * @return test data for driving a test method
   */
  private Object[][] prepareTestData(Object[][] settingsForEachRun) {
    Object [][] result = new Object[GENERATOR_LIBRARY_PAIRS.size() * settingsForEachRun.length][];
    int resultSlot = 0;

    int settingsLength = settingsForEachRun[0].length;
    for (List<String> generatorLibraryPair : GENERATOR_LIBRARY_PAIRS) {
      for (Object[] settings : settingsForEachRun) {
        result[resultSlot] = Arrays.copyOf(settings, settingsLength + 2);
        result[resultSlot][settingsLength] = generatorLibraryPair.get(0); // generator
        result[resultSlot][settingsLength + 1] = generatorLibraryPair.get(1); // library
        resultSlot++;
      }
    }
    return result;
  }

  private static List<String> listOf(String... values) {
    return new ArrayList<>(Arrays.asList(values));
  }

  private List<File> runTest(String explicitHelidonVersion,
                             String explicitPackagePrefix,
                             String generatorName,
                             String libraryName) {
    Map<String, Object> additionalProperties = new HashMap<>();
    CodegenConfigurator clientConfigurator = new CodegenConfigurator()
        .setGeneratorName(generatorName)
        .setLibrary(libraryName)
        .setInputSpec(INPUT_FILE)
        .setOutputDir(outputDir);

    if (explicitHelidonVersion != null) {
      additionalProperties.put(HELIDON_VERSION_KEY, explicitHelidonVersion);
    }
    if (explicitPackagePrefix != null) {
      additionalProperties.put(PACKAGE_PREFIX_KEY, explicitPackagePrefix);
    }

    // Use JSON-B for serialization to force jakarta or json imports into the generated POJOs.
    additionalProperties.put(CodegenConstants.SERIALIZATION_LIBRARY, "jsonb");

    clientConfigurator.setAdditionalProperties(additionalProperties);

    DefaultGenerator generator = new DefaultGenerator();
    generator.opts(clientConfigurator.toClientOptInput());

    return generator.generate();
  }

  private void checkFileForPackagePrefix(List<File> files,
                                         String generatorName,
                                         String libraryName,
                                         String expectedPrefix) {
    // The SE client does not use the rootJavaEEPackage so we don't check any file in that case.
    if (generatorName.equals("java-helidon-client") && libraryName.equals("se")) {
      return;
    }

    // The MP client and server generator create PetAPI containing a wildcard include.
    if (libraryName.equals("mp")) {
      TestUtils.ensureContainsFile(files, Paths.get(outputDir).toFile(), generatedFilePath(generatorName, libraryName));
      TestUtils.assertFileContains(Paths.get(outputDir + "/" + generatedFilePath(generatorName, libraryName)),
          "import " + expectedPrefix + ".ws.rs.*;");
      return;
    }

    // The SE server generates 'import {{rootJavaEEPackage}}.json.stream.JsonParser;' in POJOs for JSON-B seriolization.
    TestUtils.ensureContainsFile(files, Paths.get(outputDir).toFile(), generatedFilePath(generatorName, libraryName));
    TestUtils.assertFileContains(Paths.get(outputDir + "/" + generatedFilePath(generatorName, libraryName)),
        "import " + expectedPrefix + ".json.stream.JsonParser;");
  }

  private String generatedFilePath(String generatorName, String libraryName) {
    // The path to the file depends on client or server.
    String serverOrClient = (generatorName.contains("server") ? "server" : "client");

    // The file to check depends on the generator: e.g., PetApi for client, PetService for server.
    String apiFileNameSuffix = (generatorName.contains("server") ? "Service" : "Api");

    // For MP, check api/PetApi or api/PetService; for SE check model/Pet.java.
    String filePath = (libraryName.equals("mp")
          ? "api/Pet" + apiFileNameSuffix
          : "model/Pet")
        + ".java";

    return "src/main/java/org/openapitools/"
        + serverOrClient
        + "/"
        + filePath;
  }

}



