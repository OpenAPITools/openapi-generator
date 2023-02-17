/*
 * Copyright 2022 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 * Copyright (c) 2022 Oracle and/or its affiliates
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
package org.openapitools.codegen.java.helidon;

import org.openapitools.codegen.ClientOptInput;
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.List;

import static org.openapitools.codegen.java.assertions.JavaFileAssert.assertThat;

public class JavaHelidonSeClientCodegenTest {

    private String outputPath;
    private List<File> generatedFiles;

    @BeforeClass
    public void setup() throws IOException {
        File output = Files.createTempDirectory("test").toFile();
        output.deleteOnExit();
        outputPath = output.getAbsolutePath().replace('\\', '/');

        System.out.println("Generating java-helidon-client SE project in " + outputPath);

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("java-helidon-client")
                .setLibrary("se")
                .setInputSpec("src/test/resources/3_0/helidon/petstore-no-multipart-for-testing.yaml")
                .setOutputDir(outputPath);

        final ClientOptInput clientOptInput = configurator.toClientOptInput();
        DefaultGenerator generator = new DefaultGenerator();
        generator.opts(clientOptInput);
        generatedFiles = generator.generate();
    }

    @DataProvider(name = "fileSuffix")
    public Object[][] fileSuffixes() {
        return new Object[][] {
                {""},
                {"Impl"}
        };
    }

    @Test
    public void testPom() {
        TestUtils.ensureContainsFile(generatedFiles, new File(outputPath), "pom.xml");
    }

    @Test(dataProvider = "fileSuffix")
    public void testPetApi(String fileSuffix) {
        assertThat(Paths.get(outputPath + "/src/main/java/org/openapitools/client/api/PetApi" + fileSuffix + ".java"))
                .assertMethod("addPet", "Pet")
                .toFileAssert()
                .assertMethod("deletePet", "Long", "String", "Long", "String", "Integer",
                        "List<Integer>", "List<String>")
                .toFileAssert()
                .assertMethod("findPetsByStatus", "List<String>")
                .toFileAssert()
                .assertMethod("findPetsByTags", "List<Integer>")
                .toFileAssert()
                .assertMethod("getPetById", "Long")
                .toFileAssert()
                .assertMethod("updatePet", "Pet")
                .toFileAssert()
                .assertMethod("updatePetWithForm", "Long", "String", "String")
                .toFileAssert();
    }

    @Test(dataProvider = "fileSuffix")
    public void testStoreApi(String fileSuffix) {
        assertThat(Paths.get(outputPath + "/src/main/java/org/openapitools/client/api/StoreApi" + fileSuffix + ".java"))
                .assertMethod("deleteOrder", "String")
                .toFileAssert()
                .assertMethod("getInventory")
                .toFileAssert()
                .assertMethod("getOrderById", "BigDecimal")
                .toFileAssert()
                .assertMethod("placeOrder", "Order")
                .toFileAssert();
    }

    @Test(dataProvider = "fileSuffix")
    public void testUserApi(String fileSuffix) {
        assertThat(Paths.get(outputPath + "/src/main/java/org/openapitools/client/api/UserApi" + fileSuffix + ".java"))
                .assertMethod("createUser", "User")
                .toFileAssert()
                .assertMethod("createUsersWithArrayInput", "List<User>")
                .toFileAssert()
                .assertMethod("createUsersWithListInput", "List<User>")
                .toFileAssert()
                .assertMethod("getUserByName", "String")
                .toFileAssert()
                .assertMethod("loginUser", "String", "String", "String", "Long", "BigDecimal")
                .toFileAssert()
                .assertMethod("updateUser", "String", "User")
                .toFileAssert();
    }
}
