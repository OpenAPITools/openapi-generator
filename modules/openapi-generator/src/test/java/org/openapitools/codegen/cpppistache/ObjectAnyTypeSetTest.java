/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
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

package org.openapitools.codegen.cpppistache;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.testng.annotations.Test;
import org.testng.asserts.SoftAssert;

import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.Objects;

/**
 * Generate from an input spec containing various abstract objects and sets
 */
public class ObjectAnyTypeSetTest extends AbstractGeneratorsTest {
    /**
     * Logger.
     */
    private static final Logger LOGGER = LoggerFactory.getLogger(ObjectAnyTypeSetTest.class);

    /**
     * A Petstore inputspec with abstract properties added in the Pet
     */
    private static final String INPUT_SPEC = "src/test/resources/3_0/issues-anytype-object-set-petstore-everything.yaml";
    private static final String ISSUE_6726 = "src/test/resources/3_0/issue_6726.yaml";

    /**
     * Soft assert to check all the generators before eventually failing a test
     */
    private final SoftAssert softAssert = new SoftAssert();

    /**
     * Test some generators with an input spec requiring generation of abstract properties
     *
     * @throws IOException if the test folder cannot be created
     */
    @Test
    public void testSomeWithPetstoreWithAbstract() throws IOException {
//        assertGeneratedFiles("c");
//        assertGeneratedFiles("cpp-restsdk");
        generateFiles("cpp-pistache-server", ISSUE_6726);
//        assertGeneratedFiles("typescript");
        this.softAssert.assertAll();
    }

    /**
     * Asserts that a generator has produced some files
     *
     * @param generatorName The generator name to test
     * @param inputSpec     The inputspec to use.
     * @return List of files generated
     * @throws IOException if the test folder cannot be created
     */
    private List<File> generateFiles(String generatorName, String inputSpec) throws IOException {
        Objects.requireNonNull(generatorName, "A generator name is expected for this assertion");
        return oneWith(generatorName, inputSpec);
    }
}
