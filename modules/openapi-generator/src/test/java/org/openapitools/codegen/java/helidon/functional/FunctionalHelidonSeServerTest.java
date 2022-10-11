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

package org.openapitools.codegen.java.helidon.functional;

import java.nio.file.Files;
import java.nio.file.Path;

import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.not;
import static org.hamcrest.MatcherAssert.assertThat;

public class FunctionalHelidonSeServerTest extends FunctionalBase {

    @BeforeClass
    public void setup() {
        library("se");
        generatorName("java-helidon-server");
    }

    @Test
    void buildPetstoreWithDefaultOptions() {
        generate("src/test/resources/3_0/petstore.yaml");
        buildAndVerify("target/openapi-java-server.jar");
    }

    @Test
    void buildPetstoreWithAbstractClasses() {
        inputSpec("src/test/resources/3_0/petstore.yaml");
        generate(createConfigurator().addAdditionalProperty(FunctionalBase.USE_ABSTRACT_CLASS, "true"));
        buildAndVerify("target/openapi-java-server.jar");
    }

    @Test
    void verifyFullProject() {
        inputSpec("src/test/resources/3_0/petstore.yaml");

        // Generate project for first time and record pom's timestamp
        generate(createConfigurator());
        buildAndVerify("target/openapi-java-server.jar");
        Path pom1 = outputPath.resolve("pom.xml");
        assertThat(Files.exists(pom1), is(true));
        long lastModified = pom1.toFile().lastModified();

        // Re-generate project over same directory with fullProject unspecified
        generate(createConfigurator(outputPath));
        Path pom2 = outputPath.resolve("pom.xml");
        assertThat(Files.exists(pom2), is(true));
        assertThat(pom2.toFile().lastModified(), is(lastModified));         // not overwritten

        // Re-generate project over same directory with fullProject false
        generate(createConfigurator(outputPath).addAdditionalProperty(FULL_PROJECT, "false"));
        Path pom3 = outputPath.resolve("pom.xml");
        assertThat(Files.exists(pom3), is(true));
        assertThat(pom3.toFile().lastModified(), is(lastModified));         // not overwritten

        // Re-generate project over same directory with fullProject true
        generate(createConfigurator(outputPath).addAdditionalProperty(FULL_PROJECT, "true"));
        Path pom4 = outputPath.resolve("pom.xml");
        assertThat(Files.exists(pom4), is(true));
        assertThat(pom4.toFile().lastModified(), is(not(lastModified)));    // overwritten
    }
}
