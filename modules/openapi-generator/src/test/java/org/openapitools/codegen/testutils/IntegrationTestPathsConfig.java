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

package org.openapitools.codegen.testutils;

import java.nio.file.Path;
import java.nio.file.Paths;

public class IntegrationTestPathsConfig {
    private static final Path INTEGRATION_TEST_PATH = Paths.get("target/test-classes/integrationtests").toAbsolutePath();
    private final Path outputPath;
    private final Path specPath;
    private final Path expectedPath;
    private final Path ignoreFilePath;

    public IntegrationTestPathsConfig(String location) {
        this(location + "-spec.json", location + "-result", location + "-expected", location + ".ignore");
    }

    public IntegrationTestPathsConfig(String specLocation, String outputLocation, String expectedLocation, String ignoreFileLocation) {
        outputPath = INTEGRATION_TEST_PATH.resolve(outputLocation);
        expectedPath = INTEGRATION_TEST_PATH.resolve(expectedLocation);
        specPath = INTEGRATION_TEST_PATH.resolve(specLocation);
        ignoreFilePath = INTEGRATION_TEST_PATH.resolve(ignoreFileLocation);
    }

    public Path getOutputPath() {
        return outputPath;
    }

    public Path getSpecPath() {
        return specPath;
    }

    public Path getExpectedPath() {
        return expectedPath;
    }

    public Path getIgnoreFilePath() {
        return ignoreFilePath;
    }
}
