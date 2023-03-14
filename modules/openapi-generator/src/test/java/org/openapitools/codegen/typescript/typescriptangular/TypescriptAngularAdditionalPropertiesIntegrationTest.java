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

package org.openapitools.codegen.typescript.typescriptangular;

import org.openapitools.codegen.AbstractIntegrationTest;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.languages.TypeScriptAngularClientCodegen;
import org.openapitools.codegen.testutils.IntegrationTestPathsConfig;
import org.openapitools.codegen.typescript.TypeScriptGroups;
import org.testng.annotations.Test;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

@Test(groups = {TypeScriptGroups.TYPESCRIPT, TypeScriptGroups.TYPESCRIPT_ANGULAR})
public class TypescriptAngularAdditionalPropertiesIntegrationTest extends AbstractIntegrationTest {

    {
        // when running repeatedly from an IDE, some files do not get overwritten
        // and thus are missing from the FILES metadata.
        // Since metadata-verification is not the core responsibility of this test: disable it.
        generateMetadata = false;
    }

    @Override
    protected CodegenConfig getCodegenConfig() {
        return new TypeScriptAngularClientCodegen();
    }

    @Override
    protected Map<String, String> configProperties() {
        Map<String, String> properties = new HashMap<>();
        properties.put("npmName", "additionalPropertiesTest");
        properties.put("npmVersion", "1.0.2");
        properties.put("snapshot", "false");

        return properties;
    }

    @Override
    protected IntegrationTestPathsConfig getIntegrationTestPathsConfig() {
        return new IntegrationTestPathsConfig("typescript/additional-properties");
    }

    @Test(enabled = false)
    @Override
    public void generatesCorrectDirectoryStructure() throws IOException {
        // test are currently disabled in Superclass
        super.generatesCorrectDirectoryStructure();
    }
}
