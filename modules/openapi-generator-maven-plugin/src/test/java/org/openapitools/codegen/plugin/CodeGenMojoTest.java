/*
 * Copyright 2020 OpenAPI-Generator Contributors (https://openapi-generator.tech)
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

package org.openapitools.codegen.plugin;

import org.openapitools.codegen.plugin.stubs.StubUtility;

import java.io.File;
import java.util.Map;

public class CodeGenMojoTest extends BaseTestCase {
    @Override
    protected void setUp() throws Exception {
        super.setUp();
    }

    @SuppressWarnings("unchecked")
    public void testCommonConfiguration() throws Exception {
        File testPom = StubUtility.basedPath(getUnitTestDir().toFile(), "common-maven", "common-maven.xml").toFile();
        final CodeGenMojo mojo = (CodeGenMojo) lookupMojo("generate", testPom);
        mojo.execute();
        assertEquals("java", getVariableValueFromObject(mojo, "generatorName"));
        assertEquals("jersey2", getVariableValueFromObject(mojo, "library"));
        assertEquals("remote.org.openapitools.client.api", getVariableValueFromObject(mojo, "apiPackage"));
        assertEquals("remote.org.openapitools.client.model", getVariableValueFromObject(mojo, "modelPackage"));
        assertEquals("remote.org.openapitools.client", getVariableValueFromObject(mojo, "invokerPackage"));

        Map<String, Object> configOptions = (Map<String, Object>) getVariableValueFromObject(mojo, "configOptions");
        assertNotNull(configOptions);
        assertEquals("joda", configOptions.get("dateLibrary"));
    }
}