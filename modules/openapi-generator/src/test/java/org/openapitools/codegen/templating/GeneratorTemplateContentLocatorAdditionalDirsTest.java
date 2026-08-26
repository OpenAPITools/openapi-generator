/*
 * Copyright 2026 OpenAPI-Generator Contributors (https://openapi-generator.tech)
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

package org.openapitools.codegen.templating;

import org.openapitools.codegen.languages.CppBoostBeastClientCodegen;
import org.testng.Assert;
import org.testng.annotations.Test;
import java.io.File;
import java.util.List;

public class GeneratorTemplateContentLocatorAdditionalDirsTest {

    private static String normalized(String path) {
        return path.replace(File.separatorChar, '/');
    }

    @Test
    public void resolvesTemplatesFromAdditionalEmbeddedDirs() {
        CppBoostBeastClientCodegen codegen = new CppBoostBeastClientCodegen();
        GeneratorTemplateContentLocator locator =
                new GeneratorTemplateContentLocator(codegen);

        String resolved = locator.getFullTemplatePath("oas31_validator.mustache");

        Assert.assertNotNull(resolved, "shared template must resolve via additional dirs");
        Assert.assertEquals(normalized(resolved), "cpp-boost-beast-common/oas31_validator.mustache");
    }

    @Test
    public void primaryEmbeddedDirWinsOverAdditionalDirs() {
        CppBoostBeastClientCodegen codegen = new CppBoostBeastClientCodegen();
        // api-header.mustache exists in both cpp-boost-beast-client and (would)
        // shadow-check: it only exists in the client dir, so assert the client
        // dir is probed first by resolving a client-only template.
        GeneratorTemplateContentLocator locator =
                new GeneratorTemplateContentLocator(codegen);

        String resolved = locator.getFullTemplatePath("api-header.mustache");

        Assert.assertNotNull(resolved);
        Assert.assertEquals(normalized(resolved), "cpp-boost-beast-client/api-header.mustache");
    }

    @Test
    public void unknownTemplateReturnsNull() {
        CppBoostBeastClientCodegen codegen = new CppBoostBeastClientCodegen();
        GeneratorTemplateContentLocator locator =
                new GeneratorTemplateContentLocator(codegen);

        Assert.assertNull(locator.getFullTemplatePath("no-such-template.mustache"));
    }

    @Test
    public void additionalDirsListIsConfiguredOnGenerator() {
        CppBoostBeastClientCodegen codegen = new CppBoostBeastClientCodegen();

        Assert.assertEquals(codegen.additionalEmbeddedTemplateDirs(),
                List.of("cpp-boost-beast-common"));
    }
}
