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

import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.languages.CppBoostBeastClientCodegen;
import org.openapitools.codegen.languages.CppBoostBeastServerCodegen;
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
        // licenseInfo.mustache exists in BOTH cpp-boost-beast-client (primary)
        // and cpp-boost-beast-common (additional): only a template present in
        // both directories can detect an inverted probe order.
        GeneratorTemplateContentLocator locator =
                new GeneratorTemplateContentLocator(codegen);

        String resolved = locator.getFullTemplatePath("licenseInfo.mustache");

        Assert.assertNotNull(resolved);
        Assert.assertEquals(normalized(resolved), "cpp-boost-beast-client/licenseInfo.mustache");
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

    @Test
    public void schemaIrChunkTemplatesResolveForBothGenerators() {
        // The chunked IR path (large specs, e.g. the OpenAI corpus) is only
        // reachable when every chunk template resolves from the shared
        // common dir: the server's own embedded dir does not carry them.
        for (CodegenConfig config : List.<CodegenConfig>of(
                new CppBoostBeastClientCodegen(), new CppBoostBeastServerCodegen())) {
            GeneratorTemplateContentLocator locator =
                    new GeneratorTemplateContentLocator(config);
            for (int chunk = 0; chunk <= 15; chunk++) {
                String resolved = locator.getFullTemplatePath(
                        "oas31_schema_ir_chunk" + chunk + ".mustache");
                Assert.assertNotNull(resolved,
                        config.getName() + " must resolve chunk " + chunk);
                Assert.assertEquals(normalized(resolved),
                        "cpp-boost-beast-common/oas31_schema_ir_chunk" + chunk + ".mustache",
                        config.getName() + " chunk " + chunk + " must come from the common dir");
            }
        }
    }
}
