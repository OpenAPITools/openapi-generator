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

package org.openapitools.codegen.php;

import org.assertj.core.api.Assertions;
import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.openapitools.codegen.languages.PhpLaravelServerCodegen;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.util.List;

public class PhpLaravelServerCodegenTest {

    @Test
    public void testInitialConfigValues() throws Exception {
        final PhpLaravelServerCodegen codegen = new PhpLaravelServerCodegen();
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.TRUE);
        Assert.assertEquals(codegen.isHideGenerationTimestamp(), true);
    }

    @Test
    public void testSettersForConfigValues() throws Exception {
        final PhpLaravelServerCodegen codegen = new PhpLaravelServerCodegen();
        codegen.setHideGenerationTimestamp(false);
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.FALSE);
        Assert.assertEquals(codegen.isHideGenerationTimestamp(), false);
    }

    @Test
    public void testAdditionalPropertiesPutForConfigValues() throws Exception {
        final PhpLaravelServerCodegen codegen = new PhpLaravelServerCodegen();
        codegen.additionalProperties().put(CodegenConstants.HIDE_GENERATION_TIMESTAMP, false);
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.FALSE);
        Assert.assertEquals(codegen.isHideGenerationTimestamp(), false);
    }

    /**
     * Transitional test, just to check the file is still in the list after the fix
     */
    @Test void SupportingFilesContainFailedJobsTableMigrationIn() throws IOException {
        var tempDir = Files.createTempDirectory("temp");
        tempDir.toFile().deleteOnExit();
        final CodegenConfigurator configurator = new CodegenConfigurator()
            .setGeneratorName("php-laravel")
            .setInputSpec("src/test/resources/3_0/pingSomeObj.yaml")
            .setOutputDir(tempDir.toString().replace("\\", "/"));

        List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();
        
        Assertions.assertThat(files).contains(
            tempDir.resolve( "lib/database/migrations/2019_08_19_000000_create_failed_jobs_table.php").toFile()
        );
    }
}
