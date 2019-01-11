/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 * Copyright 2018 SmartBear Software
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

package org.openapitools.codegen.java;

import com.google.common.collect.Sets;
import io.swagger.v3.oas.models.media.ComposedSchema;
import io.swagger.v3.oas.models.media.Discriminator;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.media.StringSchema;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.CodegenModel;
import org.openapitools.codegen.languages.JavaClientCodegen;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.util.HashMap;
import java.util.Map;

public class JavaInheritanceTest {

    @Test(description = "convert a composed model without discriminator")
    public void javaInheritanceTest() {
        final Schema allOfModel = new Schema().name("Base");

        final Schema schema = new ComposedSchema()
                .addAllOfItem(new Schema().$ref("Base"))
                .name("composed");

        final Map<String, Schema> allSchemas = new HashMap<>();
        allSchemas.put(allOfModel.getName(), allOfModel);
        allSchemas.put(schema.getName(), schema);

        final JavaClientCodegen codegen = new JavaClientCodegen();
        final CodegenModel cm = codegen.fromModel("sample", schema, allSchemas);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.parent, null);
        Assert.assertEquals(cm.imports, Sets.newHashSet("Base"));
    }

    @Test(description = "convert a composed model with discriminator")
    public void javaInheritanceWithDiscriminatorTest() {
        final Schema base = new Schema().name("Base");
        Discriminator discriminator = new Discriminator().mapping("name", StringUtils.EMPTY);
        discriminator.setPropertyName("model_type");
        base.setDiscriminator(discriminator);

        final Schema schema = new ComposedSchema()
                .addAllOfItem(new Schema().$ref("Base"));

        final Map<String, Schema> allDefinitions = new HashMap<String, Schema>();
        allDefinitions.put("Base", base);

        final JavaClientCodegen codegen = new JavaClientCodegen();
        final CodegenModel cm = codegen.fromModel("sample", schema, allDefinitions);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.parent, "Base");
        Assert.assertEquals(cm.imports, Sets.newHashSet("Base"));
    }
}
