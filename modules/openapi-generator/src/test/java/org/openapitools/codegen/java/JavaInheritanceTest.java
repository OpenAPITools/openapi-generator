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

package org.openapitools.codegen.java;

import com.google.common.collect.Sets;
import io.swagger.v3.oas.models.Components;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.media.ComposedSchema;
import io.swagger.v3.oas.models.media.Discriminator;
import io.swagger.v3.oas.models.media.ObjectSchema;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.media.StringSchema;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.CodegenModel;
import org.openapitools.codegen.CodegenProperty;
import org.openapitools.codegen.DefaultCodegen;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.languages.JavaClientCodegen;
import org.testng.Assert;
import org.testng.annotations.Test;

public class JavaInheritanceTest {


    @Test(description = "convert a composed model with parent")
    public void javaInheritanceTest() {
        final Schema parentModel = new Schema().name("Base");

        final Schema schema = new ComposedSchema()
                .addAllOfItem(new Schema().$ref("Base"))
                .name("composed");

        OpenAPI openAPI = TestUtils.createOpenAPI();
        openAPI.setComponents(new Components()
                .addSchemas(parentModel.getName(),parentModel)
                .addSchemas(schema.getName(), schema)
        );

        final JavaClientCodegen codegen = new JavaClientCodegen();
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", schema);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.parent, "Base");
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

        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("Base", base);

        final JavaClientCodegen codegen = new JavaClientCodegen();
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", schema);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.parent, "Base");
        Assert.assertEquals(cm.imports, Sets.newHashSet("Base"));
    }

    @Test(description = "composed model has the required attributes on the child")
    public void javaInheritanceWithRequiredAttributesOnAllOfObject() {
        Schema parent = new ObjectSchema()
                .addProperties("a", new StringSchema())
                .addProperties("b", new StringSchema())
                .addRequiredItem("a")
                .name("Parent");
        Schema child = new ComposedSchema()
                .addAllOfItem(new Schema().$ref("Parent"))
                .addAllOfItem(new ObjectSchema()
                        .addProperties("c", new StringSchema())
                        .addProperties("d", new StringSchema())
                        .addRequiredItem("a")
                        .addRequiredItem("c"))
                .name("Child");
        OpenAPI openAPI = TestUtils.createOpenAPI();
        openAPI.getComponents().addSchemas(parent.getName(), parent);
        openAPI.getComponents().addSchemas(child.getName(), child);

        final DefaultCodegen codegen = new JavaClientCodegen();
        codegen.setOpenAPI(openAPI);

        final CodegenModel pm = codegen
                .fromModel("Parent", parent);
        final CodegenProperty propertyPA = pm.allVars.get(0);
        Assert.assertEquals(propertyPA.name, "a");
        Assert.assertTrue(propertyPA.required);
        final CodegenProperty propertyPB = pm.allVars.get(1);
        Assert.assertEquals(propertyPB.name, "b");
        Assert.assertFalse(propertyPB.required);
        Assert.assertEquals(pm.requiredVars.size() + pm.optionalVars.size(), pm.allVars.size());

        final CodegenModel cm = codegen
                .fromModel("Child", child);
        final CodegenProperty propertyCA = cm.allVars.get(0);
        Assert.assertEquals(propertyCA.name, "a");
        Assert.assertTrue(propertyCA.required);
        final CodegenProperty propertyCB = cm.allVars.get(1);
        Assert.assertEquals(propertyCB.name, "b");
        Assert.assertFalse(propertyCB.required);
        final CodegenProperty propertyCC = cm.allVars.get(2);
        Assert.assertEquals(propertyCC.name, "c");
        Assert.assertTrue(propertyCC.required);
        final CodegenProperty propertyCD = cm.allVars.get(3);
        Assert.assertEquals(propertyCD.name, "d");
        Assert.assertFalse(propertyCD.required);
        Assert.assertEquals(cm.requiredVars.size() + cm.optionalVars.size(), cm.allVars.size());
    }

    @Test(description = "composed model has the required attributes for both parent & child")
    public void javaInheritanceWithRequiredAttributesOnComposedObject() {
        Schema parent = new ObjectSchema()
                .addProperties("a", new StringSchema())
                .addProperties("b", new StringSchema())
                .addRequiredItem("a")
                .name("Parent");
        Schema child = new ComposedSchema()
                .addAllOfItem(new Schema().$ref("Parent"))
                .addAllOfItem(new ObjectSchema()
                        .addProperties("c", new StringSchema())
                        .addProperties("d", new StringSchema()))
                .name("Child")
                .addRequiredItem("a")
                .addRequiredItem("c");
        OpenAPI openAPI = TestUtils.createOpenAPI();
        openAPI.getComponents().addSchemas(parent.getName(), parent);
        openAPI.getComponents().addSchemas(child.getName(), child);

        final DefaultCodegen codegen = new JavaClientCodegen();
        codegen.setOpenAPI(openAPI);

        final CodegenModel pm = codegen
                .fromModel("Parent", parent);
        final CodegenProperty propertyPA = pm.allVars.get(0);
        Assert.assertEquals(propertyPA.name, "a");
        Assert.assertTrue(propertyPA.required);
        final CodegenProperty propertyPB = pm.allVars.get(1);
        Assert.assertEquals(propertyPB.name, "b");
        Assert.assertFalse(propertyPB.required);
        Assert.assertEquals(pm.requiredVars.size() + pm.optionalVars.size(), pm.allVars.size());

        final CodegenModel cm = codegen
                .fromModel("Child", child);
        final CodegenProperty propertyCA = cm.allVars.get(0);
        Assert.assertEquals(propertyCA.name, "a");
        Assert.assertTrue(propertyCA.required);
        final CodegenProperty propertyCB = cm.allVars.get(1);
        Assert.assertEquals(propertyCB.name, "b");
        Assert.assertFalse(propertyCB.required);
        final CodegenProperty propertyCC = cm.allVars.get(2);
        Assert.assertEquals(propertyCC.name, "c");
        Assert.assertTrue(propertyCC.required);
        final CodegenProperty propertyCD = cm.allVars.get(3);
        Assert.assertEquals(propertyCD.name, "d");
        Assert.assertFalse(propertyCD.required);
        Assert.assertEquals(cm.requiredVars.size() + cm.optionalVars.size(), cm.allVars.size());
    }
}
