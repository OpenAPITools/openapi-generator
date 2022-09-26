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

package org.openapitools.codegen.csharp;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.media.StringSchema;
import org.openapitools.codegen.CodegenModel;
import org.openapitools.codegen.CodegenProperty;
import org.openapitools.codegen.DefaultCodegen;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.languages.AspNetCoreServerCodegen;
import org.openapitools.codegen.languages.CSharpClientCodegen;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

public class CsharpModelEnumTest {
    // TODO there's no parent/child method in ComposeSchema so we will need to revise the code
    // before we can re-enable the test case below
    @Test(description = "not override identical parent enums", enabled = false)
    public void overrideEnumTest() {
        final StringSchema identicalEnumProperty = new StringSchema();
        identicalEnumProperty.setEnum(Arrays.asList("VALUE1", "VALUE2", "VALUE3"));

        final StringSchema subEnumProperty = new StringSchema();
        subEnumProperty.setEnum(Arrays.asList("SUB1", "SUB2", "SUB3"));

        // Add one enum property to the parent
        final Map<String, Schema> parentProperties = new HashMap<String, Schema>();
        parentProperties.put("sharedThing", identicalEnumProperty);

        // Add TWO enums to the subType model; one of which is identical to the one in parent class
        final Map<String, Schema> subProperties = new HashMap<String, Schema>();
        subProperties.put("sharedThing", identicalEnumProperty);
        subProperties.put("unsharedThing", identicalEnumProperty);

        final Schema parentModel = new Schema()
                .description("parentModel");
        parentModel.setProperties(parentProperties);
        parentModel.name("parentModel");

        final Schema subModel = new Schema()
                .description("subModel");
        subModel.setProperties(subProperties);
        subModel.name("subModel");

        /* TODO revise the following as there's parent/child method
        final ComposedSchema model = new ComposedSchema().
                .parent(new RefModel(parentModel.getName()))
                .child(subModel)
                .interfaces(new ArrayList<RefModel>());
                */
        final DefaultCodegen codegen = new CSharpClientCodegen();
        final Map<String, Schema> allModels = new HashMap<>();
        allModels.put("ParentModel", parentModel);
        allModels.put("SubModel", subModel);

        /*
        codegen.setOpenAPI(allModels);
        final CodegenModel cm = codegen.fromModel("sample", model);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.parent, "ParentModel");
        Assert.assertTrue(cm.imports.contains("ParentModel"));

        // Assert that only the unshared/uninherited enum remains
        Assert.assertEquals(cm.vars.size(), 1);
        final CodegenProperty enumVar = cm.vars.get(0);
        Assert.assertEquals(enumVar.baseName, "unsharedThing");
        Assert.assertEquals(enumVar.datatype, "string");
        Assert.assertEquals(enumVar.datatypeWithEnum, "UnsharedThingEnum");
        Assert.assertTrue(enumVar.isEnum);
        */
    }

    @Test(description = "use custom suffixes for enums")
    public void useCustomEnumSuffixes() {
        final AspNetCoreServerCodegen codegen = new AspNetCoreServerCodegen();
        codegen.setEnumNameSuffix("EnumName");
        codegen.setEnumValueSuffix("EnumValue");

        OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/petstore.yaml");
        codegen.setOpenAPI(openAPI);

        final Schema petSchema = openAPI.getComponents().getSchemas().get("Pet");
        final CodegenModel cm = codegen.fromModel("Pet", petSchema);
        final CodegenProperty statusProperty = cm.vars.get(5);
        Assert.assertEquals(statusProperty.name, "Status");
        Assert.assertTrue(statusProperty.isEnum);
        Assert.assertEquals(statusProperty.datatypeWithEnum, "StatusEnumName");

        Assert.assertEquals(codegen.toEnumVarName("Aaaa", ""), "AaaaEnumValue");
    }

    @Test(description = "use default suffixes for enums")
    public void useDefaultEnumSuffixes() {
        final AspNetCoreServerCodegen codegen = new AspNetCoreServerCodegen();

        OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/petstore.yaml");
        codegen.setOpenAPI(openAPI);

        final Schema petSchema = openAPI.getComponents().getSchemas().get("Pet");
        final CodegenModel cm = codegen.fromModel("Pet", petSchema);
        final CodegenProperty statusProperty = cm.vars.get(5);
        Assert.assertEquals(statusProperty.name, "Status");
        Assert.assertTrue(statusProperty.isEnum);
        Assert.assertEquals(statusProperty.datatypeWithEnum, "StatusEnum");

        Assert.assertEquals(codegen.toEnumVarName("Aaaa", ""), "AaaaEnum");
    }

    @Test(description = "support empty suffixes for enums")
    public void useEmptyEnumSuffixes() {
        final AspNetCoreServerCodegen codegen = new AspNetCoreServerCodegen();
        codegen.setEnumNameSuffix("");
        codegen.setEnumValueSuffix("");

        OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/petstore.yaml");
        codegen.setOpenAPI(openAPI);

        final Schema petSchema = openAPI.getComponents().getSchemas().get("Pet");
        final CodegenModel cm = codegen.fromModel("Pet", petSchema);
        final CodegenProperty statusProperty = cm.vars.get(5);
        Assert.assertEquals(statusProperty.name, "Status");
        Assert.assertTrue(statusProperty.isEnum);
        Assert.assertEquals(statusProperty.datatypeWithEnum, "Status");

        Assert.assertEquals(codegen.toEnumVarName("Aaaa", ""), "Aaaa");
    }

}
