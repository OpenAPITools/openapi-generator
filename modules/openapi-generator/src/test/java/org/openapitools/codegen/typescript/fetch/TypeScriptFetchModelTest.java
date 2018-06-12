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

package org.openapitools.codegen.typescript.fetch;

import com.google.common.collect.Sets;

import io.swagger.parser.OpenAPIParser;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.DateSchema;
import io.swagger.v3.oas.models.media.DateTimeSchema;
import io.swagger.v3.oas.models.media.IntegerSchema;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.media.StringSchema;
import io.swagger.v3.parser.core.models.ParseOptions;
import io.swagger.v3.parser.util.SchemaTypeUtil;

import org.openapitools.codegen.CodegenModel;
import org.openapitools.codegen.CodegenProperty;
import org.openapitools.codegen.DefaultCodegen;
import org.openapitools.codegen.languages.TypeScriptFetchClientCodegen;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;

/*
import static io.swagger.codegen.CodegenConstants.IS_ENUM_EXT_NAME;
import static io.swagger.codegen.languages.helpers.ExtensionHelper.getBooleanValue;
import static io.swagger.codegen.utils.ModelUtils.updateCodegenPropertyEnum;
*/

@SuppressWarnings("static-method")
public class TypeScriptFetchModelTest {

    @Test(description = "convert a simple TypeScript Angular model")
    public void simpleModelTest() {
        final Schema model = new Schema()
                .description("a sample model")
                .addProperties("id", new IntegerSchema().format(SchemaTypeUtil.INTEGER64_FORMAT))
                .addProperties("name", new StringSchema())
                .addProperties("createdAt", new DateTimeSchema())
                .addProperties("birthDate", new DateSchema())
                .addRequiredItem("id")
                .addRequiredItem("name");

        final DefaultCodegen codegen = new TypeScriptFetchClientCodegen();
        final CodegenModel cm = codegen.fromModel("sample", model, Collections.singletonMap("sample", model));

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.description, "a sample model");
        Assert.assertEquals(cm.vars.size(), 4);

        final CodegenProperty property1 = cm.vars.get(0);
        Assert.assertEquals(property1.baseName, "id");
        Assert.assertEquals(property1.dataType, "number");
        Assert.assertEquals(property1.name, "id");
        Assert.assertEquals(property1.defaultValue, "undefined");
        Assert.assertEquals(property1.baseType, "number");
        Assert.assertTrue(property1.hasMore);
        Assert.assertTrue(property1.required);
        Assert.assertTrue(property1.isNotContainer);

        final CodegenProperty property2 = cm.vars.get(1);
        Assert.assertEquals(property2.baseName, "name");
        Assert.assertEquals(property2.dataType, "string");
        Assert.assertEquals(property2.name, "name");
        Assert.assertEquals(property2.defaultValue, "undefined");
        Assert.assertEquals(property2.baseType, "string");
        Assert.assertTrue(property2.hasMore);
        Assert.assertTrue(property2.required);
        Assert.assertTrue(property2.isNotContainer);

        final CodegenProperty property3 = cm.vars.get(2);
        Assert.assertEquals(property3.baseName, "createdAt");
        Assert.assertEquals(property3.complexType, null);
        Assert.assertEquals(property3.dataType, "Date");
        Assert.assertEquals(property3.name, "createdAt");
        Assert.assertEquals(property3.defaultValue, "undefined");
        Assert.assertTrue(property3.hasMore);
        Assert.assertFalse(property3.required);
        Assert.assertTrue(property3.isNotContainer);

        final CodegenProperty property4 = cm.vars.get(3);
        Assert.assertEquals(property4.baseName, "birthDate");
        Assert.assertEquals(property4.complexType, null);
        Assert.assertEquals(property4.dataType, "string");
        Assert.assertEquals(property4.name, "birthDate");
        Assert.assertEquals(property4.defaultValue, "undefined");
        Assert.assertFalse(property4.hasMore);
        Assert.assertFalse(property4.required);
        Assert.assertTrue(property4.isNotContainer);
    }

    @Test(description = "convert a model with list property")
    public void listPropertyTest() {
        final Schema model = new Schema()
                .description("a sample model")
                .addProperties("id", new IntegerSchema().format(SchemaTypeUtil.INTEGER64_FORMAT))
                .addProperties("urls", new ArraySchema().items(new StringSchema()))
                .addRequiredItem("id");
        final DefaultCodegen codegen = new TypeScriptFetchClientCodegen();
        final CodegenModel cm = codegen.fromModel("sample", model, Collections.singletonMap("sample", model));

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.description, "a sample model");
        Assert.assertEquals(cm.vars.size(), 2);

        final CodegenProperty property1 = cm.vars.get(0);
        Assert.assertEquals(property1.baseName, "id");
        Assert.assertEquals(property1.dataType, "number");
        Assert.assertEquals(property1.name, "id");
        Assert.assertEquals(property1.defaultValue, "undefined");
        Assert.assertEquals(property1.baseType, "number");
        Assert.assertTrue(property1.hasMore);
        Assert.assertTrue(property1.required);
        Assert.assertTrue(property1.isNotContainer);

        final CodegenProperty property2 = cm.vars.get(1);
        Assert.assertEquals(property2.baseName, "urls");
        Assert.assertEquals(property2.dataType, "Array<string>");
        Assert.assertEquals(property2.name, "urls");
        Assert.assertEquals(property2.baseType, "Array");
        Assert.assertFalse(property2.hasMore);
        Assert.assertFalse(property2.required);
    }

    @Test(description = "convert a model with complex property")
    public void complexPropertyTest() {
        final Schema model = new Schema()
                .description("a sample model")
                .addProperties("children", new Schema().$ref("#/definitions/Children"));
        final DefaultCodegen codegen = new TypeScriptFetchClientCodegen();
        final CodegenModel cm = codegen.fromModel("sample", model, Collections.singletonMap("sample", model));

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.description, "a sample model");
        Assert.assertEquals(cm.vars.size(), 1);

        final CodegenProperty property1 = cm.vars.get(0);
        Assert.assertEquals(property1.baseName, "children");
        Assert.assertEquals(property1.dataType, "Children");
        Assert.assertEquals(property1.name, "children");
        Assert.assertEquals(property1.defaultValue, "undefined");
        Assert.assertEquals(property1.baseType, "Children");
        Assert.assertFalse(property1.required);
    }

    @Test(description = "convert a model with complex list property")
    public void complexListPropertyTest() {
        final Schema model = new Schema()
                .description("a sample model")
                .addProperties("children", new ArraySchema()
                        .items(new Schema().$ref("#/definitions/Children")));
        final DefaultCodegen codegen = new TypeScriptFetchClientCodegen();
        final CodegenModel cm = codegen.fromModel("sample", model, Collections.singletonMap("sample", model));

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.description, "a sample model");
        Assert.assertEquals(cm.vars.size(), 1);

        final CodegenProperty property1 = cm.vars.get(0);
        Assert.assertEquals(property1.baseName, "children");
        Assert.assertEquals(property1.complexType, "Children");
        Assert.assertEquals(property1.dataType, "Array<Children>");
        Assert.assertEquals(property1.name, "children");
        Assert.assertEquals(property1.baseType, "Array");
        Assert.assertFalse(property1.required);
    }

    @Test(description = "convert an array model")
    public void arrayModelTest() {
        final Schema model = new ArraySchema()
                .items(new Schema().$ref("#/definitions/Children"))
                .description("an array model");
        final DefaultCodegen codegen = new TypeScriptFetchClientCodegen();
        final CodegenModel cm = codegen.fromModel("sample", model, Collections.singletonMap("sample", model));

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.description, "an array model");
        Assert.assertEquals(cm.vars.size(), 0);
    }

    @Test(description = "convert a map model")
    public void mapModelTest() {
        final Schema model = new Schema()
                .description("a map model")
                .additionalProperties(new Schema().$ref("#/definitions/Children"));
        final DefaultCodegen codegen = new TypeScriptFetchClientCodegen();
        final CodegenModel cm = codegen.fromModel("sample", model, Collections.singletonMap("sample", model));

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.description, "a map model");
        Assert.assertEquals(cm.vars.size(), 0);
        Assert.assertEquals(cm.imports.size(), 1);
        Assert.assertEquals(Sets.intersection(cm.imports, Sets.newHashSet("Children")).size(), 1);
    }

    @Test(description = "test enum array model")
    public void enumArrayMdoelTest() {
        // TODO: update yaml file.
        final OpenAPI openAPI = new OpenAPIParser().readLocation("src/test/resources/2_0/petstore-with-fake-endpoints-models-for-testing.yaml", null, new ParseOptions()).getOpenAPI();
        final DefaultCodegen codegen = new TypeScriptFetchClientCodegen();
        final Schema schema = openAPI.getComponents().getSchemas().get("EnumArrays");

        Schema property = (Schema) schema.getProperties().get("array_enum");
        CodegenProperty prope = codegen.fromProperty("array_enum", property);
        codegen.updateCodegenPropertyEnum(prope);
        Assert.assertEquals(prope.datatypeWithEnum, "Array<ArrayEnumEnum>");
        Assert.assertEquals(prope.enumName, "ArrayEnumEnum");
        Assert.assertTrue(prope.isEnum);
        Assert.assertEquals(prope.allowableValues.get("values"), Arrays.asList("fish", "crab"));

        HashMap<String, String> fish = new HashMap<String, String>();
        fish.put("name", "Fish");
        fish.put("value", "'fish'");
        HashMap<String, String> crab = new HashMap<String, String>();
        crab.put("name", "Crab");
        crab.put("value", "'crab'");
        Assert.assertEquals(prope.allowableValues.get("enumVars"), Arrays.asList(fish, crab));

        // assert inner items
        Assert.assertEquals(prope.datatypeWithEnum, "Array<ArrayEnumEnum>");
        Assert.assertEquals(prope.enumName, "ArrayEnumEnum");
        Assert.assertTrue(prope.items.isEnum);
        Assert.assertEquals(prope.items.allowableValues.get("values"), Arrays.asList("fish", "crab"));
        Assert.assertEquals(prope.items.allowableValues.get("enumVars"), Arrays.asList(fish, crab));

        //IMPORTANT: these are not final enum values, which may be further updated
        //by postProcessModels

    }

    @Test(description = "test enum model for values (numeric, string, etc)")
    public void enumMdoelValueTest() {
        final OpenAPI openAPI = new OpenAPIParser().readLocation("src/test/resources/2_0/petstore-with-fake-endpoints-models-for-testing.yaml", null, new ParseOptions()).getOpenAPI();
        final DefaultCodegen codegen = new TypeScriptFetchClientCodegen();
        final Schema schema = openAPI.getComponents().getSchemas().get("Enum_Test");

        Schema property = (Schema) schema.getProperties().get("enum_integer");
        CodegenProperty prope = codegen.fromProperty("enum_integer", property);
        codegen.updateCodegenPropertyEnum(prope);
        Assert.assertEquals(prope.datatypeWithEnum, "EnumIntegerEnum");
        Assert.assertEquals(prope.enumName, "EnumIntegerEnum");
        Assert.assertTrue(prope.isEnum);
        Assert.assertFalse(prope.isContainer);
        Assert.assertNull(prope.items);
        Assert.assertEquals(prope.allowableValues.get("values"), Arrays.asList(1, -1));

        HashMap<String, String> one = new HashMap<String, String>();
        one.put("name", "NUMBER_1");
        one.put("value", "1");
        HashMap<String, String> minusOne = new HashMap<String, String>();
        minusOne.put("name", "NUMBER_MINUS_1");
        minusOne.put("value", "-1");
        Assert.assertEquals(prope.allowableValues.get("enumVars"), Arrays.asList(one, minusOne));

        //IMPORTANT: these are not final enum values, which may be further updated
        //by postProcessModels

    }

}
