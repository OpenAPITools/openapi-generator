package org.openapitools.codegen.typescript;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.media.*;
import org.openapitools.codegen.CodegenModel;
import org.openapitools.codegen.DefaultCodegen;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.languages.TypeScriptClientCodegen;
import org.testng.Assert;
import org.testng.annotations.Test;


@Test(groups = {TypeScriptGroups.TYPESCRIPT})
public class TypeScriptClientModelTest {

    @Test(description = "convert an array oneof model")
    public void arrayOneOfModelTest() {
        final Schema schema = new ArraySchema()
                .items(new ComposedSchema()
                        .addOneOfItem(new StringSchema())
                        .addOneOfItem(new IntegerSchema().format("int64")))
                .description("an array oneof model");
        final DefaultCodegen codegen = new TypeScriptClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", schema);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", schema);


        Assertions.assertEquals(cm.name, "sample");
        Assertions.assertEquals(cm.classname, "Sample");
        Assertions.assertEquals(cm.description, "an array oneof model");
        Assertions.assertEquals(cm.arrayModelType, "string | number");
        Assertions.assertEquals(cm.vars.size(), 0);
    }

    @Test(description = "convert an any of with array oneof model")
    public void objectPropertyAnyOfWithArrayOneOfModelTest() {
        final Schema schema = new ObjectSchema().addProperty("value",
                new ComposedSchema().addAnyOfItem(new StringSchema()).addAnyOfItem(new ArraySchema()
                        .items(new ComposedSchema()
                                .addOneOfItem(new StringSchema())
                                .addOneOfItem(new IntegerSchema().format("int64")))))
                .description("an any of with array oneof model");
        final DefaultCodegen codegen = new TypeScriptClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", schema);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", schema);

        String s = codegen.getSchemaType((Schema)schema.getProperties().get("value"));

        Assertions.assertEquals(cm.name, "sample");
        Assertions.assertEquals(cm.classname, "Sample");
        Assertions.assertEquals(cm.description, "an any of with array oneof model");
        Assertions.assertEquals(cm.vars.size(), 1);
        Assertions.assertEquals(s, "string | Array<string | number>");
    }
}
