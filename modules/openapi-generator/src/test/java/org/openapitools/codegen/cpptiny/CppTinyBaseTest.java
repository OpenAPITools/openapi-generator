package org.openapitools.codegen.cpptiny;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.media.IntegerSchema;
import io.swagger.v3.oas.models.media.ObjectSchema;
import io.swagger.v3.oas.models.media.Schema;
import org.openapitools.codegen.CodegenModel;
import org.openapitools.codegen.CodegenProperty;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.languages.CppTinyClientCodegen;
import org.testng.Assert;
import org.testng.annotations.Test;

@SuppressWarnings("rawtypes")
public class CppTinyBaseTest {
    protected CppTinyClientCodegen codegen = new CppTinyClientCodegen();

    protected CodegenModel makeCodeGenFrom(Schema model) {
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", model);
        codegen.setOpenAPI(openAPI);
        return codegen.fromModel("sample", model);
    }

    protected Schema testSchema() {
        return new ObjectSchema()
                .description("a sample model");
    }


    @Test(description = "An example of testing the code model before it is rendered")
    public void exampleOfATest() {

        // Make a schema object. We pretend this model comes from an OAS (openapi specification)
        // https://github.com/OAI/OpenAPI-Specification/blob/main/versions/3.0.3.md#schemaObject
        // Then add the properties to your model.

        // Arrange
        Schema schemaModel = testSchema();
        schemaModel.addProperties("id", new IntegerSchema().format("int64"));


        // Then we generated a codemodel, with the cpp tiny code generator.
        // Act
        CodegenModel model_to_be_generated = makeCodeGenFrom(schemaModel);

        // We can then test, and see if the codegen model have the right data.
        // Assert
        CodegenProperty id_property = model_to_be_generated.vars.get(0);
        Assert.assertEquals(id_property.dataType, "long");
    }

    // Make example test of assert that some codegen is generated correctly
    // Assert.assertEquals(generated_code, "long id = 0;"


}
