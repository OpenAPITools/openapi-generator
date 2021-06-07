package org.openapitools.codegen.cpptiny;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.media.IntegerSchema;
import io.swagger.v3.oas.models.media.ObjectSchema;
import io.swagger.v3.oas.models.media.Schema;
import org.openapitools.codegen.*;
import org.openapitools.codegen.languages.CppTinyClientCodegen;
import org.testng.Assert;
import org.testng.annotations.Test;

@SuppressWarnings("rawtypes")
public class CppTinyClientServiceTest {

    private CppTinyClientCodegen codegen = new CppTinyClientCodegen();

    @Test(description = "sets example value")
    public void intTypeIsLong() {
        // Arrange
        Schema model = testSchema();
        model.addProperties("id", new IntegerSchema().format("int64"));

        // Act
        CodegenModel generated = makeCodeGenFrom(model);

        // Assert
        CodegenProperty id_property = generated.vars.get(0);
        Assert.assertEquals(id_property.dataType, "long");
    }

    private CodegenModel makeCodeGenFrom(Schema model) {
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", model);
        codegen.setOpenAPI(openAPI);
        return codegen.fromModel("sample", model);
    }

    private Schema testSchema() {
        return new ObjectSchema()
                .description("a sample model");
    }
}
