package org.openapitools.codegen.cpptiny;

import io.swagger.v3.oas.models.media.IntegerSchema;
import io.swagger.v3.oas.models.media.Schema;
import org.openapitools.codegen.CodegenModel;
import org.openapitools.codegen.CodegenProperty;
import org.testng.Assert;
import org.testng.annotations.Test;

@SuppressWarnings("rawtypes")
public class CppTinyServiceServiceTest extends CppTinyBaseTest {
    @Test(description = "sets example value")
    public void intTypeIsLong() {
        // Arrange
        Schema schema = testSchema();
        schema.addProperties("id", new IntegerSchema().format("int64"));

        // Act
        CodegenModel model_to_be_generated = makeCodeGenFrom(schema);

        // Assert
        CodegenProperty id_property = model_to_be_generated.vars.get(0);
        Assert.assertEquals(id_property.dataType, "long");
    }
}
