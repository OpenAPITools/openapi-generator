package org.openapitools.codegen.scala;

import org.openapitools.codegen.languages.ScalaSttpClientCodegen;
import org.testng.Assert;
import org.testng.annotations.Test;

import io.swagger.v3.oas.models.media.Schema;

public class SttpCodegenTest {

    private final ScalaSttpClientCodegen codegen = new ScalaSttpClientCodegen();

    @Test
    public void encodePath() {
        Assert.assertEquals(codegen.encodePath("{user_name}"), "${userName}");
        Assert.assertEquals(codegen.encodePath("{userName}"), "${userName}");
        Assert.assertEquals(codegen.encodePath("{UserName}"), "${userName}");
        Assert.assertEquals(codegen.encodePath("user_name"), "user_name");
        Assert.assertEquals(codegen.encodePath("before/{UserName}/after"), "before/${userName}/after");
    }

    @Test
    public void typeByteArray() {
      final Schema<?> schema = new Schema<Object>()
          .description("Schema with byte string");
      schema.setType("string");
      schema.setFormat("byte");
      String type = codegen.getTypeDeclaration(schema);
      Assert.assertEquals(type, "Array[Byte]");
    }

}
