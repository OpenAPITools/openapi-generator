package org.openapitools.codegen.typescript.express.zod;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.media.IntegerSchema;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.media.StringSchema;
import io.swagger.v3.parser.util.SchemaTypeUtil;

import org.openapitools.codegen.CodegenModel;
import org.openapitools.codegen.CodegenProperty;
import org.openapitools.codegen.DefaultCodegen;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.languages.TypeScriptExpressZodServerCodegen;

import org.testng.Assert;
import org.testng.annotations.Test;

@SuppressWarnings("static-method")
public class TypeScriptExpressZodServerCodegenModelTest {

    @Test(description = "convert a simple TypeScript Express Zod model")
    public void simpleModelTest() {
        final Schema schema = new Schema()
                .description("a sample model")
                .addProperties("id", new IntegerSchema().format(SchemaTypeUtil.INTEGER64_FORMAT))
                .addProperties("name", new StringSchema())
                .addRequiredItem("id")
                .addRequiredItem("name");
        final DefaultCodegen codegen = new TypeScriptExpressZodServerCodegen();
        final OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("Sample", schema);
        codegen.setOpenAPI(openAPI);

        final CodegenModel cm = codegen.fromModel("Sample", schema);

        Assert.assertEquals(cm.name, "Sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.description, "a sample model");
        Assert.assertEquals(cm.vars.size(), 2);

        final CodegenProperty id = cm.vars.get(0);
        Assert.assertEquals(id.baseName, "id");
        Assert.assertEquals(id.dataType, "number");
        Assert.assertTrue(id.required);

        final CodegenProperty name = cm.vars.get(1);
        Assert.assertEquals(name.baseName, "name");
        Assert.assertEquals(name.dataType, "string");
        Assert.assertTrue(name.required);
    }
}
