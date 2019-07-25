package org.openapitools.codegen.java.jaxrs;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.media.MapSchema;
import io.swagger.v3.oas.models.media.Schema;
import org.openapitools.codegen.CodegenModel;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.languages.JavaResteasyServerCodegen;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertTrue;

public class JavaJaxrsResteasyServerCodegenModelTest extends JavaJaxrsBaseTest {

    @BeforeMethod
    public void beforeMethod() {
        codegen = new JavaResteasyServerCodegen();
    }

    @Test(description = "convert a simple java model with java8 types")
    public void mapModelTest() {
        final Schema model = new Schema()
                .description("A model with a map")
                .addProperties("map", new MapSchema());

        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", model);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", model);

        assertEquals(cm.vars.get(0).baseType, "Map");
        assertTrue(cm.imports.contains("HashMap"));
    }

}
