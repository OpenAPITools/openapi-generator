package org.openapitools.codegen.java.jaxrs;

import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertTrue;

import java.util.Collections;

import org.openapitools.codegen.CodegenModel;
import org.openapitools.codegen.languages.JavaResteasyServerCodegen;
import org.testng.annotations.Test;

import io.swagger.v3.oas.models.media.MapSchema;
import io.swagger.v3.oas.models.media.Schema;

public class JavaJaxrsResteasyServerCodegenModelTest {

    @Test(description = "convert a simple java model with java8 types")
    public void mapModelTest() {
        final Schema model = new Schema()
                .description("A model with a map")
                .addProperties("map", new MapSchema());

        final JavaResteasyServerCodegen codegen = new JavaResteasyServerCodegen();
        final CodegenModel cm = codegen.fromModel("sample", model, Collections.singletonMap("sample", model));

        assertEquals(cm.vars.get(0).baseType, "Map");
        assertTrue(cm.imports.contains("HashMap"));
    }

}
