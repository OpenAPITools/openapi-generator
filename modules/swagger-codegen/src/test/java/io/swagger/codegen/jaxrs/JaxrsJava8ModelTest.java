package io.swagger.codegen.jaxrs;

import io.swagger.codegen.CodegenModel;
import io.swagger.codegen.languages.JavaJerseyServerCodegen;
import io.swagger.oas.models.media.DateSchema;
import io.swagger.oas.models.media.DateTimeSchema;
import io.swagger.oas.models.media.IntegerSchema;
import io.swagger.oas.models.media.Schema;
import io.swagger.parser.v3.util.SchemaTypeUtil;
import io.swagger.util.Json;
import org.testng.annotations.Test;

import static org.testng.Assert.assertEquals;

@SuppressWarnings("static-method")
public class JaxrsJava8ModelTest {
    
    @Test(description = "convert a simple java model with java8 types")
    public void simpleModelTest() {
        final Schema model = new Schema()
                .description("a sample model")
                .addProperties("id", new IntegerSchema().format(SchemaTypeUtil.INTEGER64_FORMAT))
                .addProperties("theDate", new DateSchema())
                .addProperties("createdAt", new DateTimeSchema())
                .addRequiredItem("id")
                .addRequiredItem("name");

        final JavaJerseyServerCodegen codegen = new JavaJerseyServerCodegen();
        codegen.setDateLibrary("java8");
        codegen.processOpts();
        final CodegenModel cm = codegen.fromModel("sample", model);

        Json.prettyPrint(cm);
        assertEquals(cm.vars.get(0).datatype, "Long");
        assertEquals(cm.vars.get(1).datatype, "LocalDate");
        assertEquals(cm.vars.get(2).datatype, "OffsetDateTime");
    }
}
