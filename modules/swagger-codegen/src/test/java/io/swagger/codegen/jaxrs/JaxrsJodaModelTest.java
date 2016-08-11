package io.swagger.codegen.jaxrs;

import io.swagger.codegen.CodegenModel;
import io.swagger.codegen.languages.JavaJerseyServerCodegen;
import io.swagger.models.Model;
import io.swagger.models.ModelImpl;
import io.swagger.models.properties.DateProperty;
import io.swagger.models.properties.DateTimeProperty;
import io.swagger.models.properties.LongProperty;
import io.swagger.util.Json;
import org.testng.annotations.Test;

import static org.testng.Assert.assertEquals;

@SuppressWarnings("static-method")
public class JaxrsJodaModelTest {
    
    @Test(description = "convert a simple java model with Joda types")
    public void simpleModelTest() {
        final Model model = new ModelImpl()
                .description("a sample model")
                .property("id", new LongProperty())
                .property("theDate", new DateProperty())
                .property("createdAt", new DateTimeProperty())
                .required("id")
                .required("name");

        final JavaJerseyServerCodegen codegen = new JavaJerseyServerCodegen();
        codegen.setDateLibrary("joda");
        codegen.processOpts();
        final CodegenModel cm = codegen.fromModel("sample", model);

        Json.prettyPrint(cm);
        assertEquals(cm.vars.get(0).datatype, "Long");
        assertEquals(cm.vars.get(1).datatype, "LocalDate");
        assertEquals(cm.vars.get(2).datatype, "DateTime");
    }
}
