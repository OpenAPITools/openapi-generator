package org.openapitools.codegen.languages;

import io.swagger.v3.oas.models.Components;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.media.ObjectSchema;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.media.StringSchema;
import io.swagger.v3.oas.models.parameters.PathParameter;
import org.assertj.core.api.Assertions;
import org.openapitools.codegen.CodegenParameter;
import org.openapitools.codegen.CodegenProperty;
import org.openapitools.codegen.GeneratorLanguage;
import org.testng.annotations.BeforeTest;
import org.testng.annotations.Test;

import java.util.HashSet;

import static org.assertj.core.api.Assertions.assertThat;

public class RustServerCodegenTest {

    @Test public void generatorLanguageIsRust() {
        Assertions.assertThat(new RustServerCodegen().generatorLanguage())
            .isSameAs(GeneratorLanguage.RUST);
    }

    @SuppressWarnings("NewClassNamingConvention")
    public static class fromParameter {

        RustServerCodegen codegen = new RustServerCodegen();

        @BeforeTest void setOpenAPI() {
            codegen.setOpenAPI(new OpenAPI().components(new Components().addSchemas(
                "SampleRequestInput",
                new ObjectSchema().addProperty(
                    "FormProp1",
                    new StringSchema()
                )
            )));
        }


        @Test public void doesNotResolveReferencedSchema() {
            var parameter = new PathParameter()
                .name("input")
                .schema(new Schema<>().$ref("SampleRequestInput"));

            CodegenParameter result = codegen.fromParameter(parameter, new HashSet<>());

            assertThat(result.getSchema())
                .returns("SampleRequestInput", CodegenProperty::getBaseType)
                .returns("models::SampleRequestInput", CodegenProperty::getDataType)
                .returns(false, CodegenProperty::getHasVars)
                .extracting(CodegenProperty::getRef).asString().endsWith("SampleRequestInput");
        }
    }
}