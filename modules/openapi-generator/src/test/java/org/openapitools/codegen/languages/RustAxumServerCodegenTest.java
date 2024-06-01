package org.openapitools.codegen.languages;

import io.swagger.v3.oas.models.Components;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.media.*;
import io.swagger.v3.oas.models.parameters.RequestBody;
import org.assertj.core.api.Assertions;
import org.openapitools.codegen.CodegenOperation;
import org.openapitools.codegen.GeneratorLanguage;
import org.testng.annotations.BeforeTest;
import org.testng.annotations.Test;

import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;

public class RustAxumServerCodegenTest {
    
    @Test public void generatorLanguageIsRust() {
        Assertions.assertThat(new RustAxumServerCodegen().generatorLanguage())
            .isSameAs(GeneratorLanguage.RUST);
    }
    
    @SuppressWarnings("NewClassNamingConvention")
    public static class fromOperation {

        RustAxumServerCodegen codegen = new RustAxumServerCodegen();

        @BeforeTest void setOpenAPI() {
            codegen.setOpenAPI(new OpenAPI().components(new Components().addSchemas("SampleRequestInput", new ObjectSchema().addProperty("FormProp1", new StringSchema()))));
        }


        @Test public void addsFormParametersFromRequestProperties_forFormContentType() {
            var operation = new Operation()
                .parameters(List.of())
                .operationId("foobar")
                .requestBody(new RequestBody().content(new Content().addMediaType(
                    "application/x-www-form-urlencoded",
                    new MediaType().schema(new Schema<>().$ref("SampleRequestInput"))
                )));

            CodegenOperation result = codegen.fromOperation("/", "GET", operation, List.of());

            assertThat(result.formParams).isEmpty();
            assertThat(result.bodyParams).hasSize(1).first()
                .hasFieldOrPropertyWithValue("baseName", "SampleRequestInput")
                .hasFieldOrPropertyWithValue("dataType", "models::SampleRequestInput")
                .hasFieldOrPropertyWithValue("paramName", "sample_request_input");
        }

        @Test public void doesNotAddFormParametersFromRequestProperties_forJsonContentType() {
            var operation = new Operation()
                .parameters(List.of())
                .operationId("foobar")
                .requestBody(new RequestBody().content(new Content().addMediaType(
                    "application/json",
                    new MediaType().schema(new Schema<>().$ref("SampleRequestInput"))
                )));

            CodegenOperation result = codegen.fromOperation("", "GET", operation, List.of());

            assertThat(result.formParams).isEmpty();
            assertThat(result.bodyParams).hasSize(1).first()
                .hasFieldOrPropertyWithValue("baseName", "SampleRequestInput")
                .hasFieldOrPropertyWithValue("dataType", "models::SampleRequestInput")
                .hasFieldOrPropertyWithValue("paramName", "sample_request_input");
        }
    }
}