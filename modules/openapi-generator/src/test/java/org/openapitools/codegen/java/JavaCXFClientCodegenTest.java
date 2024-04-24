/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 * Copyright 2018 SmartBear Software
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.openapitools.codegen.java;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.media.*;
import io.swagger.v3.oas.models.responses.ApiResponse;
import io.swagger.v3.oas.models.responses.ApiResponses;
import org.openapitools.codegen.*;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.openapitools.codegen.languages.AbstractJavaCodegen;
import org.openapitools.codegen.languages.JavaCXFClientCodegen;
import org.openapitools.codegen.languages.features.BeanValidationFeatures;
import org.openapitools.codegen.languages.features.GzipTestFeatures;
import org.openapitools.codegen.languages.features.LoggingTestFeatures;
import org.openapitools.codegen.languages.features.UseGenericResponseFeatures;
import org.openapitools.codegen.model.OperationMap;
import org.openapitools.codegen.model.OperationsMap;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.openapitools.codegen.TestUtils.validateJavaSourceFiles;

public class JavaCXFClientCodegenTest {

    @Test
    public void responseWithoutContent() throws Exception {
        final Schema listOfPets = new ArraySchema().items(new Schema<>().$ref("#/components/schemas/Pet"));
        Operation operation = new Operation()
                .responses(new ApiResponses()
                        .addApiResponse("200",
                                new ApiResponse().description("Return a list of pets")
                                        .content(new Content().addMediaType("application/json",
                                                new MediaType().schema(listOfPets))))
                        .addApiResponse("400", new ApiResponse().description("Error")));
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("Pet", new ObjectSchema());
        final JavaCXFClientCodegen codegen = new JavaCXFClientCodegen();
        final CodegenOperation co = codegen.fromOperation("getAllPets", "GET", operation, null);

        OperationMap operationMap = new OperationMap();
        operationMap.setOperation(co);

        OperationsMap objs = new OperationsMap();
        objs.setOperation(operationMap);
        objs.setImports(Collections.emptyList());
        codegen.postProcessOperationsWithModels(objs, Collections.emptyList());

        Assertions.assertEquals(co.responses.size(), 2);
        CodegenResponse cr1 = co.responses.get(0);
        Assertions.assertEquals(cr1.code, "200");
        Assertions.assertEquals(cr1.baseType, "Pet");
        Assertions.assertEquals(cr1.dataType, "List<Pet>");
        Assertions.assertFalse(cr1.vendorExtensions.containsKey("x-java-is-response-void"));

        CodegenResponse cr2 = co.responses.get(1);
        Assertions.assertEquals(cr2.code, "400");
        Assertions.assertEquals(cr2.baseType, "Void");
        Assertions.assertEquals(cr2.dataType, "void");
        Assertions.assertEquals(cr2.vendorExtensions.get("x-java-is-response-void"), Boolean.TRUE);
    }

    @Test
    public void testInitialConfigValues() throws Exception {
        final JavaCXFClientCodegen codegen = new JavaCXFClientCodegen();
        codegen.processOpts();

        Assertions.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.FALSE);
        Assertions.assertFalse(codegen.isHideGenerationTimestamp());

        Assertions.assertEquals(codegen.modelPackage(), "org.openapitools.model");
        Assertions.assertEquals(codegen.additionalProperties().get(CodegenConstants.MODEL_PACKAGE), "org.openapitools.model");
        Assertions.assertEquals(codegen.apiPackage(), "org.openapitools.api");
        Assertions.assertEquals(codegen.additionalProperties().get(CodegenConstants.API_PACKAGE), "org.openapitools.api");
        Assertions.assertEquals(codegen.getInvokerPackage(), "org.openapitools.api");
        Assertions.assertEquals(codegen.additionalProperties().get(CodegenConstants.INVOKER_PACKAGE), "org.openapitools.api");
    }

    @Test
    public void testSettersForConfigValues() throws Exception {
        final JavaCXFClientCodegen codegen = new JavaCXFClientCodegen();
        codegen.setHideGenerationTimestamp(true);
        codegen.setInvokerPackage("org.openapitools.client.xyz.invoker");
        codegen.processOpts();

        Assertions.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.TRUE);
        Assertions.assertTrue(codegen.isHideGenerationTimestamp());
        Assertions.assertEquals(codegen.modelPackage(), "org.openapitools.model");
        Assertions.assertEquals(codegen.additionalProperties().get(CodegenConstants.MODEL_PACKAGE), "org.openapitools.model");
        Assertions.assertEquals(codegen.apiPackage(), "org.openapitools.api");
        Assertions.assertEquals(codegen.additionalProperties().get(CodegenConstants.API_PACKAGE), "org.openapitools.api");
        Assertions.assertEquals(codegen.getInvokerPackage(), "org.openapitools.client.xyz.invoker");
        Assertions.assertEquals(codegen.additionalProperties().get(CodegenConstants.INVOKER_PACKAGE), "org.openapitools.client.xyz.invoker");
    }

    @Test
    public void testAdditionalPropertiesPutForConfigValues() throws Exception {
        final JavaCXFClientCodegen codegen = new JavaCXFClientCodegen();
        codegen.additionalProperties().put(CodegenConstants.HIDE_GENERATION_TIMESTAMP, "false");
        codegen.additionalProperties().put(CodegenConstants.INVOKER_PACKAGE,"org.openapitools.client.xyz.invoker");
        codegen.processOpts();

        Assertions.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.FALSE);
        Assertions.assertFalse(codegen.isHideGenerationTimestamp());
        Assertions.assertEquals(codegen.modelPackage(), "org.openapitools.model");
        Assertions.assertEquals(codegen.additionalProperties().get(CodegenConstants.MODEL_PACKAGE), "org.openapitools.model");
        Assertions.assertEquals(codegen.apiPackage(), "org.openapitools.api");
        Assertions.assertEquals(codegen.additionalProperties().get(CodegenConstants.API_PACKAGE), "org.openapitools.api");
        Assertions.assertEquals(codegen.getInvokerPackage(), "org.openapitools.client.xyz.invoker");
        Assertions.assertEquals(codegen.additionalProperties().get(CodegenConstants.INVOKER_PACKAGE), "org.openapitools.client.xyz.invoker");
    }

    @Test
    public void testUseBeanValidationAdditionalProperty() throws Exception {
        final JavaCXFClientCodegen codegen = new JavaCXFClientCodegen();

        codegen.processOpts();
        Assertions.assertNull(codegen.additionalProperties().get(BeanValidationFeatures.USE_BEANVALIDATION));
        Assertions.assertFalse(codegen.isUseBeanValidation());

        codegen.additionalProperties().put(BeanValidationFeatures.USE_BEANVALIDATION, true);
        codegen.processOpts();
        Assertions.assertEquals(codegen.additionalProperties().get(BeanValidationFeatures.USE_BEANVALIDATION), Boolean.TRUE);
        Assertions.assertTrue(codegen.isUseBeanValidation());
    }

    @Test
    public void testUseGenericResponseAdditionalProperty() throws Exception {
        final JavaCXFClientCodegen codegen = new JavaCXFClientCodegen();

        codegen.processOpts();
        Assertions.assertNull(codegen.additionalProperties().get(UseGenericResponseFeatures.USE_GENERIC_RESPONSE));
        Assertions.assertFalse(codegen.isUseGenericResponse());

        codegen.additionalProperties().put(UseGenericResponseFeatures.USE_GENERIC_RESPONSE, true);
        codegen.processOpts();
        Assertions.assertEquals(codegen.additionalProperties().get(UseGenericResponseFeatures.USE_GENERIC_RESPONSE), Boolean.TRUE);
        Assertions.assertTrue(codegen.isUseGenericResponse());
    }

    @Test
    public void testUseLoggingFeatureForTestsAdditionalProperty() throws Exception {
        final JavaCXFClientCodegen codegen = new JavaCXFClientCodegen();

        codegen.processOpts();
        Assertions.assertNull(codegen.additionalProperties().get(LoggingTestFeatures.USE_LOGGING_FEATURE_FOR_TESTS));
        Assertions.assertFalse(codegen.isUseLoggingFeatureForTests());

        codegen.additionalProperties().put(LoggingTestFeatures.USE_LOGGING_FEATURE_FOR_TESTS, true);
        codegen.processOpts();
        Assertions.assertEquals(codegen.additionalProperties().get(LoggingTestFeatures.USE_LOGGING_FEATURE_FOR_TESTS), Boolean.TRUE);
        Assertions.assertTrue(codegen.isUseLoggingFeatureForTests());
    }

    @Test
    public void testUseGzipFeatureForTestsAdditionalProperty() throws Exception {
        final JavaCXFClientCodegen codegen = new JavaCXFClientCodegen();

        codegen.processOpts();
        Assertions.assertNull(codegen.additionalProperties().get(GzipTestFeatures.USE_GZIP_FEATURE_FOR_TESTS));
        Assertions.assertFalse(codegen.isUseLoggingFeatureForTests());

        codegen.additionalProperties().put(GzipTestFeatures.USE_GZIP_FEATURE_FOR_TESTS, true);
        codegen.processOpts();
        Assertions.assertEquals(codegen.additionalProperties().get(GzipTestFeatures.USE_GZIP_FEATURE_FOR_TESTS), Boolean.TRUE);
        Assertions.assertTrue(codegen.isUseGzipFeatureForTests());
    }

    @Test
    public void testOpenApiNullableAdditionalProperty() throws Exception {
        JavaCXFClientCodegen codegen = new JavaCXFClientCodegen();

        codegen.processOpts();
        Assertions.assertNotNull(codegen.additionalProperties().get(AbstractJavaCodegen.OPENAPI_NULLABLE));
        Assertions.assertTrue(codegen.isOpenApiNullable());

        codegen.additionalProperties().put(AbstractJavaCodegen.OPENAPI_NULLABLE, false);
        codegen.processOpts();
        Assertions.assertEquals(codegen.additionalProperties().get(AbstractJavaCodegen.OPENAPI_NULLABLE), Boolean.FALSE);
        Assertions.assertFalse(codegen.isOpenApiNullable());
    }

    @Test
    public void testPostProcessNullableModelPropertyWithOpenApiNullableEnabled() throws Exception {
        JavaCXFClientCodegen codegen = new JavaCXFClientCodegen();
        codegen.additionalProperties().put(AbstractJavaCodegen.JACKSON, true);
        codegen.additionalProperties().put(AbstractJavaCodegen.OPENAPI_NULLABLE, true);
        codegen.processOpts();

        CodegenModel codegenModel = new CodegenModel();
        CodegenProperty codegenProperty = new CodegenProperty();
        codegenProperty.required = false;
        codegenProperty.isNullable = true;

        codegen.postProcessModelProperty(codegenModel, codegenProperty);
        Assertions.assertTrue(codegenModel.imports.contains("JsonNullable"));
        Assertions.assertTrue(codegenModel.imports.contains("JsonIgnore"));
        Assertions.assertEquals(codegenProperty.getVendorExtensions().get("x-is-jackson-optional-nullable"), Boolean.TRUE);
    }

    @Test
    public void testPostProcessNullableModelPropertyWithOpenApiNullableDisabled() throws Exception {
        JavaCXFClientCodegen codegen = new JavaCXFClientCodegen();
        codegen.additionalProperties().put(AbstractJavaCodegen.JACKSON, true);
        codegen.additionalProperties().put(AbstractJavaCodegen.OPENAPI_NULLABLE, false);
        codegen.processOpts();

        CodegenModel codegenModel = new CodegenModel();
        CodegenProperty codegenProperty = new CodegenProperty();
        codegenProperty.required = false;
        codegenProperty.isNullable = true;

        codegen.postProcessModelProperty(codegenModel, codegenProperty);
        Assertions.assertFalse(codegenModel.imports.contains("JsonNullable"));
        Assertions.assertFalse(codegenModel.imports.contains("JsonIgnore"));
        Assertions.assertNull(codegenProperty.getVendorExtensions().get("x-is-jackson-optional-nullable"));
    }

    @Test
    public void testPostProcessNullableModelPropertyWithOpenApiNullableEnabledForRequiredProperties() throws Exception {
        JavaCXFClientCodegen codegen = new JavaCXFClientCodegen();
        codegen.additionalProperties().put(AbstractJavaCodegen.JACKSON, true);
        codegen.additionalProperties().put(AbstractJavaCodegen.OPENAPI_NULLABLE, true);
        codegen.processOpts();

        CodegenModel codegenModel = new CodegenModel();
        CodegenProperty codegenProperty = new CodegenProperty();
        codegenProperty.required = true;
        codegenProperty.isNullable = true;

        codegen.postProcessModelProperty(codegenModel, codegenProperty);
        Assertions.assertFalse(codegenModel.imports.contains("JsonNullable"));
        Assertions.assertFalse(codegenModel.imports.contains("JsonIgnore"));
        Assertions.assertNull(codegenProperty.getVendorExtensions().get("x-is-jackson-optional-nullable"));
    }

    @Test
    public void testPostProcessNotNullableModelPropertyWithOpenApiNullableEnabled() throws Exception {
        JavaCXFClientCodegen codegen = new JavaCXFClientCodegen();
        codegen.additionalProperties().put(AbstractJavaCodegen.JACKSON, true);
        codegen.additionalProperties().put(AbstractJavaCodegen.OPENAPI_NULLABLE, true);
        codegen.processOpts();

        CodegenModel codegenModel = new CodegenModel();
        CodegenProperty codegenProperty = new CodegenProperty();
        codegenProperty.required = false;
        codegenProperty.isNullable = false;

        codegen.postProcessModelProperty(codegenModel, codegenProperty);
        Assertions.assertFalse(codegenModel.imports.contains("JsonNullable"));
        Assertions.assertFalse(codegenModel.imports.contains("JsonIgnore"));
        Assertions.assertNull(codegenProperty.getVendorExtensions().get("x-is-jackson-optional-nullable"));
    }

    @Test
    public void testPostProcessNullableModelPropertyWithOpenApiNullableEnabledButJacksonDisabled() throws Exception {
        JavaCXFClientCodegen codegen = new JavaCXFClientCodegen();
        codegen.additionalProperties().put(AbstractJavaCodegen.JACKSON, false);
        codegen.additionalProperties().put(AbstractJavaCodegen.OPENAPI_NULLABLE, true);
        codegen.processOpts();

        CodegenModel codegenModel = new CodegenModel();
        CodegenProperty codegenProperty = new CodegenProperty();
        codegenProperty.required = false;
        codegenProperty.isNullable = true;

        codegen.postProcessModelProperty(codegenModel, codegenProperty);
        Assertions.assertTrue(codegenModel.imports.contains("JsonNullable"));
        Assertions.assertFalse(codegenModel.imports.contains("JsonIgnore"));
        Assertions.assertNull(codegenProperty.getVendorExtensions().get("x-is-jackson-optional-nullable"));
    }

    @Test
    public void testUseJackson() throws Exception {
        JavaCXFClientCodegen codegen = new JavaCXFClientCodegen();

        codegen.processOpts();
        Assertions.assertNull(codegen.additionalProperties().get(AbstractJavaCodegen.JACKSON));
        Assertions.assertFalse(codegen.isUseJackson());

        codegen.additionalProperties().put(AbstractJavaCodegen.JACKSON, true);
        codegen.processOpts();
        Assertions.assertEquals(codegen.additionalProperties().get(AbstractJavaCodegen.JACKSON), Boolean.TRUE);
        Assertions.assertTrue(codegen.isUseJackson());
    }

    @Test
    public void testUseAbstractionForFiles() throws Exception {

        Map<String, Object> properties = new HashMap<>();
        properties.put(CodegenConstants.API_PACKAGE, "xyz.abcdef.api");
        properties.put(CodegenConstants.MODEL_PACKAGE, "xyz.abcdef.api");

        properties.put(JavaCXFClientCodegen.USE_ABSTRACTION_FOR_FILES, true);


        File output = Files.createTempDirectory("test").toFile();
        output.deleteOnExit();

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("jaxrs-cxf-client")
                .setAdditionalProperties(properties)
                .setInputSpec("src/test/resources/3_0/issue8792.yaml")
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));


        DefaultGenerator generator = new DefaultGenerator();
        List<File> files = generator.opts(configurator.toClientOptInput()).generate();
        files.forEach(File::deleteOnExit);

        validateJavaSourceFiles(files);

        Path defaultApi = Paths.get(output + "/src/gen/java/xyz/abcdef/api/DefaultApi.java");
        TestUtils.assertFileContains(defaultApi,
                //get file
                "@ApiResponse(code = 200, message = \"File content\", response = InputStream.class)",
                "public InputStream filesIdGet(@PathParam(\"id\") String id);",

                //upload
                "public FilesUploadPost200Response filesUploadPost(InputStream body);"
        );
    }
}
