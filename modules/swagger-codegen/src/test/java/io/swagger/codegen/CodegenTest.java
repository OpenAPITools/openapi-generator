package io.swagger.codegen;

import io.swagger.models.Operation;
import io.swagger.models.Swagger;
import io.swagger.models.properties.Property;
import io.swagger.parser.SwaggerParser;

import org.testng.Assert;
import org.testng.annotations.Test;

import java.util.List;

public class CodegenTest {

    @Test(description = "read a file upload param from a 2.0 spec")
    public void fileUploadParamTest() {
        final Swagger model = parseAndPrepareSwagger("src/test/resources/2_0/petstore.json");
        final DefaultCodegen codegen = new DefaultCodegen();
        final String path = "/pet/{petId}/uploadImage";
        final Operation p = model.getPaths().get(path).getPost();
        final CodegenOperation op = codegen.fromOperation(path, "post", p, model.getDefinitions());

        Assert.assertEquals(op.operationId, "uploadFile");
        Assert.assertEquals(op.httpMethod, "POST");
        Assert.assertTrue(op.hasConsumes);
        Assert.assertEquals(op.consumes.size(), 1);
        Assert.assertEquals(op.consumes.get(0).get("mediaType"), "multipart/form-data");
        Assert.assertTrue(op.hasProduces);
        Assert.assertEquals(op.allParams.size(), 3);
        Assert.assertEquals(op.formParams.size(), 2);

        final CodegenParameter file = op.formParams.get(1);
        Assert.assertTrue(file.isFormParam);
        Assert.assertEquals(file.dataType, "file");
        Assert.assertNull(file.required);
        Assert.assertTrue(file.isFile);
        Assert.assertNull(file.hasMore);
    }

    @Test(description = "read formParam values from a 2.0 spec")
    public void formParamTest() {
        final Swagger model = parseAndPrepareSwagger("src/test/resources/2_0/petstore.json");
        final DefaultCodegen codegen = new DefaultCodegen();
        final String path = "/pet/{petId}";
        final Operation p = model.getPaths().get(path).getPost();
        final CodegenOperation op = codegen.fromOperation(path, "post", p, model.getDefinitions());

        Assert.assertEquals(op.operationId, "updatePetWithForm");
        Assert.assertEquals(op.httpMethod, "POST");
        Assert.assertTrue(op.hasConsumes);
        Assert.assertEquals(op.consumes.size(), 1);
        Assert.assertEquals(op.consumes.get(0).get("mediaType"), "application/x-www-form-urlencoded");
        Assert.assertTrue(op.hasProduces);
        Assert.assertEquals(op.produces.size(), 2);
        Assert.assertEquals(op.produces.get(0).get("mediaType"), "application/json");
        Assert.assertEquals(op.produces.get(0).get("hasMore"), "true");
        Assert.assertEquals(op.produces.get(1).get("mediaType"), "application/xml");
        Assert.assertEquals(op.pathParams.size(), 1);

        final CodegenParameter idParam = op.pathParams.get(0);
        Assert.assertTrue(idParam.isPathParam);
        Assert.assertEquals(idParam.dataType, "String");
        Assert.assertTrue(idParam.required);
        Assert.assertNull(idParam.hasMore);

        Assert.assertEquals(op.allParams.size(), 3);
        Assert.assertEquals(op.formParams.size(), 2);

        final CodegenParameter nameParam = op.formParams.get(0);
        Assert.assertTrue(nameParam.isFormParam);
        Assert.assertTrue(nameParam.notFile);
        Assert.assertEquals(nameParam.dataType, "String");
        Assert.assertNull(nameParam.required);
        Assert.assertTrue(nameParam.hasMore);

        final CodegenParameter statusParam = op.formParams.get(1);
        Assert.assertTrue(statusParam.isFormParam);
        Assert.assertTrue(statusParam.notFile);
        Assert.assertEquals(statusParam.dataType, "String");
        Assert.assertNull(statusParam.required);
        Assert.assertNull(statusParam.hasMore);
    }

    @Test(description = "handle required parameters from a 2.0 spec as required when figuring out Swagger types")
    public void requiredParametersTest() {
        final Swagger model = parseAndPrepareSwagger("src/test/resources/2_0/requiredTest.json");

        final DefaultCodegen codegen = new DefaultCodegen() {
            @Override
            public String getSwaggerType(Property p) {
                if (p != null && !p.getRequired()) {
                    return "Optional<" + super.getSwaggerType(p) + ">";
                }
                return super.getSwaggerType(p);
            }
        };
        final String path = "/tests/requiredParams";
        final Operation p = model.getPaths().get(path).getGet();
        final CodegenOperation op = codegen.fromOperation(path, "get", p, model.getDefinitions());

        final List<CodegenParameter> formParams = op.formParams;
        Assert.assertEquals(formParams.size(), 2);
        Assert.assertEquals(formParams.get(0).dataType, "Long");
        Assert.assertEquals(formParams.get(1).dataType, "Optional<string>");
        Assert.assertEquals(op.returnType, "Long");
    }

    @Test(description = "select main response from a 2.0 spec using the lowest 2XX code")
    public void responseSelectionTest1() {
        final Swagger model = parseAndPrepareSwagger("src/test/resources/2_0/responseSelectionTest.json");
        final DefaultCodegen codegen = new DefaultCodegen();
        final String path = "/tests/withTwoHundredAndDefault";
        final Operation p = model.getPaths().get(path).getGet();
        final CodegenOperation op = codegen.fromOperation(path, "get", p, model.getDefinitions());

        Assert.assertEquals(op.returnType, "String");
    }

    @Test(description = "select main response from a 2.0 spec using the default keyword when no 2XX code")
    public void responseSelectionTest2() {
        final Swagger model = parseAndPrepareSwagger("src/test/resources/2_0/responseSelectionTest.json");
        final DefaultCodegen codegen = new DefaultCodegen();
        final String path = "/tests/withoutTwoHundredButDefault";
        final Operation p = model.getPaths().get(path).getGet();
        final CodegenOperation op = codegen.fromOperation(path, "get", p, model.getDefinitions());

        Assert.assertEquals(op.returnType, "String");
    }

    @Test(description = "return byte array when response format is byte")
    public void binaryDataTest() {
        final Swagger model = parseAndPrepareSwagger("src/test/resources/2_0/binaryDataTest.json");
        final DefaultCodegen codegen = new DefaultCodegen();
        final String path = "/tests/binaryResponse";
        final Operation p = model.getPaths().get(path).getPost();
        final CodegenOperation op = codegen.fromOperation(path, "post", p, model.getDefinitions());

        Assert.assertEquals(op.returnType, "byte[]");
        Assert.assertEquals(op.bodyParam.dataType, "byte[]");
        Assert.assertTrue(op.bodyParam.isBinary);
        Assert.assertTrue(op.responses.get(0).isBinary);
    }
    
    @Test(description = "use operation consumes and produces")
    public void localConsumesAndProducesTest() {
        final Swagger model = parseAndPrepareSwagger("src/test/resources/2_0/globalConsumesAndProduces.json");
        final DefaultCodegen codegen = new DefaultCodegen();
        final String path = "/tests/localConsumesAndProduces";
        final Operation p = model.getPaths().get(path).getGet();
        CodegenOperation op = codegen.fromOperation(path, "get", p, model.getDefinitions(), model);
        
        Assert.assertTrue(op.hasConsumes);
        Assert.assertEquals(op.consumes.size(), 1);
        Assert.assertEquals(op.consumes.get(0).get("mediaType"), "application/json");
        Assert.assertTrue(op.hasProduces);
        Assert.assertEquals(op.produces.size(), 1);
        Assert.assertEquals(op.produces.get(0).get("mediaType"), "application/json");
    }
    
    @Test(description = "use spec consumes and produces")
    public void globalConsumesAndProducesTest() {
        final Swagger model = parseAndPrepareSwagger("src/test/resources/2_0/globalConsumesAndProduces.json");
        final DefaultCodegen codegen = new DefaultCodegen();
        final String path = "/tests/globalConsumesAndProduces";
        final Operation p = model.getPaths().get(path).getGet();
        CodegenOperation op = codegen.fromOperation(path, "get", p, model.getDefinitions(), model);
        
        Assert.assertTrue(op.hasConsumes);
        Assert.assertEquals(op.consumes.size(), 1);
        Assert.assertEquals(op.consumes.get(0).get("mediaType"), "application/global_consumes");
        Assert.assertTrue(op.hasProduces);
        Assert.assertEquals(op.produces.size(), 1);
        Assert.assertEquals(op.produces.get(0).get("mediaType"), "application/global_produces");
    }
 
    @Test(description = "use operation consumes and produces (reset in operation with empty array)")
    public void localResetConsumesAndProducesTest() {
        final Swagger model = parseAndPrepareSwagger("src/test/resources/2_0/globalConsumesAndProduces.json");
        final DefaultCodegen codegen = new DefaultCodegen();
        final String path = "/tests/localResetConsumesAndProduces";
        final Operation p = model.getPaths().get(path).getGet();
        CodegenOperation op = codegen.fromOperation(path, "get", p, model.getDefinitions(), model);
        
        Assert.assertNotNull(op);
        Assert.assertFalse(op.hasConsumes);
        Assert.assertNull(op.consumes);
        Assert.assertFalse(op.hasProduces);
        Assert.assertNull(op.produces);

    }

    private static Swagger parseAndPrepareSwagger(String path) {
        Swagger swagger = new SwaggerParser().read(path);
        // resolve inline models
        new InlineModelResolver().flatten(swagger);
        return swagger;
    }
}
