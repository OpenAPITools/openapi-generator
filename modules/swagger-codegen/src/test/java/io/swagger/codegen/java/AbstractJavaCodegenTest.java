package io.swagger.codegen.java;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.PathItem;
import io.swagger.v3.oas.models.parameters.RequestBody;
import org.testng.Assert;
import org.testng.annotations.Test;

import io.swagger.codegen.CodegenType;
import io.swagger.codegen.languages.AbstractJavaCodegen;

public class AbstractJavaCodegenTest {

    private final AbstractJavaCodegen fakeJavaCodegen = new AbstractJavaCodegen() {
        @Override
        public CodegenType getTag() {
            return null;
        }

        @Override
        public String getName() {
            return null;
        }

        @Override
        public String getHelp() {
            return null;
        }
    };

    @Test(enabled = false)
    public void toEnumVarNameShouldNotShortenUnderScore() throws Exception {
        Assert.assertEquals("UNDERSCORE", fakeJavaCodegen.toEnumVarName("_", "String"));
        Assert.assertEquals("__", fakeJavaCodegen.toEnumVarName("__", "String"));
        Assert.assertEquals("__", fakeJavaCodegen.toEnumVarName("_,.", "String"));
    }

    @Test(enabled = false)
    public void toVarNameShouldAvoidOverloadingGetClassMethod() throws Exception {
        Assert.assertEquals("propertyClass", fakeJavaCodegen.toVarName("class"));
        Assert.assertEquals("propertyClass", fakeJavaCodegen.toVarName("_class"));
        Assert.assertEquals("propertyClass", fakeJavaCodegen.toVarName("__class"));
    }

    @Test(enabled = false)
    public void toModelNameShouldUseProvidedMapping() throws Exception {
        fakeJavaCodegen.importMapping().put("json_myclass", "com.test.MyClass");
        Assert.assertEquals("com.test.MyClass", fakeJavaCodegen.toModelName("json_myclass"));
    }

    @Test(enabled = false)
    public void toModelNameUsesPascalCase() throws Exception {
        Assert.assertEquals("JsonAnotherclass", fakeJavaCodegen.toModelName("json_anotherclass"));
    }

    @Test(enabled = false)
    public void preprocessSwaggerWithFormParamsSetsContentType() {
        PathItem dummyPath = new PathItem()
                .post(new Operation().requestBody(new RequestBody()))
                .get(new Operation());

        OpenAPI openAPI = new OpenAPI()
                .path("dummy", dummyPath);

        fakeJavaCodegen.preprocessOpenAPI(openAPI);

        Assert.assertNull(openAPI.getPaths().get("dummy").getGet().getExtensions().get("x-contentType"));
        // TODO: Assert.assertEquals(openAPI.getPath("dummy").getPost().getVendorExtensions().get("x-contentType"), "application/x-www-form-urlencoded");
    }

    @Test(enabled = false)
    public void preprocessSwaggerWithBodyParamsSetsContentType() {
        PathItem dummyPath = new PathItem()
                .post(new Operation().requestBody(new RequestBody()))
                .get(new Operation());

        OpenAPI openAPI = new OpenAPI()
                .path("dummy", dummyPath);

        fakeJavaCodegen.preprocessOpenAPI(openAPI);

        Assert.assertNull(openAPI.getPaths().get("dummy").getGet().getExtensions().get("x-contentType"));
        Assert.assertEquals(openAPI.getPaths().get("dummy").getPost().getExtensions().get("x-contentType"), "application/json");
    }

    @Test(enabled = false)
    public void preprocessSwaggerWithNoFormOrBodyParamsDoesNotSetContentType() {
        PathItem dummyPath = new PathItem()
                .post(new Operation())
                .get(new Operation());

        OpenAPI openAPI = new OpenAPI()
                .path("dummy", dummyPath);

        fakeJavaCodegen.preprocessOpenAPI(openAPI);

        Assert.assertNull(openAPI.getPaths().get("dummy").getGet().getExtensions().get("x-contentType"));
        Assert.assertNotNull(openAPI.getPaths().get("dummy").getPost().getExtensions().get("x-contentType"));
    }

}
