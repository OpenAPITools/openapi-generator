package io.swagger.codegen.java;

import org.testng.Assert;
import org.testng.annotations.Test;

import io.swagger.codegen.CodegenType;
import io.swagger.codegen.languages.AbstractJavaCodegen;
import io.swagger.models.*;
import io.swagger.models.parameters.*;

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

    @Test
    public void toEnumVarNameShouldNotShortenUnderScore() throws Exception {
        Assert.assertEquals("UNDERSCORE", fakeJavaCodegen.toEnumVarName("_", "String"));
        Assert.assertEquals("__", fakeJavaCodegen.toEnumVarName("__", "String"));
        Assert.assertEquals("__", fakeJavaCodegen.toEnumVarName("_,.", "String"));
    }

    @Test
    public void toVarNameShouldAvoidOverloadingGetClassMethod() throws Exception {
        Assert.assertEquals("propertyClass", fakeJavaCodegen.toVarName("class"));
        Assert.assertEquals("propertyClass", fakeJavaCodegen.toVarName("_class"));
        Assert.assertEquals("propertyClass", fakeJavaCodegen.toVarName("__class"));
    }

    @Test
    public void toModelNameShouldUseProvidedMapping() throws Exception {
        fakeJavaCodegen.importMapping().put("json_myclass", "com.test.MyClass");
        Assert.assertEquals("com.test.MyClass", fakeJavaCodegen.toModelName("json_myclass"));
    }

    @Test
    public void toModelNameUsesPascalCase() throws Exception {
        Assert.assertEquals("JsonAnotherclass", fakeJavaCodegen.toModelName("json_anotherclass"));
    }

    @Test
    public void preprocessSwaggerWithFormParamsSetsContentType() {
        Path dummyPath = new Path()
                .post(new Operation().parameter(new FormParameter()))
                .get(new Operation());

        Swagger swagger = new Swagger()
                .path("dummy", dummyPath);

        fakeJavaCodegen.preprocessSwagger(swagger);

        Assert.assertNull(swagger.getPath("dummy").getGet().getVendorExtensions().get("x-contentType"));
        Assert.assertEquals(swagger.getPath("dummy").getPost().getVendorExtensions().get("x-contentType"), "application/x-www-form-urlencoded");
    }

    @Test
    public void preprocessSwaggerWithBodyParamsSetsContentType() {
        Path dummyPath = new Path()
                .post(new Operation().parameter(new BodyParameter()))
                .get(new Operation());

        Swagger swagger = new Swagger()
                .path("dummy", dummyPath);

        fakeJavaCodegen.preprocessSwagger(swagger);

        Assert.assertNull(swagger.getPath("dummy").getGet().getVendorExtensions().get("x-contentType"));
        Assert.assertEquals(swagger.getPath("dummy").getPost().getVendorExtensions().get("x-contentType"), "application/json");
    }

    @Test
    public void preprocessSwaggerWithNoFormOrBodyParamsDoesNotSetContentType() {
        Path dummyPath = new Path()
                .post(new Operation())
                .get(new Operation());
        
        Swagger swagger = new Swagger()
                .path("dummy", dummyPath);

        fakeJavaCodegen.preprocessSwagger(swagger);

        Assert.assertNull(swagger.getPath("dummy").getGet().getVendorExtensions().get("x-contentType"));
        Assert.assertNull(swagger.getPath("dummy").getPost().getVendorExtensions().get("x-contentType"));
    }

}
