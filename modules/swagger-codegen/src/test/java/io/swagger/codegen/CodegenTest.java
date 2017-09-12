package io.swagger.codegen;

import io.swagger.models.Model;
import io.swagger.models.Operation;
import io.swagger.models.Swagger;
import io.swagger.models.properties.Property;
import io.swagger.parser.SwaggerParser;

import org.testng.Assert;
import org.testng.annotations.Test;

import java.util.List;

public class CodegenTest {

    @Test(description = "test sanitizeTag")
    public void sanitizeTagTest() {
        final DefaultCodegen codegen = new DefaultCodegen();
        Assert.assertEquals(codegen.sanitizeTag("foo"), "Foo");
        Assert.assertEquals(codegen.sanitizeTag("foo bar"), "FooBar");
        Assert.assertEquals(codegen.sanitizeTag("foo_bar"), "FooBar");
        Assert.assertEquals(codegen.sanitizeTag("foo1 bar2"), "Foo1Bar2");
        Assert.assertEquals(codegen.sanitizeTag("foo bar 1"), "FooBar1");
        Assert.assertEquals(codegen.sanitizeTag("1foo"), "_1foo");
    }

    @Test(description = "test camelize")
    public void camelizeNamesTest() {
        final DefaultCodegen codegen = new DefaultCodegen();

        Assert.assertEquals(codegen.camelize("foo"), "Foo");
        Assert.assertEquals(codegen.camelize(".foo"), "Foo");
        Assert.assertEquals(codegen.camelize(".foo.bar"), "FooBar");
        Assert.assertEquals(codegen.camelize("foo$bar"), "Foo$bar");
        Assert.assertEquals(codegen.camelize("foo_bar"), "FooBar");
        Assert.assertEquals(codegen.camelize("foo_bar_baz"), "FooBarBaz");
        Assert.assertEquals(codegen.camelize("foo/bar.baz"), "FooBarBaz");
        Assert.assertEquals(codegen.camelize("/foo/bar/baz.qux/corge"), "FooBarBazQuxCorge");
        Assert.assertEquals(codegen.camelize("foo-bar"), "FooBar");
        Assert.assertEquals(codegen.camelize("foo-bar-xyzzy"), "FooBarXyzzy");
    }

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
        Assert.assertEquals(file.dataType, "File");
        Assert.assertFalse(file.required);
        Assert.assertTrue(file.isFile);
        Assert.assertFalse(file.hasMore);
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
        Assert.assertFalse(idParam.hasMore);

        Assert.assertEquals(op.allParams.size(), 3);
        Assert.assertEquals(op.formParams.size(), 2);

        final CodegenParameter nameParam = op.formParams.get(0);
        Assert.assertTrue(nameParam.isFormParam);
        Assert.assertTrue(nameParam.notFile);
        Assert.assertEquals(nameParam.dataType, "String");
        Assert.assertFalse(nameParam.required);
        Assert.assertTrue(nameParam.hasMore);

        final CodegenParameter statusParam = op.formParams.get(1);
        Assert.assertTrue(statusParam.isFormParam);
        Assert.assertTrue(statusParam.notFile);
        Assert.assertEquals(statusParam.dataType, "String");
        Assert.assertFalse(statusParam.required);
        Assert.assertFalse(statusParam.hasMore);
    }

    @Test(description = "handle enum array in query parameter test")
    public void enumArrayQueryParameterTest() {
        final Swagger model = parseAndPrepareSwagger("src/test/resources/2_0/petstore.json");
        final DefaultCodegen codegen = new DefaultCodegen();
        final String path = "/pet/findByStatus";
        final Operation p = model.getPaths().get(path).getGet();
        final CodegenOperation op = codegen.fromOperation(path, "get", p, model.getDefinitions());

        Assert.assertEquals(op.queryParams.size(), 1);

        final CodegenParameter statusParam = op.queryParams.get(0);
        Assert.assertEquals(statusParam.items.datatypeWithEnum, "StatusEnum");
        Assert.assertNotNull(statusParam.items);
        Assert.assertTrue(statusParam.items.isEnum);
        Assert.assertEquals(statusParam.items._enum.size(), 3);
    }

    @Test(description = "handle enum in query parameter test")
    public void enumQueryParameterTest() {
        final Swagger model = parseAndPrepareSwagger("src/test/resources/2_0/petstore.json");
        final DefaultCodegen codegen = new DefaultCodegen();
        final String path = "/pet/findByStatus";
        final Operation p = model.getPaths().get(path).getGet();
        final CodegenOperation op = codegen.fromOperation(path, "get", p, model.getDefinitions());

        Assert.assertEquals(op.queryParams.size(), 1);

        final CodegenParameter statusParam = op.queryParams.get(0);
        Assert.assertEquals(statusParam.datatypeWithEnum, "List");
        Assert.assertEquals(statusParam.baseType, "String");
        Assert.assertTrue(statusParam.isEnum);
        Assert.assertEquals(((List)statusParam.allowableValues.get("values")).size(), 3);
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

    @Test(description = "return file when response format is file")
    public void fileResponeseTest() {
        final Swagger model = parseAndPrepareSwagger("src/test/resources/2_0/fileResponseTest.json");
        final DefaultCodegen codegen = new DefaultCodegen();
        final String path = "/tests/fileResponse";
        final Operation p = model.getPaths().get(path).getGet();
        final CodegenOperation op = codegen.fromOperation(path, "get", p, model.getDefinitions());

        Assert.assertEquals(op.returnType, "File");
        Assert.assertTrue(op.responses.get(0).isFile);
        Assert.assertTrue(op.isResponseFile);
    }
    
    @Test(description = "discriminator is present")
    public void discriminatorTest() {
        final Swagger model = parseAndPrepareSwagger("src/test/resources/2_0/discriminatorTest.json");
        final DefaultCodegen codegen = new DefaultCodegen();
        final String path = "/pets";
        final Operation p = model.getPaths().get(path).getGet();
        final CodegenOperation op = codegen.fromOperation(path, "get", p, model.getDefinitions());

        Assert.assertEquals(op.discriminator, "className");
    }

    @Test(description = "handle simple composition")
    public void  simpleCompositionTest() {
        final Swagger swagger = parseAndPrepareSwagger("src/test/resources/2_0/allOfTest.yaml");
        final DefaultCodegen codegen = new DefaultCodegen();
        codegen.supportsInheritance = true;
        final Model model = swagger.getDefinitions().get("SimpleComposition");
        CodegenModel composed = codegen.fromModel("SimpleComposition", model, swagger.getDefinitions());

        Assert.assertEquals(composed.vars.size(), 3);
        Assert.assertEquals(composed.vars.get(0).baseName, "modelOneProp");
        Assert.assertEquals(composed.vars.get(1).baseName, "modelTwoProp");
        Assert.assertEquals(composed.vars.get(2).baseName, "simpleCompositionProp");
        Assert.assertNull(composed.parent);
    }

    @Test(description = "handle multi level composition")
    public void  multiCompositionTest() {
        final Swagger swagger = parseAndPrepareSwagger("src/test/resources/2_0/allOfTest.yaml");
        final DefaultCodegen codegen = new DefaultCodegen();
        codegen.supportsInheritance = true;
        final Model model = swagger.getDefinitions().get("CompositionOfSimpleComposition");
        CodegenModel composed = codegen.fromModel("CompositionOfSimpleComposition", model, swagger.getDefinitions());

        Assert.assertEquals(composed.vars.size(), 5);
        Assert.assertEquals(composed.vars.get(0).baseName, "modelOneProp");
        Assert.assertEquals(composed.vars.get(1).baseName, "modelTwoProp");
        Assert.assertEquals(composed.vars.get(2).baseName, "simpleCompositionProp");
        Assert.assertEquals(composed.vars.get(3).baseName, "modelThreeProp");
        Assert.assertEquals(composed.vars.get(4).baseName, "compositionOfSimpleCompositionProp");
        Assert.assertNull(composed.parent);
    }

    @Test(description = "handle simple inheritance")
    public void  simpleInheritanceTest() {
        final Swagger swagger = parseAndPrepareSwagger("src/test/resources/2_0/allOfTest.yaml");
        final DefaultCodegen codegen = new DefaultCodegen();
        codegen.supportsInheritance = true;
        final Model model = swagger.getDefinitions().get("ChildOfSimpleParent");
        CodegenModel child = codegen.fromModel("ChildOfSimpleParent", model, swagger.getDefinitions());

        Assert.assertEquals(child.vars.size(), 2);
        Assert.assertEquals(child.vars.get(0).baseName, "modelOneProp");
        Assert.assertEquals(child.vars.get(1).baseName, "childOfSimpleParentProp");
        Assert.assertEquals(child.parent, "SimpleParent");
    }

    @Test(description = "handle multi level inheritance")
    public void  multiInheritanceTest() {
        final Swagger swagger = parseAndPrepareSwagger("src/test/resources/2_0/allOfTest.yaml");
        final DefaultCodegen codegen = new DefaultCodegen();
        codegen.supportsInheritance = true;
        final Model model = swagger.getDefinitions().get("ChildOfChildOfSimpleParent");
        CodegenModel child = codegen.fromModel("ChildOfChildOfSimpleParent", model, swagger.getDefinitions());

        Assert.assertEquals(child.vars.size(), 1);
        Assert.assertEquals(child.vars.get(0).baseName, "childOfChildOfSimpleParentProp");
        Assert.assertEquals(child.parent, "ChildOfSimpleParent");
    }

    @Test(description = "copy properties in multi level inheritance if supportsInheritance is false")
    public void  noSupportsInheritanceTest() {
        final Swagger swagger = parseAndPrepareSwagger("src/test/resources/2_0/allOfTest.yaml");
        final DefaultCodegen codegen = new DefaultCodegen();
        final Model model = swagger.getDefinitions().get("ChildOfChildOfSimpleParent");
        CodegenModel child = codegen.fromModel("ChildOfChildOfSimpleParent", model, swagger.getDefinitions());

        Assert.assertEquals(child.vars.size(), 5);
        Assert.assertEquals(child.vars.get(0).baseName, "modelOneProp");
        Assert.assertEquals(child.vars.get(1).baseName, "disc");
        Assert.assertEquals(child.vars.get(2).baseName, "simpleParentProp");
        Assert.assertEquals(child.vars.get(3).baseName, "childOfSimpleParentProp");
        Assert.assertEquals(child.vars.get(4).baseName, "childOfChildOfSimpleParentProp");
        Assert.assertEquals(child.parent, "ChildOfSimpleParent");
    }

    @Test(description = "don't copy interfaces properties if supportsMixins is true")
    public void  supportsMixinsTest() {
        final Swagger swagger = parseAndPrepareSwagger("src/test/resources/2_0/allOfTest.yaml");
        final DefaultCodegen codegen = new DefaultCodegen();
        codegen.supportsInheritance = true;
        codegen.supportsMixins = true;
        final Model model = swagger.getDefinitions().get("ChildOfChildOfSimpleParent");
        CodegenModel child = codegen.fromModel("ChildOfChildOfSimpleParent", model, swagger.getDefinitions());

        Assert.assertEquals(child.vars.size(), 1);
        Assert.assertEquals(child.vars.get(0).baseName, "childOfChildOfSimpleParentProp");
        Assert.assertEquals(child.allVars.size(), 5);
        Assert.assertEquals(child.allVars.get(0).baseName, "modelOneProp");
        Assert.assertEquals(child.allVars.get(1).baseName, "disc");
        Assert.assertEquals(child.allVars.get(2).baseName, "simpleParentProp");
        Assert.assertEquals(child.allVars.get(3).baseName, "childOfSimpleParentProp");
        Assert.assertEquals(child.allVars.get(4).baseName, "childOfChildOfSimpleParentProp");

        Assert.assertEquals(child.parent, "ChildOfSimpleParent");
    }

    @Test(description = "handle inheritance from composed model")
    public void  inheritanceOfComposedModelTest() {
        final Swagger swagger = parseAndPrepareSwagger("src/test/resources/2_0/allOfTest.yaml");
        final DefaultCodegen codegen = new DefaultCodegen();
        codegen.supportsInheritance = true;
        final Model model = swagger.getDefinitions().get("ChildOfComposedParent");
        CodegenModel child = codegen.fromModel("ChildOfComposedParent", model, swagger.getDefinitions());

        Assert.assertEquals(child.vars.size(), 1);
        Assert.assertEquals(child.vars.get(0).baseName, "childOfComposedParentProp");
        Assert.assertEquals(child.parent, "ComposedParent");
    }

    @Test(description = "handle multi level inheritance from composed model")
    public void  multiInheritanceOfComposedModelTest() {
        final Swagger swagger = parseAndPrepareSwagger("src/test/resources/2_0/allOfTest.yaml");
        final DefaultCodegen codegen = new DefaultCodegen();
        codegen.supportsInheritance = true;
        final Model model = swagger.getDefinitions().get("ChildOfChildOfComposedParent");
        CodegenModel child = codegen.fromModel("ChildOfChildOfComposedParent", model, swagger.getDefinitions());

        Assert.assertEquals(child.vars.size(), 1);
        Assert.assertEquals(child.vars.get(0).baseName, "childOfChildOfComposedParentProp");
        Assert.assertEquals(child.parent, "ChildOfComposedParent");
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

    @Test(description = "isDeprecated is present")
    public void deprecatedParamTest() {
        final Swagger model = parseAndPrepareSwagger("src/test/resources/2_0/petstore.json");
        final DefaultCodegen codegen = new DefaultCodegen();
        final String path = "/pet/findByTags";
        final Operation p = model.getPaths().get(path).getGet();
        final CodegenOperation op = codegen.fromOperation(path, "get", p, model.getDefinitions());

        Assert.assertTrue(op.isDeprecated);
    }
}
