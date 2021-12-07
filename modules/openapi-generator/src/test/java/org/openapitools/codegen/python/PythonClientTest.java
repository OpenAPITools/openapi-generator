/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
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

package org.openapitools.codegen.python;
import com.google.common.io.Resources;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import org.openapitools.codegen.config.CodegenConfigurator;

import com.google.common.collect.Sets;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.media.*;
import io.swagger.v3.parser.util.SchemaTypeUtil;

import java.io.File;
import java.math.BigDecimal;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.openapitools.codegen.*;
import org.openapitools.codegen.languages.PythonClientCodegen;
import org.openapitools.codegen.utils.ModelUtils;
import org.testng.Assert;
import org.testng.annotations.Test;

@SuppressWarnings("static-method")
public class PythonClientTest {

    @Test(description = "convert a python model with dots")
    public void modelTest() {
        final OpenAPI openAPI= TestUtils.parseFlattenSpec("src/test/resources/2_0/v1beta3.json");
        final DefaultCodegen codegen = new PythonClientCodegen();
        codegen.setOpenAPI(openAPI);

        codegen.setOpenAPI(openAPI);
        final CodegenModel simpleName = codegen.fromModel("v1beta3.Binding", openAPI.getComponents().getSchemas().get("v1beta3.Binding"));
        Assert.assertEquals(simpleName.name, "v1beta3.Binding");
        Assert.assertEquals(simpleName.classname, "V1beta3Binding");
        Assert.assertEquals(simpleName.classVarName, "v1beta3_binding");

        codegen.setOpenAPI(openAPI);
        final CodegenModel compoundName = codegen.fromModel("v1beta3.ComponentStatus", openAPI.getComponents().getSchemas().get("v1beta3.ComponentStatus"));
        Assert.assertEquals(compoundName.name, "v1beta3.ComponentStatus");
        Assert.assertEquals(compoundName.classname, "V1beta3ComponentStatus");
        Assert.assertEquals(compoundName.classVarName, "v1beta3_component_status");

        final String path = "/api/v1beta3/namespaces/{namespaces}/bindings";
        final Operation operation = openAPI.getPaths().get(path).getPost();
        final CodegenOperation codegenOperation = codegen.fromOperation(path, "post", operation, null);
        Assert.assertEquals(codegenOperation.returnType, "V1beta3Binding");
        Assert.assertEquals(codegenOperation.returnBaseType, "V1beta3Binding");
    }

    @Test(description = "convert a simple java model")
    public void simpleModelTest() {
        final Schema schema = new Schema()
                .description("a sample model")
                .addProperties("id", new IntegerSchema().format(SchemaTypeUtil.INTEGER64_FORMAT))
                .addProperties("name", new StringSchema())
                .addProperties("createdAt", new DateTimeSchema())
                .addRequiredItem("id")
                .addRequiredItem("name");
        final DefaultCodegen codegen = new PythonClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", schema);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", schema);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.description, "a sample model");
        Assert.assertEquals(cm.vars.size(), 3);

        final CodegenProperty property1 = cm.vars.get(0);
        Assert.assertEquals(property1.baseName, "id");
        Assert.assertEquals(property1.dataType, "int");
        Assert.assertEquals(property1.name, "id");
        Assert.assertNull(property1.defaultValue);
        Assert.assertEquals(property1.baseType, "int");
        Assert.assertTrue(property1.required);
        Assert.assertTrue(property1.isPrimitiveType);

        final CodegenProperty property2 = cm.vars.get(1);
        Assert.assertEquals(property2.baseName, "name");
        Assert.assertEquals(property2.dataType, "str");
        Assert.assertEquals(property2.name, "name");
        Assert.assertNull(property2.defaultValue);
        Assert.assertEquals(property2.baseType, "str");
        Assert.assertTrue(property2.required);
        Assert.assertTrue(property2.isPrimitiveType);

        final CodegenProperty property3 = cm.vars.get(2);
        Assert.assertEquals(property3.baseName, "createdAt");
        Assert.assertEquals(property3.dataType, "datetime");
        Assert.assertEquals(property3.name, "created_at");
        Assert.assertNull(property3.defaultValue);
        Assert.assertEquals(property3.baseType, "datetime");
        Assert.assertFalse(property3.required);
    }

    @Test(description = "convert a model with list property")
    public void listPropertyTest() {
        final Schema model = new Schema()
                .description("a sample model")
                .addProperties("id", new IntegerSchema().format(SchemaTypeUtil.INTEGER64_FORMAT))
                .addProperties("urls", new ArraySchema()
                        .items(new StringSchema()))
                .addRequiredItem("id");
        final DefaultCodegen codegen = new PythonClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", model);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", model);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.description, "a sample model");
        Assert.assertEquals(cm.vars.size(), 2);

        final CodegenProperty property1 = cm.vars.get(0);
        Assert.assertEquals(property1.baseName, "id");
        Assert.assertEquals(property1.dataType, "int");
        Assert.assertEquals(property1.name, "id");
        Assert.assertNull(property1.defaultValue);
        Assert.assertEquals(property1.baseType, "int");
        Assert.assertTrue(property1.required);
        Assert.assertTrue(property1.isPrimitiveType);

        final CodegenProperty property2 = cm.vars.get(1);
        Assert.assertEquals(property2.baseName, "urls");
        Assert.assertEquals(property2.dataType, "[str]");
        Assert.assertEquals(property2.name, "urls");
        Assert.assertNull(property2.defaultValue);
        Assert.assertEquals(property2.baseType, "list");
        Assert.assertEquals(property2.containerType, "array");
        Assert.assertFalse(property2.required);
        Assert.assertTrue(property2.isPrimitiveType);
        Assert.assertTrue(property2.isContainer);
    }

    @Test(description = "convert a model with a map property")
    public void mapPropertyTest() {
        final Schema model = new Schema()
                .description("a sample model")
                .addProperties("translations", new MapSchema()
                        .additionalProperties(new StringSchema()))
                .addRequiredItem("id");
        final DefaultCodegen codegen = new PythonClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", model);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", model);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.description, "a sample model");
        Assert.assertEquals(cm.vars.size(), 1);

        final CodegenProperty property1 = cm.vars.get(0);
        Assert.assertEquals(property1.baseName, "translations");
        Assert.assertEquals(property1.dataType, "{str: (str,)}");
        Assert.assertEquals(property1.name, "translations");
        Assert.assertEquals(property1.baseType, "dict");
        Assert.assertEquals(property1.containerType, "map");
        Assert.assertFalse(property1.required);
        Assert.assertTrue(property1.isContainer);
        Assert.assertTrue(property1.isPrimitiveType);
    }

    @Test(description = "convert a model with complex property")
    public void complexPropertyTest() {
        final DefaultCodegen codegen = new PythonClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPI();
        final Schema model = new Schema()
                .description("a sample model")
                .addProperties("children", new Schema().$ref("#/components/schemas/Children"));
        final Schema children = new Schema()
                .type("object")
                .addProperties("number", new Schema().type("integer"));
        openAPI.getComponents().addSchemas("sample", model);
        openAPI.getComponents().addSchemas("Children", children);
        codegen.setOpenAPI(openAPI);

        final CodegenModel cm = codegen.fromModel("sample", model);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.description, "a sample model");
        Assert.assertEquals(cm.vars.size(), 1);

        final CodegenProperty property1 = cm.vars.get(0);
        Assert.assertEquals(property1.baseName, "children");
        Assert.assertEquals(property1.dataType, "Children");
        Assert.assertEquals(property1.name, "children");
        Assert.assertEquals(property1.baseType, "Children");
        Assert.assertFalse(property1.required);
        Assert.assertFalse(property1.isContainer);
    }

    @Test(description = "convert a model with complex list property")
    public void complexListPropertyTest() {
        final DefaultCodegen codegen = new PythonClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPI();
        final Schema model = new Schema()
                .description("a sample model")
                .addProperties("children", new ArraySchema()
                        .items(new Schema().$ref("#/components/schemas/Children")));
        final Schema children = new Schema()
                .type("object")
                .addProperties("number", new Schema().type("integer"));
        openAPI.getComponents().addSchemas("sample", model);
        openAPI.getComponents().addSchemas("Children", children);
        codegen.setOpenAPI(openAPI);

        final CodegenModel cm = codegen.fromModel("sample", model);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.description, "a sample model");
        Assert.assertEquals(cm.vars.size(), 1);

        final CodegenProperty property1 = cm.vars.get(0);
        Assert.assertEquals(property1.baseName, "children");
        Assert.assertEquals(property1.complexType, "Children");
        Assert.assertEquals(property1.dataType, "[Children]");
        Assert.assertEquals(property1.name, "children");
        Assert.assertEquals(property1.baseType, "list");
        Assert.assertEquals(property1.containerType, "array");
        Assert.assertFalse(property1.required);
        Assert.assertTrue(property1.isContainer);
    }

    @Test(description = "convert a model with complex map property")
    public void complexMapPropertyTest() {
        final DefaultCodegen codegen = new PythonClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPI();
        final Schema model = new Schema()
                .description("a sample model")
                .addProperties("children", new MapSchema()
                        .additionalProperties(new Schema().$ref("#/components/schemas/Children")));
        final Schema children = new Schema()
                .type("object")
                .addProperties("number", new Schema().type("integer"));
        openAPI.getComponents().addSchemas("sample", model);
        openAPI.getComponents().addSchemas("Children", children);
        codegen.setOpenAPI(openAPI);

        final CodegenModel cm = codegen.fromModel("sample", model);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.description, "a sample model");
        Assert.assertEquals(cm.vars.size(), 1);
        Assert.assertEquals(Sets.intersection(cm.imports, Sets.newHashSet("Children")).size(), 1);

        final CodegenProperty property1 = cm.vars.get(0);
        Assert.assertEquals(property1.baseName, "children");
        Assert.assertEquals(property1.complexType, "Children");
        Assert.assertEquals(property1.dataType, "{str: (Children,)}");
        Assert.assertEquals(property1.name, "children");
        Assert.assertEquals(property1.baseType, "dict");
        Assert.assertEquals(property1.containerType, "map");
        Assert.assertFalse(property1.required);
        Assert.assertTrue(property1.isContainer);
    }


    // should not start with 'null'. need help from the community to investigate further
    @Test(description = "convert an array model")
    public void arrayModelTest() {
        final PythonClientCodegen codegen = new PythonClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPI();

        final Schema model = new ArraySchema()
                .items(new Schema().$ref("#/components/schemas/Children"))
                .description("an array model");
        final Schema children = new Schema()
                .type("object")
                .addProperties("number", new Schema().type("integer"));
        openAPI.getComponents().addSchemas("sample", model);
        openAPI.getComponents().addSchemas("Children", children);
        codegen.setOpenAPI(openAPI);

        final CodegenModel cm = codegen.fromModel("sample", model);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.classVarName, "sample");
        Assert.assertEquals(cm.description, "an array model");
        Assert.assertEquals(cm.vars.size(), 0);  // the array model has no vars
        Assert.assertEquals(cm.parent, "list");
        Assert.assertEquals(cm.imports.size(), 1);
        Assert.assertEquals(Sets.intersection(cm.imports, Sets.newHashSet("Children")).size(), 1);

        final Map<String, Integer> childExample = new HashMap<>();
        childExample.put("number", 3);
        final List<Map<String, Integer>> example =  Arrays.asList(childExample);
        String exampleValue = codegen.toExampleValue(model, example);
        Assert.assertEquals("[Children(number=1,),]", exampleValue.replaceAll("\\s+",""));
    }

    // should not start with 'null'. need help from the community to investigate further
    @Test(description = "convert a map model")
    public void mapModelTest() {
        final DefaultCodegen codegen = new PythonClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPI();
        final Schema sample = new Schema()
                .description("a map model")
                .additionalProperties(new Schema().$ref("#/components/schemas/Children"));
        final Schema children = new Schema()
                .type("object")
                .addProperties("number", new Schema().type("integer"));
        openAPI.getComponents().addSchemas("sample", sample);
        openAPI.getComponents().addSchemas("Children", children);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", sample);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.description, "a map model");
        Assert.assertEquals(cm.vars.size(), 0);
        Assert.assertEquals(cm.parent, null);
        Assert.assertEquals(cm.imports.size(), 1);
    }

    @Test(description = "parse date and date-time example value")
    public void parseDateAndDateTimeExamplesTest() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/python/petstore-with-fake-endpoints-models-for-testing-with-http-signature.yaml");
        final DefaultCodegen codegen = new PythonClientCodegen();

        Schema modelSchema = ModelUtils.getSchema(openAPI, "DateTimeTest");
        String defaultValue = codegen.toDefaultValue(modelSchema);
        Assert.assertEquals(defaultValue, "dateutil_parser('2010-01-01T10:10:10.000111+01:00')");
    }

    @Test(description = "format imports of models containing special characters")
    public void importSpecialModelNameTest() {
        final PythonClientCodegen codegen = new PythonClientCodegen();

        String importValue = codegen.toModelImport("special.ModelName");
        Assert.assertEquals(importValue, "from models.special_model_name import SpecialModelName");
    }

    @Test(description = "format imports of models containing special characters")
    public void defaultSettingInPrimitiveModelWithValidations() {
        final PythonClientCodegen codegen = new PythonClientCodegen();

        OpenAPI openAPI = TestUtils.createOpenAPI();
        final Schema noDefault = new Schema()
                .type("number")
                .minimum(new BigDecimal("10"));
        final Schema hasDefault = new Schema()
                .type("number")
                .minimum(new BigDecimal("10"));
        hasDefault.setDefault("15.0");
        final Schema noDefaultEumLengthOne = new Schema()
                .type("number")
                .minimum(new BigDecimal("10"));
        noDefaultEumLengthOne.setEnum(Arrays.asList("15.0"));
        openAPI.getComponents().addSchemas("noDefaultModel", noDefault);
        openAPI.getComponents().addSchemas("hasDefaultModel", hasDefault);
        openAPI.getComponents().addSchemas("noDefaultEumLengthOneModel", noDefaultEumLengthOne);
        codegen.setOpenAPI(openAPI);

        final CodegenModel noDefaultModel = codegen.fromModel("noDefaultModel", noDefault);
        Assert.assertEquals(noDefaultModel.defaultValue, null);
        Assert.assertEquals(noDefaultModel.hasRequired, true);

        final CodegenModel hasDefaultModel = codegen.fromModel("hasDefaultModel", hasDefault);
        Assert.assertEquals(hasDefaultModel.defaultValue, "15.0");
        Assert.assertEquals(hasDefaultModel.hasRequired, false);

        final CodegenModel noDefaultEumLengthOneModel = codegen.fromModel("noDefaultEumLengthOneModel", noDefaultEumLengthOne);
        Assert.assertEquals(noDefaultEumLengthOneModel.defaultValue, "15.0");
        Assert.assertEquals(noDefaultEumLengthOneModel.hasRequired, false);
    }

    @Test
    public void testObjectModelWithRefedAdditionalPropertiesIsGenerated() throws Exception {
        File output = Files.createTempDirectory("test").toFile();

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("python")
                .setInputSpec("src/test/resources/3_0/issue_7372.yaml")
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        final ClientOptInput clientOptInput = configurator.toClientOptInput();
        DefaultGenerator generator = new DefaultGenerator();
        List<File> files = generator.opts(clientOptInput).generate();

        TestUtils.ensureContainsFile(files, output, "openapi_client/model/a.py");
        TestUtils.ensureContainsFile(files, output, "openapi_client/model/b.py");
        output.deleteOnExit();
    }

    @Test
    public void testFreeFormSchemas() throws Exception {
        File output = Files.createTempDirectory("test").toFile();

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("python")
                .setInputSpec("src/test/resources/3_0/issue_7361.yaml")
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        final ClientOptInput clientOptInput = configurator.toClientOptInput();
        DefaultGenerator generator = new DefaultGenerator();
        List<File> files = generator.opts(clientOptInput).generate();

        TestUtils.ensureContainsFile(files, output, "openapi_client/model/free_form_with_validation.py");
        TestUtils.ensureContainsFile(files, output, "openapi_client/model/free_form_interface.py");
        TestUtils.ensureDoesNotContainsFile(files, output, "openapi_client/model/free_form.py");
        output.deleteOnExit();
    }

    @Test(description = "tests ObjectWithValidations")
    public void testObjectWithValidations() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/issue_7361.yaml");
        final DefaultCodegen codegen = new PythonClientCodegen();
        codegen.setOpenAPI(openAPI);

        String modelName = "FreeFormWithValidation";
        Schema modelSchema = ModelUtils.getSchema(openAPI, modelName);
        final CodegenModel model = codegen.fromModel(modelName, modelSchema);
        Assert.assertEquals((int) model.getMinProperties(), 1);
    }

    @Test(description = "tests RecursiveToExample")
    public void testRecursiveToExample() throws IOException {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/issue_8052_recursive_model.yaml");
        final PythonClientCodegen codegen = new PythonClientCodegen();
        codegen.setOpenAPI(openAPI);

        final Operation operation = openAPI.getPaths().get("/geojson").getPost();
        Schema schema = ModelUtils.getSchemaFromRequestBody(operation.getRequestBody());
        String exampleValue = codegen.toExampleValue(schema, null);

        // uncomment if you need to regenerate the expected value
        //        PrintWriter printWriter = new PrintWriter("src/test/resources/3_0/issue_8052_recursive_model_expected_value.txt");
        //        printWriter.write(exampleValue);
        //        printWriter.close();
        //        org.junit.Assert.assertTrue(false);

        String expectedValue = Resources.toString(
                Resources.getResource("3_0/issue_8052_recursive_model_expected_value.txt"),
                StandardCharsets.UTF_8);
        expectedValue = expectedValue.replaceAll("\\r\\n", "\n");


        Assert.assertEquals(exampleValue.trim(), expectedValue.trim());

    }

    @Test(description = "tests NoProxyPyClient")
    public void testNoProxyPyClient() throws Exception {

        final String gen = "python";
        final String spec = "src/test/resources/3_0/petstore.yaml";

        File output = Files.createTempDirectory("test").toFile();
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName(gen)
                .setInputSpec(spec)
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));
        final ClientOptInput clientOptInput = configurator.toClientOptInput();
        DefaultGenerator generator = new DefaultGenerator();
        List<File> files = generator.opts(clientOptInput).generate();

        for (String f : new String[] { "openapi_client/configuration.py", "openapi_client/rest.py" } ) {
            TestUtils.ensureContainsFile(files, output, f);
            Path p = output.toPath().resolve(f);
            TestUtils.assertFileContains(p, "no_proxy");
        }
    }

}
