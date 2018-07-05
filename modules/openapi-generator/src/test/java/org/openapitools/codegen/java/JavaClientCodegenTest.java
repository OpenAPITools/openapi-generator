/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 * Copyright 2018 SmartBear Software
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.openapitools.codegen.java;

import com.google.common.collect.ImmutableMap;

import io.swagger.parser.OpenAPIParser;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.ComposedSchema;
import io.swagger.v3.oas.models.media.Content;
import io.swagger.v3.oas.models.media.IntegerSchema;
import io.swagger.v3.oas.models.media.MediaType;
import io.swagger.v3.oas.models.media.ObjectSchema;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.media.StringSchema;
import io.swagger.v3.oas.models.parameters.RequestBody;
import io.swagger.v3.parser.core.models.ParseOptions;
import io.swagger.v3.parser.util.SchemaTypeUtil;

import org.openapitools.codegen.ClientOptInput;
import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.CodegenModel;
import org.openapitools.codegen.CodegenModelFactory;
import org.openapitools.codegen.CodegenModelType;
import org.openapitools.codegen.CodegenOperation;
import org.openapitools.codegen.CodegenParameter;
import org.openapitools.codegen.CodegenProperty;
import org.openapitools.codegen.MockDefaultGenerator;
import org.openapitools.codegen.MockDefaultGenerator.WrittenTemplateBasedFile;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.openapitools.codegen.languages.JavaClientCodegen;
import org.openapitools.codegen.utils.ModelUtils;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.io.File;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

public class JavaClientCodegenTest {

    @Test
    public void modelInheritanceSupportInGson() throws Exception {
        List<Map<String, Object>> allModels = new ArrayList<>();

        CodegenModel parent1 = CodegenModelFactory.newInstance(CodegenModelType.MODEL);
        parent1.setName("parent1");
        parent1.setClassname("test.Parent1");

        Map<String, Object> modelMap = new HashMap<>();
        modelMap.put("model", parent1);
        allModels.add(modelMap);

        CodegenModel parent2 = CodegenModelFactory.newInstance(CodegenModelType.MODEL);
        parent2.setName("parent2");
        parent2.setClassname("test.Parent2");

        modelMap = new HashMap<>();
        modelMap.put("model", parent2);
        allModels.add(modelMap);

        CodegenModel model1 = CodegenModelFactory.newInstance(CodegenModelType.MODEL);
        model1.setName("model1");
        model1.setClassname("test.Model1");
        model1.setParentModel(parent1);

        modelMap = new HashMap<>();
        modelMap.put("model", model1);
        allModels.add(modelMap);

        CodegenModel model2 = CodegenModelFactory.newInstance(CodegenModelType.MODEL);
        model2.setName("model2");
        model2.setClassname("test.Model2");
        model2.setParentModel(parent1);

        modelMap = new HashMap<>();
        modelMap.put("model", model2);
        allModels.add(modelMap);

        CodegenModel model3 = CodegenModelFactory.newInstance(CodegenModelType.MODEL);
        model3.setName("model3");
        model3.setClassname("test.Model3");
        model3.setParentModel(parent1);

        modelMap = new HashMap<>();
        modelMap.put("model", model3);
        allModels.add(modelMap);

        CodegenModel model4 = CodegenModelFactory.newInstance(CodegenModelType.MODEL);
        model4.setName("model4");
        model4.setClassname("test.Model4");
        model4.setParentModel(parent2);

        modelMap = new HashMap<>();
        modelMap.put("model", model4);
        allModels.add(modelMap);

        CodegenModel model5 = CodegenModelFactory.newInstance(CodegenModelType.MODEL);
        model5.setName("model5");
        model5.setClassname("test.Model5");
        model5.setParentModel(parent2);

        modelMap = new HashMap<>();
        modelMap.put("model", model5);
        allModels.add(modelMap);

        List<Map<String, Object>> parentsList = JavaClientCodegen.modelInheritanceSupportInGson(allModels);

        Assert.assertNotNull(parentsList);
        Assert.assertEquals(parentsList.size(), 2);

        Map<String, Object> parent = parentsList.get(0);
        Assert.assertEquals(parent.get("classname"), "test.Parent1");

        List<CodegenModel> children = (List<CodegenModel>) parent.get("children");
        Assert.assertNotNull(children);
        Assert.assertEquals(children.size(), 3);

        Map<String, Object> models = (Map<String, Object>) children.get(0);
        Assert.assertEquals(models.get("name"), "model1");
        Assert.assertEquals(models.get("classname"), "test.Model1");

        models = (Map<String, Object>) children.get(1);
        Assert.assertEquals(models.get("name"), "model2");
        Assert.assertEquals(models.get("classname"), "test.Model2");

        models = (Map<String, Object>) children.get(2);
        Assert.assertEquals(models.get("name"), "model3");
        Assert.assertEquals(models.get("classname"), "test.Model3");

        parent = parentsList.get(1);
        Assert.assertEquals(parent.get("classname"), "test.Parent2");

        children = (List<CodegenModel>) parent.get("children");
        Assert.assertNotNull(children);
        Assert.assertEquals(children.size(), 2);

        models = (Map<String, Object>) children.get(0);
        Assert.assertEquals(models.get("name"), "model4");
        Assert.assertEquals(models.get("classname"), "test.Model4");

        models = (Map<String, Object>) children.get(1);
        Assert.assertEquals(models.get("name"), "model5");
        Assert.assertEquals(models.get("classname"), "test.Model5");
    }

    @Test
    public void arraysInRequestBody() throws Exception {
        final JavaClientCodegen codegen = new JavaClientCodegen();

        RequestBody body1 = new RequestBody();
        body1.setDescription("A list of ids");
        body1.setContent(new Content().addMediaType("application/json", new MediaType().schema(new ArraySchema().items(new StringSchema()))));
        CodegenParameter codegenParameter1 = codegen.fromRequestBody(body1 , new HashMap<String, Schema>(), new HashSet<String>(), null);
        Assert.assertEquals(codegenParameter1.description, "A list of ids");
        Assert.assertEquals(codegenParameter1.dataType, "List<String>");
        Assert.assertEquals(codegenParameter1.baseType, "List");

        RequestBody body2 = new RequestBody();
        body2.setDescription("A list of list of values");
        body2.setContent(new Content().addMediaType("application/json", new MediaType().schema(new ArraySchema().items(new ArraySchema().items(new IntegerSchema())))));
        CodegenParameter codegenParameter2 = codegen.fromRequestBody(body2 , new HashMap<String, Schema>(), new HashSet<String>(), null);
        Assert.assertEquals(codegenParameter2.description, "A list of list of values");
        Assert.assertEquals(codegenParameter2.dataType, "List<List<Integer>>");
        Assert.assertEquals(codegenParameter2.baseType, "List");

        RequestBody body3 = new RequestBody();
        body3.setDescription("A list of points");
        body3.setContent(new Content().addMediaType("application/json", new MediaType().schema(new ArraySchema().items(new ObjectSchema().$ref("#/components/schemas/Point")))));
        ObjectSchema point = new ObjectSchema();
        point.addProperties("message", new StringSchema());
        point.addProperties("x", new IntegerSchema().format(SchemaTypeUtil.INTEGER32_FORMAT));
        point.addProperties("y", new IntegerSchema().format(SchemaTypeUtil.INTEGER32_FORMAT));
        CodegenParameter codegenParameter3 = codegen.fromRequestBody(body3 , Collections.<String, Schema>singletonMap("Point", point), new HashSet<String>(), null);
        Assert.assertEquals(codegenParameter3.description, "A list of points");
        Assert.assertEquals(codegenParameter3.dataType, "List<Point>");
        Assert.assertEquals(codegenParameter3.baseType, "List");
    }

    @Test
    public void nullValuesInComposedSchema() throws Exception {
        final JavaClientCodegen codegen = new JavaClientCodegen();
        ComposedSchema schema = new ComposedSchema();
        CodegenModel result = codegen.fromModel("CompSche",
                schema, Collections.singletonMap("CompSche", schema));
        Assert.assertEquals(result.name, "CompSche");
    }

    @Test
    public void testParametersAreCorrectlyOrderedWhenUsingRetrofit(){
        JavaClientCodegen javaClientCodegen = new JavaClientCodegen();
        javaClientCodegen.setLibrary(JavaClientCodegen.RETROFIT_2);

        CodegenOperation codegenOperation = new CodegenOperation();
        CodegenParameter queryParamRequired = createQueryParam("queryParam1", true);
        CodegenParameter queryParamOptional = createQueryParam("queryParam2", false);
        CodegenParameter pathParam1 = createPathParam("pathParam1");
        CodegenParameter pathParam2 = createPathParam("pathParam2");

        codegenOperation.allParams = Arrays.asList(queryParamRequired, pathParam1, pathParam2, queryParamOptional);
        Map<String, Object> operations = ImmutableMap.<String, Object>of("operation", Arrays.asList(codegenOperation));

        Map<String, Object> objs = ImmutableMap.of("operations", operations, "imports", new ArrayList<Map<String, String>>());

        javaClientCodegen.postProcessOperationsWithModels(objs, Collections.emptyList());

        Assert.assertEquals(Arrays.asList(pathParam1, pathParam2, queryParamRequired, queryParamOptional), codegenOperation.allParams);
        Assert.assertTrue(pathParam1.hasMore);
        Assert.assertTrue(pathParam2.hasMore);
        Assert.assertTrue(queryParamRequired.hasMore);
        Assert.assertFalse(queryParamOptional.hasMore);
    }

    @Test
    public void testInitialConfigValues() throws Exception {
        final JavaClientCodegen codegen = new JavaClientCodegen();
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.FALSE);
        Assert.assertEquals(codegen.isHideGenerationTimestamp(), false);

        Assert.assertEquals(codegen.modelPackage(), "org.openapitools.client.model");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.MODEL_PACKAGE), "org.openapitools.client.model");
        Assert.assertEquals(codegen.apiPackage(), "org.openapitools.client.api");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.API_PACKAGE), "org.openapitools.client.api");
        Assert.assertEquals(codegen.getInvokerPackage(), "org.openapitools.client");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.INVOKER_PACKAGE), "org.openapitools.client");
    }

    @Test
    public void testSettersForConfigValues() throws Exception {
        final JavaClientCodegen codegen = new JavaClientCodegen();
        codegen.setHideGenerationTimestamp(true);
        codegen.setModelPackage("xyz.yyyyy.zzzzzzz.model");
        codegen.setApiPackage("xyz.yyyyy.zzzzzzz.api");
        codegen.setInvokerPackage("xyz.yyyyy.zzzzzzz.invoker");
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.TRUE);
        Assert.assertEquals(codegen.isHideGenerationTimestamp(), true);
        Assert.assertEquals(codegen.modelPackage(), "xyz.yyyyy.zzzzzzz.model");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.MODEL_PACKAGE), "xyz.yyyyy.zzzzzzz.model");
        Assert.assertEquals(codegen.apiPackage(), "xyz.yyyyy.zzzzzzz.api");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.API_PACKAGE), "xyz.yyyyy.zzzzzzz.api");
        Assert.assertEquals(codegen.getInvokerPackage(), "xyz.yyyyy.zzzzzzz.invoker");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.INVOKER_PACKAGE), "xyz.yyyyy.zzzzzzz.invoker");
    }

    @Test
    public void testAdditionalPropertiesPutForConfigValues() throws Exception {
        final JavaClientCodegen codegen = new JavaClientCodegen();
        codegen.additionalProperties().put(CodegenConstants.HIDE_GENERATION_TIMESTAMP, "true");
        codegen.additionalProperties().put(CodegenConstants.MODEL_PACKAGE, "xyz.yyyyy.zzzzzzz.mmmmm.model");
        codegen.additionalProperties().put(CodegenConstants.API_PACKAGE, "xyz.yyyyy.zzzzzzz.aaaaa.api");
        codegen.additionalProperties().put(CodegenConstants.INVOKER_PACKAGE,"xyz.yyyyy.zzzzzzz.iiii.invoker");
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.TRUE);
        Assert.assertEquals(codegen.isHideGenerationTimestamp(), true);
        Assert.assertEquals(codegen.modelPackage(), "xyz.yyyyy.zzzzzzz.mmmmm.model");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.MODEL_PACKAGE), "xyz.yyyyy.zzzzzzz.mmmmm.model");
        Assert.assertEquals(codegen.apiPackage(), "xyz.yyyyy.zzzzzzz.aaaaa.api");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.API_PACKAGE), "xyz.yyyyy.zzzzzzz.aaaaa.api");
        Assert.assertEquals(codegen.getInvokerPackage(), "xyz.yyyyy.zzzzzzz.iiii.invoker");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.INVOKER_PACKAGE), "xyz.yyyyy.zzzzzzz.iiii.invoker");
    }

    @Test
    public void testPackageNamesSetInvokerDerivedFromApi() throws Exception {
        final JavaClientCodegen codegen = new JavaClientCodegen();
        codegen.additionalProperties().put(CodegenConstants.MODEL_PACKAGE, "xyz.yyyyy.zzzzzzz.mmmmm.model");
        codegen.additionalProperties().put(CodegenConstants.API_PACKAGE, "xyz.yyyyy.zzzzzzz.aaaaa.api");
        codegen.processOpts();

        Assert.assertEquals(codegen.modelPackage(), "xyz.yyyyy.zzzzzzz.mmmmm.model");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.MODEL_PACKAGE), "xyz.yyyyy.zzzzzzz.mmmmm.model");
        Assert.assertEquals(codegen.apiPackage(), "xyz.yyyyy.zzzzzzz.aaaaa.api");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.API_PACKAGE), "xyz.yyyyy.zzzzzzz.aaaaa.api");
        Assert.assertEquals(codegen.getInvokerPackage(), "xyz.yyyyy.zzzzzzz.aaaaa");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.INVOKER_PACKAGE), "xyz.yyyyy.zzzzzzz.aaaaa");
    }

    @Test
    public void testPackageNamesSetInvokerDerivedFromModel() throws Exception {
        final JavaClientCodegen codegen = new JavaClientCodegen();
        codegen.additionalProperties().put(CodegenConstants.MODEL_PACKAGE, "xyz.yyyyy.zzzzzzz.mmmmm.model");
        codegen.processOpts();

        Assert.assertEquals(codegen.modelPackage(), "xyz.yyyyy.zzzzzzz.mmmmm.model");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.MODEL_PACKAGE), "xyz.yyyyy.zzzzzzz.mmmmm.model");
        Assert.assertEquals(codegen.apiPackage(), "org.openapitools.client.api");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.API_PACKAGE), "org.openapitools.client.api");
        Assert.assertEquals(codegen.getInvokerPackage(), "xyz.yyyyy.zzzzzzz.mmmmm");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.INVOKER_PACKAGE), "xyz.yyyyy.zzzzzzz.mmmmm");
    }
    
    @Test
    public void testGetSchemaTypeWithComposedSchemaWithAllOf() {
        final OpenAPI openAPI = new OpenAPIParser().readLocation("src/test/resources/2_0/composed-allof.yaml", null, new ParseOptions()).getOpenAPI();
        final JavaClientCodegen codegen = new JavaClientCodegen();

        Operation operation = openAPI.getPaths().get("/ping").getPost();
        CodegenOperation co = codegen.fromOperation("/ping", "POST", operation, ModelUtils.getSchemas(openAPI), openAPI);
        Assert.assertEquals(co.allParams.size(), 1);
        Assert.assertEquals(co.allParams.get(0).baseType, "MessageEventCoreWithTimeListEntries");
    }

    @Test
    public void updateCodegenPropertyEnum() {
        final JavaClientCodegen codegen = new JavaClientCodegen();
        CodegenProperty array = codegenPropertyWithArrayOfIntegerValues();

        codegen.updateCodegenPropertyEnum(array);

        List<Map<String, String>> enumVars = (List<Map<String, String>>) array.getItems().getAllowableValues().get("enumVars");
        Assert.assertNotNull(enumVars);
        Map<String, String> testedEnumVar = enumVars.get(0);
        Assert.assertNotNull(testedEnumVar);
        Assert.assertEquals(testedEnumVar.getOrDefault("name", ""),"NUMBER_1");
        Assert.assertEquals(testedEnumVar.getOrDefault("value", ""), "1");
    }

    @Test
    public void testGeneratePing() throws Exception {
        Map<String, Object> properties = new HashMap<>();
        properties.put(JavaClientCodegen.JAVA8_MODE, true);
        properties.put(CodegenConstants.API_PACKAGE, "xyz.abcdef.api");

        File output = Files.createTempDirectory("test").toFile();

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("java")
                .setLibrary(JavaClientCodegen.OKHTTP_GSON)
                .setAdditionalProperties(properties)
                .setInputSpec("src/test/resources/3_0/ping.yaml")
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        final ClientOptInput clientOptInput = configurator.toClientOptInput();
        MockDefaultGenerator generator = new MockDefaultGenerator();
        generator.opts(clientOptInput).generate();

        Map<String, String> generatedFiles = generator.getFiles();
        Assert.assertEquals(generatedFiles.size(), 35);
        ensureContainsFile(generatedFiles, output, ".gitignore");
        ensureContainsFile(generatedFiles, output, ".openapi-generator-ignore");
        ensureContainsFile(generatedFiles, output, ".openapi-generator/VERSION");
        ensureContainsFile(generatedFiles, output, ".travis.yml");
        ensureContainsFile(generatedFiles, output, "build.gradle");
        ensureContainsFile(generatedFiles, output, "build.sbt");
        ensureContainsFile(generatedFiles, output, "docs/DefaultApi.md");
        ensureContainsFile(generatedFiles, output, "git_push.sh");
        ensureContainsFile(generatedFiles, output, "gradle.properties");
        ensureContainsFile(generatedFiles, output, "gradle/wrapper/gradle-wrapper.jar");
        ensureContainsFile(generatedFiles, output, "gradle/wrapper/gradle-wrapper.properties");
        ensureContainsFile(generatedFiles, output, "gradlew.bat");
        ensureContainsFile(generatedFiles, output, "gradlew");
        ensureContainsFile(generatedFiles, output, "pom.xml");
        ensureContainsFile(generatedFiles, output, "README.md");
        ensureContainsFile(generatedFiles, output, "settings.gradle");
        ensureContainsFile(generatedFiles, output, "src/main/AndroidManifest.xml");
        ensureContainsFile(generatedFiles, output, "src/main/java/xyz/abcdef/api/DefaultApi.java");
        ensureContainsFile(generatedFiles, output, "src/main/java/xyz/abcdef/ApiCallback.java");
        ensureContainsFile(generatedFiles, output, "src/main/java/xyz/abcdef/ApiClient.java");
        ensureContainsFile(generatedFiles, output, "src/main/java/xyz/abcdef/ApiException.java");
        ensureContainsFile(generatedFiles, output, "src/main/java/xyz/abcdef/ApiResponse.java");
        ensureContainsFile(generatedFiles, output, "src/main/java/xyz/abcdef/auth/ApiKeyAuth.java");
        ensureContainsFile(generatedFiles, output, "src/main/java/xyz/abcdef/auth/Authentication.java");
        ensureContainsFile(generatedFiles, output, "src/main/java/xyz/abcdef/auth/HttpBasicAuth.java");
        ensureContainsFile(generatedFiles, output, "src/main/java/xyz/abcdef/auth/OAuth.java");
        ensureContainsFile(generatedFiles, output, "src/main/java/xyz/abcdef/auth/OAuthFlow.java");
        ensureContainsFile(generatedFiles, output, "src/main/java/xyz/abcdef/Configuration.java");
        ensureContainsFile(generatedFiles, output, "src/main/java/xyz/abcdef/GzipRequestInterceptor.java");
        ensureContainsFile(generatedFiles, output, "src/main/java/xyz/abcdef/JSON.java");
        ensureContainsFile(generatedFiles, output, "src/main/java/xyz/abcdef/Pair.java");
        ensureContainsFile(generatedFiles, output, "src/main/java/xyz/abcdef/ProgressRequestBody.java");
        ensureContainsFile(generatedFiles, output, "src/main/java/xyz/abcdef/ProgressResponseBody.java");
        ensureContainsFile(generatedFiles, output, "src/main/java/xyz/abcdef/StringUtil.java");
        ensureContainsFile(generatedFiles, output, "src/test/java/xyz/abcdef/api/DefaultApiTest.java");

        String defaultApiFilename = new File(output, "src/main/java/xyz/abcdef/api/DefaultApi.java").getAbsolutePath().replace("\\", "/");
        String defaultApiConent = generatedFiles.get(defaultApiFilename);
        Assert.assertTrue(defaultApiConent.contains("public class DefaultApi")); 

        WrittenTemplateBasedFile templateBasedFile = TestUtils.getTemplateBasedFile(generator, output, "src/main/java/xyz/abcdef/api/DefaultApi.java");
        Assert.assertEquals(templateBasedFile.getTemplateData().get("classname"), "DefaultApi");

        output.deleteOnExit();
    }

    private void ensureContainsFile(Map<String, String> generatedFiles, File root, String filename) {
        File file = new File(root, filename);
        String absoluteFilename = file.getAbsolutePath().replace("\\", "/");
        if(!generatedFiles.containsKey(absoluteFilename)) {
            Assert.fail("Could not find '" + absoluteFilename + "' file in list:\n" + 
                    generatedFiles.keySet().stream().sorted().collect(Collectors.joining(",\n")));
        }
        Assert.assertTrue(generatedFiles.containsKey(absoluteFilename), "File '" + absoluteFilename + "' was not fould in the list of generated files");
    }

    private CodegenProperty codegenPropertyWithArrayOfIntegerValues() {
        CodegenProperty array = new CodegenProperty();
        final CodegenProperty items = new CodegenProperty();
        final HashMap<String, Object> allowableValues = new HashMap<>();
        allowableValues.put("values", Collections.singletonList(1));
        items.setAllowableValues(allowableValues);
        items.dataType = "Integer";
        array.setItems(items);
        array.dataType = "Array";
        array.mostInnerItems = items;
        return array;
    }

    private CodegenParameter createPathParam(String name) {
        CodegenParameter codegenParameter = createStringParam(name);
        codegenParameter.isPathParam = true;
        return codegenParameter;
    }

    private CodegenParameter createQueryParam(String name, boolean required) {
        CodegenParameter codegenParameter = createStringParam(name);
        codegenParameter.isQueryParam = true;
        codegenParameter.required = required;
        return codegenParameter;
    }

    private CodegenParameter createStringParam(String name){
        CodegenParameter codegenParameter = new CodegenParameter();
        codegenParameter.paramName = name;
        codegenParameter.baseName = name;
        codegenParameter.dataType = "String";
        return codegenParameter;
    }
}
