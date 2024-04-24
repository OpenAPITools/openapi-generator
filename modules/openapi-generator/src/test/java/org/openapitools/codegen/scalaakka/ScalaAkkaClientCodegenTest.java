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

package org.openapitools.codegen.scalaakka;

import com.google.common.collect.Sets;
import com.google.common.io.Resources;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.media.*;
import io.swagger.v3.parser.util.SchemaTypeUtil;
import org.openapitools.codegen.*;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.openapitools.codegen.languages.ScalaAkkaClientCodegen;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.io.File;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@SuppressWarnings({"UnstableApiUsage", "OptionalGetWithoutIsPresent"})
public class ScalaAkkaClientCodegenTest {

    private ScalaAkkaClientCodegen scalaAkkaClientCodegen;

    @Test(description = "convert a simple scala model")
    public void simpleModelTest() {
        final Schema model = new Schema()
                .description("a sample model")
                .addProperties("id", new IntegerSchema().format(SchemaTypeUtil.INTEGER64_FORMAT))
                .addProperties("name", new StringSchema())
                .addProperties("createdAt", new DateTimeSchema())
                .addRequiredItem("id")
                .addRequiredItem("name");
        final DefaultCodegen codegen = new ScalaAkkaClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", model);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", model);

        Assertions.assertEquals(cm.name, "sample");
        Assertions.assertEquals(cm.classname, "Sample");
        Assertions.assertEquals(cm.description, "a sample model");
        Assertions.assertEquals(cm.vars.size(), 3);

        final CodegenProperty property1 = cm.vars.get(0);
        Assertions.assertEquals(property1.baseName, "id");
        Assertions.assertEquals(property1.getter, "getId");
        Assertions.assertEquals(property1.setter, "setId");
        Assertions.assertEquals(property1.dataType, "Long");
        Assertions.assertEquals(property1.name, "id");
        Assertions.assertNull(property1.defaultValue);
        Assertions.assertEquals(property1.baseType, "Long");
        Assertions.assertTrue(property1.required);
        Assertions.assertFalse(property1.isContainer);

        final CodegenProperty property2 = cm.vars.get(1);
        Assertions.assertEquals(property2.baseName, "name");
        Assertions.assertEquals(property2.getter, "getName");
        Assertions.assertEquals(property2.setter, "setName");
        Assertions.assertEquals(property2.dataType, "String");
        Assertions.assertEquals(property2.name, "name");
        Assertions.assertNull(property2.defaultValue);
        Assertions.assertEquals(property2.baseType, "String");
        Assertions.assertTrue(property2.required);
        Assertions.assertFalse(property2.isContainer);

        final CodegenProperty property3 = cm.vars.get(2);
        Assertions.assertEquals(property3.baseName, "createdAt");
        Assertions.assertEquals(property3.getter, "getCreatedAt");
        Assertions.assertEquals(property3.setter, "setCreatedAt");
        Assertions.assertEquals(property3.dataType, "DateTime");
        Assertions.assertEquals(property3.name, "createdAt");
        Assertions.assertNull(property3.defaultValue);
        Assertions.assertEquals(property3.baseType, "DateTime");
        Assertions.assertFalse(property3.required);
        Assertions.assertFalse(property3.isContainer);
    }

    @Test(description = "convert a model with list property")
    public void listPropertyTest() {
        final Schema model = new Schema()
                .description("a sample model")
                .addProperties("id", new IntegerSchema().format(SchemaTypeUtil.INTEGER64_FORMAT))
                .addProperties("urls", new ArraySchema()
                        .items(new StringSchema()))
                .addRequiredItem("id");
        final DefaultCodegen codegen = new ScalaAkkaClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", model);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", model);

        Assertions.assertEquals(cm.name, "sample");
        Assertions.assertEquals(cm.classname, "Sample");
        Assertions.assertEquals(cm.description, "a sample model");
        //Assertions.assertEquals(cm.vars.size(), 2);

        final CodegenProperty property1 = cm.vars.get(1);
        Assertions.assertEquals(property1.baseName, "urls");
        Assertions.assertEquals(property1.getter, "getUrls");
        Assertions.assertEquals(property1.setter, "setUrls");
        Assertions.assertEquals(property1.dataType, "Seq[String]");
        Assertions.assertEquals(property1.name, "urls");
        Assertions.assertEquals(property1.defaultValue, "Seq[String].empty ");
        Assertions.assertEquals(property1.baseType, "Seq");
        Assertions.assertEquals(property1.containerType, "array");
        Assertions.assertFalse(property1.required);
        Assertions.assertTrue(property1.isContainer);
    }

    @Test(description = "convert a model with a map property")
    public void mapPropertyTest() {
        final Schema model = new Schema()
                .description("a sample model")
                .addProperties("translations", new MapSchema()
                        .additionalProperties(new StringSchema()))
                .addRequiredItem("id");
        final DefaultCodegen codegen = new ScalaAkkaClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", model);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", model);

        Assertions.assertEquals(cm.name, "sample");
        Assertions.assertEquals(cm.classname, "Sample");
        Assertions.assertEquals(cm.description, "a sample model");
        Assertions.assertEquals(cm.vars.size(), 1);

        final CodegenProperty property1 = cm.vars.get(0);
        Assertions.assertEquals(property1.baseName, "translations");
        Assertions.assertEquals(property1.getter, "getTranslations");
        Assertions.assertEquals(property1.setter, "setTranslations");
        Assertions.assertEquals(property1.dataType, "Map[String, String]");
        Assertions.assertEquals(property1.name, "translations");
        Assertions.assertEquals(property1.defaultValue, "Map[String, String].empty ");
        Assertions.assertEquals(property1.baseType, "Map");
        Assertions.assertEquals(property1.containerType, "map");
        Assertions.assertFalse(property1.required);
        Assertions.assertTrue(property1.isContainer);
    }

    @Test(description = "convert a model with complex properties")
    public void complexPropertyTest() {
        final Schema model = new Schema()
                .description("a sample model")
                .addProperties("children", new Schema().$ref("#/definitions/Children"));
        final DefaultCodegen codegen = new ScalaAkkaClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", model);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", model);

        Assertions.assertEquals(cm.name, "sample");
        Assertions.assertEquals(cm.classname, "Sample");
        Assertions.assertEquals(cm.description, "a sample model");
        Assertions.assertEquals(cm.vars.size(), 1);

        final CodegenProperty property1 = cm.vars.get(0);
        Assertions.assertEquals(property1.baseName, "children");
        Assertions.assertEquals(property1.getter, "getChildren");
        Assertions.assertEquals(property1.setter, "setChildren");
        Assertions.assertEquals(property1.dataType, "Children");
        Assertions.assertEquals(property1.name, "children");
        Assertions.assertNull(property1.defaultValue);
        Assertions.assertEquals(property1.baseType, "Children");
        Assertions.assertFalse(property1.required);
        Assertions.assertFalse(property1.isContainer);
    }

    @Test(description = "convert a model with complex list property")
    public void complexListPropertyTest() {
        final Schema model = new Schema()
                .description("a sample model")
                .addProperties("children", new ArraySchema()
                        .items(new Schema().$ref("#/definitions/Children")));
        final DefaultCodegen codegen = new ScalaAkkaClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", model);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", model);

        Assertions.assertEquals(cm.name, "sample");
        Assertions.assertEquals(cm.classname, "Sample");
        Assertions.assertEquals(cm.description, "a sample model");
        Assertions.assertEquals(cm.vars.size(), 1);

        final CodegenProperty property1 = cm.vars.get(0);
        Assertions.assertEquals(property1.baseName, "children");
        Assertions.assertEquals(property1.complexType, "Children");
        Assertions.assertEquals(property1.getter, "getChildren");
        Assertions.assertEquals(property1.setter, "setChildren");
        Assertions.assertEquals(property1.dataType, "Seq[Children]");
        Assertions.assertEquals(property1.name, "children");
        Assertions.assertEquals(property1.defaultValue, "Seq[Children].empty ");
        Assertions.assertEquals(property1.baseType, "Seq");
        Assertions.assertEquals(property1.containerType, "array");
        Assertions.assertFalse(property1.required);
        Assertions.assertTrue(property1.isContainer);
    }

    @Test(description = "convert a model with set (unique array) property")
    public void complexSetPropertyTest() {
        final Schema model = new Schema()
                .description("a sample model")
                .addProperties("children", new ArraySchema()
                        .items(new Schema().$ref("#/definitions/Children"))
                        .uniqueItems(Boolean.TRUE));
        final DefaultCodegen codegen = new ScalaAkkaClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", model);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", model);

        Assertions.assertEquals(cm.name, "sample");
        Assertions.assertEquals(cm.classname, "Sample");
        Assertions.assertEquals(cm.description, "a sample model");
        Assertions.assertEquals(cm.vars.size(), 1);

        final CodegenProperty property1 = cm.vars.get(0);
        Assertions.assertEquals(property1.baseName, "children");
        Assertions.assertEquals(property1.complexType, "Children");
        Assertions.assertEquals(property1.getter, "getChildren");
        Assertions.assertEquals(property1.setter, "setChildren");
        Assertions.assertEquals(property1.dataType, "Set[Children]");
        Assertions.assertEquals(property1.name, "children");
        Assertions.assertEquals(property1.defaultValue, "Set[Children].empty ");
        Assertions.assertEquals(property1.baseType, "Set");
        Assertions.assertEquals(property1.containerType, "set");
        Assertions.assertFalse(property1.required);
        Assertions.assertTrue(property1.isContainer);
    }

    @Test(description = "convert a model with complex map property")
    public void complexMapPropertyTest() {
        final Schema model = new Schema()
                .description("a sample model")
                .addProperties("children", new MapSchema()
                        .additionalProperties(new Schema().$ref("#/definitions/Children")));
        final DefaultCodegen codegen = new ScalaAkkaClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", model);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", model);

        Assertions.assertEquals(cm.name, "sample");
        Assertions.assertEquals(cm.classname, "Sample");
        Assertions.assertEquals(cm.description, "a sample model");
        Assertions.assertEquals(cm.vars.size(), 1);
        Assertions.assertEquals(Sets.intersection(cm.imports, Sets.newHashSet("Children")).size(), 1);

        final CodegenProperty property1 = cm.vars.get(0);
        Assertions.assertEquals(property1.baseName, "children");
        Assertions.assertEquals(property1.complexType, "Children");
        Assertions.assertEquals(property1.getter, "getChildren");
        Assertions.assertEquals(property1.setter, "setChildren");
        Assertions.assertEquals(property1.dataType, "Map[String, Children]");
        Assertions.assertEquals(property1.name, "children");
        Assertions.assertEquals(property1.defaultValue, "Map[String, Children].empty ");
        Assertions.assertEquals(property1.baseType, "Map");
        Assertions.assertEquals(property1.containerType, "map");
        Assertions.assertFalse(property1.required);
        Assertions.assertTrue(property1.isContainer);
    }

    @Test(description = "convert an array model")
    public void arrayModelTest() {
        final Schema schema = new ArraySchema()
                .items(new Schema().$ref("#/definitions/Children"))
                .description("an array model");
        final DefaultCodegen codegen = new ScalaAkkaClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", schema);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", schema);

        Assertions.assertEquals(cm.name, "sample");
        Assertions.assertEquals(cm.classname, "Sample");
        Assertions.assertEquals(cm.description, "an array model");
        Assertions.assertEquals(cm.vars.size(), 0);
        Assertions.assertEquals(cm.parent, "ListBuffer[Children]");
        Assertions.assertEquals(cm.arrayModelType, "Children");
        Assertions.assertEquals(cm.imports.size(), 2);
        Assertions.assertEquals(Sets.intersection(cm.imports, Sets.newHashSet("ListBuffer", "Children")).size(), 2);
    }

    @Test(description = "convert an array model with unique items to set")
    public void arrayAsSetModelTest() {
        final Schema schema = new ArraySchema()
                .items(new Schema().$ref("#/definitions/Children"))
                .description("a set of Children models");
        schema.setUniqueItems(true);

        final DefaultCodegen codegen = new ScalaAkkaClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", schema);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", schema);

        Assertions.assertEquals(cm.name, "sample");
        Assertions.assertEquals(cm.classname, "Sample");
        Assertions.assertEquals(cm.description, "a set of Children models");
        Assertions.assertEquals(cm.vars.size(), 0);
        Assertions.assertEquals(cm.parent, "Set[Children]");
        Assertions.assertEquals(cm.arrayModelType, "Children");
        Assertions.assertEquals(cm.imports.size(), 2);
        Assertions.assertEquals(Sets.intersection(cm.imports, Sets.newHashSet("Set", "Children")).size(), 2);
    }

    @Test(description = "convert a map model")
    public void mapModelTest() {
        final Schema model = new Schema()
                .description("a map model")
                .additionalProperties(new Schema().$ref("#/definitions/Children"));
        final DefaultCodegen codegen = new ScalaAkkaClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", model);
        codegen.setOpenAPI(openAPI);
        final CodegenModel cm = codegen.fromModel("sample", model);

        Assertions.assertEquals(cm.name, "sample");
        Assertions.assertEquals(cm.classname, "Sample");
        Assertions.assertEquals(cm.description, "a map model");
        Assertions.assertEquals(cm.vars.size(), 0);
        Assertions.assertEquals(cm.parent, "Map[String, Children]");
        Assertions.assertEquals(cm.imports.size(), 1);
        Assertions.assertEquals(Sets.intersection(cm.imports, Sets.newHashSet("Map", "Children")).size(), 1);
    }

    @Test(description = "validate codegen joda output")
    public void codeGenerationTest() throws Exception {
        Map<String, Object> properties = new HashMap<>();
        properties.put("mainPackage", "hello.world");
        properties.put("dateLibrary", "joda");

        File output = Files.createTempDirectory("test").toFile();
        output.deleteOnExit();

        final DefaultCodegen codegen = new ScalaAkkaClientCodegen();

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName(codegen.getName())
                .setAdditionalProperties(properties)
                .setInputSpec("src/test/resources/3_0/scala_reserved_words.yaml")
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        final ClientOptInput clientOptInput = configurator.toClientOptInput();
        DefaultGenerator generator = new DefaultGenerator();

        generator.setGenerateMetadata(false);

        generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "true");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.SUPPORTING_FILES, "false");

        List<File> files = generator.opts(clientOptInput).generate();

        Assertions.assertEquals(files.size(), 1);

        TestUtils.ensureContainsFile(files, output, "src/main/scala/hello/world/model/SomeObj.scala");

        File someObj = files.stream().filter(f -> f.getName().equals("SomeObj.scala"))
                .findFirst().get();

        byte[] fileContents = Files.readAllBytes(someObj.toPath());
        Assertions.assertEquals(
                new String(fileContents, StandardCharsets.UTF_8),
                Resources.toString(Resources.getResource("codegen/scala/SomeObj.scala.txt"), StandardCharsets.UTF_8));
    }

    @Test(description = "validate codegen java8 output")
    public void codeGenerationJava8Test() throws Exception {
        Map<String, Object> properties = new HashMap<>();
        properties.put("mainPackage", "hello.world");
        properties.put("dateLibrary", "java8");

        File output = Files.createTempDirectory("test").toFile();
        output.deleteOnExit();

        final DefaultCodegen codegen = new ScalaAkkaClientCodegen();

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName(codegen.getName())
                .setAdditionalProperties(properties)
                .setInputSpec("src/test/resources/3_0/scala_reserved_words.yaml")
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        final ClientOptInput clientOptInput = configurator.toClientOptInput();
        DefaultGenerator generator = new DefaultGenerator();

        generator.setGenerateMetadata(false);

        generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "true");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.SUPPORTING_FILES, "false");

        List<File> files = generator.opts(clientOptInput).generate();

        Assertions.assertEquals(files.size(), 1);

        TestUtils.ensureContainsFile(files, output, "src/main/scala/hello/world/model/SomeObj.scala");

        File someObj = files.stream().filter(f -> f.getName().equals("SomeObj.scala"))
                .findFirst().get();

        byte[] fileContents = Files.readAllBytes(someObj.toPath());
        Assertions.assertEquals(
                new String(fileContents, StandardCharsets.UTF_8),
                Resources.toString(Resources.getResource("codegen/scala/JavaTimeObj.scala.txt"), StandardCharsets.UTF_8));
    }


    @Test(description = "strip model name")
    public void stripModelNameTest() throws Exception {
        final Schema model = new Schema()
                .description("a map model");
        final DefaultCodegen codegen = new ScalaAkkaClientCodegen();
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", model);
        codegen.setOpenAPI(openAPI);

        final CodegenModel cms = codegen.fromModel("Stripped.ByDefault.ModelName", model);
        Assertions.assertEquals(cms.name, "Stripped.ByDefault.ModelName");
        Assertions.assertEquals(cms.classname, "ModelName");
        Assertions.assertEquals(cms.classFilename, "ModelName");

        codegen.additionalProperties().put(CodegenConstants.STRIP_PACKAGE_NAME, "false");
        codegen.processOpts();

        final CodegenModel cm = codegen.fromModel("Non.Stripped.ModelName", model);

        Assertions.assertEquals(cm.name, "Non.Stripped.ModelName");
        Assertions.assertEquals(cm.classname, "NonStrippedModelName");
        Assertions.assertEquals(cm.classFilename, "NonStrippedModelName");

    }

    @Test(description = "override only mainPackage")
    public void mainPackageTest() throws Exception {
        Map<String, Object> properties = new HashMap<>();
        properties.put("mainPackage", "hello.world");

        File output = Files.createTempDirectory("test").toFile();
        output.deleteOnExit();

        final DefaultCodegen codegen = new ScalaAkkaClientCodegen();
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName(codegen.getName())
                .setAdditionalProperties(properties)
                .setInputSpec("src/test/resources/3_0/scala_reserved_words.yaml")
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        final ClientOptInput clientOptInput = configurator.toClientOptInput();
        DefaultGenerator generator = new DefaultGenerator(false);

        generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "true");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "true");
        generator.setGeneratorPropertyDefault(CodegenConstants.API_DOCS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.SUPPORTING_FILES, "true");

        Generator gen = generator.opts(clientOptInput);
        List<File> files = gen.generate();

        Assertions.assertEquals(files.size(), 16);

        TestUtils.ensureContainsFile(files, output, "src/main/scala/hello/world/model/SomeObj.scala");
        TestUtils.ensureContainsFile(files, output, "src/main/scala/hello/world/core/ApiSettings.scala");
        TestUtils.ensureContainsFile(files, output, "src/main/scala/hello/world/api/PingApi.scala");
    }

    @Test(description = "override api packages")
    public void overridePackagesTest() throws Exception {
        Map<String, Object> properties = new HashMap<>();
        properties.put("mainPackage", "hello.world");
        properties.put("apiPackage", "hello.world.api.package");
        properties.put("modelPackage", "hello.world.model.package");
        properties.put("invokerPackage", "hello.world.package.invoker");

        File output = Files.createTempDirectory("test").toFile();
        output.deleteOnExit();

        final DefaultCodegen codegen = new ScalaAkkaClientCodegen();
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName(codegen.getName())
                .setAdditionalProperties(properties)
                .setInputSpec("src/test/resources/3_0/scala_reserved_words.yaml")
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        final ClientOptInput clientOptInput = configurator.toClientOptInput();
        DefaultGenerator generator = new DefaultGenerator();

        generator.setGeneratorPropertyDefault(CodegenConstants.MODELS, "true");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_TESTS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.MODEL_DOCS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.APIS, "true");
        generator.setGeneratorPropertyDefault(CodegenConstants.API_DOCS, "false");
        generator.setGeneratorPropertyDefault(CodegenConstants.SUPPORTING_FILES, "true");

        Generator gen = generator.opts(clientOptInput);

        List<File> files = gen.generate();
        Assertions.assertEquals(files.size(), 16);

        TestUtils.ensureContainsFile(files, output, "src/main/scala/hello/world/model/package/SomeObj.scala");
        TestUtils.ensureContainsFile(files, output, "src/main/scala/hello/world/package/invoker/ApiSettings.scala");
        TestUtils.ensureContainsFile(files, output, "src/main/scala/hello/world/api/package/PingApi.scala");
    }
}
