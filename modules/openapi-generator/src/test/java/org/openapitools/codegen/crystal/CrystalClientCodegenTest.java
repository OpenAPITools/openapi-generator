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

package org.openapitools.codegen.crystal;

import io.swagger.v3.oas.models.OpenAPI;
import org.apache.commons.io.FileUtils;
import org.openapitools.codegen.*;
import org.openapitools.codegen.languages.CrystalClientCodegen;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.io.File;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.List;
import java.util.Map;

import static org.testng.Assert.assertTrue;
import static org.testng.Assert.fail;

/**
 * Tests for CrystalClientCodegen-generated templates
 */
public class CrystalClientCodegenTest {

    @Test
    public void testGenerateCrystalClientWithHtmlEntity() throws Exception {
        final File output = Files.createTempDirectory("test").toFile();
        output.mkdirs();
        output.deleteOnExit();

        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/2_0/pathWithHtmlEntity.yaml");
        CodegenConfig codegenConfig = new CrystalClientCodegen();
        codegenConfig.setOutputDir(output.getAbsolutePath());

        ClientOptInput clientOptInput = new ClientOptInput().openAPI(openAPI).config(codegenConfig);

        DefaultGenerator generator = new DefaultGenerator();
        List<File> files = generator.opts(clientOptInput).generate();
        boolean apiFileGenerated = false;
        for (File file : files) {
            // After namespaced routing + segment sanitization:
            // /foo=bar -> namespace "foo=bar" -> sanitizeName strips '=' -> "foobar" -> file "foobar.cr"
            if (file.getName().equals("foobar.cr")) {
                apiFileGenerated = true;
                String content = FileUtils.readFileToString(file, StandardCharsets.UTF_8);
                // Crystal client should set the path in the @conn.request call
                assertTrue(content.contains("path: \"/foo=bar\""));
                // The generated class is nested under the api namespace module and its name must
                // be valid Crystal (no '=' in identifier): `module Api` + `class Foobar`.
                assertTrue(content.contains("module Api") && content.contains("class Foobar"),
                        "Generated class must be Api::Foobar (sanitized, no '=')");
                Assert.assertFalse(content.contains("Foo=bar"),
                        "Generated class name must not contain '=' (invalid Crystal identifier)");
            }
        }
        if (!apiFileGenerated) {
            fail("API file is not generated!");
        }
    }

    @Test
    public void testInitialConfigValues() throws Exception {
        final CrystalClientCodegen codegen = new CrystalClientCodegen();
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP),
                Boolean.TRUE);
        Assert.assertEquals(codegen.isHideGenerationTimestamp(), true);
        Assert.assertEquals(codegen.modelPackage(), "models");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.MODEL_PACKAGE), null);
        Assert.assertEquals(codegen.apiPackage(), "api");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.API_PACKAGE), null);
    }

    @Test
    public void testSettersForConfigValues() throws Exception {
        final CrystalClientCodegen codegen = new CrystalClientCodegen();
        codegen.setHideGenerationTimestamp(false);
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP),
                Boolean.FALSE);
        Assert.assertEquals(codegen.isHideGenerationTimestamp(), false);
    }

    @Test
    public void testAdditionalPropertiesPutForConfigValues() throws Exception {
        final CrystalClientCodegen codegen = new CrystalClientCodegen();
        codegen.additionalProperties().put(CodegenConstants.HIDE_GENERATION_TIMESTAMP, false);
        codegen.additionalProperties().put(CodegenConstants.MODEL_PACKAGE, "crystal-models");
        codegen.additionalProperties().put(CodegenConstants.API_PACKAGE, "crystal-api");
        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.HIDE_GENERATION_TIMESTAMP),
                Boolean.FALSE);
        Assert.assertEquals(codegen.isHideGenerationTimestamp(), false);
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.MODEL_PACKAGE), "crystal-models");
        Assert.assertEquals(codegen.additionalProperties().get(CodegenConstants.API_PACKAGE), "crystal-api");
    }

    @Test
    public void testBooleanDefaultValue() throws Exception {
        final File output = Files.createTempDirectory("test").toFile();
        output.mkdirs();
        output.deleteOnExit();

        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/2_0/npe1.yaml");
        CodegenConfig codegenConfig = new CrystalClientCodegen();
        codegenConfig.setOutputDir(output.getAbsolutePath());

        ClientOptInput clientOptInput = new ClientOptInput().openAPI(openAPI).config(codegenConfig);

        DefaultGenerator generator = new DefaultGenerator();
        List<File> files = generator.opts(clientOptInput).generate();
        boolean apiFileGenerated = false;
        for (File file : files) {
            // After namespaced routing: /default/Resources/{id} with commonBasePrefix="default/Resources"
            // -> lits=[] -> namespace="root" -> file "root.cr"
            if (file.getName().equals("root.cr")) {
                apiFileGenerated = true;
                // Crystal client should set the path in the @conn.request call
                assertTrue(FileUtils.readFileToString(file, StandardCharsets.UTF_8)
                        .contains("path: \"/default/Resources/{id}\""));
            }
        }
        if (!apiFileGenerated) {
            fail("API file is not generated!");
        }
    }

    @Test
    public void testConfigurationHasNoAuthSettingsMethod() throws Exception {
        // auth_settings was a dead utility method (not called by apply_auth!) and has
        // been removed. Verify it is absent and apply_auth! is still present.
        final File output = Files.createTempDirectory("test").toFile();
        output.mkdirs();
        output.deleteOnExit();

        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/crystal/petstore.yaml");
        CodegenConfig codegenConfig = new CrystalClientCodegen();
        codegenConfig.setOutputDir(output.getAbsolutePath());

        ClientOptInput clientOptInput = new ClientOptInput().openAPI(openAPI).config(codegenConfig);

        DefaultGenerator generator = new DefaultGenerator();
        List<File> files = generator.opts(clientOptInput).generate();
        boolean configFileGenerated = false;
        for (File file : files) {
            if (file.getName().equals("configuration.cr")) {
                configFileGenerated = true;
                String content = FileUtils.readFileToString(file, StandardCharsets.UTF_8);
                // auth_settings has been removed (dead code — not called by apply_auth!)
                Assert.assertFalse(content.contains("def auth_settings"),
                        "configuration.cr must not contain the removed auth_settings method");
                // apply_auth! is still present (the real auth entry point)
                assertTrue(content.contains("def apply_auth!"),
                        "configuration.cr must still contain apply_auth!");
                // server machinery has been removed too
                Assert.assertFalse(content.contains("def server_settings"),
                        "configuration.cr must not contain the removed server_settings method");
                Assert.assertFalse(content.contains("def server_url"),
                        "configuration.cr must not contain the removed server_url method");
            }
        }
        if (!configFileGenerated) {
            fail("configuration.cr file is not generated!");
        }
    }

    @Test
    public void testSanitizeModelName() throws Exception {
        final CrystalClientCodegen codegen = new CrystalClientCodegen();
        codegen.setHideGenerationTimestamp(false);
        codegen.processOpts();

        Assert.assertEquals(codegen.sanitizeModelName("JSON::Any"), "JSON::Any");
        // Disallows single colons
        Assert.assertEquals(codegen.sanitizeModelName("JSON:Any"), "JSONAny");
    }

    @Test
    @SuppressWarnings("unchecked")
    public void testNamespaceTreeAssembled() throws Exception {
        final File output = Files.createTempDirectory("test").toFile();
        output.deleteOnExit();
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/crystal/petstore.yaml");
        CrystalClientCodegen codegen = new CrystalClientCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        new DefaultGenerator().opts(new ClientOptInput().openAPI(openAPI).config(codegen)).generate();

        List<Map<String, Object>> ns = (List<Map<String, Object>>) codegen.additionalProperties().get("crNamespaces");
        assertTrue(ns != null && !ns.isEmpty(), "crNamespaces must be populated");

        // Find the "pet" namespace entry
        Map<String, Object> petNs = ns.stream()
                .filter(m -> "pet".equals(m.get("name")))
                .findFirst()
                .orElse(null);
        assertTrue(petNs != null, "expected a 'pet' namespace in the tree");

        // className must be produced by toApiName("pet") == "Api::Pet"
        Assert.assertEquals(petNs.get("className"), "Api::Pet",
                "pet namespace className must equal Api::Pet (produced by toApiName)");

        // resources list must be present (may be empty if petstore has no sub-resource segments under pet)
        List<Map<String, Object>> resources = (List<Map<String, Object>>) petNs.get("resources");
        assertTrue(resources != null, "pet namespace must have a resources list (may be empty)");

        // Each resource entry must have a non-null accessor and a className starting with "Api::Pet"
        for (Map<String, Object> res : resources) {
            assertTrue(res.get("accessor") != null, "resource accessor must not be null");
            String resClassName = (String) res.get("className");
            assertTrue(resClassName != null && resClassName.startsWith("Api::Pet"),
                    "resource className must start with 'Api::Pet', got: " + resClassName);
        }
    }

    @Test
    public void testRuntimeFilesGenerated() throws Exception {
        final File output = Files.createTempDirectory("test").toFile();
        output.deleteOnExit();
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/crystal/petstore.yaml");
        CodegenConfig codegen = new CrystalClientCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        List<File> files = new DefaultGenerator().opts(
            new ClientOptInput().openAPI(openAPI).config(codegen)).generate();

        File conn = files.stream().filter(f -> f.getName().equals("connection.cr")).findFirst().orElse(null);
        assertTrue(conn != null, "connection.cr generated");
        String c = FileUtils.readFileToString(conn, StandardCharsets.UTF_8);
        assertTrue(c.contains("def request(klass : T.class"), "generic request present");
        assertTrue(c.contains("forall T"), "request is generic");
        assertTrue(files.stream().anyMatch(f -> f.getName().equals("response.cr")), "response.cr generated");
    }

    @Test
    public void testResourceMethodIsDeclarative() throws Exception {
        final File output = Files.createTempDirectory("test").toFile();
        output.deleteOnExit();
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/crystal/petstore.yaml");
        CodegenConfig codegen = new CrystalClientCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        List<File> files = new DefaultGenerator().opts(
            new ClientOptInput().openAPI(openAPI).config(codegen)).generate();

        File pet = files.stream()
            .filter(f -> f.getName().equals("pet.cr") &&
                f.getPath().replace(File.separatorChar, '/').contains("/api/"))
            .findFirst().orElseThrow(() -> new AssertionError("api/pet.cr missing"));
        String src = FileUtils.readFileToString(pet, StandardCharsets.UTF_8);
        assertTrue(src.contains("module Api") && src.contains("class Pet"), "namespaced class (module Api + class Pet)");
        assertTrue(src.contains("@conn.request("), "delegates to Connection#request");
        Assert.assertFalse(src.contains("_with_http_info"), "no http_info twins");
        Assert.assertFalse(src.contains("client_side_validation"), "no dead validation");
    }

    @Test
    public void testNamespacedFileAndClassNames() throws Exception {
        final File output = Files.createTempDirectory("test").toFile();
        output.deleteOnExit();
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/crystal/petstore.yaml");
        CodegenConfig codegenConfig = new CrystalClientCodegen();
        codegenConfig.setOutputDir(output.getAbsolutePath());
        ClientOptInput input = new ClientOptInput().openAPI(openAPI).config(codegenConfig);
        List<File> files = new DefaultGenerator().opts(input).generate();

        // petstore /pet, /pet/{petId} -> namespace "pet", file pet.cr (no /api prefix in petstore)
        boolean found = files.stream().anyMatch(f ->
            f.getPath().replace(File.separatorChar, '/').endsWith("/api/pet.cr"));
        assertTrue(found, "expected a namespaced resource file src/.../api/pet.cr");
    }

    @Test
    public void testFacadeGenerated() throws Exception {
        final File output = Files.createTempDirectory("test").toFile();
        output.deleteOnExit();
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/crystal/petstore.yaml");
        CodegenConfig codegen = new CrystalClientCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        List<File> files = new DefaultGenerator().opts(
            new ClientOptInput().openAPI(openAPI).config(codegen)).generate();

        File client = files.stream().filter(f -> f.getName().equals("client.cr"))
            .findFirst().orElseThrow(() -> new AssertionError("client.cr missing"));
        String src = FileUtils.readFileToString(client, StandardCharsets.UTF_8);
        assertTrue(src.contains("class Client"), "facade class");
        assertTrue(src.contains("def pet"), "lazy namespace accessor for pet");
        assertTrue(src.contains("Connection.new"), "builds a Connection");
    }

    /**
     * Regression test for api.mustache defects:
     * 1. Trailing comma in method signatures (e.g. "def foo(pet_id : Int64, )")
     * 2. Duplicated splat separator ("*, *,")
     * 3. Trailing space in %w[] arrays producing spurious empty element
     * 4. Form params not declared in method signature / body references undeclared params
     */
    @Test
    public void testNamespacingEdgeCases() throws Exception {
        final File output = Files.createTempDirectory("test").toFile();
        output.deleteOnExit();
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/crystal/namespacing.yaml");
        CodegenConfig codegen = new CrystalClientCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        List<File> files = new DefaultGenerator().opts(
            new ClientOptInput().openAPI(openAPI).config(codegen)).generate();

        // /store/orders + /store/orders/{id} -> orders precedes {id} -> resource
        // groupKey="store/orders" -> file .../api/store/orders.cr, class Api::Store::Orders
        assertTrue(files.stream().anyMatch(f -> f.getPath().replace(File.separatorChar, '/').endsWith("/store/orders.cr")),
            "store/orders.cr must be generated (orders is a resource segment)");

        // /store/refresh -> refresh not in R -> action on namespace "store" -> groupKey="store"
        // file .../api/store.cr must contain def refresh
        File store = files.stream().filter(f -> f.getName().equals("store.cr") &&
            f.getPath().replace(File.separatorChar, '/').contains("/api/")).findFirst().orElse(null);
        assertTrue(store != null, "store.cr (degenerate action on store namespace) must be generated");
        assertTrue(FileUtils.readFileToString(store, StandardCharsets.UTF_8).contains("def refresh"),
            "store.cr must contain def refresh (action derived from operationId store_refresh)");

        // /ping -> lits=["ping"] (basePrefix="" since paths diverge at root) -> namespace="ping"
        // Design: route() returns namespace="ping", not "root" (root only for empty-lits case)
        // -> groupKey="ping" -> file .../api/ping.cr
        assertTrue(files.stream().anyMatch(f -> f.getName().equals("ping.cr") &&
            f.getPath().replace(File.separatorChar, '/').contains("/api/")),
            "ping.cr must be generated for /ping (namespace 'ping', not 'root', since lits is non-empty)");
    }

    @Test
    public void testApiMustacheSignaturesAndArrays() throws Exception {
        final File output = Files.createTempDirectory("test").toFile();
        output.deleteOnExit();
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/crystal/petstore.yaml");
        CodegenConfig codegen = new CrystalClientCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        List<File> files = new DefaultGenerator().opts(
            new ClientOptInput().openAPI(openAPI).config(codegen)).generate();

        // Collect all generated API .cr files
        List<File> apiFiles = files.stream()
            .filter(f -> f.getName().endsWith(".cr") &&
                f.getPath().replace(File.separatorChar, '/').contains("/api/"))
            .collect(java.util.stream.Collectors.toList());
        assertTrue(!apiFiles.isEmpty(), "At least one API .cr file must be generated");

        for (File apiFile : apiFiles) {
            String src = FileUtils.readFileToString(apiFile, StandardCharsets.UTF_8);
            String fname = apiFile.getName();

            // DEFECT 1: No trailing comma before closing paren in method signatures
            Assert.assertFalse(src.contains(", )"),
                fname + ": found trailing comma in method signature: ', )'");

            // DEFECT 2: No duplicated splat separator
            Assert.assertFalse(src.contains("*, *,"),
                fname + ": found duplicated splat separator '*, *,'");

            // DEFECT 5: No trailing space in %w[] arrays
            // A trailing space before ] means a spurious empty string element e.g. %w[foo ]
            Assert.assertFalse(src.contains(" ]"),
                fname + ": found trailing space before ']' in %w[] array (spurious empty element)");
        }

        // DEFECT 3+4: Form params must be declared in signature and referenced in form: body
        // Find pet.cr - it has updatePetWithForm (path+form) and uploadFile (path+form)
        File pet = files.stream()
            .filter(f -> f.getName().equals("pet.cr") &&
                f.getPath().replace(File.separatorChar, '/').contains("/api/"))
            .findFirst().orElseThrow(() -> new AssertionError("api/pet.cr missing"));
        String petSrc = FileUtils.readFileToString(pet, StandardCharsets.UTF_8);

        // updatePetWithForm: path param petId + form params name + status
        // The form: { ... } body must reference declared variables name and status
        assertTrue(petSrc.contains("form: Hash(String, Crest::ParamsValue){"),
            "pet.cr: expected a typed form hash for form-param operations");
        // The form body must reference "name" and "status" as local variables (not string literals)
        // Specifically: form: { "name" => name, "status" => status }
        assertTrue(petSrc.contains("\"name\" => name"),
            "pet.cr: form body must reference declared param 'name'");
        assertTrue(petSrc.contains("\"status\" => status"),
            "pet.cr: form body must reference declared param 'status'");

        // The method declaration for updatePetWithForm must include name and status as params
        // We check that the form param names appear in the def line before the form: call
        // A simple proxy: the file must contain "name : " (form param declaration)
        assertTrue(petSrc.contains("name : "),
            "pet.cr: form param 'name' must be declared in method signature");

        // uploadFile: path param petId + form params additionalMetadata + file
        // baseName is "additionalMetadata" (camelCase from spec), variable is snake_cased
        assertTrue(petSrc.contains("\"additionalMetadata\" => additional_metadata"),
            "pet.cr: uploadFile form body must reference declared param 'additional_metadata'");
    }

    @Test
    public void testModelsIncludeSerializableMixin() throws Exception {
        final File output = Files.createTempDirectory("test").toFile();
        output.deleteOnExit();
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/crystal/petstore.yaml");
        CrystalClientCodegen codegen = new CrystalClientCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put("moduleName", "Petstore");
        List<File> files = new DefaultGenerator().opts(
            new ClientOptInput().openAPI(openAPI).config(codegen)).generate();
        assertTrue(files.stream().anyMatch(f -> f.getName().equals("serializable.cr")),
            "serializable.cr (shared mixin) must be generated");
        File pet = files.stream().filter(f -> f.getPath().replace(File.separatorChar,'/').endsWith("/models/pet.cr"))
            .findFirst().orElseThrow(() -> new AssertionError("pet.cr missing"));
        String src = FileUtils.readFileToString(pet, StandardCharsets.UTF_8);
        assertTrue(src.contains("include Petstore::Serializable"), "model must include the mixin");
        Assert.assertFalse(src.contains("def to_h"), "to_h must no longer be inlined in the model");
    }

    @Test
    public void testValidDelegatesToListInvalidProperties() throws Exception {
        final File output = Files.createTempDirectory("test").toFile();
        output.deleteOnExit();
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/crystal/petstore.yaml");
        CodegenConfig codegen = new CrystalClientCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        List<File> files = new DefaultGenerator().opts(
            new ClientOptInput().openAPI(openAPI).config(codegen)).generate();
        File model = files.stream().filter(f -> f.getPath().replace(File.separatorChar,'/').endsWith("/models/format_test.cr"))
            .findFirst().orElseThrow(() -> new AssertionError("format_test.cr missing"));
        String src = FileUtils.readFileToString(model, StandardCharsets.UTF_8);
        assertTrue(src.contains("list_invalid_properties.empty?"), "valid? must delegate to list_invalid_properties");
        Assert.assertFalse(src.contains("nillable:"), "JSON::Field annotation noise must be gone");
    }

    @Test
    public void testHeaderParamsAndParamsEncoder() throws Exception {
        final File output = Files.createTempDirectory("test").toFile();
        output.deleteOnExit();
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/crystal/petstore.yaml");
        CodegenConfig codegen = new CrystalClientCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        List<File> files = new DefaultGenerator().opts(
            new ClientOptInput().openAPI(openAPI).config(codegen)).generate();
        File conn = files.stream().filter(f -> f.getName().equals("connection.cr")).findFirst().orElseThrow(() -> new AssertionError("connection.cr"));
        String c = FileUtils.readFileToString(conn, StandardCharsets.UTF_8);
        assertTrue(c.contains("header :"), "request must accept a header argument");
        assertTrue(c.contains("params_encoder: config.params_encoder"), "request must pass the params encoder");
        File pet = files.stream().filter(f -> f.getPath().replace(File.separatorChar,'/').endsWith("/api/pet.cr")).findFirst().orElseThrow(() -> new AssertionError("pet.cr"));
        String p = FileUtils.readFileToString(pet, StandardCharsets.UTF_8);
        assertTrue(p.contains("header: {") && p.contains("api_key"), "deletePet must wire its api_key header param");
    }

    @Test
    public void testUnifiedValidatesMacro() throws Exception {
        final File output = Files.createTempDirectory("test").toFile();
        output.deleteOnExit();
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/crystal/petstore.yaml");
        CrystalClientCodegen codegen = new CrystalClientCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put("moduleName", "Petstore");
        List<File> files = new DefaultGenerator().opts(
            new ClientOptInput().openAPI(openAPI).config(codegen)).generate();
        assertTrue(files.stream().anyMatch(f -> f.getName().equals("validation.cr")),
            "validation.cr (shared validates macro) must be generated");
        Assert.assertFalse(files.stream().anyMatch(f -> f.getName().equals("enum_validation.cr")),
            "old enum_validation.cr must be gone");
        File order = files.stream().filter(f -> f.getPath().replace(File.separatorChar,'/').endsWith("/models/order.cr"))
            .findFirst().orElseThrow(() -> new AssertionError("order.cr missing"));
        String src = FileUtils.readFileToString(order, StandardCharsets.UTF_8);
        assertTrue(src.contains("include Petstore::Validation"), "enum model must include the Validation macro module");
        assertTrue(src.contains("validates("), "validated model must use the validates macro");
        Assert.assertFalse(src.contains("EnumAttributeValidator"), "old validator class must be gone");
        Assert.assertFalse(src.contains("enum_attribute("), "old enum_attribute macro must be gone");
        // list_invalid_properties must delegate to per-property _validation_error helpers
        assertTrue(src.contains("_validation_error(@"), "validated model must call _validation_error in list_invalid_properties");
    }

    @Test
    public void testApiNamespaceEmptyNestsDirectly() throws Exception {
        final File output = Files.createTempDirectory("test").toFile();
        output.deleteOnExit();
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/crystal/petstore.yaml");
        CrystalClientCodegen codegen = new CrystalClientCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put("apiNamespace", ""); // nest api classes directly under moduleName
        List<File> files = new DefaultGenerator().opts(
            new ClientOptInput().openAPI(openAPI).config(codegen)).generate();

        File pet = files.stream()
            .filter(f -> f.getName().equals("pet.cr") &&
                f.getPath().replace(File.separatorChar, '/').contains("/api/"))
            .findFirst().orElseThrow(() -> new AssertionError("api/pet.cr missing"));
        String src = FileUtils.readFileToString(pet, StandardCharsets.UTF_8);
        assertTrue(src.contains("class Pet"), "api class present");
        Assert.assertFalse(src.contains("module Api"), "with apiNamespace=\"\" there must be no Api sub-namespace");
    }

    @Test
    public void testInheritanceAndReservedModelName() throws Exception {
        final File output = Files.createTempDirectory("test").toFile();
        output.deleteOnExit();
        final OpenAPI openAPI = TestUtils.parseFlattenSpec(
            "src/test/resources/3_0/petstore-with-fake-endpoints-models-for-testing.yaml");
        CodegenConfig codegen = new CrystalClientCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        List<File> files = new DefaultGenerator().opts(
            new ClientOptInput().openAPI(openAPI).config(codegen)).generate();

        // allOf inheritance: child must not re-declare inherited properties and must call super.
        File cat = files.stream().filter(f -> f.getName().equals("cat.cr")).findFirst()
            .orElseThrow(() -> new AssertionError("cat.cr missing"));
        String catSrc = FileUtils.readFileToString(cat, StandardCharsets.UTF_8);
        assertTrue(catSrc.contains("class Cat < Animal"), "Cat must inherit Animal");
        assertTrue(catSrc.contains("property declawed"), "Cat keeps its own property");
        Assert.assertFalse(catSrc.contains("property class_name"),
            "Cat must NOT re-declare the inherited class_name property");
        assertTrue(catSrc.contains("super("), "Cat constructor must call super for inherited props");

        // discriminated base type emits use_json_discriminator/use_yaml_discriminator dispatching
        // to the mapped subtypes.
        File animal = files.stream().filter(f -> f.getName().equals("animal.cr")).findFirst()
            .orElseThrow(() -> new AssertionError("animal.cr missing"));
        String animalSrc = FileUtils.readFileToString(animal, StandardCharsets.UTF_8);
        assertTrue(animalSrc.contains("use_json_discriminator \"className\""),
            "Animal must dispatch on its discriminator for JSON");
        assertTrue(animalSrc.contains("use_yaml_discriminator \"className\""),
            "Animal must dispatch on its discriminator for YAML");
        assertTrue(animalSrc.contains("=> Cat") && animalSrc.contains("=> Dog"),
            "discriminator mapping must reference the mapped subtypes");

        // api return/param types referencing a same-named model must be module-qualified, else
        // inside `Api::Pet` the bare `Pet` resolves to the resource class, not the model.
        File petApi = files.stream().filter(f -> f.getName().equals("pet.cr") &&
            f.getPath().replace(File.separatorChar, '/').contains("/api/")).findFirst()
            .orElseThrow(() -> new AssertionError("api/pet.cr missing"));
        String petApiSrc = FileUtils.readFileToString(petApi, StandardCharsets.UTF_8);
        assertTrue(petApiSrc.contains("Array(OpenAPIClient::Pet)"),
            "api types referencing a same-named model must be module-qualified (OpenAPIClient::Pet)");
        Assert.assertFalse(petApiSrc.contains("Response(Array(Pet))"),
            "must not reference the bare model name (would resolve to the Api::Pet resource class)");

        // a model named like a generated infrastructure class is renamed (Client -> ModelClient)
        assertTrue(files.stream().anyMatch(f -> f.getName().equals("model_client.cr")),
            "a 'Client' schema must be renamed to ModelClient to avoid clashing with the facade");
        Assert.assertFalse(
            files.stream().anyMatch(f -> f.getPath().replace(File.separatorChar, '/').endsWith("/models/client.cr")),
            "there must be no models/client.cr clashing with the generated Client facade");
    }

    @Test
    public void testAdditionalPropertiesPreserved() throws Exception {
        final File output = Files.createTempDirectory("test").toFile();
        output.deleteOnExit();
        final OpenAPI openAPI = TestUtils.parseFlattenSpec(
            "src/test/resources/3_0/petstore-with-fake-endpoints-models-for-testing.yaml");
        CodegenConfig codegen = new CrystalClientCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        List<File> files = new DefaultGenerator().opts(
            new ClientOptInput().openAPI(openAPI).config(codegen)).generate();

        // a model that allows additionalProperties captures+round-trips unknown keys via Unmapped
        File nc = files.stream().filter(f -> f.getName().equals("nullable_class.cr")).findFirst()
            .orElseThrow(() -> new AssertionError("nullable_class.cr missing"));
        assertTrue(FileUtils.readFileToString(nc, StandardCharsets.UTF_8)
                .contains("include JSON::Serializable::Unmapped"),
            "a model allowing additionalProperties must include JSON::Serializable::Unmapped");
        // a plain model without additionalProperties must NOT include it
        File tag = files.stream().filter(f -> f.getName().equals("tag.cr")).findFirst()
            .orElseThrow(() -> new AssertionError("tag.cr missing"));
        Assert.assertFalse(FileUtils.readFileToString(tag, StandardCharsets.UTF_8)
                .contains("JSON::Serializable::Unmapped"),
            "a model without additionalProperties must not include Unmapped");
    }

    @Test
    public void testScalarDefaultValues() throws Exception {
        final File output = Files.createTempDirectory("test").toFile();
        output.deleteOnExit();
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/crystal/scalar-defaults.yaml");
        CodegenConfig codegen = new CrystalClientCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        List<File> files = new DefaultGenerator().opts(
            new ClientOptInput().openAPI(openAPI).config(codegen)).generate();
        File w = files.stream().filter(f -> f.getName().equals("widget.cr")).findFirst()
            .orElseThrow(() -> new AssertionError("widget.cr missing"));
        String src = FileUtils.readFileToString(w, StandardCharsets.UTF_8);
        assertTrue(src.contains("property size : Int32? = 10"), "integer default applied");
        assertTrue(src.contains("property label : String? = \"hi\""), "string default applied");
        assertTrue(src.contains("property active : Bool? = true"), "boolean default applied");
        // date/time defaults are intentionally NOT emitted (rendering not guaranteed valid Crystal)
        assertTrue(src.contains("property created : Time?") && !src.contains("property created : Time? ="),
            "date-time default must be skipped");
    }

    @Test
    public void testCookieParams() throws Exception {
        final File output = Files.createTempDirectory("test").toFile();
        output.deleteOnExit();
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/crystal/cookie-params.yaml");
        CodegenConfig codegen = new CrystalClientCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        List<File> files = new DefaultGenerator().opts(
            new ClientOptInput().openAPI(openAPI).config(codegen)).generate();
        File api = files.stream().filter(f -> f.getName().endsWith(".cr") &&
            f.getPath().replace(File.separatorChar, '/').contains("/api/")).findFirst()
            .orElseThrow(() -> new AssertionError("api file missing"));
        String src = FileUtils.readFileToString(api, StandardCharsets.UTF_8);
        // cookie params must appear in the signature and be sent via a combined Cookie header
        assertTrue(src.contains("session : String? = nil") && src.contains("tracking : String? = nil"),
            "cookie params must be declared in the signature");
        assertTrue(src.contains("\"Cookie\" =>") && src.contains("join(\"; \")"),
            "cookie params must be sent as a combined Cookie header");
    }

    @Test
    public void testOneOfDiscriminatorDispatch() throws Exception {
        final File output = Files.createTempDirectory("test").toFile();
        output.deleteOnExit();
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/crystal/oneof-discriminator.yaml");
        CodegenConfig codegen = new CrystalClientCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        List<File> files = new DefaultGenerator().opts(
            new ClientOptInput().openAPI(openAPI).config(codegen)).generate();
        File pet = files.stream().filter(f -> f.getName().equals("pet.cr")).findFirst()
            .orElseThrow(() -> new AssertionError("pet.cr missing"));
        String src = FileUtils.readFileToString(pet, StandardCharsets.UTF_8);
        // the discriminator must be read from the raw JSON field name (petType, not pet_type)
        assertTrue(src.contains("disc_h[\"petType\"]"), "discriminator must use the JSON field name");
        assertTrue(src.contains("when \"cat\" then return new(Cat.from_json") &&
                   src.contains("when \"dog\" then return new(Dog.from_json"),
            "discriminator must dispatch to the mapped member types");
    }

    @Test
    public void testNamedEnumAliasAndAnyOfUnion() throws Exception {
        final File output = Files.createTempDirectory("test").toFile();
        output.deleteOnExit();
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/crystal/qdrant.json");
        CodegenConfig codegen = new CrystalClientCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        List<File> files = new DefaultGenerator().opts(
            new ClientOptInput().openAPI(openAPI).config(codegen)).generate();

        // named enum -> plain alias to its underlying primitive (consistent with inline enums)
        File scalarType = files.stream().filter(f -> f.getName().equals("scalar_type.cr")).findFirst()
            .orElseThrow(() -> new AssertionError("scalar_type.cr missing"));
        String enumSrc = FileUtils.readFileToString(scalarType, StandardCharsets.UTF_8);
        assertTrue(enumSrc.contains("alias ScalarType = String"),
            "named enum must be an alias to its underlying type");
        Assert.assertFalse(enumSrc.contains("build_from_hash"), "legacy enum class machinery must be gone");

        // anyOf -> a wrapper union that deserialises by trying each member
        File orderValue = files.stream().filter(f -> f.getName().equals("order_value.cr")).findFirst()
            .orElseThrow(() -> new AssertionError("order_value.cr missing"));
        String anyOfSrc = FileUtils.readFileToString(orderValue, StandardCharsets.UTF_8);
        assertTrue(anyOfSrc.contains("class OrderValue"), "anyOf model is a wrapper class");
        assertTrue(anyOfSrc.contains("Float64") && anyOfSrc.contains("Int64"), "anyOf members present");
        assertTrue(anyOfSrc.contains("def self.from_json"), "anyOf wrapper deserialises by trying each member");
        Assert.assertFalse(anyOfSrc.contains("const_get"), "broken const_get machinery must be gone");

        // oneOf -> wrapper with its own try-each from_json. It must NOT `include JSON::Serializable`
        // (that would generate a field-based new(pull) that can't build a union -> runtime crash).
        File oneOf = files.stream().filter(f -> f.getName().equals("optimizers_status.cr")).findFirst()
            .orElseThrow(() -> new AssertionError("optimizers_status.cr missing"));
        String oneOfSrc = FileUtils.readFileToString(oneOf, StandardCharsets.UTF_8);
        assertTrue(oneOfSrc.contains("def self.openapi_one_of"), "oneOf wrapper lists its members");
        assertTrue(oneOfSrc.contains("def self.from_json") && oneOfSrc.contains("def self.new(pull"),
            "oneOf wrapper must define its own JSON deserialisation");
        Assert.assertFalse(oneOfSrc.contains("include JSON::Serializable"),
            "oneOf wrapper must not include JSON::Serializable (generated new(pull) can't build a union)");
    }

    @Test
    public void testHyphenatedApiKeySchemeQuotesSymbol() throws Exception {
        final File output = Files.createTempDirectory("test").toFile();
        output.deleteOnExit();
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/crystal/auth-hyphen.yaml");
        CodegenConfig codegen = new CrystalClientCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        List<File> files = new DefaultGenerator().opts(
            new ClientOptInput().openAPI(openAPI).config(codegen)).generate();

        File config = files.stream().filter(f -> f.getName().equals("configuration.cr")).findFirst()
            .orElseThrow(() -> new AssertionError("configuration.cr missing"));
        String src = FileUtils.readFileToString(config, StandardCharsets.UTF_8);
        // A scheme/param name with a hyphen must be a quoted symbol, else Crystal parses
        // `:api-key` as `:api - key` and fails to compile.
        assertTrue(src.contains("api_key_with_prefix(:\"api-key\")"),
            "hyphenated api key scheme must use a quoted symbol");
        Assert.assertFalse(src.contains("api_key_with_prefix(:api-key)"),
            "must not emit a bare symbol containing a hyphen (won't compile)");
    }

    @Test
    public void testQueryArrayCollectionFormat() throws Exception {
        final File output = Files.createTempDirectory("test").toFile();
        output.deleteOnExit();
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/crystal/collection-format.yaml");
        CodegenConfig codegen = new CrystalClientCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        List<File> files = new DefaultGenerator().opts(
            new ClientOptInput().openAPI(openAPI).config(codegen)).generate();

        File api = files.stream().filter(f -> f.getName().equals("search.cr") &&
            f.getPath().replace(File.separatorChar, '/').contains("/api/")).findFirst()
            .orElseThrow(() -> new AssertionError("api/search.cr missing"));
        String src = FileUtils.readFileToString(api, StandardCharsets.UTF_8);
        // non-multi array query params are joined with their separator; multi stays an array.
        assertTrue(src.contains("\"csv_ids\" => csv_ids.try(&.map(&.to_s).join(\",\"))"),
            "csv array param must be comma-joined");
        assertTrue(src.contains("\"ssv_ids\" => ssv_ids.try(&.map(&.to_s).join(\" \"))"),
            "ssv array param must be space-joined");
        assertTrue(src.contains("\"pipe_ids\" => pipe_ids.try(&.map(&.to_s).join(\"|\"))"),
            "pipe array param must be pipe-joined");
        assertTrue(src.contains("\"multi_ids\" => multi_ids,") || src.contains("\"multi_ids\" => multi_ids "),
            "multi array param must stay an array (no join)");
    }
}
