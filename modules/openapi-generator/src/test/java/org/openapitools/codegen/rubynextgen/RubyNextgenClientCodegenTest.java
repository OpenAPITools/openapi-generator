package org.openapitools.codegen.rubynextgen;

import org.openapitools.codegen.languages.RubyNextgenClientCodegen;
import org.testng.annotations.Test;
import static org.testng.Assert.assertEquals;

public class RubyNextgenClientCodegenTest {
    @Test
    public void testGeneratorName() {
        RubyNextgenClientCodegen gen = new RubyNextgenClientCodegen();
        assertEquals(gen.getName(), "ruby-nextgen");
        assertEquals(gen.getTag().toString(), "CLIENT");
    }

    @Test
    public void testModelNameAvoidsInfraCollision() {
        RubyNextgenClientCodegen gen = new RubyNextgenClientCodegen();
        assertEquals(gen.toModelName("Client"), "ModelClient");
        assertEquals(gen.toModelName("Pet"), "Pet");
        assertEquals(gen.toModelFilename("ApiResponse"), "api_response");
    }

    @Test
    public void testApiNamespacedNames() {
        RubyNextgenClientCodegen gen = new RubyNextgenClientCodegen();
        assertEquals(gen.toApiFilename("dcim/cable-terminations"), "dcim/cable_terminations");
        assertEquals(gen.toApiName("dcim/cable-terminations"), "Dcim::CableTerminations");
    }

    @Test
    public void testNumericEnumValueNotQuoted() {
        RubyNextgenClientCodegen g = new RubyNextgenClientCodegen();
        org.testng.Assert.assertEquals(g.toEnumValue("1", "Integer"), "1");
        org.testng.Assert.assertEquals(g.toEnumValue("1.5", "Float"), "1.5");
        org.testng.Assert.assertEquals(g.toEnumValue("available", "String"), "'available'");
    }

    @Test
    public void testNumericEnumVarNameIsValidConstant() {
        RubyNextgenClientCodegen g = new RubyNextgenClientCodegen();
        // Ruby 3.0+ reserves _1.._9 as numbered block parameters, so a bare "_" + digit
        // prefix (DefaultCodegen's behavior) emits an illegal constant: `_1 = '1'` raises
        // "_1 is reserved for numbered parameters". Numeric enum values must instead get a
        // letter prefix so the constant is legal (mirrors the stock ruby generator's `N`).
        assertEquals(g.toEnumVarName("1", "String"), "N1");
        assertEquals(g.toEnumVarName("9", "String"), "N9");
        assertEquals(g.toEnumVarName("10", "String"), "N10");
        assertEquals(g.toEnumVarName("100", "String"), "N100");
        assertEquals(g.toEnumVarName("1", "Integer"), "N1");
        assertEquals(g.toEnumVarName("1.5", "Float"), "N1_DOT_5");
        // non-numeric values are unaffected
        assertEquals(g.toEnumVarName("available", "String"), "AVAILABLE");
        assertEquals(g.toEnumVarName("", "String"), "EMPTY");
        // purely symbolic values (telephony IVR keys) sanitize to empty/illegal constants
        // with the naive rule; translate them to their word names instead.
        assertEquals(g.toEnumVarName("#", "String"), "HASH");
        assertEquals(g.toEnumVarName("*", "String"), "STAR");
        // the reserved forms must never leak out
        for (int i = 1; i <= 9; i++) {
            org.testng.Assert.assertNotEquals(g.toEnumVarName(String.valueOf(i), "String"), "_" + i);
        }
    }

    @Test
    public void testLongModelNameIsTruncatedForTar() {
        RubyNextgenClientCodegen g = new RubyNextgenClientCodegen();
        // Real OVH inline body models concatenate the whole deep path, producing 120+ char
        // file names. The tar ustar format caps each path component at 100 bytes, so
        // `gem build` raises Gem::Package::TooLongFileName. The model file name -- including
        // the "_spec.rb" test variant -- must stay comfortably under that limit.
        String longName = "TelephonyBillingAccountOvhPabxServiceNameDialplanDialplanIdExtension"
                + "ExtensionIdConditionScreenListPostRequest";
        String file = g.toModelFilename(longName);
        org.testng.Assert.assertTrue((file + "_spec.rb").length() < 100,
                "file name too long for tar: " + file + " (" + file.length() + " chars)");

        // deterministic: identical input yields identical output on every call
        assertEquals(g.toModelName(longName), g.toModelName(longName));

        // collision-free: two distinct long names sharing a truncated head must differ
        String sibling = "TelephonyBillingAccountOvhPabxServiceNameDialplanDialplanIdExtension"
                + "ExtensionIdConditionTimePostRequest";
        org.testng.Assert.assertNotEquals(g.toModelName(longName), g.toModelName(sibling));

        // Zeitwerk consistency: the file name is exactly underscore(class name)
        assertEquals(g.toModelFilename(longName),
                org.openapitools.codegen.utils.StringUtils.underscore(g.toModelName(longName)));

        // short names are left completely untouched
        assertEquals(g.toModelName("Pet"), "Pet");
        assertEquals(g.toModelName("ApiResponse"), "ApiResponse");
    }

    @Test
    public void testLeadingDigitOperationIdIsPrefixed() {
        RubyNextgenClientCodegen g = new RubyNextgenClientCodegen();
        // Ruby method names cannot begin with a digit; OVH price-plan path segments like
        // "2013v1_bhs1a_filer_hourly" would otherwise emit `def 2013v1...` (SyntaxError).
        // Prefix with "call_" as the stock ruby generator does.
        assertEquals(g.toOperationId("2013v1_bhs1a_filer_hourly"), "call_2013v1_bhs1a_filer_hourly");
        assertEquals(g.toOperationId("2fa"), "call_2fa");
        // ordinary names are untouched (delegated to the parent behavior)
        assertEquals(g.toOperationId("getPets"), "get_pets");
        assertEquals(g.toOperationId("list"), "list");
    }

    @Test
    public void testOperationGroupingBuildsNamespaces() {
        io.swagger.v3.oas.models.OpenAPI openAPI = org.openapitools.codegen.TestUtils
                .parseSpec("src/test/resources/3_0/petstore.yaml");
        RubyNextgenClientCodegen codegen = new RubyNextgenClientCodegen();
        codegen.setOpenAPI(openAPI);
        codegen.preprocessOpenAPI(openAPI);

        io.swagger.v3.oas.models.Operation op = openAPI.getPaths().get("/pet/{petId}").getGet();
        org.openapitools.codegen.CodegenOperation co = codegen.fromOperation(
                "/pet/{petId}", "GET", op, null);
        java.util.Map<String, java.util.List<org.openapitools.codegen.CodegenOperation>> groups = new java.util.HashMap<>();
        codegen.addOperationToGroup("pet", "/pet/{petId}", op, co, groups);

        assertEquals(co.vendorExtensions.get("x-rb-namespace"), "pet");
        assertEquals(co.operationId, "get");
    }

    @Test
    public void testOperationIdCollisionIsDeduped() {
        io.swagger.v3.oas.models.OpenAPI openAPI = org.openapitools.codegen.TestUtils
                .parseSpec("src/test/resources/3_0/petstore.yaml");
        RubyNextgenClientCodegen codegen = new RubyNextgenClientCodegen();
        codegen.setOpenAPI(openAPI);
        codegen.preprocessOpenAPI(openAPI);

        // POST /pet (addPet) and POST /pet/{petId} (updatePetWithForm) both route to
        // action "create" within the "pet" group; the second must not silently overwrite
        // the first's operationId.
        io.swagger.v3.oas.models.Operation addPetOp = openAPI.getPaths().get("/pet").getPost();
        org.openapitools.codegen.CodegenOperation addPetCo = codegen.fromOperation(
                "/pet", "POST", addPetOp, null);
        io.swagger.v3.oas.models.Operation updatePetWithFormOp = openAPI.getPaths().get("/pet/{petId}").getPost();
        org.openapitools.codegen.CodegenOperation updatePetWithFormCo = codegen.fromOperation(
                "/pet/{petId}", "POST", updatePetWithFormOp, null);

        java.util.Map<String, java.util.List<org.openapitools.codegen.CodegenOperation>> groups = new java.util.HashMap<>();
        codegen.addOperationToGroup("pet", "/pet", addPetOp, addPetCo, groups);
        codegen.addOperationToGroup("pet", "/pet/{petId}", updatePetWithFormOp, updatePetWithFormCo, groups);

        java.util.List<org.openapitools.codegen.CodegenOperation> petOps = groups.get("pet");
        org.testng.Assert.assertEquals(petOps.size(), 2);
        org.testng.Assert.assertNotEquals(addPetCo.operationId, updatePetWithFormCo.operationId);
        assertEquals(addPetCo.operationId, "create");
        assertEquals(updatePetWithFormCo.operationId, "create_post");
    }

    @Test
    public void testMultiTagOperationNotDuplicated() {
        io.swagger.v3.oas.models.OpenAPI openAPI = org.openapitools.codegen.TestUtils
                .parseSpec("src/test/resources/3_0/crystal/qdrant.json");
        RubyNextgenClientCodegen codegen = new RubyNextgenClientCodegen();
        codegen.setOpenAPI(openAPI);
        codegen.preprocessOpenAPI(openAPI);

        // GET /collections/{collection_name}/cluster carries tags ["collections", "cluster"],
        // so the core delivers it to addOperationToGroup once per tag. Because we group by
        // path (not tag), both deliveries land in the same group -- and must NOT produce two
        // methods (`cluster` and `cluster_get`).
        io.swagger.v3.oas.models.Operation op = openAPI.getPaths()
                .get("/collections/{collection_name}/cluster").getGet();
        org.openapitools.codegen.CodegenOperation co = codegen.fromOperation(
                "/collections/{collection_name}/cluster", "GET", op, null);

        java.util.Map<String, java.util.List<org.openapitools.codegen.CodegenOperation>> groups = new java.util.HashMap<>();
        codegen.addOperationToGroup("collections", "/collections/{collection_name}/cluster", op, co, groups);
        codegen.addOperationToGroup("cluster", "/collections/{collection_name}/cluster", op, co, groups);

        int total = groups.values().stream().mapToInt(java.util.List::size).sum();
        org.testng.Assert.assertEquals(total, 1, "multi-tag operation must be emitted once");
    }

    @Test
    public void testModelPropertyValidationFlags() {
        io.swagger.v3.oas.models.OpenAPI openAPI = org.openapitools.codegen.TestUtils
            .parseSpec("src/test/resources/3_0/petstore.yaml");
        RubyNextgenClientCodegen codegen = new RubyNextgenClientCodegen();
        codegen.setOpenAPI(openAPI);
        org.openapitools.codegen.CodegenModel cm = codegen.fromModel("Pet",
            openAPI.getComponents().getSchemas().get("Pet"));
        org.openapitools.codegen.model.ModelsMap mm = org.openapitools.codegen.TestUtils
            .createCodegenModelWrapper(cm);
        codegen.postProcessModels(mm);
        org.openapitools.codegen.CodegenProperty status = cm.vars.stream()
            .filter(v -> v.baseName.equals("status")).findFirst().orElseThrow(RuntimeException::new);
        org.testng.Assert.assertEquals(status.vendorExtensions.get("x-rb-validated"), Boolean.TRUE);
    }

    @Test
    public void testArrayItemsValidation() {
        io.swagger.v3.oas.models.OpenAPI openAPI = org.openapitools.codegen.TestUtils
            .parseSpec("src/test/resources/3_0/petstore-with-fake-endpoints-models-for-testing.yaml");
        RubyNextgenClientCodegen codegen = new RubyNextgenClientCodegen();
        codegen.setOpenAPI(openAPI);
        org.openapitools.codegen.CodegenModel cm = codegen.fromModel("ArrayTest",
            openAPI.getComponents().getSchemas().get("ArrayTest"));
        org.openapitools.codegen.model.ModelsMap mm = org.openapitools.codegen.TestUtils
            .createCodegenModelWrapper(cm);
        codegen.postProcessModels(mm);
        org.openapitools.codegen.CodegenProperty arrayOfString = cm.vars.stream()
            .filter(v -> v.baseName.equals("array_of_string")).findFirst().orElseThrow(RuntimeException::new);
        org.testng.Assert.assertTrue(arrayOfString.isArray);
        org.testng.Assert.assertEquals(arrayOfString.maxItems, Integer.valueOf(3));
        org.testng.Assert.assertEquals(arrayOfString.vendorExtensions.get("x-rb-validated"), Boolean.TRUE);
    }

    @Test
    public void testAdditionalPropertiesExtension() {
        io.swagger.v3.oas.models.OpenAPI openAPI = org.openapitools.codegen.TestUtils
            .parseSpec("src/test/resources/3_0/petstore-with-fake-endpoints-models-for-testing.yaml");
        RubyNextgenClientCodegen codegen = new RubyNextgenClientCodegen();
        codegen.setOpenAPI(openAPI);

        // FreeFormObject is `additionalProperties: true` with no declared properties --
        // exercises isAdditionalPropertiesTrue.
        org.openapitools.codegen.CodegenModel freeForm = codegen.fromModel("FreeFormObject",
            openAPI.getComponents().getSchemas().get("FreeFormObject"));
        org.openapitools.codegen.model.ModelsMap freeFormMap = org.openapitools.codegen.TestUtils
            .createCodegenModelWrapper(freeForm);
        codegen.postProcessModels(freeFormMap);
        assertEquals(freeForm.vendorExtensions.get("x-rb-additional-properties"), Boolean.TRUE);

        // Pet has no additionalProperties at all -- the extension must be absent so
        // partial_model_generic.mustache does not emit the overflow accessor.
        org.openapitools.codegen.CodegenModel pet = codegen.fromModel("Pet",
            openAPI.getComponents().getSchemas().get("Pet"));
        org.openapitools.codegen.model.ModelsMap petMap = org.openapitools.codegen.TestUtils
            .createCodegenModelWrapper(pet);
        codegen.postProcessModels(petMap);
        org.testng.Assert.assertNull(pet.vendorExtensions.get("x-rb-additional-properties"));
    }

    @Test
    public void testApiNamespaceOption() {
        RubyNextgenClientCodegen gen = new RubyNextgenClientCodegen();
        gen.additionalProperties().put(org.openapitools.codegen.CodegenConstants.GEM_NAME, "petstore");
        gen.processOpts();
        assertEquals(gen.additionalProperties().get("apiNamespacePresent"), Boolean.TRUE);
        assertEquals(gen.additionalProperties().get("apiNamespace"), "Api");

        RubyNextgenClientCodegen disabled = new RubyNextgenClientCodegen();
        disabled.additionalProperties().put(org.openapitools.codegen.CodegenConstants.GEM_NAME, "petstore");
        disabled.additionalProperties().put("apiNamespace", "");
        disabled.processOpts();
        assertEquals(disabled.additionalProperties().get("apiNamespacePresent"), Boolean.FALSE);
    }

    @Test
    @SuppressWarnings("unchecked")
    public void testNestedModuleName() {
        // Single-level module: nesting flags off, no parents to pre-define.
        RubyNextgenClientCodegen simple = new RubyNextgenClientCodegen();
        simple.additionalProperties().put(org.openapitools.codegen.CodegenConstants.MODULE_NAME, "Petstore");
        simple.processOpts();
        assertEquals(simple.additionalProperties().get("moduleNameNested"), Boolean.FALSE);

        // Two-level module: parent must be pre-defined so `module A::B` doesn't raise.
        RubyNextgenClientCodegen nested = new RubyNextgenClientCodegen();
        nested.additionalProperties().put(org.openapitools.codegen.CodegenConstants.MODULE_NAME, "Dolibarr::Api");
        nested.processOpts();
        assertEquals(nested.additionalProperties().get("moduleNameNested"), Boolean.TRUE);
        assertEquals(nested.additionalProperties().get("moduleNameParents"),
                java.util.Arrays.asList("Dolibarr"));

        // Three-level module: cumulative parent prefixes, deepest-but-one last.
        RubyNextgenClientCodegen deep = new RubyNextgenClientCodegen();
        deep.additionalProperties().put(org.openapitools.codegen.CodegenConstants.MODULE_NAME, "A::B::C");
        deep.processOpts();
        assertEquals(deep.additionalProperties().get("moduleNameParents"),
                java.util.Arrays.asList("A", "A::B"));
    }

    @Test
    public void testLicenseMitFlag() {
        RubyNextgenClientCodegen mit = new RubyNextgenClientCodegen();
        mit.additionalProperties().put(org.openapitools.codegen.CodegenConstants.GEM_NAME, "petstore");
        mit.additionalProperties().put("gemLicense", "MIT");
        mit.processOpts();
        assertEquals(mit.additionalProperties().get("isLicenseMIT"), Boolean.TRUE);

        RubyNextgenClientCodegen def = new RubyNextgenClientCodegen();
        def.additionalProperties().put(org.openapitools.codegen.CodegenConstants.GEM_NAME, "petstore");
        def.processOpts();
        assertEquals(def.additionalProperties().get("isLicenseMIT"), Boolean.FALSE);
    }

    @Test
    public void testModelReturnTypeIsModelsQualified() {
        RubyNextgenClientCodegen gen = new RubyNextgenClientCodegen();
        gen.additionalProperties().put(org.openapitools.codegen.CodegenConstants.GEM_NAME, "petstore");
        gen.processOpts(); // sets moduleName = Petstore

        org.openapitools.codegen.CodegenOperation modelOp = new org.openapitools.codegen.CodegenOperation();
        modelOp.returnBaseType = "Pet";
        modelOp.returnType = "Pet";
        org.openapitools.codegen.CodegenOperation arrayOp = new org.openapitools.codegen.CodegenOperation();
        arrayOp.returnBaseType = "Pet";
        arrayOp.returnType = "Array<Pet>";
        arrayOp.isArray = true;
        org.openapitools.codegen.CodegenOperation intOp = new org.openapitools.codegen.CodegenOperation();
        intOp.returnBaseType = "integer";
        intOp.returnType = "Integer";

        org.openapitools.codegen.model.OperationMap opMap = new org.openapitools.codegen.model.OperationMap();
        opMap.setOperation(java.util.Arrays.asList(modelOp, arrayOp, intOp));
        org.openapitools.codegen.model.OperationsMap ops = new org.openapitools.codegen.model.OperationsMap();
        ops.setOperation(opMap);

        gen.postProcessOperationsWithModels(ops, java.util.Collections.emptyList());

        // Models live under Petstore::Models:: -- a bare Petstore::Pet would raise NameError at call time.
        assertEquals(modelOp.vendorExtensions.get("x-rb-return-type"), "Petstore::Models::Pet");
        assertEquals(arrayOp.vendorExtensions.get("x-rb-return-type"), "[Petstore::Models::Pet]");
        // Primitives pass through as nil (Integer is a core class, not a dispatchable model).
        assertEquals(intOp.vendorExtensions.get("x-rb-return-type"), "nil");
    }

    @Test
    public void testSupportingFilesRegistered() {
        RubyNextgenClientCodegen gen = new RubyNextgenClientCodegen();
        gen.additionalProperties().put(org.openapitools.codegen.CodegenConstants.GEM_NAME, "petstore");
        gen.processOpts();
        java.util.Set<String> names = new java.util.HashSet<>();
        for (org.openapitools.codegen.SupportingFile sf : gen.supportingFiles()) names.add(sf.getTemplateFile());
        org.testng.Assert.assertTrue(names.contains("gem.mustache"));
        org.testng.Assert.assertTrue(names.contains("connection.mustache"));
        org.testng.Assert.assertTrue(names.contains("serializable.mustache"));
        org.testng.Assert.assertTrue(names.contains("validations.mustache"));
    }

    @Test
    public void testConfigurationExposesMiddlewareSeam() throws Exception {
        java.nio.file.Path target = java.nio.file.Files.createTempDirectory("test");
        target.toFile().deleteOnExit();
        org.openapitools.codegen.ClientOptInput input =
            new org.openapitools.codegen.config.CodegenConfigurator()
                .setGeneratorName("ruby-nextgen")
                .setInputSpec("src/test/resources/3_0/petstore.yaml")
                .setOutputDir(target.toString())
                .addAdditionalProperty("gemName", "petstore")
                .addAdditionalProperty("moduleName", "Petstore")
                .toClientOptInput();
        new org.openapitools.codegen.DefaultGenerator(false).opts(input).generate();
        org.openapitools.codegen.TestUtils.assertFileContains(
            target.resolve("lib/petstore/configuration.rb"),
            "def use(", "@middlewares.each");
    }

    @Test
    public void testAcronymModelsGetZeitwerkInflections() throws Exception {
        java.nio.file.Path target = java.nio.file.Files.createTempDirectory("test");
        target.toFile().deleteOnExit();
        org.openapitools.codegen.ClientOptInput input =
            new org.openapitools.codegen.config.CodegenConfigurator()
                .setGeneratorName("ruby-nextgen")
                .setInputSpec("src/test/resources/3_0/ruby-nextgen/acronym.yaml")
                .setOutputDir(target.toString())
                .addAdditionalProperty("gemName", "acme")
                .addAdditionalProperty("moduleName", "Acme")
                .toClientOptInput();
        new org.openapitools.codegen.DefaultGenerator(false).opts(input).generate();
        // gem.rb must register the acronym exception so Zeitwerk can autoload it: the model
        // class is HTTPConfig but the file is http_config.rb, which Zeitwerk would otherwise
        // expect to define HttpConfig.
        org.openapitools.codegen.TestUtils.assertFileContains(
            target.resolve("lib/acme.rb"),
            "@loader.inflector.inflect(", "\"http_config\" => \"HTTPConfig\"");
        // and the model file itself must define the acronym-cased constant
        org.openapitools.codegen.TestUtils.assertFileContains(
            target.resolve("lib/acme/models/http_config.rb"), "HTTPConfig");
        // Acronyms in API resource classes must be registered too: the file
        // api/dedicated_cloud/two_fa_whitelist.rb defines DedicatedCloud::TwoFAWhitelist,
        // which the default inflector (expecting TwoFaWhitelist) would fail to autoload.
        org.openapitools.codegen.TestUtils.assertFileContains(
            target.resolve("lib/acme.rb"), "\"two_fa_whitelist\" => \"TwoFAWhitelist\"");
        org.openapitools.codegen.TestUtils.assertFileContains(
            target.resolve("lib/acme/api/dedicated_cloud/two_fa_whitelist.rb"), "TwoFAWhitelist");
    }
}
