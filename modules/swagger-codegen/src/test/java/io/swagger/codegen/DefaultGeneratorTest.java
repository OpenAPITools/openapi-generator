package io.swagger.codegen;

import io.swagger.codegen.languages.JavaClientCodegen;
import io.swagger.models.ExternalDocs;
import io.swagger.models.Swagger;
import io.swagger.models.Tag;
import io.swagger.parser.SwaggerParser;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
import org.junit.rules.TemporaryFolder;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import java.io.*;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.*;

import static io.swagger.codegen.CodegenConstants.TEMPLATE_DIR;
import static java.nio.charset.StandardCharsets.UTF_8;
import static org.junit.Assert.fail;
import static org.testng.Assert.*;

/**
 * Tests for DefaultGenerator logic
 */
public class DefaultGeneratorTest {

    private static final String TEST_SKIP_OVERWRITE = "testSkipOverwrite";
    private static final String POM_FILE = "pom.xml";
    private static final String MODEL_ORDER_FILE = "/src/main/java/io/swagger/client/model/Order.java";
    private static final String API_CLIENT_FILE = "/src/main/java/io/swagger/client/ApiClient.java";
    private static final String BUILD_GRADLE_FILE = "build.gradle";

    private static final String LIBRARY_COMMENT = "//overloaded template file within library folder to add this comment";
    private static final String TEMPLATE_COMMENT = "//overloaded main template file to add this comment";

    public TemporaryFolder folder = new TemporaryFolder();

    @BeforeMethod
    public void setUp() throws Exception {
        folder.create();
    }

    @AfterMethod
    public void tearDown() throws Exception {
        folder.delete();
    }

    @Test
    public void testSecurityWithoutGlobal() throws Exception {
        final Swagger swagger = new SwaggerParser().read("src/test/resources/2_0/petstore.json");
        CodegenConfig codegenConfig = new JavaClientCodegen();

        ClientOptInput clientOptInput = new ClientOptInput().opts(new ClientOpts()).swagger(swagger).config(codegenConfig);

        DefaultGenerator gen = new DefaultGenerator();
        gen.opts(clientOptInput);
        Map<String, List<CodegenOperation>> paths = gen.processPaths(swagger.getPaths());

        CodegenSecurity cs, apiKey, petstoreAuth;

        // security of "getPetById": api_key
        CodegenOperation getPetById = findCodegenOperationByOperationId(paths, "getPetById");
        assertEquals(getPetById.authMethods.size(), 2);
        cs = getPetById.authMethods.get(0);
        if ("api_key".equals(cs.name)) {
            apiKey = cs;
            petstoreAuth = getPetById.authMethods.get(1);
        } else {
            petstoreAuth = cs;
            apiKey = getPetById.authMethods.get(1);
        }
        assertEquals(petstoreAuth.name, "petstore_auth");
        assertEquals(petstoreAuth.type, "oauth2");


        assertEquals(apiKey.name, "api_key");
        assertEquals(apiKey.type, "apiKey");

        // security of "updatePetWithForm": petstore_auth
        CodegenOperation updatePetWithForm = findCodegenOperationByOperationId(paths, "updatePetWithForm");
        assertEquals(updatePetWithForm.authMethods.size(), 1);
        petstoreAuth = updatePetWithForm.authMethods.iterator().next();
        assertEquals(petstoreAuth.name, "petstore_auth");
        assertEquals(petstoreAuth.type, "oauth2");

        // security of "loginUser": null (no global security either)
        CodegenOperation loginUser = findCodegenOperationByOperationId(paths, "loginUser");
        assertNull(loginUser.authMethods);
    }

    @Test
    public void testSecurityWithGlobal() throws Exception {
        final Swagger swagger = new SwaggerParser().read("src/test/resources/2_0/globalSecurity.json");
        CodegenConfig codegenConfig = new JavaClientCodegen();

        ClientOptInput clientOptInput = new ClientOptInput().opts(new ClientOpts()).swagger(swagger).config(codegenConfig);

        DefaultGenerator gen = new DefaultGenerator();
        gen.opts(clientOptInput);
        Map<String, List<CodegenOperation>> paths = gen.processPaths(swagger.getPaths());

        CodegenSecurity cs, apiKey, apiKey2, petstoreAuth;

        // security of "getPetById": api_key
        CodegenOperation getPetById = findCodegenOperationByOperationId(paths, "getPetById");
        assertEquals(getPetById.authMethods.size(), 2);
        cs = getPetById.authMethods.get(0);
        if ("api_key".equals(cs.name)) {
            apiKey = cs;
            petstoreAuth = getPetById.authMethods.get(1);
        } else {
            petstoreAuth = cs;
            apiKey = getPetById.authMethods.get(1);
        }
        assertEquals(petstoreAuth.type, "oauth2");
        assertEquals(petstoreAuth.name, "petstore_auth");
        assertEquals(apiKey.name, "api_key");
        assertEquals(apiKey.type, "apiKey");

        // security of "updatePetWithForm": petstore_auth
        CodegenOperation updatePetWithForm = findCodegenOperationByOperationId(paths, "updatePetWithForm");
        assertEquals(updatePetWithForm.authMethods.size(), 1);
        petstoreAuth = updatePetWithForm.authMethods.iterator().next();
        assertEquals(petstoreAuth.name, "petstore_auth");
        assertEquals(petstoreAuth.type, "oauth2");

        // security of "loginUser": api_key, petstore_auth (from global security)
        CodegenOperation loginUser = findCodegenOperationByOperationId(paths, "loginUser");
        assertEquals(loginUser.authMethods.size(), 2);
        cs = loginUser.authMethods.get(0);
        if ("api_key".equals(cs.name)) {
            apiKey = cs;
            petstoreAuth = loginUser.authMethods.get(1);
        } else {
            petstoreAuth = cs;
            apiKey = loginUser.authMethods.get(1);
        }
        assertEquals(apiKey.name, "api_key");
        assertEquals(apiKey.type, "apiKey");
        assertEquals(petstoreAuth.name, "petstore_auth");
        assertEquals(petstoreAuth.type, "oauth2");

        // security of "logoutUser": null (override global security)
        CodegenOperation logoutUser = findCodegenOperationByOperationId(paths, "logoutUser");
        assertNull(logoutUser.authMethods);

        // security of "getUserByName": api_key, api_key2 (override global security)
        CodegenOperation getUserByName = findCodegenOperationByOperationId(paths, "getUserByName");
        assertEquals(getUserByName.authMethods.size(), 2);
        cs = getUserByName.authMethods.get(0);
        if ("api_key".equals(cs.name)) {
            apiKey = cs;
            apiKey2 = getUserByName.authMethods.get(1);
        } else {
            apiKey2 = cs;
            apiKey = getUserByName.authMethods.get(1);
        }
        assertEquals(apiKey.name, "api_key");
        assertEquals(apiKey.type, "apiKey");
        assertEquals(apiKey2.name, "api_key2");
        assertEquals(apiKey2.type, "apiKey");
    }

    @Test
    public void testSkipOverwrite() throws Exception {
        final File output = folder.getRoot();

        final Swagger swagger = new SwaggerParser().read("src/test/resources/petstore.json");
        CodegenConfig codegenConfig = new JavaClientCodegen();
        codegenConfig.setLibrary("jersey1");
        codegenConfig.setOutputDir(output.getAbsolutePath());

        ClientOptInput clientOptInput = new ClientOptInput().opts(new ClientOpts()).swagger(swagger).config(codegenConfig);

        //generate content first time without skipOverwrite flag, so all generated files should be recorded
        new DefaultGenerator().opts(clientOptInput).generate();
        final File order = new File(output, MODEL_ORDER_FILE);
        assertTrue(order.exists());

        //change content of one file
        changeContent(order);

        //generate content second time without skipOverwrite flag, so changed file should be rewritten
        new DefaultGenerator().opts(clientOptInput).generate();

        assertTrue(!TEST_SKIP_OVERWRITE.equals(FileUtils.readFileToString(order, StandardCharsets.UTF_8)));

        //change content again
        changeContent(order);
        //delete file
        final File pom = new File(output, POM_FILE);
        if (pom.exists() && !pom.delete()) {
            fail("it doesn't delete");
        }

        //generate content third time with skipOverwrite flag, so changed file should not be rewritten
        //and deleted file should be recorded
        codegenConfig.setSkipOverwrite(true);
        new DefaultGenerator().opts(clientOptInput).generate();
        assertEquals(FileUtils.readFileToString(order, StandardCharsets.UTF_8), TEST_SKIP_OVERWRITE);
        // Disabling this check, it's not valid with the DefaultCodegen.writeOptional(...) arg
//        assertTrue(pom.exists());
    }

    private boolean containsOverloadedComments(File file, String ...search) throws IOException {
        for (String line : Files.readAllLines(file.toPath(), Charset.defaultCharset())) {
            if (StringUtils.containsAny(line, search)) {
                return true;
            }
        }

        return false;
    }

    @Test
    public void testOverloadingTemplateFiles() throws Exception {
        final File output = folder.getRoot();

        final Swagger swagger = new SwaggerParser().read("src/test/resources/petstore.json");
        CodegenConfig codegenConfig = new JavaClientCodegen();
        codegenConfig.setLibrary("jersey2");
        codegenConfig.setOutputDir(output.getAbsolutePath());

        ClientOptInput clientOptInput = new ClientOptInput().opts(new ClientOpts()).swagger(swagger).config(codegenConfig);
        //generate content first time without specifying an overloaded template file, so the default mustache files are used instead
        new DefaultGenerator().opts(clientOptInput).generate();

        final File order = new File(output, MODEL_ORDER_FILE);
        assertTrue(order.exists());
        assertFalse(containsOverloadedComments(order, TEMPLATE_COMMENT, LIBRARY_COMMENT));

        final File gradle = new File(output, BUILD_GRADLE_FILE);
        assertTrue(gradle.exists());
        assertFalse(containsOverloadedComments(gradle, TEMPLATE_COMMENT, LIBRARY_COMMENT));

        final File apiClient = new File(output, API_CLIENT_FILE);
        assertTrue(apiClient.exists());
        assertFalse(containsOverloadedComments(apiClient, TEMPLATE_COMMENT, LIBRARY_COMMENT));

        codegenConfig.additionalProperties().put(TEMPLATE_DIR, "src/test/resources/2_0/templates/Java");
        //generate content second time while specifying a template folder, so the files from the template are used instead
        new DefaultGenerator().opts(clientOptInput).generate();

        //this file won't contain the library comment because Jersey2 doesn't override the model template
        assertTrue(order.exists());
        assertTrue(containsOverloadedComments(order, TEMPLATE_COMMENT));

        assertTrue(gradle.exists());
        assertTrue(containsOverloadedComments(gradle, LIBRARY_COMMENT));

        assertTrue(apiClient.exists());
        assertTrue(containsOverloadedComments(apiClient, LIBRARY_COMMENT));
    }

    @Test
    public void testGenerateUniqueOperationIds() {
        final File output = folder.getRoot();

        final Swagger swagger = new SwaggerParser().read("src/test/resources/2_0/duplicateOperationIds.yaml");
        CodegenConfig codegenConfig = new JavaClientCodegen();
        codegenConfig.setOutputDir(output.getAbsolutePath());

        ClientOptInput clientOptInput = new ClientOptInput().opts(new ClientOpts()).swagger(swagger).config(codegenConfig);

        DefaultGenerator generator = new DefaultGenerator();
        generator.opts(clientOptInput);

        Map<String, List<CodegenOperation>> paths = generator.processPaths(swagger.getPaths());
        Set<String> opIds = new HashSet<String>();
        for(String path : paths.keySet()) {
            List<CodegenOperation> ops = paths.get(path);
            for(CodegenOperation op : ops) {
                assertFalse(opIds.contains(op.operationId));
                opIds.add(op.operationId);
            }
        }
    }

    @Test
    public void testResolveTagsAgainstSwaggerTagsDefinition() {
        final File output = folder.getRoot();

        String spec =
                "swagger: '2.0'\n" +
                "info:\n" +
                "  version: 1.0.0\n" +
                "  title: Swagger Petstore\n" +
                "tags:\n" +
                "  - name: pet\n" +
                "    description: Everything about your Pets\n" +
                "    externalDocs:\n" +
                "      description: Find out more\n" +
                "      url: 'http://swagger.io'\n" +
                "    x-vendor-ext: 'tag'\n" +
                "  - name: store\n" +
                "    description: Access to Petstore orders\n" +
                "  - name: user\n" +
                "    description: Operations about user\n" +
                "    externalDocs:\n" +
                "      x-vendor-ext: 'foo'\n" +
                "paths:\n" +
                "  /pet:\n" +
                "    get:\n" +
                "      tags:\n" +
                "        - pet\n" +
                "        - store\n" +
                "        - user\n" +
                "      responses:\n" +
                "        '200':\n" +
                "          description: OK";

        final List<Tag> expectedTags = new ArrayList<Tag>();
        expectedTags.add(new Tag().name("pet").description("Everything about your Pets").externalDocs(new ExternalDocs().description("Find out more").url("http://swagger.io")));
        expectedTags.add(new Tag().name("store").description("Access to Petstore orders"));
        expectedTags.add(new Tag().name("user").description("Operations about user").externalDocs(new ExternalDocs()));

        expectedTags.get(0).getVendorExtensions().put("x-vendor-ext", "tag");
        expectedTags.get(2).getExternalDocs().getVendorExtensions().put("x-vendor-ext", "foo");

        final Swagger swagger = new SwaggerParser().readWithInfo(spec).getSwagger();
        CodegenConfig codegenConfig = new JavaClientCodegen();
        codegenConfig.setOutputDir(output.getAbsolutePath());

        ClientOptInput clientOptInput = new ClientOptInput().opts(new ClientOpts()).swagger(swagger).config(codegenConfig);

        DefaultGenerator generator = new DefaultGenerator();
        generator.opts(clientOptInput);

        Map<String, List<CodegenOperation>> paths = generator.processPaths(swagger.getPaths());
        assertEquals(3, paths.size());

        List<String> sanitizedTags = Arrays.asList("Pet", "Store", "User");
        for (String tag : sanitizedTags) {
            List<CodegenOperation> operations = paths.get(tag);
            assertNotNull(operations);
            for (CodegenOperation operation : operations) {
                assertOperationHasTags(operation, expectedTags);
            }
        }
    }

    @Test
    public void testResolveTagsNoSwaggerTagsDefinition() {
        final File output = folder.getRoot();

        String spec =
                "swagger: '2.0'\n" +
                "info:\n" +
                "  version: 1.0.0\n" +
                "  title: Swagger Petstore\n" +
                "paths:\n" +
                "  /pet:\n" +
                "    get:\n" +
                "      tags:\n" +
                "        - pet\n" +
                "        - store\n" +
                "        - user\n" +
                "      responses:\n" +
                "        '200':\n" +
                "          description: OK";

        final List<Tag> expectedTags = new ArrayList<Tag>();
        expectedTags.add(new Tag().name("pet"));
        expectedTags.add(new Tag().name("store"));
        expectedTags.add(new Tag().name("user"));

        final Swagger swagger = new SwaggerParser().readWithInfo(spec).getSwagger();
        CodegenConfig codegenConfig = new JavaClientCodegen();
        codegenConfig.setOutputDir(output.getAbsolutePath());

        ClientOptInput clientOptInput = new ClientOptInput().opts(new ClientOpts()).swagger(swagger).config(codegenConfig);

        DefaultGenerator generator = new DefaultGenerator();
        generator.opts(clientOptInput);

        Map<String, List<CodegenOperation>> paths = generator.processPaths(swagger.getPaths());
        assertEquals(3, paths.size());

        List<String> sanitizedTags = Arrays.asList("Pet", "Store", "User");
        for (String tag : sanitizedTags) {
            List<CodegenOperation> operations = paths.get(tag);
            assertNotNull(operations);
            for (CodegenOperation operation : operations) {
                assertOperationHasTags(operation, expectedTags);
            }
        }
    }

    @Test
    public void testResolveTagsNoTagsDefined() {
        final File output = folder.getRoot();

        String spec =
                "swagger: '2.0'\n" +
                "info:\n" +
                "  version: 1.0.0\n" +
                "  title: Swagger Petstore\n" +
                "paths:\n" +
                "  /pet:\n" +
                "    get:\n" +
                "      responses:\n" +
                "        '200':\n" +
                "          description: OK";

        final List<Tag> expectedTags = new ArrayList<Tag>();
        expectedTags.add(new Tag().name("default"));

        final Swagger swagger = new SwaggerParser().readWithInfo(spec).getSwagger();
        CodegenConfig codegenConfig = new JavaClientCodegen();
        codegenConfig.setOutputDir(output.getAbsolutePath());

        ClientOptInput clientOptInput = new ClientOptInput().opts(new ClientOpts()).swagger(swagger).config(codegenConfig);

        DefaultGenerator generator = new DefaultGenerator();
        generator.opts(clientOptInput);

        Map<String, List<CodegenOperation>> paths = generator.processPaths(swagger.getPaths());
        assertEquals(1, paths.size());

        List<String> sanitizedTags = Collections.singletonList("Default");
        for (String tag : sanitizedTags) {
            List<CodegenOperation> operations = paths.get(tag);
            assertNotNull(operations);
            for (CodegenOperation operation : operations) {
                assertOperationHasTags(operation, expectedTags);
            }
        }
    }

    @Test
    public void testResolveTagNotDefinedInSwaggerTagsDefinition() {
        final File output = folder.getRoot();

        String spec =
                "swagger: '2.0'\n" +
                        "info:\n" +
                        "  version: 1.0.0\n" +
                        "  title: Swagger Petstore\n" +
                        "tags:\n" +
                        "  - name: pet\n" +
                        "    description: Everything about your Pets\n" +
                        "paths:\n" +
                        "  /pet:\n" +
                        "    get:\n" +
                        "      tags:\n" +
                        "        - pet\n" +
                        "        - store\n" + // Not defined above
                        "        - user\n" +  // Not defined above
                        "      responses:\n" +
                        "        '200':\n" +
                        "          description: OK";

        final List<Tag> expectedTags = new ArrayList<Tag>();
        expectedTags.add(new Tag().name("pet").description("Everything about your Pets"));
        expectedTags.add(new Tag().name("store"));
        expectedTags.add(new Tag().name("user"));

        final Swagger swagger = new SwaggerParser().readWithInfo(spec).getSwagger();
        CodegenConfig codegenConfig = new JavaClientCodegen();
        codegenConfig.setOutputDir(output.getAbsolutePath());

        ClientOptInput clientOptInput = new ClientOptInput().opts(new ClientOpts()).swagger(swagger).config(codegenConfig);

        DefaultGenerator generator = new DefaultGenerator();
        generator.opts(clientOptInput);

        Map<String, List<CodegenOperation>> paths = generator.processPaths(swagger.getPaths());
        assertEquals(3, paths.size());

        List<String> sanitizedTags = Arrays.asList("Pet", "Store", "User");
        for (String tag : sanitizedTags) {
            List<CodegenOperation> operations = paths.get(tag);
            assertNotNull(operations);
            for (CodegenOperation operation : operations) {
                assertOperationHasTags(operation, expectedTags);
            }
        }
    }

    private void assertOperationHasTags(CodegenOperation op, List<Tag> expectedTags) {
        assertNotNull(op.tags);
        assertEquals(op.tags.size(), expectedTags.size());

        for (Tag tag : expectedTags) {
            Tag foundTag = null;

            for (Tag opTag : op.tags) {
                if (tag.getName().equals(opTag.getName())) {
                    foundTag = opTag;
                    break;
                }
            }

            if (foundTag == null) {
                fail("Expected tag '" + tag.getName() + "' was not found on operation " + op.operationId);
            }

            assertEquals(tag, foundTag);
            if (!tag.getVendorExtensions().isEmpty()) {
                assertEquals(tag.getVendorExtensions(), foundTag.getVendorExtensions());
            }

            if (tag.getExternalDocs() != null && !tag.getExternalDocs().getVendorExtensions().isEmpty()) {
                assertEquals(tag.getExternalDocs().getVendorExtensions(), foundTag.getExternalDocs().getVendorExtensions());
            }
        }
    }

    private static void changeContent(File file) throws IOException {
        Writer out = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(file), UTF_8));
        out.write(TEST_SKIP_OVERWRITE);
        out.close();
    }

    private static CodegenOperation findCodegenOperationByOperationId(Map<String, List<CodegenOperation>> paths, String operationId) {
        for (List<CodegenOperation> ops : paths.values()) {
            for (CodegenOperation co : ops) {
                if (operationId.equals(co.operationId)) {
                    return co;
                }
            }
        }
        return null;
    }
}
