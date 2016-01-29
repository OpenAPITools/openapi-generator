package io.swagger.codegen;

import io.swagger.codegen.languages.JavaClientCodegen;
import io.swagger.models.Swagger;
import io.swagger.parser.SwaggerParser;
import org.apache.commons.io.FileUtils;
import org.junit.rules.TemporaryFolder;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

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

        CodegenSecurity apiKey, petstoreAuth;

        // security of "getPetById": api_key
        CodegenOperation getPetById = findCodegenOperationByOperationId(paths, "getPetById");
        assertEquals(getPetById.authMethods.size(), 1);
        apiKey = getPetById.authMethods.iterator().next();
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
        assertEquals(getPetById.authMethods.size(), 1);
        apiKey = getPetById.authMethods.iterator().next();
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
        if (!pom.delete()) {
            fail();
        }

        //generate content third time with skipOverwrite flag, so changed file should not be rewritten
        //and deleted file should be recorded
        codegenConfig.setSkipOverwrite(true);
        new DefaultGenerator().opts(clientOptInput).generate();
        assertEquals(FileUtils.readFileToString(order, StandardCharsets.UTF_8), TEST_SKIP_OVERWRITE);
        assertTrue(pom.exists());
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
