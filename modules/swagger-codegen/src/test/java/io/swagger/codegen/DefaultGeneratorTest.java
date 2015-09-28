package io.swagger.codegen;

import io.swagger.codegen.languages.JavaClientCodegen;
import io.swagger.models.Swagger;
import io.swagger.parser.SwaggerParser;
import org.apache.commons.io.FileUtils;
import org.junit.rules.TemporaryFolder;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;

import static java.nio.charset.StandardCharsets.UTF_8;
import static org.junit.Assert.fail;
import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertTrue;

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

    private void changeContent(File file) throws IOException {
        Writer out = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(file), UTF_8));
        out.write(TEST_SKIP_OVERWRITE);
        out.close();
    }

}
