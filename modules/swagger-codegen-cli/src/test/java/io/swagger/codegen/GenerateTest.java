package io.swagger.codegen;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import org.apache.commons.io.FileUtils;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;

import static java.nio.charset.StandardCharsets.UTF_8;
import static org.junit.Assert.fail;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;

public class GenerateTest {

    private static final String TEST_SKIP_OVERWRITE = "testSkipOverwrite";
    private static final String POM_FILE = "pom.xml";
    private static final String MODEL_ORDER_FILE = "/src/main/java/io/swagger/client/model/Order.java";

    @Rule
    public TemporaryFolder folder = new TemporaryFolder();

    @Test
    public void testSkipOverwrite() throws IOException {
        final File output = folder.getRoot();

        String[] args = {"generate", "-i", "src/test/resources/petstore.json", "-l", "java", "-o", output.getAbsolutePath()};
        String[] argsWithSparam = Arrays.copyOf(args, args.length + 1);
        argsWithSparam[args.length] = "-s";

        //generate content first time without -s flag, so all generated files should be recorded
        SwaggerCodegen.main(args);
        final File order = new File(output, MODEL_ORDER_FILE);
        assertTrue(order.exists());

        //change content of one file
        changeContent(order);

        //generate content second time without -s flag, so changed file should be rewritten
        SwaggerCodegen.main(args);
        //order = new File(output, MODEL_ORDER_FILE);
        assertTrue(!TEST_SKIP_OVERWRITE.equals(FileUtils.readFileToString(order, StandardCharsets.UTF_8)));

        //change content again
        changeContent(order);
        //delete file
        final File pom = new File(output, POM_FILE);
        if (!pom.delete()) {
            fail();
        }

        //generate content third time with -s flag, so changed file should not be rewritten
        //and deleted file should be recorded
        SwaggerCodegen.main(argsWithSparam);
        assertEquals(FileUtils.readFileToString(order, StandardCharsets.UTF_8), TEST_SKIP_OVERWRITE);
        assertTrue(pom.exists());
    }

    private void changeContent(File file) throws IOException {
        Writer out = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(file), UTF_8));
        out.write(TEST_SKIP_OVERWRITE);
        out.close();
    }
}
