package org.openapitools.codegen;

import org.openapitools.codegen.api.TemplatePathLocator;
import org.openapitools.codegen.templating.HandlebarsEngineAdapter;
import org.openapitools.codegen.templating.MustacheEngineAdapter;
import org.openapitools.codegen.templating.TemplateManagerOptions;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

import static org.testng.Assert.*;

@SuppressWarnings("ResultOfMethodCallIgnored")
public class TemplateManagerTest {
    static class ResourceTemplateLoader implements TemplatePathLocator {

        /**
         * Get the full path to a relative template file.
         *
         * @param relativeTemplateFile Template file
         * @return String Full template file path
         */
        @Override
        public String getFullTemplatePath(String relativeTemplateFile) {
            return Paths.get("templating","templates", relativeTemplateFile).toString();
        }
    }

    private final HandlebarsEngineAdapter handlebarsEngineAdapter = new HandlebarsEngineAdapter();
    private final MustacheEngineAdapter mustacheEngineAdapter = new MustacheEngineAdapter();
    private final TemplatePathLocator locator = new ResourceTemplateLoader();

    @Test
    public void loadTemplateContents(){
        TemplateManagerOptions opts = new TemplateManagerOptions(false,false);
        TemplateManager manager = new TemplateManager(opts, mustacheEngineAdapter, new TemplatePathLocator[]{ locator });

        assertEquals(manager.getFullTemplateContents("simple.mustache"), "{{name}} and {{age}}");
    }

    @Test
    public void readTemplate(){
        TemplateManagerOptions opts = new TemplateManagerOptions(false,false);
        TemplateManager manager = new TemplateManager(opts, mustacheEngineAdapter, new TemplatePathLocator[]{ locator });

        assertEquals(manager.readTemplate("templating/templates/simple.mustache"), "{{name}} and {{age}}");
    }

    @Test
    public void loadTemplatePath(){
        TemplateManagerOptions opts = new TemplateManagerOptions(false,false);
        TemplateManager manager = new TemplateManager(opts, mustacheEngineAdapter, new TemplatePathLocator[]{ locator });

        assertEquals(manager.getFullTemplatePath("simple.mustache"), Paths.get("templating/templates/simple.mustache"));
    }

    @Test
    public void getTemplateReader(){
        TemplateManagerOptions opts = new TemplateManagerOptions(false,false);
        TemplateManager manager = new TemplateManager(opts, mustacheEngineAdapter, new TemplatePathLocator[]{ locator });

        assertTrue(manager.getTemplateReader("templating/templates/simple.mustache") instanceof InputStreamReader);
    }

    @Test
    public void writeViaMustacheAdapter() throws IOException {
        TemplateManagerOptions opts = new TemplateManagerOptions(false,false);
        TemplateManager manager = new TemplateManager(opts, mustacheEngineAdapter, new TemplatePathLocator[]{ locator });
        Map<String, Object> data = new HashMap<>();
        data.put("name","Teddy");
        data.put("age", "3");

        Path target = Files.createTempDirectory("test-templatemanager");
        try {
            File output = new File(target.toFile(), "simple.txt");

            File written = manager.write(data, "simple.mustache", output);

            assertEquals(Files.readAllLines(written.toPath()).get(0), "Teddy and 3");
        } finally {
            target.toFile().delete();
        }
    }

    @Test
    public void skipOverwriteViaOption() throws IOException {
        TemplateManagerOptions opts = new TemplateManagerOptions(false,true);
        TemplateManager manager = new TemplateManager(opts, mustacheEngineAdapter, new TemplatePathLocator[]{ locator });
        Map<String, Object> data = new HashMap<>();
        data.put("name","Teddy");
        data.put("age", "3");

        Path target = Files.createTempDirectory("test-templatemanager");
        try {
            File output = new File(target.toFile(), "simple.txt");
            Files.write(output.toPath(), "original data".getBytes(StandardCharsets.UTF_8));

            File written = manager.write(data, "simple.mustache", output);

            assertEquals(Files.readAllLines(written.toPath()).get(0), "original data");
        } finally {
            target.toFile().delete();
        }
    }

    @Test
    public void minimalUpdateOnlyWritesChangedContents() throws IOException {
        TemplateManagerOptions opts = new TemplateManagerOptions(true,false);
        TemplateManager manager = new TemplateManager(opts, mustacheEngineAdapter, new TemplatePathLocator[]{ locator });
        Map<String, Object> data = new HashMap<>();
        data.put("name","Teddy");
        data.put("age", "3");

        Path target = Files.createTempDirectory("test-templatemanager");
        try {
            File output = new File(target.toFile(), "simple.txt");
            Files.write(output.toPath(), "original data".getBytes(StandardCharsets.UTF_8));
            long originalModified = output.lastModified();

            Thread.sleep(1000L);

            File written = manager.write(data, "simple.mustache", output);
            long firstWriteModified = written.lastModified();

            assertNotEquals(firstWriteModified, originalModified);

            // sanity check here.
            assertEquals(Files.readAllLines(written.toPath()).get(0), "Teddy and 3");

            File rewritten = manager.write(data, "simple.mustache", output);
            long lastModified = rewritten.lastModified();
            assertEquals(lastModified, firstWriteModified);
        } catch (InterruptedException e) {
            e.printStackTrace();
        } finally {
            target.toFile().delete();
        }
    }

    @Test
    public void overwritesWhenSkipOverwriteFalse() throws IOException {
        TemplateManagerOptions opts = new TemplateManagerOptions(false,false);
        TemplateManager manager = new TemplateManager(opts, mustacheEngineAdapter, new TemplatePathLocator[]{ locator });
        Map<String, Object> data = new HashMap<>();
        data.put("name","Teddy");
        data.put("age", "3");

        Path target = Files.createTempDirectory("test-templatemanager");
        try {
            File output = new File(target.toFile(), "simple.txt");
            Files.write(output.toPath(), "original data".getBytes(StandardCharsets.UTF_8));

            File written = manager.write(data, "simple.mustache", output);

            assertEquals(Files.readAllLines(written.toPath()).get(0), "Teddy and 3");
        } finally {
            target.toFile().delete();
        }
    }

    @Test
    public void writeViaHandlebarsAdapter() throws IOException {
        TemplateManagerOptions opts = new TemplateManagerOptions(false,false);
        TemplateManager manager = new TemplateManager(opts, handlebarsEngineAdapter, new TemplatePathLocator[]{ locator });
        Map<String, Object> data = new HashMap<>();
        data.put("name", "Jack");
        data.put("numbers", Arrays.asList(1, 2, 3, 4, 5));

        Path target = Files.createTempDirectory("test-templatemanager");
        try {
            File output = new File(target.toFile(), "simple.txt");

            File written = manager.write(data, "numbers.handlebars", output);

            assertEquals(Files.readAllLines(written.toPath()).get(0), "Jack counts 1 2 3 4 5");
        } finally {
            target.toFile().delete();
        }
    }
}