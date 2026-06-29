package org.openapitools.codegen;

import org.apache.commons.io.IOUtils;
import org.openapitools.codegen.api.TemplatePathLocator;
import org.openapitools.codegen.api.TemplatingEngineAdapter;
import org.openapitools.codegen.api.TemplatingExecutor;
import org.openapitools.codegen.templating.HandlebarsEngineAdapter;
import org.openapitools.codegen.templating.MustacheEngineAdapter;
import org.openapitools.codegen.templating.TemplateManagerOptions;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Writer;
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
            return Paths.get("templating", "templates", relativeTemplateFile).toString();
        }
    }

    private final HandlebarsEngineAdapter handlebarsEngineAdapter = new HandlebarsEngineAdapter();
    private final MustacheEngineAdapter mustacheEngineAdapter = new MustacheEngineAdapter();
    private final TemplatePathLocator locator = new ResourceTemplateLoader();

    static class WriterOnlyTemplateEngineAdapter implements TemplatingEngineAdapter {
        @Override
        public String getIdentifier() {
            return "writer-only";
        }

        @Override
        public String[] getFileExtensions() {
            return new String[]{"writer"};
        }

        @Override
        public String compileTemplate(TemplatingExecutor executor, Map<String, Object> bundle, String templateFile) {
            throw new AssertionError("compileTemplate should not be called by TemplateManager.write");
        }

        @Override
        public void writeTemplate(TemplatingExecutor executor, Map<String, Object> bundle, String templateFile, Writer writer) throws IOException {
            writer.write((String) bundle.get("contents"));
        }
    }

    static class FailingWriterTemplateEngineAdapter implements TemplatingEngineAdapter {
        @Override
        public String getIdentifier() {
            return "failing-writer";
        }

        @Override
        public String[] getFileExtensions() {
            return new String[]{"failwriter"};
        }

        @Override
        public String compileTemplate(TemplatingExecutor executor, Map<String, Object> bundle, String templateFile) {
            throw new AssertionError("compileTemplate should not be called by TemplateManager.write");
        }

        @Override
        public void writeTemplate(TemplatingExecutor executor, Map<String, Object> bundle, String templateFile, Writer writer) throws IOException {
            writer.write("partial contents");
            throw new IOException("render failed");
        }
    }

    @Test
    public void loadTemplateContents() {
        TemplateManagerOptions opts = new TemplateManagerOptions(false, false);
        TemplateManager manager = new TemplateManager(opts, mustacheEngineAdapter, new TemplatePathLocator[]{locator});

        assertEquals(manager.getFullTemplateContents("simple.mustache"), "{{name}} and {{age}}");
    }

    @Test(expectedExceptions = IllegalArgumentException.class, expectedExceptionsMessageRegExp = "Template location must be constrained to template directory\\.")
    public void loadTemplateContentsThrowsForEscapingTemplates() {
        TemplateManagerOptions opts = new TemplateManagerOptions(false, false);
        TemplateManager manager = new TemplateManager(opts, mustacheEngineAdapter, new TemplatePathLocator[]{locator});

        manager.getFullTemplateContents("../simple.mustache");
        fail("Expected an exception that did not occur");
    }

    @Test
    public void readTemplate() {
        TemplateManagerOptions opts = new TemplateManagerOptions(false, false);
        TemplateManager manager = new TemplateManager(opts, mustacheEngineAdapter, new TemplatePathLocator[]{locator});

        assertEquals(manager.readTemplate("templating/templates/simple.mustache"), "{{name}} and {{age}}");
    }

    @Test
    public void loadTemplatePath() {
        TemplateManagerOptions opts = new TemplateManagerOptions(false, false);
        TemplateManager manager = new TemplateManager(opts, mustacheEngineAdapter, new TemplatePathLocator[]{locator});

        assertEquals(manager.getFullTemplatePath("simple.mustache"), Paths.get("templating/templates/simple.mustache"));
    }

    @Test
    public void getTemplateReader() {
        TemplateManagerOptions opts = new TemplateManagerOptions(false, false);
        TemplateManager manager = new TemplateManager(opts, mustacheEngineAdapter, new TemplatePathLocator[]{locator});

        assertTrue(manager.getTemplateReader("templating/templates/simple.mustache") instanceof InputStreamReader);
    }

    @Test(expectedExceptions = IllegalArgumentException.class, expectedExceptionsMessageRegExp = "Template location must be constrained to template directory\\.")
    public void getTemplateReaderThrowsForEscapingTemplates() {
        TemplateManagerOptions opts = new TemplateManagerOptions(false, false);
        TemplateManager manager = new TemplateManager(opts, mustacheEngineAdapter, new TemplatePathLocator[]{locator});

        manager.getTemplateReader("../templating/templates/simple.mustache");
        fail("Expected an exception that did not occur");
    }

    @Test
    public void writeViaMustacheAdapter() throws IOException {
        TemplateManagerOptions opts = new TemplateManagerOptions(false, false);
        TemplateManager manager = new TemplateManager(opts, mustacheEngineAdapter, new TemplatePathLocator[]{locator});
        Map<String, Object> data = new HashMap<>();
        data.put("name", "Teddy");
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
    public void writeUsesTemplateEngineWriterPath() throws IOException {
        TemplateManagerOptions opts = new TemplateManagerOptions(false, false);
        TemplateManager manager = new TemplateManager(opts, new WriterOnlyTemplateEngineAdapter(), new TemplatePathLocator[]{locator});
        Map<String, Object> data = new HashMap<>();
        data.put("contents", "streamed contents");

        Path target = Files.createTempDirectory("test-templatemanager");
        try {
            File output = new File(target.toFile(), "simple.txt");

            File written = manager.write(data, "simple.writer", output);

            assertEquals(Files.readAllLines(written.toPath()).get(0), "streamed contents");
        } finally {
            target.toFile().delete();
        }
    }

    @Test
    public void writePreservesExistingFileWhenStreamingTemplateFails() throws IOException {
        TemplateManagerOptions opts = new TemplateManagerOptions(false, false);
        TemplateManager manager = new TemplateManager(opts, new FailingWriterTemplateEngineAdapter(), new TemplatePathLocator[]{locator});
        Map<String, Object> data = new HashMap<>();

        Path target = Files.createTempDirectory("test-templatemanager");
        try {
            File output = new File(target.toFile(), "simple.txt");
            Files.write(output.toPath(), "original data".getBytes(StandardCharsets.UTF_8));

            IOException thrown = expectThrows(IOException.class, () -> manager.write(data, "simple.failwriter", output));

            assertEquals(thrown.getMessage(), "render failed");
            assertEquals(Files.readAllLines(output.toPath()).get(0), "original data");
            assertFalse(new File(target.toFile(), "simple.txt.tmp").exists());
        } finally {
            target.toFile().delete();
        }
    }

    @Test
    public void writeDoesNotLeaveOutputFileWhenStreamingTemplateFails() throws IOException {
        TemplateManagerOptions opts = new TemplateManagerOptions(false, false);
        TemplateManager manager = new TemplateManager(opts, new FailingWriterTemplateEngineAdapter(), new TemplatePathLocator[]{locator});
        Map<String, Object> data = new HashMap<>();

        Path target = Files.createTempDirectory("test-templatemanager");
        try {
            File output = new File(target.toFile(), "simple.txt");

            IOException thrown = expectThrows(IOException.class, () -> manager.write(data, "simple.failwriter", output));

            assertEquals(thrown.getMessage(), "render failed");
            assertFalse(output.exists());
            assertFalse(new File(target.toFile(), "simple.txt.tmp").exists());
        } finally {
            target.toFile().delete();
        }
    }

    @Test(enabled = false)
    public void writeUsingMustacheAdapterSkipsNonMustache() throws IOException {
        TemplateManagerOptions opts = new TemplateManagerOptions(false, false);
        TemplateManager manager = new TemplateManager(opts, mustacheEngineAdapter, new TemplatePathLocator[]{locator});
        Map<String, Object> data = new HashMap<>();
        data.put("this", "this");
        data.put("that", "1234");

        Path target = Files.createTempDirectory("test-templatemanager");
        try {
            File output = new File(target.toFile(), "simple.txt");
            File written = manager.write(data, "simple.txt", output);
            assertEquals(Files.readAllLines(written.toPath()).get(0), "# Should not escape {{this}} or that: {{{that}}}");

            output = new File(target.toFile(), "README.md");
            written = manager.write(data, "README.md", output);
            assertEquals(Files.readAllLines(written.toPath()).get(0), "This should not escape `{{this}}` or `{{{that}}}` or `{{name}} counts{{#each numbers}} {{.}}{{/each}}`");
        } finally {
            target.toFile().delete();
        }
    }

    @Test
    public void skipOverwriteViaOption() throws IOException {
        TemplateManagerOptions opts = new TemplateManagerOptions(false, true);
        TemplateManager manager = new TemplateManager(opts, mustacheEngineAdapter, new TemplatePathLocator[]{locator});
        Map<String, Object> data = new HashMap<>();
        data.put("name", "Teddy");
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
        TemplateManagerOptions opts = new TemplateManagerOptions(true, false);
        TemplateManager manager = new TemplateManager(opts, mustacheEngineAdapter, new TemplatePathLocator[]{locator});
        Map<String, Object> data = new HashMap<>();
        data.put("name", "Teddy");
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
    public void streamComparisonHandlesDifferentReadChunkSizes() throws IOException {
        byte[] contents = new byte[20_000];
        for (int i = 0; i < contents.length; i++) {
            contents[i] = (byte) (i % 251);
        }

        InputStream is1 = new ShortReadInputStream(contents, contents.length);
        assertTrue(IOUtils.contentEquals(is1, new ShortReadInputStream(contents, 3)));
    }

    @Test
    public void overwritesWhenSkipOverwriteFalse() throws IOException {
        TemplateManagerOptions opts = new TemplateManagerOptions(false, false);
        TemplateManager manager = new TemplateManager(opts, mustacheEngineAdapter, new TemplatePathLocator[]{locator});
        Map<String, Object> data = new HashMap<>();
        data.put("name", "Teddy");
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
        TemplateManagerOptions opts = new TemplateManagerOptions(false, false);
        TemplateManager manager = new TemplateManager(opts, handlebarsEngineAdapter, new TemplatePathLocator[]{locator});
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

    @Test(enabled = false)
    public void writeUsingHandlebarsAdapterSkipsNonHandlebars() throws IOException {
        TemplateManagerOptions opts = new TemplateManagerOptions(false, false);
        TemplateManager manager = new TemplateManager(opts, handlebarsEngineAdapter, new TemplatePathLocator[]{locator});
        Map<String, Object> data = new HashMap<>();
        data.put("this", "this");
        data.put("that", "1234");

        Path target = Files.createTempDirectory("test-templatemanager");
        try {
            File output = new File(target.toFile(), "simple.txt");
            File written = manager.write(data, "simple.txt", output);
            assertEquals(Files.readAllLines(written.toPath()).get(0), "# Should not escape {{this}} or that: {{{that}}}");

            output = new File(target.toFile(), "README.md");
            written = manager.write(data, "README.md", output);
            assertEquals(Files.readAllLines(written.toPath()).get(0), "This should not escape `{{this}}` or `{{{that}}}` or `{{name}} counts{{#each numbers}} {{.}}{{/each}}`");
        } finally {
            target.toFile().delete();
        }
    }

    private static class ShortReadInputStream extends InputStream {
        private final byte[] contents;
        private final int maxReadSize;
        private int offset;

        private ShortReadInputStream(byte[] contents, int maxReadSize) {
            this.contents = contents;
            this.maxReadSize = maxReadSize;
        }

        @Override
        public int read() {
            if (offset >= contents.length) {
                return -1;
            }
            return contents[offset++] & 0xff;
        }

        @Override
        public int read(byte[] buffer, int off, int len) {
            if (offset >= contents.length) {
                return -1;
            }
            int read = Math.min(Math.min(len, maxReadSize), contents.length - offset);
            System.arraycopy(contents, offset, buffer, off, read);
            offset += read;
            return read;
        }
    }
}
