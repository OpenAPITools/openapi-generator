package org.openapitools.codegen;

import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.api.TemplatePathLocator;
import org.openapitools.codegen.api.TemplateProcessor;
import org.openapitools.codegen.api.TemplatingEngineAdapter;
import org.openapitools.codegen.api.TemplatingExecutor;
import org.openapitools.codegen.templating.TemplateManagerOptions;
import org.openapitools.codegen.templating.TemplateNotFoundException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.*;
import java.nio.charset.StandardCharsets;
import java.nio.file.*;
import java.util.*;
import java.util.regex.Pattern;

/**
 * Manages the lookup, compilation, and writing of template files
 */
public class TemplateManager implements TemplatingExecutor, TemplateProcessor {
    private final TemplateManagerOptions options;
    private final TemplatingEngineAdapter engineAdapter;
    private final TemplatePathLocator[] templateLoaders;

    private final Logger LOGGER = LoggerFactory.getLogger(TemplateManager.class);

    /**
     * Constructs a new instance of a {@link TemplateManager}
     *
     * @param options The {@link TemplateManagerOptions} for reading and writing templates
     * @param engineAdapter The adaptor to underlying templating engine
     * @param templateLoaders Loaders which define where we look for templates
     */
    public TemplateManager(
            TemplateManagerOptions options,
            TemplatingEngineAdapter engineAdapter,
            TemplatePathLocator[] templateLoaders) {
        this.options = options;
        this.engineAdapter = engineAdapter;
        this.templateLoaders = templateLoaders;
    }

    private String getFullTemplateFile(String name) {
        String template = Arrays.stream(this.templateLoaders)
                .map(i -> i.getFullTemplatePath(name))
                .filter(Objects::nonNull)
                .findFirst()
                .orElse("");

        if (StringUtils.isEmpty(template)) {
            throw new TemplateNotFoundException(name);
        }

        if (name == null || name.contains("..")) {
            throw new IllegalArgumentException("Template location must be constrained to template directory.");
        }

        return template;
    }

    /**
     * returns the template content by name
     *
     * @param name the template name (e.g. model.mustache)
     * @return the contents of that template
     */
    @Override
    public String getFullTemplateContents(String name) {
        return readTemplate(getFullTemplateFile(name));
    }

    /**
     * Returns the path of a template, allowing access to the template where consuming literal contents aren't desirable or possible.
     *
     * @param name the template name (e.g. model.mustache)
     * @return The {@link Path} to the template
     */
    @Override
    public Path getFullTemplatePath(String name) {
        return Paths.get(getFullTemplateFile(name));
    }

    /**
     * Gets a normalized classpath resource location according to OS-specific file separator
     *
     * @param name The name of the resource file/directory to find
     *
     * @return A normalized string according to OS-specific file separator
     */
    public static String getCPResourcePath(final String name) {
        if (!"/".equals(File.separator)) {
            return name.replaceAll(Pattern.quote(File.separator), "/");
        }
        return name;
    }

    /**
     * Reads a template's contents from the specified location
     *
     * @param name The location of the template
     * @return The raw template contents
     */
    @SuppressWarnings("java:S112")
    // ignored rule java:S112 as RuntimeException is used to match previous exception type
    public String readTemplate(String name) {
        if (name == null || name.contains("..")) {
            throw new IllegalArgumentException("Template location must be constrained to template directory.");
        }
        try (Reader reader = getTemplateReader(name)) {
            if (reader == null) {
                throw new RuntimeException("no file found");
            }
            try (Scanner s = new Scanner(reader).useDelimiter("\\A")) {
                return s.hasNext() ? s.next() : "";
            }
        } catch (Exception e) {
            LOGGER.error("{}", e.getMessage(), e);
        }
        throw new RuntimeException("can't load template " + name);
    }

    @SuppressWarnings({"squid:S2095", "java:S112"})
    // ignored rule squid:S2095 as used in the CLI and it's required to return a reader
    // ignored rule java:S112 as RuntimeException is used to match previous exception type
    public Reader getTemplateReader(String name) {
        try {
            InputStream is = getInputStream(name);
            return new InputStreamReader(is, StandardCharsets.UTF_8);
        } catch (FileNotFoundException e) {
            LOGGER.error(e.getMessage());
            throw new RuntimeException("can't load template " + name);
        }
    }

    private InputStream getInputStream(String name) throws FileNotFoundException {
        InputStream is;
        is = this.getClass().getClassLoader().getResourceAsStream(getCPResourcePath(name));
        if (is == null) {
            if (name == null || name.contains("..")) {
                throw new IllegalArgumentException("Template location must be constrained to template directory.");
            }
            is = new FileInputStream(name); // May throw but never return a null value
        }
        return is;
    }

    /**
     * Writes data to a compiled template
     *
     * @param data     Input data
     * @param template Input template location
     * @param target The targeted file output location
     *
     * @return The actual file
     */
    @Override
    public File write(Map<String, Object> data, String template, File target) throws IOException {
        if (this.engineAdapter.handlesFile(template)) {
            // Only pass files with valid endings through template engine
            String templateContent = this.engineAdapter.compileTemplate(this, data, template);
            return writeToFile(target.getPath(), templateContent);
        } else {
            // Do a straight copy of the file if not listed as supported by the template engine.
            InputStream is;
            try {
                // look up the file using the same template resolution logic the adapters would use.
                String fullTemplatePath = getFullTemplateFile(template);
                is = getInputStream(fullTemplatePath);
            } catch (TemplateNotFoundException ex) {
                is = new FileInputStream(Paths.get(template).toFile());
            }
            return writeToFile(target.getAbsolutePath(), IOUtils.toByteArray(is));
        }
    }

    @Override
    public void ignore(Path path, String context) {
        LOGGER.info("Ignored {} ({})", path, context);
    }

    @Override
    public void skip(Path path, String context) {
        LOGGER.info("Skipped {} ({})", path, context);
    }

    /**
     * Write String to a file, formatting as UTF-8
     *
     * @param filename The name of file to write
     * @param contents The contents string.
     * @return File representing the written file.
     * @throws IOException If file cannot be written.
     */
    public File writeToFile(String filename, String contents) throws IOException {
        return writeToFile(filename, contents.getBytes(StandardCharsets.UTF_8));
    }

    /**
     * Write bytes to a file
     *
     * @param filename The name of file to write
     * @param contents The contents bytes.  Typically, this is a UTF-8 formatted string.
     * @return File representing the written file.
     * @throws IOException If file cannot be written.
     */
    @Override
    public File writeToFile(String filename, byte[] contents) throws IOException {
        // Use Paths.get here to normalize path (for Windows file separator, space escaping on Linux/Mac, etc)
        File outputFile = Paths.get(filename).toFile();

        if (this.options.isMinimalUpdate()) {
            String tempFilename = filename + ".tmp";
            File tempFile = null;
            try {
                tempFile = writeToFileRaw(tempFilename, contents);
                if (!filesEqual(tempFile, outputFile)) {
                    LOGGER.info("writing file {}", filename);
                    Files.move(tempFile.toPath(), outputFile.toPath(), StandardCopyOption.REPLACE_EXISTING);
                    tempFile = null;
                } else {
                    LOGGER.info("skipping unchanged file {}", filename);
                }
            } finally {
                if (tempFile != null && tempFile.exists()) {
                    try {
                        Files.delete(tempFile.toPath());
                    } catch (Exception ex) {
                        LOGGER.error("Error removing temporary file {}", tempFile, ex);
                    }
                }
            }
        } else {
            LOGGER.info("writing file {}", filename);
            outputFile = writeToFileRaw(filename, contents);
        }

        return outputFile;
    }

    private File writeToFileRaw(String filename, byte[] contents) throws IOException {
        // Use Paths.get here to normalize path (for Windows file separator, space escaping on Linux/Mac, etc)
        File output = Paths.get(filename).toFile();
        if (this.options.isSkipOverwrite() && output.exists()) {
            LOGGER.info("skip overwrite of file {}", filename);
            return output;
        }

        if (output.getParent() != null && !new File(output.getParent()).exists()) {
            File parent = Paths.get(output.getParent()).toFile();
            parent.mkdirs();
        }
        Files.write(output.toPath(), contents);

        return output;
    }

    private boolean filesEqual(File file1, File file2) throws IOException {
        return file1.exists() && file2.exists() && Arrays.equals(Files.readAllBytes(file1.toPath()), Files.readAllBytes(file2.toPath()));
    }
}
