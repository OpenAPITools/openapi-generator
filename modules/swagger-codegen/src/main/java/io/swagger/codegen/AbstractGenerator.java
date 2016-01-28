package io.swagger.codegen;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.Reader;
import java.io.Writer;
import java.util.Scanner;
import java.util.regex.Pattern;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public abstract class AbstractGenerator {
    private static final Logger LOGGER = LoggerFactory.getLogger(AbstractGenerator.class);

    @SuppressWarnings("static-method")
    public File writeToFile(String filename, String contents) throws IOException {
        LOGGER.info("writing file " + filename);
        File output = new File(filename);

        if (output.getParent() != null && !new File(output.getParent()).exists()) {
            File parent = new File(output.getParent());
            parent.mkdirs();
        }
        Writer out = new BufferedWriter(new OutputStreamWriter(
                new FileOutputStream(output), "UTF-8"));

        out.write(contents);
        out.close();
        return output;
    }

    public String readTemplate(String name) {
        try {
            Reader reader = getTemplateReader(name);
            if (reader == null) {
                throw new RuntimeException("no file found");
            }
            Scanner s = new Scanner(reader).useDelimiter("\\A");
            return s.hasNext() ? s.next() : "";
        } catch (Exception e) {
            LOGGER.error(e.getMessage());
        }
        throw new RuntimeException("can't load template " + name);
    }

    public Reader getTemplateReader(String name) {
        try {
            InputStream is = this.getClass().getClassLoader().getResourceAsStream(getCPResourcePath(name));
            if (is == null) {
                is = new FileInputStream(new File(name)); // May throw but never return a null value
            }
            return new InputStreamReader(is);
        } catch (Exception e) {
            LOGGER.error(e.getMessage());
        }
        throw new RuntimeException("can't load template " + name);
    }

    /**
     * Get the template file path with template dir prepended, and use the
     * library template if exists.
     */
    public String getFullTemplateFile(CodegenConfig config, String templateFile) {
        String library = config.getLibrary();
        if (library != null && !"".equals(library)) {
            String libTemplateFile = config.templateDir() + File.separator +
                "libraries" + File.separator + library + File.separator +
                templateFile;

            if (new File(libTemplateFile).exists()) {
                return libTemplateFile;
            }

            libTemplateFile = config.embeddedTemplateDir() + File.separator +
                "libraries" + File.separator + library + File.separator +
                templateFile;
            if (embeddedTemplateExists(libTemplateFile)) {
                // Fall back to the template file embedded/packaged in the JAR file...
                return libTemplateFile;
            }
        }
        String template = config.templateDir() + File.separator + templateFile;
        if (new File(template).exists()) {
            return template;
        } else {
            // Fall back to the template file embedded/packaged in the JAR file...
            return config.embeddedTemplateDir() + File.separator + templateFile;
        }
    }

    public boolean embeddedTemplateExists(String name) {
        return this.getClass().getClassLoader().getResource(getCPResourcePath(name)) != null;
    }

    @SuppressWarnings("static-method")
    public String getCPResourcePath(String name) {
        if (!"/".equals(File.separator)) {
            return name.replaceAll(Pattern.quote(File.separator), "/");
        }
        return name;
    }
}
