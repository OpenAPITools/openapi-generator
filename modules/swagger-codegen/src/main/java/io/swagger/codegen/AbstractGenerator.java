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
import java.util.regex.Pattern;

public abstract class AbstractGenerator {

    public File writeToFile(String filename, String contents) throws IOException {
        System.out.println("writing file " + filename);
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
            java.util.Scanner s = new java.util.Scanner(reader).useDelimiter("\\A");
            return s.hasNext() ? s.next() : "";
        } catch (Exception e) {
            e.printStackTrace();
        }
        throw new RuntimeException("can't load template " + name);
    }

    public Reader getTemplateReader(String name) {
        try {
            InputStream is = this.getClass().getClassLoader().getResourceAsStream(getCPResourcePath(name));
            if (is == null) {
                is = new FileInputStream(new File(name));
            }
            if (is == null) {
                throw new RuntimeException("no file found");
            }
            return new InputStreamReader(is);
        } catch (Exception e) {
            e.printStackTrace();
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
            if (templateExists(libTemplateFile)) {
                return libTemplateFile;
            }
        }
        return config.templateDir() + File.separator + templateFile;
    }

    public boolean templateExists(String name) {
        return this.getClass().getClassLoader().getResource(getCPResourcePath(name)) != null;
    }

    public String getCPResourcePath(String name) {
        if (!"/".equals(File.separator)) {
            return name.replaceAll(Pattern.quote(File.separator), "/");
        }
        return name;
    }
}
