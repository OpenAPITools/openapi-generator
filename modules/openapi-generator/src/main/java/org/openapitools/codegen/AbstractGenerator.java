/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 * Copyright 2018 SmartBear Software
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.openapitools.codegen;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.StandardCopyOption;
import java.util.Arrays;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.api.TemplatingGenerator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.*;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.Map;
import java.util.Scanner;
import java.util.regex.Pattern;

public abstract class AbstractGenerator implements TemplatingGenerator {
    private static final Logger LOGGER = LoggerFactory.getLogger(AbstractGenerator.class);
    protected boolean dryRun = false;
    protected Map<String, DryRunStatus> dryRunStatusMap = new HashMap<>();

    /**
     * Is the minimal-file-update option enabled?
     * 
     * @return Option value
     */
    public abstract boolean getEnableMinimalUpdate();

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
    @SuppressWarnings("static-method")
    public File writeToFile(String filename, byte[] contents) throws IOException {
        if (getEnableMinimalUpdate()) {
            String tempFilename = filename + ".tmp";
            // Use Paths.get here to normalize path (for Windows file separator, space escaping on Linux/Mac, etc)
            File outputFile = Paths.get(filename).toFile();
            File tempFile = null;
            try {
                tempFile = writeToFileRaw(tempFilename, contents);
                if (!filesEqual(tempFile, outputFile)) {
                    LOGGER.info("writing file " + filename);
                    Files.move(tempFile.toPath(), outputFile.toPath(), StandardCopyOption.REPLACE_EXISTING);
                    tempFile = null;
                } else {
                    LOGGER.info("skipping unchanged file " + filename);
                }
            } finally {
                if (tempFile != null && tempFile.exists()) {
                    try {
                        tempFile.delete();
                    } catch (Exception ex) {
                        LOGGER.error("Error removing temporary file " + tempFile, ex);
                    }
                }
            }
            return outputFile;
        } else {
            LOGGER.info("writing file " + filename);
            return writeToFileRaw(filename, contents);
        }
    }

    private boolean filesEqual(File file1, File file2) throws IOException {
        return file1.exists() && file2.exists() && Arrays.equals(Files.readAllBytes(file1.toPath()), Files.readAllBytes(file2.toPath()));
    }
    
    private File writeToFileRaw(String filename, byte[] contents) throws IOException {
        // Use Paths.get here to normalize path (for Windows file separator, space escaping on Linux/Mac, etc)
        File output = Paths.get(filename).toFile();
        if (output.getParent() != null && !new File(output.getParent()).exists()) {
            File parent = Paths.get(output.getParent()).toFile();
            parent.mkdirs();
        }
        Files.write(output.toPath(), contents);
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

    @SuppressWarnings("squid:S2095")
    // ignored rule as used in the CLI and it's required to return a reader
    public Reader getTemplateReader(String name) {
        InputStream is = null;
        try {
            is = this.getClass().getClassLoader().getResourceAsStream(getCPResourcePath(name));
            if (is == null) {
                is = new FileInputStream(new File(name)); // May throw but never return a null value
            }
            return new InputStreamReader(is, "UTF-8");
        } catch (FileNotFoundException | UnsupportedEncodingException e) {
            LOGGER.error(e.getMessage());
            throw new RuntimeException("can't load template " + name);
        }
    }

    private String buildLibraryFilePath(String dir, String library, String file) {
        return dir + File.separator + "libraries" + File.separator + library + File.separator + file;
    }

    /**
     * Get the template file path with template dir prepended, and use the
     * library template if exists.
     *
     * @param config Codegen config
     * @param templateFile Template file
     * @return String Full template file path
     */
    public String getFullTemplateFile(CodegenConfig config, String templateFile) {
        //1st the code will check if there's a <template folder>/libraries/<library> folder containing the file
        //2nd it will check for the file in the specified <template folder> folder
        //3rd it will check if there's an <embedded template>/libraries/<library> folder containing the file
        //4th and last it will assume the file is in <embedded template> folder.

        //check the supplied template library folder for the file
        final String library = config.getLibrary();
        if (StringUtils.isNotEmpty(library)) {
            //look for the file in the library subfolder of the supplied template
            final String libTemplateFile = buildLibraryFilePath(config.templateDir(), library, templateFile);
            if (new File(libTemplateFile).exists() || this.getClass().getClassLoader().getResource(libTemplateFile) != null) {
                return libTemplateFile;
            }
        }

        //check the supplied template main folder for the file
        final String template = config.templateDir() + File.separator + templateFile;
        if (new File(template).exists() || this.getClass().getClassLoader().getResource(template) != null) {
            return template;
        }

        //try the embedded template library folder next
        if (StringUtils.isNotEmpty(library)) {
            final String embeddedLibTemplateFile = buildLibraryFilePath(config.embeddedTemplateDir(), library, templateFile);
            if (embeddedTemplateExists(embeddedLibTemplateFile)) {
                // Fall back to the template file embedded/packaged in the JAR file library folder...
                return embeddedLibTemplateFile;
            }
        }
            
        // Fall back to the template file embedded/packaged in the JAR file...
        return config.embeddedTemplateDir() + File.separator + templateFile;
    }

    public String readResourceContents(String resourceFilePath) {
        StringBuilder sb = new StringBuilder();
        Scanner scanner = new Scanner(this.getClass().getResourceAsStream(getCPResourcePath(resourceFilePath)), "UTF-8");
        while (scanner.hasNextLine()) {
            String line = scanner.nextLine();
            sb.append(line).append('\n');
        }
        return sb.toString();
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
