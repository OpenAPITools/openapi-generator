package org.openapitools.codegen.templating;

import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.TemplateManager;
import org.openapitools.codegen.api.TemplatePathLocator;

import java.io.File;
import java.nio.file.Paths;

/**
 * Locates templates according to {@link CodegenConfig} settings.
 */
public class GeneratorTemplateContentLocator implements TemplatePathLocator {
    private final CodegenConfig codegenConfig;

    /**
     * Constructs a new instance of {@link GeneratorTemplateContentLocator} for the provided {@link CodegenConfig}
     *
     * @param codegenConfig A generator's configuration used for determining template file location.
     */
    public GeneratorTemplateContentLocator(CodegenConfig codegenConfig) {
        this.codegenConfig = codegenConfig;
    }

    private String buildLibraryFilePath(String dir, String library, String file) {
        return Paths.get(dir, "libraries", library, file).normalize().toString();
    }

    /**
     * Determines whether an embedded file with the specified name exists.
     *
     * @param name The name of the file (i.e. relative to resource root)
     *
     * @return true if file is an embedded resource, false if it does not exist
     */
    public boolean embeddedTemplateExists(String name) {
        return classpathTemplateExists(name);
    }

    private boolean classpathTemplateExists(String name) {
        return this.getClass().getClassLoader().getResource(TemplateManager.getCPResourcePath(name)) != null;
    }

    /**
     * Get the template file path with template dir prepended, and use the library template if exists.
     *
     * Precedence:
     * 1) (template dir)/libraries/(library)
     * 2) (template dir)
     * 3) (embedded template dir)/libraries/(library)
     * 4) (embedded template dir)
     *
     * Where "template dir" may be user defined and "embedded template dir" are the built-in templates for the given generator.
     *
     * @param relativeTemplateFile Template file
     * @return String Full template file path
     */
    @Override
    public String getFullTemplatePath(String relativeTemplateFile) {
        CodegenConfig config = this.codegenConfig;

        //check the supplied template library folder for the file
        final String library = config.getLibrary();
        if (StringUtils.isNotEmpty(library)) {
            //look for the file in the library subfolder of the supplied template
            final String libTemplateFile = buildLibraryFilePath(config.templateDir(), library, relativeTemplateFile);
            // looks for user-defined file or classpath
            // supports template dir which refers to local file system or custom path in classpath as defined by templateDir
            if (new File(libTemplateFile).exists() || classpathTemplateExists(libTemplateFile)) {
                return libTemplateFile;
            }
        }

        // check the supplied template main folder for the file
        // File.separator is necessary here as the file load is OS-specific
        final String template = config.templateDir() + File.separator + relativeTemplateFile;
        // looks for user-defined file or classpath
        // supports template dir which refers to local file system or custom path in classpath as defined by templateDir
        if (new File(template).exists() || classpathTemplateExists(template)) {
            return template;
        }

        //try the embedded template library folder next
        if (StringUtils.isNotEmpty(library)) {
            final String embeddedLibTemplateFile = buildLibraryFilePath(config.embeddedTemplateDir(), library, relativeTemplateFile);
            // *only* looks for those files in classpath as defined by embeddedTemplateDir
            if (embeddedTemplateExists(embeddedLibTemplateFile)) {
                // Fall back to the template file embedded/packaged in the JAR file library folder...
                return embeddedLibTemplateFile;
            }
        }

        // Fall back to the template file for generator root directory embedded/packaged in the JAR file...
        String loc = config.embeddedTemplateDir() + File.separator + relativeTemplateFile;
        // *only* looks for those files in classpath as defined by embeddedTemplateDir
        if (embeddedTemplateExists(loc)) {
            return loc;
        }

        return null;
    }
}
