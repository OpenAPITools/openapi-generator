package org.openapitools.codegen.api;

import java.util.Objects;
import java.util.StringJoiner;

/**
 * TemplateDefinition is a type which defines the basics of a template file and target output location.
 */
public class TemplateDefinition {
    private final String templateFile;
    private final String folder;
    private final String destinationFilename;

    /**
     * <p>Constructor for TemplateDefinition.</p>
     *
     * @param templateFile a template path relative to user template or embedded template.
     * @param destinationFilename a target output location for the file, relative to the output directory.
     */
    public TemplateDefinition(String templateFile, String destinationFilename) {
        this(templateFile, "", destinationFilename);
    }

    /**
     * <p>Constructor for TemplateDefinition.</p>
     *
     * @param templateFile a template path relative to user template or embedded template.
     * @param folder a folder in the target output directory in which to place the target file.
     * @param destinationFilename a target output location for the file, relative to the output directory.
     */
    public TemplateDefinition(String templateFile, String folder, String destinationFilename) {
        if (templateFile == null) throw new IllegalArgumentException("templateFile may not be null.");
        if (folder == null) throw new IllegalArgumentException("folder may not be null.");
        if (destinationFilename == null) throw new IllegalArgumentException("destinationFilename may not be null.");

        this.templateFile = templateFile;
        this.folder = folder;
        this.destinationFilename = destinationFilename;
    }

    /**
     * Gets target output location for the file, relative to the output directory.
     *
     * @return a target output location for the file, relative to the output directory.
     */
    public String getDestinationFilename() {
        return destinationFilename;
    }

    /**
     * Gets a folder in the target output directory in which to place the target file.
     *
     * @return a a folder in the target output directory in which to place the target file.
     */
    public String getFolder() {
        return folder;
    }

    /**
     * Gets a template path relative to user template or embedded template.
     *
     * @return a template path relative to user template or embedded template.
     */
    public String getTemplateFile() {
        return templateFile;
    }

    /** {@inheritDoc} */
    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof TemplateDefinition)) return false;
        TemplateDefinition that = (TemplateDefinition) o;
        return getTemplateFile().equals(that.getTemplateFile()) &&
                getFolder().equals(that.getFolder()) &&
                getDestinationFilename().equals(that.getDestinationFilename());
    }

    /** {@inheritDoc} */
    @Override
    public int hashCode() {
        return Objects.hash(getTemplateFile(), getFolder(), getDestinationFilename());
    }

    /** {@inheritDoc} */
    @Override
    public String toString() {
        return new StringJoiner(", ", TemplateDefinition.class.getSimpleName() + "[", "]")
                .add("templateFile='" + templateFile + "'")
                .add("folder='" + folder + "'")
                .add("destinationFilename='" + destinationFilename + "'")
                .toString();
    }
}
