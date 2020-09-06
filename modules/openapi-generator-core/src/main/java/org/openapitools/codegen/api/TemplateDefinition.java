package org.openapitools.codegen.api;

import java.util.Objects;
import java.util.StringJoiner;

public class TemplateDefinition {
    private final String templateFile;
    private final String folder;
    private final String destinationFilename;

    public TemplateDefinition(String templateFile, String destinationFilename) {
        this(templateFile, "", destinationFilename);
    }

    public TemplateDefinition(String templateFile, String folder, String destinationFilename) {
        if (templateFile == null) throw new IllegalArgumentException("templateFile may not be null.");
        if (folder == null) throw new IllegalArgumentException("folder may not be null.");
        if (destinationFilename == null) throw new IllegalArgumentException("destinationFilename may not be null.");

        this.templateFile = templateFile;
        this.folder = folder;
        this.destinationFilename = destinationFilename;
    }

    public String getDestinationFilename() {
        return destinationFilename;
    }

    public String getFolder() {
        return folder;
    }

    public String getTemplateFile() {
        return templateFile;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof TemplateDefinition)) return false;
        TemplateDefinition that = (TemplateDefinition) o;
        return getTemplateFile().equals(that.getTemplateFile()) &&
                getFolder().equals(that.getFolder()) &&
                getDestinationFilename().equals(that.getDestinationFilename());
    }

    @Override
    public int hashCode() {
        return Objects.hash(getTemplateFile(), getFolder(), getDestinationFilename());
    }

    @Override
    public String toString() {
        return new StringJoiner(", ", TemplateDefinition.class.getSimpleName() + "[", "]")
                .add("templateFile='" + templateFile + "'")
                .add("folder='" + folder + "'")
                .add("destinationFilename='" + destinationFilename + "'")
                .toString();
    }
}
