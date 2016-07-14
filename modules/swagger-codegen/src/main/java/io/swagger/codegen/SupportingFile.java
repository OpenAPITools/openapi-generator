package io.swagger.codegen;

public class SupportingFile {
    public String templateFile;
    public String folder;
    public String destinationFilename;

    public SupportingFile(String templateFile, String destinationFilename) {
        this(templateFile, "", destinationFilename);
    }

    public SupportingFile(String templateFile, String folder, String destinationFilename) {
        this.templateFile = templateFile;
        this.folder = folder;
        this.destinationFilename = destinationFilename;
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        builder.append("SupportingFile:").append("\n");
        builder.append("\ttemplateFile: ").append(templateFile).append("\n");
        builder.append("\tfolder: ").append(folder).append("\n");
        builder.append("\tdestinationFilename: ").append(destinationFilename).append("\n");

        return builder.toString();
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        SupportingFile that = (SupportingFile) o;

        if (templateFile != null ? !templateFile.equals(that.templateFile) : that.templateFile != null)
            return false;
        if (folder != null ? !folder.equals(that.folder) : that.folder != null)
            return false;
        return destinationFilename != null ? destinationFilename.equals(that.destinationFilename) : that.destinationFilename == null;

    }

    @Override
    public int hashCode() {
        int result = templateFile != null ? templateFile.hashCode() : 0;
        result = 31 * result + (folder != null ? folder.hashCode() : 0);
        result = 31 * result + (destinationFilename != null ? destinationFilename.hashCode() : 0);
        return result;
    }
}


