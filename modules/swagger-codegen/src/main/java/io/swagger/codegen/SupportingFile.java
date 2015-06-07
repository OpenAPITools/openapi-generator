package io.swagger.codegen;

public class SupportingFile {
    public String templateFile;
    public String folder;
    public String destinationFilename;

    public SupportingFile(String templateFile, String folder, String destinationFilename) {
        this.templateFile = templateFile;
        this.folder = folder;
        this.destinationFilename = destinationFilename;
    }
}