package org.openapitools.codegen;

public class GlobalSupportingFile extends SupportingFile {

    GlobalSupportingFile(String templateFile, String folder, String destinationFilename) {
        super(templateFile, folder, destinationFilename);
    }

    GlobalSupportingFile(String templateFile, String destinationFilename) {
        super(templateFile, destinationFilename);
    }
}
