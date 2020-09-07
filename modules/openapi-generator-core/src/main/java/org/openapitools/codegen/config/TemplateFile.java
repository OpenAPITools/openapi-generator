package org.openapitools.codegen.config;

import org.openapitools.codegen.api.TemplateDefinition;

public class TemplateFile extends TemplateDefinition {
    private TemplateFileType templateType;

    public TemplateFile(String templateFile, String destinationFilename) {
        super(templateFile, destinationFilename);
    }

    public TemplateFile(String templateFile, String folder, String destinationFilename) {
        super(templateFile, folder, destinationFilename);
    }

    public TemplateFile(String templateFile, String folder, String destinationFilename, TemplateFileType templateType) {
        super(templateFile, folder, destinationFilename);
        this.templateType = templateType;
    }

    public TemplateFile(){
        this("", "", "", TemplateFileType.SupportingFiles);
    }

    public TemplateFileType getTemplateType() {
        return templateType;
    }

    public void setTemplateType(TemplateFileType templateType) {
        this.templateType = templateType;
    }
}
