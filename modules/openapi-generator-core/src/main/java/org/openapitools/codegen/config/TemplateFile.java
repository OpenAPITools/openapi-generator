package org.openapitools.codegen.config;

import org.openapitools.codegen.api.TemplateDefinition;

/**
 * A TemplateFile represents the Template and targeted File along with associate type.
 */
public class TemplateFile extends TemplateDefinition {
    private TemplateFileType templateType;

    /**
     * <p>Constructor for TemplateFile.</p>
     *
     * @param templateFile a template path relative to user template or embedded template.
     * @param destinationFilename a target output location for the file, relative to the output directory.
     */
    public TemplateFile(String templateFile, String destinationFilename) {
        super(templateFile, destinationFilename);
    }

    /**
     * <p>Constructor for TemplateFile.</p>
     *
     * @param templateFile a template path relative to user template or embedded template.
     * @param folder a folder in the target output directory in which to place the target file.
     * @param destinationFilename a target output location for the file, relative to the output directory.
     */
    public TemplateFile(String templateFile, String folder, String destinationFilename) {
        super(templateFile, folder, destinationFilename);
    }

    /**
     * <p>Constructor for TemplateFile.</p>
     *
     * @param templateFile a template path relative to user template or embedded template.
     * @param folder a folder in the target output directory in which to place the target file.
     * @param destinationFilename a target output location for the file, relative to the output directory.
     * @param templateType an enum which defines the type of this templated file
     */
    public TemplateFile(String templateFile, String folder, String destinationFilename, TemplateFileType templateType) {
        super(templateFile, folder, destinationFilename);
        this.templateType = templateType;
    }

    /**
     * <p>Constructor for TemplateFile.</p>
     */
    public TemplateFile(){
        this("", "", "", TemplateFileType.SupportingFiles);
    }

    /**
     * Gets the type of template
     *
     * @return a {@link org.openapitools.codegen.config.TemplateFileType} enum which defines the type of this template.
     */
    public TemplateFileType getTemplateType() {
        return templateType;
    }

    /**
     * Sets the type of template
     *
     * @param templateType a {@link org.openapitools.codegen.config.TemplateFileType} enum which defines the type of this template
     */
    public void setTemplateType(TemplateFileType templateType) {
        this.templateType = templateType;
    }
}
