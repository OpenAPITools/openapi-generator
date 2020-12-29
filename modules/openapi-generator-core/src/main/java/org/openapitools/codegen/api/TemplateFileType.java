package org.openapitools.codegen.api;

import java.util.StringJoiner;

/**
 * Represents the type of a generator's templating files. These types of template files may be processed differently
 * (multiple times per definition, or conditionally based on user or generator configuration).
 */
public enum TemplateFileType {
    API(Constants.APIS),
    Model(Constants.MODELS),
    APIDocs(Constants.API_DOCS),
    ModelDocs(Constants.MODEL_DOCS),
    APITests(Constants.API_TESTS),
    ModelTests(Constants.MODEL_TESTS),
    SupportingFiles(Constants.SUPPORTING_FILES);

    private final String templateType;

    TemplateFileType(String templateType) { this.templateType = templateType; }

    /**
     * Returns the value for this template file type
     *
     * @return The template type of this enum.
     */
    public String value() { return this.templateType; }

    /** {@inheritDoc} */
    @Override
    public String toString() {
        return new StringJoiner(", ", TemplateFileType.class.getSimpleName() + "[", "]")
                .add("templateType='" + templateType + "'")
                .toString();
    }

    /**
     * Obtains the {@link TemplateFileType} for an input string.
     *
     * @param templateType a {@link java.lang.String} object.
     * @return a {@link TemplateFileType} object.
     */
    public static TemplateFileType forTemplateType(String templateType) {
        for (TemplateFileType value : values()) {
            if (value.templateType.equals(templateType)) {
                return value;
            }
        }
        throw new IllegalArgumentException("templateType not found in the available values.");
    }

    public static class Constants {
        public static final String APIS = "apis";
        public static final String MODELS = "models";
        public static final String SUPPORTING_FILES = "supportingFiles";
        public static final String MODEL_TESTS = "modelTests";
        public static final String MODEL_DOCS = "modelDocs";
        public static final String API_TESTS = "apiTests";
        public static final String API_DOCS = "apiDocs";
    }
}
