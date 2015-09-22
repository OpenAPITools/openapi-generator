package io.swagger.codegen;

/**
 * A class for storing constants that are used throughout the project.
 */
public class CodegenConstants {
    public static final String API_PACKAGE = "apiPackage";
    public static final String API_PACKAGE_DESC = "package for generated api classes";

    public static final String MODEL_PACKAGE = "modelPackage";
    public static final String MODEL_PACKAGE_DESC = "package for generated models";

    public static final String TEMPLATE_DIR = "templateDir";


    public static final String INVOKER_PACKAGE = "invokerPackage";
    public static final String INVOKER_PACKAGE_DESC = "root package for generated code";

    public static final String GROUP_ID = "groupId";
    public static final String GROUP_ID_DESC = "groupId in generated pom.xml";

    public static final String ARTIFACT_ID = "artifactId";
    public static final String ARTIFACT_ID_DESC = "artifactId in generated pom.xml";

    public static final String ARTIFACT_VERSION = "artifactVersion";
    public static final String ARTIFACT_VERSION_DESC = "artifact version in generated pom.xml";

    public static final String SOURCE_FOLDER = "sourceFolder";
    public static final String SOURCE_FOLDER_DESC = "source folder for generated code";

    public static final String LOCAL_VARIABLE_PREFIX = "localVariablePrefix";
    public static final String LOCAL_VARIABLE_PREFIX_DESC = "prefix for generated code members and local variables";

    public static final String SERIALIZABLE_MODEL = "serializableModel";
    public static final String SERIALIZABLE_MODEL_DESC = "boolean - toggle \"implements Serializable\" for generated models";

    public static final String LIBRARY = "library";
    public static final String LIBRARY_DESC = "library template (sub-template)";

    public static final String SORT_PARAMS_BY_REQUIRED_FLAG = "sortParamsByRequiredFlag";
    public static final String SORT_PARAMS_BY_REQUIRED_FLAG_DESC = "Sort method arguments to place required parameters before optional parameters. Default: true";

}
