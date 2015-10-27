package io.swagger.codegen.java;

import io.swagger.codegen.AbstractOptionsTest;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.languages.JavaClientCodegen;

import com.google.common.collect.ImmutableMap;
import mockit.Expectations;
import mockit.Tested;

import java.util.Map;

public class JavaClientOptionsTest extends AbstractOptionsTest {

    protected static final String ARTIFACT_ID_VALUE = "swagger-java-client-test";
    protected static final String MODEL_PACKAGE_VALUE = "package";
    protected static final String API_PACKAGE_VALUE = "apiPackage";
    protected static final String INVOKER_PACKAGE_VALUE = "io.swagger.client.test";
    protected static final String SORT_PARAMS_VALUE = "false";
    protected static final String GROUP_ID_VALUE = "io.swagger.test";
    protected static final String ARTIFACT_VERSION_VALUE = "1.0.0-SNAPSHOT";
    protected static final String SOURCE_FOLDER_VALUE = "src/main/java/test";
    protected static final String LOCAL_PREFIX_VALUE = "tst";
    protected static final String LIBRARY_VALUE = "jersey2";
    protected static final String SERIALIZABLE_MODEL_VALUE = "false";
    protected static final String FULL_JAVA_UTIL_VALUE = "true";

    @Tested
    private JavaClientCodegen clientCodegen;

    @Override
    protected CodegenConfig getCodegenConfig() {
        return clientCodegen;
    }

    @Override
    protected void setExpectations() {
        new Expectations(clientCodegen) {{
            clientCodegen.setModelPackage(MODEL_PACKAGE_VALUE);
            times = 1;
            clientCodegen.setApiPackage(API_PACKAGE_VALUE);
            times = 1;
            clientCodegen.setSortParamsByRequiredFlag(Boolean.valueOf(SORT_PARAMS_VALUE));
            times = 1;
            clientCodegen.setInvokerPackage(INVOKER_PACKAGE_VALUE);
            times = 1;
            clientCodegen.setGroupId(GROUP_ID_VALUE);
            times = 1;
            clientCodegen.setArtifactId(ARTIFACT_ID_VALUE);
            times = 1;
            clientCodegen.setArtifactVersion(ARTIFACT_VERSION_VALUE);
            times = 1;
            clientCodegen.setSourceFolder(SOURCE_FOLDER_VALUE);
            times = 1;
            clientCodegen.setLocalVariablePrefix(LOCAL_PREFIX_VALUE);
            times = 1;
            clientCodegen.setSerializableModel(Boolean.valueOf(SERIALIZABLE_MODEL_VALUE));
            times = 1;
            clientCodegen.setLibrary(LIBRARY_VALUE);
            times = 1;
            clientCodegen.setFullJavaUtil(Boolean.valueOf(FULL_JAVA_UTIL_VALUE));
            times = 1;
        }};
    }

    @Override
    protected Map<String, String> getAvaliableOptions() {
        ImmutableMap.Builder<String, String> builder = new ImmutableMap.Builder<String, String>();
        return builder.put(CodegenConstants.MODEL_PACKAGE, MODEL_PACKAGE_VALUE)
                .put(CodegenConstants.API_PACKAGE, API_PACKAGE_VALUE)
                .put(CodegenConstants.SORT_PARAMS_BY_REQUIRED_FLAG, SORT_PARAMS_VALUE)
                .put(CodegenConstants.INVOKER_PACKAGE, INVOKER_PACKAGE_VALUE)
                .put(CodegenConstants.GROUP_ID, GROUP_ID_VALUE)
                .put(CodegenConstants.ARTIFACT_ID, ARTIFACT_ID_VALUE)
                .put(CodegenConstants.ARTIFACT_VERSION, ARTIFACT_VERSION_VALUE)
                .put(CodegenConstants.SOURCE_FOLDER, SOURCE_FOLDER_VALUE)
                .put(CodegenConstants.LOCAL_VARIABLE_PREFIX, LOCAL_PREFIX_VALUE)
                .put(CodegenConstants.SERIALIZABLE_MODEL, SERIALIZABLE_MODEL_VALUE)
                .put(JavaClientCodegen.FULL_JAVA_UTIL, FULL_JAVA_UTIL_VALUE)
                .put(CodegenConstants.LIBRARY, LIBRARY_VALUE)
                .build();
    }
}
