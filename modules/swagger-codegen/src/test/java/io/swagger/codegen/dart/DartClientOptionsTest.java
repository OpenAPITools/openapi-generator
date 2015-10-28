package io.swagger.codegen.dart;

import io.swagger.codegen.AbstractOptionsTest;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.languages.DartClientCodegen;

import com.google.common.collect.ImmutableMap;
import mockit.Expectations;
import mockit.Tested;

import java.util.Map;

public class DartClientOptionsTest extends AbstractOptionsTest {
    protected static final String MODEL_PACKAGE_VALUE = "packagedart";
    protected static final String API_PACKAGE_VALUE = "apiPackageDart";
    protected static final String SORT_PARAMS_VALUE = "false";
    protected static final String BROWSER_CLIENT_VALUE = "true";
    protected static final String PUB_NAME_VALUE = "swagger";
    protected static final String PUB_VERSION_VALUE = "1.0.0-SNAPSHOT";
    protected static final String PUB_DESCRIPTION_VALUE = "Swagger API client dart";
    protected static final String SOURCE_FOLDER_VALUE = "src";

    @Tested
    private DartClientCodegen clientCodegen;

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
            clientCodegen.setBrowserClient(Boolean.valueOf(BROWSER_CLIENT_VALUE));
            times = 1;
            clientCodegen.setPubName(PUB_NAME_VALUE);
            times = 1;
            clientCodegen.setPubVersion(PUB_VERSION_VALUE);
            times = 1;
            clientCodegen.setPubDescription(PUB_DESCRIPTION_VALUE);
            times = 1;
            clientCodegen.setSourceFolder(SOURCE_FOLDER_VALUE);
            times = 1;
        }};
    }

    @Override
    protected Map<String, String> getAvaliableOptions() {
        ImmutableMap.Builder<String, String> builder = new ImmutableMap.Builder<String, String>();
        return builder.put(CodegenConstants.MODEL_PACKAGE, MODEL_PACKAGE_VALUE)
                .put(CodegenConstants.API_PACKAGE, API_PACKAGE_VALUE)
                .put(CodegenConstants.SORT_PARAMS_BY_REQUIRED_FLAG, SORT_PARAMS_VALUE)
                .put(DartClientCodegen.BROWSER_CLIENT, BROWSER_CLIENT_VALUE)
                .put(DartClientCodegen.PUB_NAME, PUB_NAME_VALUE)
                .put(DartClientCodegen.PUB_VERSION, PUB_VERSION_VALUE)
                .put(DartClientCodegen.PUB_DESCRIPTION, PUB_DESCRIPTION_VALUE)
                .put(CodegenConstants.SOURCE_FOLDER, SOURCE_FOLDER_VALUE)
                .build();
    }
}

