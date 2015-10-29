package io.swagger.codegen.objc;

import io.swagger.codegen.AbstractOptionsTest;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.languages.ObjcClientCodegen;

import com.google.common.collect.ImmutableMap;
import mockit.Expectations;
import mockit.Tested;

import java.util.Map;

public class ObjcClientOptionsTest extends AbstractOptionsTest {
    private static final String CLASS_PREFIX_VALUE = "SWGObjc";
    private static final String POD_NAME_VALUE = "SwaggerClientObjc";
    private static final String POD_VERSION_VALUE = "1.0.0-SNAPSHOT";
    private static final String AUTHOR_NAME_VALUE = "SwaggerObjc";
    private static final String AUTHOR_EMAIL_VALUE = "objc@swagger.io";
    private static final String GIT_REPO_URL_VALUE = "https://github.com/swagger-api/swagger-codegen";
    private static final String LICENSE_VALUE = "MIT";

    @Tested
    private ObjcClientCodegen clientCodegen;

    @Override
    protected CodegenConfig getCodegenConfig() {
        return clientCodegen;
    }

    @Override
    protected void setExpectations() {
        new Expectations(clientCodegen) {{
            clientCodegen.setClassPrefix(CLASS_PREFIX_VALUE);
            times = 1;
            clientCodegen.setPodName(POD_NAME_VALUE);
            times = 1;
            clientCodegen.setPodVersion(POD_VERSION_VALUE);
            times = 1;
            clientCodegen.setAuthorName(AUTHOR_NAME_VALUE);
            times = 1;
            clientCodegen.setAuthorEmail(AUTHOR_EMAIL_VALUE);
            times = 1;
            clientCodegen.setGitRepoURL(GIT_REPO_URL_VALUE);
            times = 1;
            clientCodegen.setLicense(LICENSE_VALUE);
            times = 1;
        }};
    }

    @Override
    protected Map<String, String> getAvaliableOptions() {
        ImmutableMap.Builder<String, String> builder = new ImmutableMap.Builder<String, String>();
        return builder.put(ObjcClientCodegen.CLASS_PREFIX, CLASS_PREFIX_VALUE)
                .put(ObjcClientCodegen.POD_NAME, POD_NAME_VALUE)
                .put(CodegenConstants.POD_VERSION, POD_VERSION_VALUE)
                .put(ObjcClientCodegen.AUTHOR_NAME, AUTHOR_NAME_VALUE)
                .put(ObjcClientCodegen.AUTHOR_EMAIL, AUTHOR_EMAIL_VALUE)
                .put(ObjcClientCodegen.GIT_REPO_URL, GIT_REPO_URL_VALUE)
                .put(ObjcClientCodegen.LICENSE, LICENSE_VALUE)
                .build();
    }
}
