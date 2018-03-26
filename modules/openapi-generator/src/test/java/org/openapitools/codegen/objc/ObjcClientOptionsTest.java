package io.swagger.codegen.objc;

import io.swagger.codegen.AbstractOptionsTest;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.languages.ObjcClientCodegen;
import io.swagger.codegen.options.ObjcClientOptionsProvider;

import mockit.Expectations;
import mockit.Tested;

public class ObjcClientOptionsTest extends AbstractOptionsTest {

    @Tested
    private ObjcClientCodegen clientCodegen;

    public ObjcClientOptionsTest() {
        super(new ObjcClientOptionsProvider());
    }

    @Override
    protected CodegenConfig getCodegenConfig() {
        return clientCodegen;
    }

    @SuppressWarnings("unused")
    @Override
    protected void setExpectations() {
        new Expectations(clientCodegen) {{
            clientCodegen.setClassPrefix(ObjcClientOptionsProvider.CLASS_PREFIX_VALUE);
            times = 1;
            clientCodegen.setPodName(ObjcClientOptionsProvider.POD_NAME_VALUE);
            times = 1;
            clientCodegen.setPodVersion(ObjcClientOptionsProvider.POD_VERSION_VALUE);
            times = 1;
            clientCodegen.setAuthorName(ObjcClientOptionsProvider.AUTHOR_NAME_VALUE);
            times = 1;
            clientCodegen.setAuthorEmail(ObjcClientOptionsProvider.AUTHOR_EMAIL_VALUE);
            times = 1;
            clientCodegen.setGitRepoURL(ObjcClientOptionsProvider.GIT_REPO_URL_VALUE);
            times = 1;
        }};
    }
}
