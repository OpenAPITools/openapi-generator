package io.swagger.codegen.javascript;

import io.swagger.codegen.AbstractOptionsTest;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.options.JavaScriptOptionsProvider;
import io.swagger.codegen.languages.JavascriptClientCodegen;
import io.swagger.codegen.options.OptionsProvider;

import mockit.Expectations;
import mockit.Tested;

public class JavaScriptClientOptionsTest extends AbstractOptionsTest {
    @Tested
    private JavascriptClientCodegen clientCodegen;

    public JavaScriptClientOptionsTest() {
        super(new JavaScriptOptionsProvider());
    }

    protected JavaScriptClientOptionsTest(OptionsProvider optionsProvider) {
        super(optionsProvider);
    }

    @Override
    protected CodegenConfig getCodegenConfig() {
        return clientCodegen;
    }

    @Override
    protected void setExpectations() {
        // Commented generic options not yet supported by JavaScript codegen.
        new Expectations(clientCodegen) {{
            clientCodegen.setInvokerPackage(JavaScriptOptionsProvider.INVOKER_PACKAGE_VALUE);
            times = 1;
            clientCodegen.setModelPackage(JavaScriptOptionsProvider.MODEL_PACKAGE_VALUE);
            times = 1;
            clientCodegen.setApiPackage(JavaScriptOptionsProvider.API_PACKAGE_VALUE);
            times = 1;
            clientCodegen.setSortParamsByRequiredFlag(Boolean.valueOf(JavaScriptOptionsProvider.SORT_PARAMS_VALUE));
            times = 1;
//            clientCodegen.setInvokerPackage(JavaScriptOptionsProvider.INVOKER_PACKAGE_VALUE);
//            times = 1;
//            clientCodegen.setGroupId(JavaScriptOptionsProvider.GROUP_ID_VALUE);
//            times = 1;
//            clientCodegen.setArtifactId(JavaScriptOptionsProvider.ARTIFACT_ID_VALUE);
//            times = 1;
//            clientCodegen.setArtifactVersion(JavaScriptOptionsProvider.ARTIFACT_VERSION_VALUE);
//            times = 1;
            clientCodegen.setSourceFolder(JavaScriptOptionsProvider.SOURCE_FOLDER_VALUE);
            times = 1;
            clientCodegen.setLocalVariablePrefix(JavaScriptOptionsProvider.LOCAL_PREFIX_VALUE);
            times = 1;
            clientCodegen.setProjectName(JavaScriptOptionsProvider.PROJECT_NAME_VALUE);
            times = 1;
            clientCodegen.setModuleName(JavaScriptOptionsProvider.MODULE_NAME_VALUE);
            times = 1;
            clientCodegen.setProjectDescription(JavaScriptOptionsProvider.PROJECT_DESCRIPTION_VALUE);
            times = 1;
            clientCodegen.setProjectVersion(JavaScriptOptionsProvider.PROJECT_VERSION_VALUE);
            times = 1;
            clientCodegen.setProjectLicenseName(JavaScriptOptionsProvider.PROJECT_LICENSE_NAME_VALUE);
            times = 1;
            clientCodegen.setUsePromises(Boolean.valueOf(JavaScriptOptionsProvider.USE_PROMISES_VALUE));
            times = 1;
            clientCodegen.setUseInheritance(Boolean.valueOf(JavaScriptOptionsProvider.USE_INHERITANCE_VALUE));
            times = 1;
            clientCodegen.setEmitModelMethods(Boolean.valueOf(JavaScriptOptionsProvider.EMIT_MODEL_METHODS_VALUE));
            times = 1;
            clientCodegen.setEmitJSDoc(Boolean.valueOf(JavaScriptOptionsProvider.EMIT_JS_DOC_VALUE));
            times = 1;
            clientCodegen.setUseES6(Boolean.valueOf(JavaScriptOptionsProvider.USE_ES6_VALUE));
            times = 1;
        }};
    }
}
