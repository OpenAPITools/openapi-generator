package org.openapitools.codegen.csharp;

import org.openapitools.codegen.AbstractOptionsTest;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.languages.CSharpClientCodegen;
import org.openapitools.codegen.options.CSharpClientOptionsProvider;

import mockit.Expectations;
import mockit.Tested;

public class CSharpClientOptionsTest extends AbstractOptionsTest {

    @Tested
    private CSharpClientCodegen clientCodegen;

    public CSharpClientOptionsTest() {
        super(new CSharpClientOptionsProvider());
    }

    @Override
    protected CodegenConfig getCodegenConfig() {
        return clientCodegen;
    }

    @SuppressWarnings("unused")
    @Override
    protected void setExpectations() {
        new Expectations(clientCodegen) {{
            clientCodegen.setPackageName(CSharpClientOptionsProvider.PACKAGE_NAME_VALUE);
            times = 1;
            clientCodegen.setOptionalMethodArgumentFlag(true);
            times = 1;
            clientCodegen.setPackageVersion(CSharpClientOptionsProvider.PACKAGE_VERSION_VALUE);
            times = 1;
            clientCodegen.setOptionalAssemblyInfoFlag(true);
            times = 1;
            clientCodegen.setSourceFolder(CSharpClientOptionsProvider.SOURCE_FOLDER_VALUE);
            times = 1;
            clientCodegen.useDateTimeOffset(true);
            times = 1;
            clientCodegen.setOptionalProjectFileFlag(true);
            times = 1;
            clientCodegen.setPackageGuid(CSharpClientOptionsProvider.PACKAGE_GUID_VALUE);
            times = 1;
            clientCodegen.setUseCollection(false);
            times = 1;
            clientCodegen.setReturnICollection(false);
            times = 1;
            clientCodegen.setOptionalEmitDefaultValue(true);
            times = 1;
            clientCodegen.setGeneratePropertyChanged(true);
            times = 1;
            clientCodegen.setNonPublicApi(true);
            times = 1;
            clientCodegen.setInterfacePrefix("X");
            times = 1;
            clientCodegen.setAllowUnicodeIdentifiers(Boolean.valueOf(CSharpClientOptionsProvider.ALLOW_UNICODE_IDENTIFIERS_VALUE));
            times = 1;
            clientCodegen.setNetCoreProjectFileFlag(false);
            times = 1;
            clientCodegen.setModelPropertyNaming(CSharpClientOptionsProvider.MODEL_PROPERTY_NAMING);
            times = 1;
            clientCodegen.setValidatable(true);
            times = 1;
        }};
    }
}
