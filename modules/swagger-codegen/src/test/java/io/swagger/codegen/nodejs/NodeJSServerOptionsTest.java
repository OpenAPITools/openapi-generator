package io.swagger.codegen.nodejs;

import io.swagger.codegen.AbstractOptionsTest;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.languages.NodeJSServerCodegen;
import io.swagger.codegen.options.NodeJSServerOptionsProvider;

import mockit.Expectations;
import mockit.Tested;
import org.testng.annotations.Test;

import static org.testng.Assert.assertEquals;

public class NodeJSServerOptionsTest extends AbstractOptionsTest {

    @Tested
    private NodeJSServerCodegen clientCodegen;

    public NodeJSServerOptionsTest() {
        super(new NodeJSServerOptionsProvider());
    }

    @Override
    protected CodegenConfig getCodegenConfig() {
        return clientCodegen;
    }

    @SuppressWarnings("unused")
    @Override
    protected void setExpectations() {
        new Expectations(clientCodegen) {{
            clientCodegen.setSortParamsByRequiredFlag(Boolean.valueOf(NodeJSServerOptionsProvider.SORT_PARAMS_VALUE));
            clientCodegen.setGoogleCloudFunctions(Boolean.valueOf(NodeJSServerOptionsProvider.GOOGLE_CLOUD_FUNCTIONS));
            clientCodegen.setExportedName(NodeJSServerOptionsProvider.EXPORTED_NAME);
            times = 1;
        }};
    }


    @Test
    public void testCleanTitle() {
        String dirtyTitle = "safe-title";
        String clean = dirtyTitle.replaceAll("[^a-zA-Z0-9]", "-")
                .replaceAll("^[-]*", "")
                .replaceAll("[-]*$", "")
                .replaceAll("[-]{2,}", "-");

        assertEquals(clean, "safe-title");
    }

    @Test
    public void testDirtyTitleCleansing() {
        String dirtyTitle = "_it's-$ooo//////////---_//dirty!!!!";
        String clean = dirtyTitle.replaceAll("[^a-zA-Z0-9]", "-")
                .replaceAll("^[-]*", "")
                .replaceAll("[-]*$", "")
                .replaceAll("[-]{2,}", "-");

        assertEquals(clean, "it-s-ooo-dirty");
    }
}
