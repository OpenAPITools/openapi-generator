package org.openapitools.codegen.java.jaxrs;

import org.openapitools.codegen.languages.JavaJAXRSCXFCDIServerCodegen;
import org.testng.annotations.BeforeMethod;

public class JavaJAXRSCXFCDIServerCodegenTest extends JavaJaxrsBaseTest {

    @BeforeMethod
    public void beforeMethod() {
        codegen = new JavaJAXRSCXFCDIServerCodegen();
    }
}
