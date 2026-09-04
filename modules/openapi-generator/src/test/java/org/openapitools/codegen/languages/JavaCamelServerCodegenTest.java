package org.openapitools.codegen.languages;

import org.openapitools.codegen.VendorExtension;
import org.testng.annotations.Test;

import static org.testng.Assert.assertFalse;
import static org.testng.Assert.assertTrue;

public class JavaCamelServerCodegenTest {

    @Test
    public void doesNotAdvertiseRequestBodyExtraAnnotation() {
        // The Camel REST DSL templates do not render a request-body parameter annotation, so this
        // generator must not advertise the extension it inherits from SpringCodegen.
        assertFalse(new JavaCamelServerCodegen().getSupportedVendorExtensions()
                .contains(VendorExtension.X_REQUEST_BODY_EXTRA_ANNOTATION));
        // SpringCodegen itself still supports it.
        assertTrue(new SpringCodegen().getSupportedVendorExtensions()
                .contains(VendorExtension.X_REQUEST_BODY_EXTRA_ANNOTATION));
    }
}
