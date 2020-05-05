package org.openapitools.codegen;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.testng.annotations.Test;

import java.io.IOException;
import java.net.URL;
import java.util.List;

import static org.testng.AssertJUnit.*;

public class OpenIdConnectTest {

    private static final Logger LOGGER = LoggerFactory.getLogger(OpenIdConnectTest.class);

    private static final String OPENID_CONFIGURATION = "./3_0/openidconnect/keycloak-openid-configuration.json";

    private DefaultCodegen codegen = new DefaultCodegen();

    @Test
    public void testOpenIdConfiguration() throws IOException {
        URL config = Thread.currentThread().getContextClassLoader().getResource(OPENID_CONFIGURATION);
        OpenIdConnect dut = new OpenIdConnect(codegen, config.toExternalForm()).retrieve();
        CodegenSecurity cs = new CodegenSecurity();
        List<CodegenSecurity> securities = dut.getCodegenSecurities(cs);

        assertEquals(4, securities.size());

        CodegenSecurity pw = securities.get(0);
        assertTrue(pw.isPassword);
        assertFalse(pw.isImplicit);
        assertFalse(pw.isApplication);
        assertFalse(pw.isCode);
        assertEquals(9, pw.scopes.size());

        CodegenSecurity impl = securities.get(1);
        assertFalse(impl.isPassword);
        assertTrue(impl.isImplicit);
        assertFalse(impl.isApplication);
        assertFalse(impl.isCode);
        assertEquals(9, pw.scopes.size());

        CodegenSecurity appl = securities.get(2);
        assertFalse(appl.isPassword);
        assertFalse(appl.isImplicit);
        assertTrue(appl.isApplication);
        assertFalse(appl.isCode);
        assertEquals(9, pw.scopes.size());

        CodegenSecurity accessCode = securities.get(3);
        assertFalse(accessCode.isPassword);
        assertFalse(accessCode.isImplicit);
        assertFalse(accessCode.isApplication);
        assertTrue(accessCode.isCode);
        assertEquals(9, pw.scopes.size());
    }
}
