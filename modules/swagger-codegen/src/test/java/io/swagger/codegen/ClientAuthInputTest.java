package io.swagger.codegen;

import io.swagger.models.auth.AuthorizationValue;

import org.testng.Assert;
import org.testng.annotations.Test;

import java.util.List;

@SuppressWarnings("static-method")
public class ClientAuthInputTest {

    @Test(description = "read a file upload param from a 2.0 spec")
    public void clientAuthInputTest() {
        final ClientOptInput input = new ClientOptInput();

        final String header = "api_key:special-key,api_key:hello,X-AUTHORIZATION:0e6c11d79a,Authorization:Basic 1jz0";
        input.setAuth(header);
        final List<AuthorizationValue> authValues = input.getAuthorizationValues();
        Assert.assertEquals(authValues.size(), 4);

        final AuthorizationValue a1 = authValues.get(0);
        Assert.assertEquals(a1.getKeyName(), "api_key");
        Assert.assertEquals(a1.getValue(), "special-key");
        Assert.assertEquals(a1.getType(), "header");

        final AuthorizationValue a2 = authValues.get(1);
        Assert.assertEquals(a2.getKeyName(), "api_key");
        Assert.assertEquals(a2.getValue(), "hello");
        Assert.assertEquals(a2.getType(), "header");

        final AuthorizationValue a3 = authValues.get(2);
        Assert.assertEquals(a3.getKeyName(), "X-AUTHORIZATION");
        Assert.assertEquals(a3.getValue(), "0e6c11d79a");
        Assert.assertEquals(a3.getType(), "header");

        final AuthorizationValue a4 = authValues.get(3);
        Assert.assertEquals(a4.getKeyName(), "Authorization");
        Assert.assertEquals(a4.getValue(), "Basic 1jz0");
        Assert.assertEquals(a4.getType(), "header");
    }
}
