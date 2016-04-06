package io.swagger.codegen.languages;

import org.testng.Assert;
import org.testng.annotations.Test;

public class ClojureClientCodegenTest {
    ClojureClientCodegen codegen = new ClojureClientCodegen();

    @Test
    public void testSanitizeTag() throws Exception {
        Assert.assertEquals(codegen.sanitizeTag("users-api"), "users_api");
        Assert.assertEquals(codegen.sanitizeTag("users_api"), "users_api");
        Assert.assertEquals(codegen.sanitizeTag("users api"), "users_api");
        Assert.assertEquals(codegen.sanitizeTag("users.api"), "users_api");
        Assert.assertEquals(codegen.sanitizeTag("Users Api"), "Users_Api");
        Assert.assertEquals(codegen.sanitizeTag("UsersApi"), "UsersApi");
        Assert.assertEquals(codegen.sanitizeTag("usersapi"), "usersapi");
        Assert.assertEquals(codegen.sanitizeTag("Usersapi"), "Usersapi");
    }

    @Test
    public void testToApiName() throws Exception {
        Assert.assertEquals(codegen.toApiName("users_api"), "users-api");
        Assert.assertEquals(codegen.toApiName("Users_Api"), "users-api");
        Assert.assertEquals(codegen.toApiName("UsersApi"), "users-api");
        Assert.assertEquals(codegen.toApiName("usersapi"), "usersapi");
        Assert.assertEquals(codegen.toApiName("Usersapi"), "usersapi");
    }

    @Test
    public void testToApiFilename() throws Exception {
        Assert.assertEquals(codegen.toApiFilename("users_api"), "users_api");
        Assert.assertEquals(codegen.toApiFilename("Users_Api"), "users_api");
        Assert.assertEquals(codegen.toApiFilename("UsersApi"), "users_api");
        Assert.assertEquals(codegen.toApiFilename("usersapi"), "usersapi");
        Assert.assertEquals(codegen.toApiFilename("Usersapi"), "usersapi");
    }
}
