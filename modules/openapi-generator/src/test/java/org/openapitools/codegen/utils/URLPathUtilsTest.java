package org.openapitools.codegen.utils;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.servers.Server;

import org.testng.Assert;
import org.testng.annotations.Test;

import java.net.URL;

public class URLPathUtilsTest {

    @Test
    public void testDefaultValues() throws Exception {
        OpenAPI openAPI = new OpenAPI();
        URL serverURL = URLPathUtils.getServerURL(openAPI);

        Assert.assertEquals(serverURL.getHost(), "localhost");
        Assert.assertEquals(serverURL.getPort(), -1);
        Assert.assertEquals(serverURL.getPath(), "");
        Assert.assertEquals(URLPathUtils.getScheme(serverURL, null), "http");
        Assert.assertEquals(URLPathUtils.getPort(serverURL, 8080), "8080");
        Assert.assertEquals(URLPathUtils.getPort(serverURL, "8081"), "8081");
        Assert.assertEquals(URLPathUtils.getPath(serverURL, "/abc"), "/abc");
    }

    @Test
    public void testUrl() throws Exception {
        OpenAPI openAPI = new OpenAPI();
        openAPI.addServersItem(new Server().url("https://abcdef.xyz:9999/some/path"));
        URL serverURL = URLPathUtils.getServerURL(openAPI);

        Assert.assertEquals(serverURL.getHost(), "abcdef.xyz");
        Assert.assertEquals(serverURL.getPort(), 9999);
        Assert.assertEquals(serverURL.getPath(), "/some/path");
        Assert.assertEquals(URLPathUtils.getScheme(serverURL, null), "https");
        Assert.assertEquals(URLPathUtils.getPort(serverURL, 8080), "9999");
        Assert.assertEquals(URLPathUtils.getPort(serverURL, "8081"), "9999");
        Assert.assertEquals(URLPathUtils.getPath(serverURL, "/abc"), "/some/path");
    }
}
