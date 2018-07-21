/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 * Copyright 2018 SmartBear Software
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

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

    @Test
    public void testSanitizeUrl() throws Exception {
        String[][] testData = {
            { "https://abc1.xyz:9999/some/path", "https://abc1.xyz:9999/some/path" },
            { "HTTPS://abc2.xyz:9999/some/path", "https://abc2.xyz:9999/some/path" },
            { "http://abc3.xyz:9999/some/path", "http://abc3.xyz:9999/some/path" },
            { "HTTP://abc4.xyz:9999/some/path", "http://abc4.xyz:9999/some/path" },
            { "//abc5.xyz:9999/some/path", "http://abc5.xyz:9999/some/path" },
            { "abc6.xyz:9999/some/path", "http://abc6.xyz:9999/some/path" },
            { "localhost:9000/api", "http://localhost:9000/api" },
            { "/some/path", "http://localhost/some/path" } };

        for (String[] t:testData) {
            OpenAPI openAPI = new OpenAPI();
            openAPI.addServersItem(new Server().url(t[0]));

            Assert.assertEquals(URLPathUtils.getServerURL(openAPI).toString(), t[1]);
        }
    }
}
