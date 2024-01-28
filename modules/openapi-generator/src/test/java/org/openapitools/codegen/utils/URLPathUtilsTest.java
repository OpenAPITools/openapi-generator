/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 * Copyright 2018 SmartBear Software
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
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
import io.swagger.v3.oas.models.servers.ServerVariable;
import io.swagger.v3.oas.models.servers.ServerVariables;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.net.URL;
import java.util.Arrays;
import java.util.HashMap;

public class URLPathUtilsTest {

    @Test
    public void testDefaultValues() {
        OpenAPI openAPI = new OpenAPI();
        URL serverURL = URLPathUtils.getServerURL(openAPI, null);

        Assert.assertEquals(serverURL.getHost(), "localhost");
        Assert.assertEquals(serverURL.getPort(), -1);
        Assert.assertEquals(serverURL.getPath(), "");
        Assert.assertEquals(URLPathUtils.getScheme(serverURL, null), "http");
        Assert.assertEquals(URLPathUtils.getPort(serverURL, 8080), "8080");
        Assert.assertEquals(URLPathUtils.getPort(serverURL, "8081"), "8081");
        Assert.assertEquals(URLPathUtils.getPath(serverURL, "/abc"), "/abc");
    }

    @Test
    public void testUrl() {
        OpenAPI openAPI = new OpenAPI();
        openAPI.addServersItem(new Server().url("https://abcdef.xyz:9999/some/path"));
        URL serverURL = URLPathUtils.getServerURL(openAPI, null);

        Assert.assertEquals(serverURL.getHost(), "abcdef.xyz");
        Assert.assertEquals(serverURL.getPort(), 9999);
        Assert.assertEquals(serverURL.getPath(), "/some/path");
        Assert.assertEquals(URLPathUtils.getScheme(serverURL, null), "https");
        Assert.assertEquals(URLPathUtils.getPort(serverURL, 8080), "9999");
        Assert.assertEquals(URLPathUtils.getPort(serverURL, "8081"), "9999");
        Assert.assertEquals(URLPathUtils.getPath(serverURL, "/abc"), "/some/path");
    }

    @Test
    public void testSanitizeUrl() {
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

            Assert.assertEquals(URLPathUtils.getServerURL(openAPI, null).toString(), t[1]);
        }
    }

    @Test
    public void testGetServerURLWithVariables() {
        Server s1 = new Server().url("http://localhost:{port}/").variables(new ServerVariables().addServerVariable("port", new ServerVariable()._default("8080").description("the server port")));
        Assert.assertEquals(URLPathUtils.getServerURL(s1, null).toString(), "http://localhost:8080/");

        Server s2 = new Server().url("http://{version}.test.me/{version}").variables(new ServerVariables().addServerVariable("version", new ServerVariable()._default("v1")));
        Assert.assertEquals(URLPathUtils.getServerURL(s2, null).toString(), "http://v1.test.me/v1");

        Server s3 = new Server().url("http://localhost:{port}/{version}").variables(
                    new ServerVariables().addServerVariable("version", new ServerVariable()._default("v4"))
                        .addServerVariable("port", new ServerVariable()._default("8080"))
                        .addServerVariable("other", new ServerVariable()._default("something"))
                );
        Assert.assertEquals(URLPathUtils.getServerURL(s3, null).toString(), "http://localhost:8080/v4");

        Server s4 = new Server().url("http://91.161.147.64/{targetEnv}").variables(new ServerVariables().addServerVariable("targetEnv", new ServerVariable().description("target environment")._enum(Arrays.asList("dev", "int", "prd"))._default("prd")));
        Assert.assertEquals(URLPathUtils.getServerURL(s4, null).toString(), "http://91.161.147.64/prd");

        Server s5 = new Server().url("https://api.stats.com/{country1}").variables(new ServerVariables().addServerVariable("country1", new ServerVariable()._enum(Arrays.asList("france", "germany", "italy"))));
        Assert.assertEquals(URLPathUtils.getServerURL(s5, null).toString(), "https://api.stats.com/france");

        Server s6 = new Server().url("https://api.example.com/{wrong}");
        Assert.assertEquals(URLPathUtils.getServerURL(s6, null).toString(), "https://api.example.com/");

        Server s7 = new Server().url("https://api.example.com/{wrong}").variables(new ServerVariables());
        Assert.assertEquals(URLPathUtils.getServerURL(s7, null).toString(), "https://api.example.com/");

        Server s8 = new Server().url("https://api.example.com/{wrong}").variables(new ServerVariables().addServerVariable("other", new ServerVariable()._default("something")));
        Assert.assertEquals(URLPathUtils.getServerURL(s8, null).toString(), "https://api.example.com/");

        Server s9 = new Server().url("https://{user}.example.com/{version}").variables(
                    new ServerVariables().addServerVariable("version", new ServerVariable()._default("v1"))
                        .addServerVariable("user", new ServerVariable()._default("{user}")));
        Assert.assertEquals(URLPathUtils.getServerURL(s9, null).toString(), "https://{user}.example.com/v1");
    }

    private ServerVariables serverVariables(String... entries) {
        ServerVariables variables = new ServerVariables();
        for (int i = 0; i < entries.length; i+=2) {
            String key = entries[i];
            String value = "";
            if (i+1 < entries.length) {
                value = entries[i+1];
            }
            variables.addServerVariable(key, new ServerVariable()._default(value).description("variable for: " + key));
        }
        return variables;
    }

    @Test
    public void testGetServerURLWithVariablesAndUserOverrides() {
        Server s1 = new Server().url("http://localhost:{port}/").variables(
                serverVariables("port", "8080")
        );
        Assert.assertEquals(URLPathUtils.getServerURL(s1, new HashMap<String, String>() {{ put("port", "1234"); }}).toString(), "http://localhost:1234/");

        Server s2 = new Server().url("http://{version}.test.me/{version}").variables(
                serverVariables("version", "v1")
        );
        Assert.assertEquals(URLPathUtils.getServerURL(s2, new HashMap<String, String>() {{ put("version", "v2" ); }}).toString(), "http://v2.test.me/v2");

        Server s3 = new Server().url("http://localhost:{port}/{version}").variables(
                serverVariables(
                        "version", "v4",
                        "port", "8080",
                        "other", "something"
                )
        );
        Assert.assertEquals(URLPathUtils.getServerURL(s3, new HashMap<String, String>() {{ put("port", "5678"); }}).toString(), "http://localhost:5678/v4");

        Server s4 = new Server().url("http://91.161.147.64/{targetEnv}").variables(
                new ServerVariables().addServerVariable("targetEnv", new ServerVariable().description("target environment")._enum(Arrays.asList("dev", "int", "prd"))._default("prd")));
        Assert.assertEquals(URLPathUtils.getServerURL(s4, new HashMap<String, String>() {{ put("targetEnv", "int" ); }}).toString(), "http://91.161.147.64/int");

        Server s5 = new Server().url("https://api.stats.com/{country1}").variables(
                new ServerVariables().addServerVariable("country1", new ServerVariable()._enum(Arrays.asList("france", "germany", "italy")))
        );
        Assert.assertEquals(URLPathUtils.getServerURL(s5, new HashMap<String, String>() {{ put("country1", "italy" ); }}).toString(), "https://api.stats.com/italy");

        Server s6 = new Server().url("https://api.example.com/{wrong}");
        Assert.assertEquals(URLPathUtils.getServerURL(s6, new HashMap<String, String>() {{ put("port", "8080" ); }}).toString(), "https://api.example.com/");

        Server s7 = new Server().url("https://api.example.com/{wrong}").variables(new ServerVariables());
        Assert.assertEquals(URLPathUtils.getServerURL(s7, new HashMap<String, String>() {{ put("", "8080" ); }}).toString(), "https://api.example.com/");

        Server s8 = new Server().url("https://api.example.com/{wrong}").variables(
                serverVariables("other", "something")
        );
        Assert.assertEquals(URLPathUtils.getServerURL(s8, new HashMap<String, String>() {{ put("something", "other" ); }}).toString(), "https://api.example.com/");

        Server s9 = new Server().url("https://{user}.example.com/{version}").variables(
                serverVariables(
                        "version", "v1",
                        "user", "{user}"
                )
        );
        Assert.assertEquals(URLPathUtils.getServerURL(s9, new HashMap<String, String>() {{
            put("version", "v2" );
            put("user", "jim");
        }}).toString(), "https://jim.example.com/v2");
    }

    @Test
    public void useDefaultUrlWhenServerUrlIsNull() {
        Server server = new Server().url(null);

        URL serverURL = URLPathUtils.getServerURL(server, null);
        Assert.assertEquals(serverURL.toString(), "http://localhost");
    }
}
