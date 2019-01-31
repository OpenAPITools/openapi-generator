/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
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

package org.openapitools.codegen.slim;

import org.openapitools.codegen.languages.PhpSlimServerCodegen;
import org.testng.Assert;
import org.testng.annotations.Test;

public class PhpSlimServerCodegenTest {

    @Test
    public void testEncodePath() {
        final PhpSlimServerCodegen codegen = new PhpSlimServerCodegen();

        Assert.assertEquals(codegen.encodePath("/ ' \" =end -- \\r\\n \\n \\r/v2 *_/ ' \" =end -- \\r\\n \\n \\r/fake"), "/%20%27%20%22%20%3Dend%20--%20%5C%5Cr%5C%5Cn%20%5C%5Cn%20%5C%5Cr/v2%20*_/%20%27%20%22%20%3Dend%20--%20%5C%5Cr%5C%5Cn%20%5C%5Cn%20%5C%5Cr/fake");
        Assert.assertEquals(codegen.encodePath("/o\'\"briens/v2/o\'\"henry/fake"), "/o%27%22briens/v2/o%27%22henry/fake");
        Assert.assertEquals(codegen.encodePath("/comedians/Chris D\'Elia"), "/comedians/Chris%20D%27Elia");
        Assert.assertEquals(codegen.encodePath("/разработчики/Юрий Беленко"), "/%D1%80%D0%B0%D0%B7%D1%80%D0%B0%D0%B1%D0%BE%D1%82%D1%87%D0%B8%D0%BA%D0%B8/%D0%AE%D1%80%D0%B8%D0%B9%20%D0%91%D0%B5%D0%BB%D0%B5%D0%BD%D0%BA%D0%BE");
        Assert.assertEquals(codegen.encodePath("/text with multilines \\\n\\\t\\\r"), "/text%20with%20multilines%20%5C%5C%20%5C%5C%20%5C%5C");
        Assert.assertEquals(codegen.encodePath("/path with argument {value}"), "/path%20with%20argument%20{value}");

        // few examples from Slim documentation
        Assert.assertEquals(codegen.encodePath("/users[/{id}]"), "/users[/{id}]");
        Assert.assertEquals(codegen.encodePath("/news[/{year}[/{month}]]"), "/news[/{year}[/{month}]]");
        Assert.assertEquals(codegen.encodePath("/news[/{params:.*}]"), "/news[/{params:.*}]");
        Assert.assertEquals(codegen.encodePath("/users/{id:[0-9]+}"), "/users/{id:[0-9]+}");

        // from FastRoute\RouteParser\Std.php
        Assert.assertEquals(codegen.encodePath("/user/{name}[/{id:[0-9]+}]"), "/user/{name}[/{id:[0-9]+}]");
        Assert.assertEquals(codegen.encodePath("/fixedRoutePart/{varName}[/moreFixed/{varName2:\\d+}]"), "/fixedRoutePart/{varName}[/moreFixed/{varName2:\\d+}]");
    }
}
