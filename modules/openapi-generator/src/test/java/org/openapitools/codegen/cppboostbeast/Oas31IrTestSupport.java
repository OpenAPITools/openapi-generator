/*
 * Copyright 2026 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.openapitools.codegen.cppboostbeast;

import org.testng.Assert;

abstract class Oas31IrTestSupport {
    protected static final String VALIDATION_NAMESPACE =
            "org::openapitools::client::model::detail::schema_validation";

    protected static String schemaNodeBlock(String ir, String schemaPath) {
        String marker = "n.schemaPath = \"" + schemaPath + "\";";
        int pathIndex = ir.indexOf(marker);
        Assert.assertTrue(pathIndex >= 0, "missing generated schema row for " + schemaPath);
        int start = ir.lastIndexOf("{ // node ", pathIndex);
        int end = ir.indexOf("reg.nodes.push_back(std::move(n));", pathIndex);
        Assert.assertTrue(start >= 0 && end > pathIndex,
                "malformed generated schema row for " + schemaPath);
        return ir.substring(start, end);
    }

    protected static String schemaNodeBlockForSourceName(String ir, String sourceName) {
        String marker = "n.sourceName = \"" + sourceName + "\";";
        int sourceIndex = ir.indexOf(marker);
        Assert.assertTrue(sourceIndex >= 0, "missing generated schema row for " + sourceName);
        int start = ir.lastIndexOf("{ // node ", sourceIndex);
        int end = ir.indexOf("reg.nodes.push_back(std::move(n));", sourceIndex);
        Assert.assertTrue(start >= 0 && end > sourceIndex,
                "malformed generated schema row for " + sourceName);
        return ir.substring(start, end);
    }
}
