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

import org.openapitools.codegen.TestUtils;
import org.testng.Assert;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.regex.Pattern;

/**
 * Shared helpers for the C++ Boost.Beast client generator test classes.
 */
public final class CppBoostBeastTestSupport {

    private CppBoostBeastTestSupport() {
    }

    /**
     * Checks basic C++ syntactic validity of a generated source file:
     * balanced preprocessor guards, no missing/duplicate #endif.
     */
    static void assertBalancedPreprocessorGuards(Path filePath) throws IOException {
        String content = Files.readString(filePath);
        long ifndefCount = content.lines()
                .filter(line -> line.trim().startsWith("#ifndef"))
                .count();
        long defineCount = content.lines()
                .filter(line -> line.trim().startsWith("#define") && !line.trim().startsWith("#define "))
                .count();
        long endifCount = content.lines()
                .filter(line -> line.trim().startsWith("#endif"))
                .count();
        long ifCount = content.lines()
                .filter(line -> line.trim().startsWith("#if ") || line.trim().startsWith("#ifdef"))
                .count();
        long elifCount = content.lines()
                .filter(line -> line.trim().startsWith("#elif"))
                .count();
        long elseCount = content.lines()
                .filter(line -> line.trim().startsWith("#else"))
                .count();
        // Each #ifndef must have a matching #endif, without duplicates
        long expectedEndif = ifndefCount + ifCount;
        Assert.assertEquals(endifCount, expectedEndif,
                "File " + filePath + " has unbalanced preprocessor guards: " +
                "#ifndef=" + ifndefCount + " #if=" + ifCount + " #endif=" + endifCount);
    }

    static String extractMethod(String generatedApiSource, String methodSignature) {
        int methodStart = generatedApiSource.indexOf(methodSignature);
        Assert.assertTrue(methodStart >= 0, "Missing generated method: " + methodSignature);
        int methodEnd = generatedApiSource.indexOf("\n}", methodStart);
        Assert.assertTrue(methodEnd > methodStart, "Missing closing brace for generated method: " + methodSignature);
        return generatedApiSource.substring(methodStart, methodEnd);
    }

    static int countOccurrences(String source, String expectedText) {
        return TestUtils.countOccurrences(source, Pattern.quote(expectedText));
    }
}