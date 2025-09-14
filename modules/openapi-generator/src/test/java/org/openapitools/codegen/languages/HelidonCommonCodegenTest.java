/*
 * Copyright 2024 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 * Copyright (c) 2024 Oracle and/or its affiliates.
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
package org.openapitools.codegen.languages;

import org.testng.annotations.Test;

import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;

// This test class is in this package, not org.openapitools.codegen.java.helidon, so it can refer to elements of
// JavaHelidonCommonCodegen without making them public; package-private is sufficient and we don't want to expose those methods
// more broadly.
public class HelidonCommonCodegenTest {

    JavaHelidonCommonCodegen.VersionUtil test = JavaHelidonCommonCodegen.VersionUtil.instance();

    @Test
    void checkMajorVersionMatch() {
        assertThat(test.chooseVersion("1", List.of("3.2.1", "3.2.0", "2.0.4", "1.2.3", "1.2.2", "1.1.0")))
                .isEqualTo("1.2.3");
    }

    @Test
    void checkExactMatch() {
        assertThat(test.chooseVersion("1.2.2", List.of("3.2.1", "3.2.0", "2.0.4", "1.2.3", "1.2.2", "1.1.0")))
                .isEqualTo("1.2.2");
    }

    @Test
    void testVersionNotInDefaultListWithNoNetwork() {
        // Simulate a network failure so the full list of valid versions is inaccessible and the user selects a version
        // that is not in the cached values or the hard-coded list.
        assertThat(test.chooseVersion("4.0.8", List.of("1.2.3", "2.5.6", "3.2.7", "4.0.9")))
                .isEqualTo("4.0.8");
    }


    @Test
    void checkUseOfUnpublishedRelease() {
        assertThat(test.chooseVersionBestMatchOrSelf("4.0.11-SNAPSHOT",
                List.of("4.0.10", "3.2.1", "3.2.0", "2.0.4", "1.2.3", "1.2.2", "1.1.0")))
                .isEqualTo("4.0.11-SNAPSHOT");
    }

    @Test
    void checkCommonPathWithPathParams() {
        String[] paths = List.of("/users/{userId}/profile",
                        "/users/{userId}/problems",
                        "/users/{userEmail}",
                        "/users/{username}")
                .toArray(new String[0]);

        String commonPrefix = JavaHelidonCommonCodegen.commonPathPrefix(paths);
        assertThat(commonPrefix).isEqualTo("/users");
    }

    @Test
    void checkCommonPathWithMultipleCommonLevels() {
        String[] paths = List.of("/users/a/x",
                        "/users/a/y",
                        "/users/a/z")
                .toArray(new String[0]);

        String commonPrefix = JavaHelidonCommonCodegen.commonPathPrefix(paths);
        assertThat(commonPrefix).isEqualTo("/users/a");
    }

    @Test
    void checkNoCommonSegments() {
        String[] paths = List.of("/a/x",
                        "/b/y",
                        "/c")
                .toArray(new String[0]);

        String commonPrefix = JavaHelidonCommonCodegen.commonPathPrefix(paths);
        assertThat(commonPrefix).isEqualTo("/");
    }

    @Test
    void checkSinglePathCommonSegments() {
        String commonPrefix = JavaHelidonCommonCodegen.commonPathPrefix(new String[0]);
        assertThat(commonPrefix).isEqualTo("/");
    }

    @Test
    void checkMixedWithPathParam() {
        String[] paths = List.of("/store/order/{order_id}",
                        "/store/inventory",
                        "/store/order/{order_id}",
                        "/store/order")
                .toArray(new String[0]);

        String commonPrefix = JavaHelidonCommonCodegen.commonPathPrefix(paths);
        assertThat(commonPrefix).isEqualTo("/store");
    }
}
