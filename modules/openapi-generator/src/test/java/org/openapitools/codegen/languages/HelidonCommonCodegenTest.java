/*
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

import java.util.List;

import org.junit.Assert;
import org.testng.annotations.Test;

// This test class is in this package, not org.openapitools.codegen.java.helidon, so it can refer to elements of
// JavaHelidonCommonCodegen without making them public; package-private is sufficient and we don't want to expose those methods
// more broadly.
class HelidonCommonCodegenTest {

    @Test
    void checkMajorVersionMatch() {
        Assert.assertEquals("1.2.3",
                            JavaHelidonCommonCodegen.VersionUtil.instance().chooseVersion("1",
                                                                                          List.of("3.2.1",
                                                                                                  "3.2.0",
                                                                                                  "2.0.4",
                                                                                                  "1.2.3",
                                                                                                  "1.2.2",
                                                                                                  "1.1.0")));
    }

    @Test
    void checkExactMatch() {
        Assert.assertEquals("1.2.2",
                            JavaHelidonCommonCodegen.VersionUtil.instance().chooseVersion("1.2.2",
                                                                                          List.of("3.2.1",
                                                                                                  "3.2.0",
                                                                                                  "2.0.4",
                                                                                                  "1.2.3",
                                                                                                  "1.2.2",
                                                                                                  "1.1.0")));
    }
}
