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

package org.openapitools.codegen.java;

import org.openapitools.codegen.CodegenParameter;
import org.openapitools.codegen.CodegenProperty;
import org.openapitools.codegen.CodegenType;
import org.openapitools.codegen.languages.AbstractJavaCodegen;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.util.Arrays;
import java.util.Collections;

public class AbstractJavaCodegenExampleValuesTest {

    private final AbstractJavaCodegen fakeJavaCodegen = new P_AbstractJavaCodegen();

    @Test
    void referencedEnumTakeFirstName() {
        final CodegenParameter p = new CodegenParameter();
        p.allowableValues = Collections.singletonMap("values", Arrays.asList("first", "second"));
        p.dataType = "WrappedEnum";

        fakeJavaCodegen.setParameterExampleValue(p);
        Assert.assertEquals(p.example, "WrappedEnum.fromValue(\"first\")");
    }

    @Test
    void inlineEnum() {
        final CodegenParameter p = new CodegenParameter();
        p.allowableValues = Collections.singletonMap("values", Arrays.asList("first", "second"));
        p.isEnum = true;
        p.dataType = "String";

        fakeJavaCodegen.setParameterExampleValue(p);
        Assert.assertEquals(p.example, "\"first\"");
    }

    @Test
    void inlineEnumArray() {
        final CodegenParameter p = new CodegenParameter();
        p.allowableValues = Collections.singletonMap("values", Arrays.asList("first", "second"));
        p.isEnum = true;
        p.isArray = true;
        p.dataType = "List<String>";
        p.items = new CodegenProperty();

        fakeJavaCodegen.setParameterExampleValue(p);
        Assert.assertEquals(p.example, "Arrays.asList()");
    }

    @Test
    void dateDefault() {
        final CodegenParameter p = new CodegenParameter();
        p.isDate = true;
        p.dataType = "LocalDate";

        fakeJavaCodegen.setParameterExampleValue(p);
        Assert.assertEquals(p.example, "LocalDate.now()");
    }

    @Test
    void dateGivenExample() {
        final CodegenParameter p = new CodegenParameter();
        p.isDate = true;
        p.dataType = "LocalDate";
        p.example = "2017-03-30";

        fakeJavaCodegen.setParameterExampleValue(p);
        Assert.assertEquals(p.example, "LocalDate.parse(\"2017-03-30\")");
    }

    @Test
    void dateTimeDefault() {
        final CodegenParameter p = new CodegenParameter();
        p.isDateTime = true;
        p.dataType = "OffsetDateTime";

        fakeJavaCodegen.setParameterExampleValue(p);
        Assert.assertEquals(p.example, "OffsetDateTime.now()");
    }

    @Test
    void dateTimeGivenExample() {
        final CodegenParameter p = new CodegenParameter();
        p.isDateTime = true;
        p.dataType = "OffsetDateTime";
        p.example = "2007-12-03T10:15:30+01:00";

        fakeJavaCodegen.setParameterExampleValue(p);
        Assert.assertEquals(p.example, "OffsetDateTime.parse(\"2007-12-03T10:15:30+01:00\")");
    }

    @Test
    void uuidDefault() {
        final CodegenParameter p = new CodegenParameter();
        p.isUuid = true;
        p.dataType = "UUID";

        fakeJavaCodegen.setParameterExampleValue(p);
        Assert.assertEquals(p.example, "UUID.randomUUID()");
    }

    @Test
    void uuidGivenExample() {
        final CodegenParameter p = new CodegenParameter();
        p.isUuid = true;
        p.dataType = "UUID";
        p.example = "13b48713-b931-45ea-bd60-b07491245960";

        fakeJavaCodegen.setParameterExampleValue(p);
        Assert.assertEquals(p.example, "UUID.fromString(\"13b48713-b931-45ea-bd60-b07491245960\")");
    }

    private static class P_AbstractJavaCodegen extends AbstractJavaCodegen {
        @Override
        public CodegenType getTag() {
            return null;
        }

        @Override
        public String getName() {
            return null;
        }

        @Override
        public String getHelp() {
            return null;
        }

        /**
         * Gets artifact version.
         * Only for testing purposes.
         *
         * @return version
         */
        public String getArtifactVersion() {
            return this.artifactVersion;
        }
    }
}
