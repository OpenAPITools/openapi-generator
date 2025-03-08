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

import org.mockito.Answers;
import org.mockito.Mockito;
import org.openapitools.codegen.CodegenParameter;
import org.openapitools.codegen.CodegenProperty;
import org.openapitools.codegen.languages.AbstractJavaCodegen;
import org.testng.Assert;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import java.util.Arrays;
import java.util.Collections;

public class AbstractJavaCodegenExampleValuesTest {

    private AbstractJavaCodegen codegen;

    /**
     * In TEST-NG, test class (and its fields) is only constructed once (vs. for every test in Jupiter),
     * using @BeforeMethod to have a fresh codegen mock for each test
     */
    @BeforeMethod
    void mockAbstractCodegen() {
        codegen = Mockito.mock(
                AbstractJavaCodegen.class, Mockito.withSettings().defaultAnswer(Answers.CALLS_REAL_METHODS).useConstructor()
        );
    }

    @Test
    void referencedEnumTakeFirstName() {
        final CodegenParameter p = new CodegenParameter();
        p.allowableValues = Collections.singletonMap("values", Arrays.asList("first", "second"));
        p.dataType = "WrappedEnum";

        codegen.setParameterExampleValue(p);
        Assert.assertEquals(p.example, "WrappedEnum.fromValue(\"first\")");
    }

    @Test
    void inlineEnum() {
        final CodegenParameter p = new CodegenParameter();
        p.allowableValues = Collections.singletonMap("values", Arrays.asList("first", "second"));
        p.isEnum = true;
        p.dataType = "String";

        codegen.setParameterExampleValue(p);
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

        codegen.setParameterExampleValue(p);
        Assert.assertEquals(p.example, "Arrays.asList()");
    }

    @Test
    void dateDefault() {
        final CodegenParameter p = new CodegenParameter();
        p.isDate = true;
        p.dataType = "LocalDate";

        codegen.setParameterExampleValue(p);
        Assert.assertEquals(p.example, "LocalDate.now()");
    }

    @Test
    void dateGivenExample() {
        final CodegenParameter p = new CodegenParameter();
        p.isDate = true;
        p.dataType = "LocalDate";
        p.example = "2017-03-30";

        codegen.setParameterExampleValue(p);
        Assert.assertEquals(p.example, "LocalDate.parse(\"2017-03-30\")");
    }

    @Test
    void dateTimeDefault() {
        final CodegenParameter p = new CodegenParameter();
        p.isDateTime = true;
        p.dataType = "OffsetDateTime";

        codegen.setParameterExampleValue(p);
        Assert.assertEquals(p.example, "OffsetDateTime.now()");
    }

    @Test
    void dateTimeGivenExample() {
        final CodegenParameter p = new CodegenParameter();
        p.isDateTime = true;
        p.dataType = "OffsetDateTime";
        p.example = "2007-12-03T10:15:30+01:00";

        codegen.setParameterExampleValue(p);
        Assert.assertEquals(p.example, "OffsetDateTime.parse(\"2007-12-03T10:15:30+01:00\")");
    }

    @Test
    void uuidDefault() {
        final CodegenParameter p = new CodegenParameter();
        p.isUuid = true;
        p.dataType = "UUID";

        codegen.setParameterExampleValue(p);
        Assert.assertEquals(p.example, "UUID.randomUUID()");
    }

    @Test
    void uuidGivenExample() {
        final CodegenParameter p = new CodegenParameter();
        p.isUuid = true;
        p.dataType = "UUID";
        p.example = "13b48713-b931-45ea-bd60-b07491245960";

        codegen.setParameterExampleValue(p);
        Assert.assertEquals(p.example, "UUID.fromString(\"13b48713-b931-45ea-bd60-b07491245960\")");
    }
}
