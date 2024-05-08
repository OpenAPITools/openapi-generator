/*
 * Copyright 2022 OpenAPI-Generator Contributors (https://openapi-generator.tech)
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

import io.swagger.parser.OpenAPIParser;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.parser.core.models.ParseOptions;
import org.openapitools.codegen.ClientOptInput;
import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.java.assertions.JavaFileAssert;
import org.openapitools.codegen.languages.*;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.util.Map;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.stream.Collectors;

public class JavaValidationArrayPrimitivesTest {
    private static Consumer<Map<String, File>> assertWithValidationWithoutJsonNullable() {
        return files -> JavaFileAssert.assertThat(files.get("Foo.java"))
                .isNormalClass()
                .hasProperty("stringPattern")
                .withType("Set<@Pattern(regexp = \"[a-z]\") String>")
                .toType()
                .hasProperty("stringMaxMinLength")
                .withType("Set<@Size(min = 1, max = 10) String>")
                .toType()
                .hasProperty("stringMinLength")
                .withType("List<@Size(min = 1) String>")
                .toType()
                .hasProperty("stringMaxLength")
                .withType("Set<@Size(max = 1) String>")
                .toType()
                .hasProperty("stringEmail")
                .withType("List<@Email String>")
                .toType()
                .hasProperty("intMinMax")
                .withType("List<@Min(1) @Max(10) Integer>")
                .toType()
                .hasProperty("intMin")
                .withType("List<@Min(1) Integer>")
                .toType()
                .hasProperty("intMax")
                .withType("List<@Max(10) Integer>")
                .toType()
                .hasProperty("numberMinMax")
                .withType("List<@DecimalMin(value = \"1\", inclusive = true) @DecimalMax(value = \"10\", inclusive = true) BigDecimal>")
                .toType()
                .hasProperty("numberMin")
                .withType("List<@DecimalMin(value = \"1\", inclusive = true) BigDecimal>")
                .toType()
                .hasProperty("numberMax")
                .withType("List<@DecimalMax(value = \"10\", inclusive = true) BigDecimal>")
                .toType()
                .hasProperty("stringPatternWithMin")
                .withType("Set<@Pattern(regexp = \"^\\\\d{3}-\\\\d{2}-\\\\d{4}$\") @Size(min = 10) String>")
                .toType()
                .hasProperty("stringPatternNullable")
                .withType("Set<@Pattern(regexp = \"^\\\\d{3}-\\\\d{2}-\\\\d{4}$\") String>")
                .toType()
                .hasProperty("stringMaxMinLengthNullable")
                .withType("Set<@Size(min = 1, max = 10) String>")
                .toType()
                .hasProperty("stringMinLengthNullable")
                .withType("List<@Size(min = 1) String>")
                .toType()
                .hasProperty("stringMaxLengthNullable")
                .withType("Set<@Size(max = 1) String>")
                .toType()
                .hasProperty("stringNumbers")
                .withType("Set<@DecimalMin(value = \"1\", inclusive = true) @DecimalMax(value = \"10\", inclusive = true) BigDecimal>")
                .toType()
                .hasProperty("intMinMaxNullable")
                .withType("List<@Min(1) @Max(10) Integer>")
                .toType()
                .hasProperty("intMinNullable")
                .withType("List<@Min(1) Integer>")
                .toType()
                .hasProperty("intMaxNullable")
                .withType("List<@Max(10) Integer>")
                .toType()
                .hasProperty("numberMinMaxNullable")
                .withType("List<@DecimalMin(value = \"1\", inclusive = true) @DecimalMax(value = \"10\", inclusive = true) BigDecimal>")
                .toType()
                .hasProperty("numberMinNullable")
                .withType("List<@DecimalMin(value = \"1\", inclusive = true) BigDecimal>")
                .toType()
                .hasProperty("numberMaxNullable")
                .withType("List<@DecimalMax(value = \"10\", inclusive = false) BigDecimal>")
                .toType();
    }

    private static Consumer<Map<String, File>> assertWithValidationWithJsonNullable() {
        return files -> JavaFileAssert.assertThat(files.get("Foo.java"))
                .isNormalClass()
                .hasProperty("category")
                .withType("List<@Pattern(regexp = \"^[a-zA-Z0-9 .:!()-]$\") @Size(max = 50) String>")
                .toType()
                .hasProperty("stringPattern")
                .withType("Set<@Pattern(regexp = \"[a-z]\") String>")
                .toType()
                .hasProperty("stringMaxMinLength")
                .withType("Set<@Size(min = 1, max = 10) String>")
                .toType()
                .hasProperty("stringMinLength")
                .withType("List<@Size(min = 1) String>")
                .toType()
                .hasProperty("stringMaxLength")
                .withType("Set<@Size(max = 1) String>")
                .toType()
                .hasProperty("stringEmail")
                .withType("List<@Email String>")
                .toType()
                .hasProperty("intMinMax")
                .withType("List<@Min(1) @Max(10) Integer>")
                .toType()
                .hasProperty("intMin")
                .withType("List<@Min(1) Integer>")
                .toType()
                .hasProperty("intMax")
                .withType("List<@Max(10) Integer>")
                .toType()
                .hasProperty("numberMinMax")
                .withType("List<@DecimalMin(value = \"1\", inclusive = true) @DecimalMax(value = \"10\", inclusive = true) BigDecimal>")
                .toType()
                .hasProperty("numberMin")
                .withType("List<@DecimalMin(value = \"1\", inclusive = true) BigDecimal>")
                .toType()
                .hasProperty("numberMax")
                .withType("List<@DecimalMax(value = \"10\", inclusive = true) BigDecimal>")
                .toType()
                .hasProperty("stringPatternWithMin")
                .withType("JsonNullable<Set<@Pattern(regexp = \"^\\\\d{3}-\\\\d{2}-\\\\d{4}$\") @Size(min = 10) String>>")
                .toType()
                .hasProperty("stringPatternNullable")
                .withType("JsonNullable<Set<@Pattern(regexp = \"^\\\\d{3}-\\\\d{2}-\\\\d{4}$\") String>>")
                .toType()
                .hasProperty("stringMaxMinLengthNullable")
                .withType("JsonNullable<Set<@Size(min = 1, max = 10) String>>")
                .toType()
                .hasProperty("stringMinLengthNullable")
                .withType("JsonNullable<List<@Size(min = 1) String>>")
                .toType()
                .hasProperty("stringMaxLengthNullable")
                .withType("JsonNullable<Set<@Size(max = 1) String>>")
                .toType()
                .hasProperty("stringNumbers")
                .withType("Set<@DecimalMin(value = \"1\", inclusive = true) @DecimalMax(value = \"10\", inclusive = true) BigDecimal>")
                .toType()
                .hasProperty("intMinMaxNullable")
                .withType("JsonNullable<List<@Min(1) @Max(10) Integer>>")
                .toType()
                .hasProperty("intMinNullable")
                .withType("JsonNullable<List<@Min(1) Integer>>")
                .toType()
                .hasProperty("intMaxNullable")
                .withType("JsonNullable<List<@Max(10) Integer>>")
                .toType()
                .hasProperty("numberMinMaxNullable")
                .withType("JsonNullable<List<@DecimalMin(value = \"1\", inclusive = true) @DecimalMax(value = \"10\", inclusive = true) BigDecimal>>")
                .toType()
                .hasProperty("numberMinNullable")
                .withType("JsonNullable<List<@DecimalMin(value = \"1\", inclusive = true) BigDecimal>>")
                .toType()
                .hasProperty("numberMaxNullable")
                .withType("JsonNullable<List<@DecimalMax(value = \"10\", inclusive = false) BigDecimal>>")
                .toType();
    }

    @DataProvider(name = "javaCodegensUsedBeanValidation")
    public static Object[][] javaCodegensUsedBeanValidation() {
        return new Object[][]{
                {new JavaCXFClientCodegen(), assertWithValidationWithoutJsonNullable()},
                {new JavaClientCodegen(), assertWithValidationWithoutJsonNullable()},
                {new JavaPlayFrameworkCodegen(), assertWithValidationWithoutJsonNullable()},
                {new JavaMicronautClientCodegen(), assertWithValidationWithoutJsonNullable()},
                {new JavaMicronautServerCodegen(), assertWithValidationWithoutJsonNullable()},
                {new JavaJAXRSCXFCDIServerCodegen(), assertWithValidationWithoutJsonNullable()},
                {new JavaCXFExtServerCodegen(), assertWithValidationWithoutJsonNullable()},
                {new JavaResteasyServerCodegen(), assertWithValidationWithoutJsonNullable()},
                {new JavaJAXRSSpecServerCodegen(), assertWithValidationWithoutJsonNullable()},
                {new JavaJerseyServerCodegen(), assertWithValidationWithoutJsonNullable()},
                {new JavaResteasyEapServerCodegen(), assertWithValidationWithoutJsonNullable()},
                {new SpringCodegen(), assertWithValidationWithJsonNullable()}
        };
    }

    @Test(dataProvider = "javaCodegensUsedBeanValidation")
    public void shouldAddValidAnnotationIntoCollectionWhenBeanValidationIsEnabled_issue4947(final AbstractJavaCodegen codegen, final Consumer<Map<String, File>> asserts) throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        final OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/bugs/issue_4947.yaml", null, new ParseOptions()).getOpenAPI();
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(SpringCodegen.USE_BEANVALIDATION, "true");
        codegen.additionalProperties().put(CodegenConstants.MODEL_PACKAGE, "xyz.model");
        codegen.additionalProperties().put(CodegenConstants.API_PACKAGE, "xyz.controller");

        final ClientOptInput input = new ClientOptInput()
                .openAPI(openAPI)
                .config(codegen);

        final DefaultGenerator generator = new DefaultGenerator();
        final Map<String, File> files = generator.opts(input).generate().stream()
                .collect(Collectors.toMap(File::getName, Function.identity()));

        asserts.accept(files);
    }

    private static Consumer<Map<String, File>> assertWithoutValidationWithoutJsonNullable() {
        return files -> JavaFileAssert.assertThat(files.get("Foo.java"))
                .isNormalClass()
                .hasProperty("stringPattern")
                .withType("Set<String>")
                .toType()
                .hasProperty("stringMaxMinLength")
                .withType("Set<String>")
                .toType()
                .hasProperty("stringMinLength")
                .withType("List<String>")
                .toType()
                .hasProperty("stringMaxLength")
                .withType("Set<String>")
                .toType()
                .hasProperty("stringEmail")
                .withType("List<String>")
                .toType()
                .hasProperty("intMinMax")
                .withType("List<Integer>")
                .toType()
                .hasProperty("intMin")
                .withType("List<Integer>")
                .toType()
                .hasProperty("intMax")
                .withType("List<Integer>")
                .toType()
                .hasProperty("numberMinMax")
                .withType("List<BigDecimal>")
                .toType()
                .hasProperty("numberMin")
                .withType("List<BigDecimal>")
                .toType()
                .hasProperty("numberMax")
                .withType("List<BigDecimal>")
                .toType()
                .hasProperty("stringPatternWithMin")
                .withType("Set<String>")
                .toType()
                .hasProperty("stringPatternNullable")
                .withType("Set<String>")
                .toType()
                .hasProperty("stringMaxMinLengthNullable")
                .withType("Set<String>")
                .toType()
                .hasProperty("stringMinLengthNullable")
                .withType("List<String>")
                .toType()
                .hasProperty("stringMaxLengthNullable")
                .withType("Set<String>")
                .toType()
                .hasProperty("intMinMaxNullable")
                .withType("List<Integer>")
                .toType()
                .hasProperty("intMinNullable")
                .withType("List<Integer>")
                .toType()
                .hasProperty("intMaxNullable")
                .withType("List<Integer>")
                .toType()
                .hasProperty("numberMinMaxNullable")
                .withType("List<BigDecimal>")
                .toType()
                .hasProperty("numberMinNullable")
                .withType("List<BigDecimal>")
                .toType()
                .hasProperty("numberMaxNullable")
                .withType("List<BigDecimal>")
                .toType();
    }

    private static Consumer<Map<String, File>> assertWithoutValidationWithJsonNullable() {
        return files -> JavaFileAssert.assertThat(files.get("Foo.java"))
                .isNormalClass()
                .hasProperty("stringPattern")
                .withType("Set<String>")
                .toType()
                .hasProperty("stringMaxMinLength")
                .withType("Set<String>")
                .toType()
                .hasProperty("stringMinLength")
                .withType("List<String>")
                .toType()
                .hasProperty("stringMaxLength")
                .withType("Set<String>")
                .toType()
                .hasProperty("stringEmail")
                .withType("List<String>")
                .toType()
                .hasProperty("intMinMax")
                .withType("List<Integer>")
                .toType()
                .hasProperty("intMin")
                .withType("List<Integer>")
                .toType()
                .hasProperty("intMax")
                .withType("List<Integer>")
                .toType()
                .hasProperty("numberMinMax")
                .withType("List<BigDecimal>")
                .toType()
                .hasProperty("numberMin")
                .withType("List<BigDecimal>")
                .toType()
                .hasProperty("numberMax")
                .withType("List<BigDecimal>")
                .toType()
                .hasProperty("stringPatternWithMin")
                .withType("JsonNullable<Set<String>>")
                .toType()
                .hasProperty("stringPatternNullable")
                .withType("JsonNullable<Set<String>>")
                .toType()
                .hasProperty("stringMaxMinLengthNullable")
                .withType("JsonNullable<Set<String>>")
                .toType()
                .hasProperty("stringMinLengthNullable")
                .withType("JsonNullable<List<String>>")
                .toType()
                .hasProperty("stringMaxLengthNullable")
                .withType("JsonNullable<Set<String>>")
                .toType()
                .hasProperty("intMinMaxNullable")
                .withType("JsonNullable<List<Integer>>")
                .toType()
                .hasProperty("intMinNullable")
                .withType("JsonNullable<List<Integer>>")
                .toType()
                .hasProperty("intMaxNullable")
                .withType("JsonNullable<List<Integer>>")
                .toType()
                .hasProperty("numberMinMaxNullable")
                .withType("JsonNullable<List<BigDecimal>>")
                .toType()
                .hasProperty("numberMinNullable")
                .withType("JsonNullable<List<BigDecimal>>")
                .toType()
                .hasProperty("numberMaxNullable")
                .withType("JsonNullable<List<BigDecimal>>")
                .toType();
    }

    @DataProvider(name = "javaCodegensNotUsedBeanValidation")
    public static Object[][] javaCodegensNotUsedBeanValidation() {
        return new Object[][]{
                {new JavaCXFClientCodegen(), assertWithoutValidationWithoutJsonNullable()},
                {new JavaClientCodegen(), assertWithoutValidationWithoutJsonNullable()},
                {new JavaPlayFrameworkCodegen(), assertWithoutValidationWithoutJsonNullable()},
                {new JavaMicronautClientCodegen(), assertWithoutValidationWithoutJsonNullable()},
                {new JavaMicronautServerCodegen(), assertWithoutValidationWithoutJsonNullable()},
                {new JavaJAXRSCXFCDIServerCodegen(), assertWithoutValidationWithoutJsonNullable()},
                {new JavaCXFExtServerCodegen(), assertWithoutValidationWithoutJsonNullable()},
                {new JavaResteasyServerCodegen(), assertWithoutValidationWithoutJsonNullable()},
                {new JavaJAXRSSpecServerCodegen(), assertWithoutValidationWithoutJsonNullable()},
                {new JavaJerseyServerCodegen(), assertWithoutValidationWithoutJsonNullable()},
                {new JavaResteasyEapServerCodegen(), assertWithoutValidationWithoutJsonNullable()},
                {new SpringCodegen(), assertWithoutValidationWithJsonNullable()}
        };
    }

    @Test(dataProvider = "javaCodegensNotUsedBeanValidation")
    public void shouldNotAddValidAnnotationIntoCollectionWhenBeanValidationIsNotEnabled_issue4947(final AbstractJavaCodegen codegen, final Consumer<Map<String, File>> asserts) throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        final OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/bugs/issue_4947.yaml", null, new ParseOptions()).getOpenAPI();
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(CodegenConstants.MODEL_PACKAGE, "xyz.model");
        codegen.additionalProperties().put(CodegenConstants.API_PACKAGE, "xyz.controller");
        codegen.additionalProperties().put(SpringCodegen.USE_BEANVALIDATION, "false");

        final ClientOptInput input = new ClientOptInput()
                .openAPI(openAPI)
                .config(codegen);

        final DefaultGenerator generator = new DefaultGenerator();
        final Map<String, File> files = generator.opts(input).generate().stream()
                .collect(Collectors.toMap(File::getName, Function.identity()));

        asserts.accept(files);
    }
}
