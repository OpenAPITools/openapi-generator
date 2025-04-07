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
import org.openapitools.codegen.config.GeneratorSettings;
import org.openapitools.codegen.java.assertions.JavaFileAssert;
import org.openapitools.codegen.languages.*;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.util.Collections;
import java.util.Map;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.stream.Collectors;

public class JavaValidationArrayPrimitivesTest {
    private static Consumer<Map<String, File>> assertWithValidationWithoutJsonNullable() {
        return files -> JavaFileAssert.assertThat(files.get("Foo.java"))
                .isNormalClass()
                .assertProperty("stringPattern")
                .withType("Set<@Pattern(regexp = \"[a-z]\") String>")
                .toType()
                .assertProperty("stringMaxMinLength")
                .withType("Set<@Size(min = 1, max = 10) String>")
                .toType()
                .assertProperty("stringMinLength")
                .withType("List<@Size(min = 1) String>")
                .toType()
                .assertProperty("stringMaxLength")
                .withType("Set<@Size(max = 1) String>")
                .toType()
                .assertProperty("stringEmail")
                .withType("List<@Email String>")
                .toType()
                .assertProperty("intMinMax")
                .withType("List<@Min(1) @Max(10) Integer>")
                .toType()
                .assertProperty("intMin")
                .withType("List<@Min(1) Integer>")
                .toType()
                .assertProperty("intMax")
                .withType("List<@Max(10) Integer>")
                .toType()
                .assertProperty("numberMinMax")
                .withType("List<@DecimalMin(value = \"1\", inclusive = true) @DecimalMax(value = \"10\", inclusive = true) BigDecimal>")
                .toType()
                .assertProperty("numberMin")
                .withType("List<@DecimalMin(value = \"1\", inclusive = true) BigDecimal>")
                .toType()
                .assertProperty("numberMax")
                .withType("List<@DecimalMax(value = \"10\", inclusive = true) BigDecimal>")
                .toType()
                .assertProperty("stringPatternWithMin")
                .withType("Set<@Pattern(regexp = \"^\\\\d{3}-\\\\d{2}-\\\\d{4}$\") @Size(min = 10) String>")
                .toType()
                .assertProperty("stringPatternNullable")
                .withType("Set<@Pattern(regexp = \"^\\\\d{3}-\\\\d{2}-\\\\d{4}$\") String>")
                .toType()
                .assertProperty("stringMaxMinLengthNullable")
                .withType("Set<@Size(min = 1, max = 10) String>")
                .toType()
                .assertProperty("stringMinLengthNullable")
                .withType("List<@Size(min = 1) String>")
                .toType()
                .assertProperty("stringMaxLengthNullable")
                .withType("Set<@Size(max = 1) String>")
                .toType()
                .assertProperty("stringNumbers")
                .withType("Set<@DecimalMin(value = \"1\", inclusive = true) @DecimalMax(value = \"10\", inclusive = true) BigDecimal>")
                .toType()
                .assertProperty("intMinMaxNullable")
                .withType("List<@Min(1) @Max(10) Integer>")
                .toType()
                .assertProperty("intMinNullable")
                .withType("List<@Min(1) Integer>")
                .toType()
                .assertProperty("intMaxNullable")
                .withType("List<@Max(10) Integer>")
                .toType()
                .assertProperty("numberMinMaxNullable")
                .withType("List<@DecimalMin(value = \"1\", inclusive = true) @DecimalMax(value = \"10\", inclusive = true) BigDecimal>")
                .toType()
                .assertProperty("numberMinNullable")
                .withType("List<@DecimalMin(value = \"1\", inclusive = true) BigDecimal>")
                .toType()
                .assertProperty("numberMaxNullable")
                .withType("List<@DecimalMax(value = \"10\", inclusive = false) BigDecimal>")
                .toType();
    }

    private static Consumer<Map<String, File>> assertWithValidationWithJsonNullable() {
        return files -> JavaFileAssert.assertThat(files.get("Foo.java"))
                .isNormalClass()
                .assertProperty("category")
                .withType("List<@Pattern(regexp = \"^[a-zA-Z0-9 .:!()-]$\") @Size(max = 50) String>")
                .toType()
                .assertProperty("stringPattern")
                .withType("Set<@Pattern(regexp = \"[a-z]\") String>")
                .toType()
                .assertProperty("stringMaxMinLength")
                .withType("Set<@Size(min = 1, max = 10) String>")
                .toType()
                .assertProperty("stringMinLength")
                .withType("List<@Size(min = 1) String>")
                .toType()
                .assertProperty("stringMaxLength")
                .withType("Set<@Size(max = 1) String>")
                .toType()
                .assertProperty("stringEmail")
                .withType("List<@Email String>")
                .toType()
                .assertProperty("intMinMax")
                .withType("List<@Min(1) @Max(10) Integer>")
                .toType()
                .assertProperty("intMin")
                .withType("List<@Min(1) Integer>")
                .toType()
                .assertProperty("intMax")
                .withType("List<@Max(10) Integer>")
                .toType()
                .assertProperty("numberMinMax")
                .withType("List<@DecimalMin(value = \"1\", inclusive = true) @DecimalMax(value = \"10\", inclusive = true) BigDecimal>")
                .toType()
                .assertProperty("numberMin")
                .withType("List<@DecimalMin(value = \"1\", inclusive = true) BigDecimal>")
                .toType()
                .assertProperty("numberMax")
                .withType("List<@DecimalMax(value = \"10\", inclusive = true) BigDecimal>")
                .toType()
                .assertProperty("stringPatternWithMin")
                .withType("JsonNullable<Set<@Pattern(regexp = \"^\\\\d{3}-\\\\d{2}-\\\\d{4}$\") @Size(min = 10) String>>")
                .toType()
                .assertProperty("stringPatternNullable")
                .withType("JsonNullable<Set<@Pattern(regexp = \"^\\\\d{3}-\\\\d{2}-\\\\d{4}$\") String>>")
                .toType()
                .assertProperty("stringMaxMinLengthNullable")
                .withType("JsonNullable<Set<@Size(min = 1, max = 10) String>>")
                .toType()
                .assertProperty("stringMinLengthNullable")
                .withType("JsonNullable<List<@Size(min = 1) String>>")
                .toType()
                .assertProperty("stringMaxLengthNullable")
                .withType("JsonNullable<Set<@Size(max = 1) String>>")
                .toType()
                .assertProperty("stringNumbers")
                .withType("Set<@DecimalMin(value = \"1\", inclusive = true) @DecimalMax(value = \"10\", inclusive = true) BigDecimal>")
                .toType()
                .assertProperty("intMinMaxNullable")
                .withType("JsonNullable<List<@Min(1) @Max(10) Integer>>")
                .toType()
                .assertProperty("intMinNullable")
                .withType("JsonNullable<List<@Min(1) Integer>>")
                .toType()
                .assertProperty("intMaxNullable")
                .withType("JsonNullable<List<@Max(10) Integer>>")
                .toType()
                .assertProperty("numberMinMaxNullable")
                .withType("JsonNullable<List<@DecimalMin(value = \"1\", inclusive = true) @DecimalMax(value = \"10\", inclusive = true) BigDecimal>>")
                .toType()
                .assertProperty("numberMinNullable")
                .withType("JsonNullable<List<@DecimalMin(value = \"1\", inclusive = true) BigDecimal>>")
                .toType()
                .assertProperty("numberMaxNullable")
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
                .assertProperty("stringPattern")
                .withType("Set<String>")
                .toType()
                .assertProperty("stringMaxMinLength")
                .withType("Set<String>")
                .toType()
                .assertProperty("stringMinLength")
                .withType("List<String>")
                .toType()
                .assertProperty("stringMaxLength")
                .withType("Set<String>")
                .toType()
                .assertProperty("stringEmail")
                .withType("List<String>")
                .toType()
                .assertProperty("intMinMax")
                .withType("List<Integer>")
                .toType()
                .assertProperty("intMin")
                .withType("List<Integer>")
                .toType()
                .assertProperty("intMax")
                .withType("List<Integer>")
                .toType()
                .assertProperty("numberMinMax")
                .withType("List<BigDecimal>")
                .toType()
                .assertProperty("numberMin")
                .withType("List<BigDecimal>")
                .toType()
                .assertProperty("numberMax")
                .withType("List<BigDecimal>")
                .toType()
                .assertProperty("stringPatternWithMin")
                .withType("Set<String>")
                .toType()
                .assertProperty("stringPatternNullable")
                .withType("Set<String>")
                .toType()
                .assertProperty("stringMaxMinLengthNullable")
                .withType("Set<String>")
                .toType()
                .assertProperty("stringMinLengthNullable")
                .withType("List<String>")
                .toType()
                .assertProperty("stringMaxLengthNullable")
                .withType("Set<String>")
                .toType()
                .assertProperty("intMinMaxNullable")
                .withType("List<Integer>")
                .toType()
                .assertProperty("intMinNullable")
                .withType("List<Integer>")
                .toType()
                .assertProperty("intMaxNullable")
                .withType("List<Integer>")
                .toType()
                .assertProperty("numberMinMaxNullable")
                .withType("List<BigDecimal>")
                .toType()
                .assertProperty("numberMinNullable")
                .withType("List<BigDecimal>")
                .toType()
                .assertProperty("numberMaxNullable")
                .withType("List<BigDecimal>")
                .toType();
    }

    private static Consumer<Map<String, File>> assertWithoutValidationWithJsonNullable() {
        return files -> JavaFileAssert.assertThat(files.get("Foo.java"))
                .isNormalClass()
                .assertProperty("stringPattern")
                .withType("Set<String>")
                .toType()
                .assertProperty("stringMaxMinLength")
                .withType("Set<String>")
                .toType()
                .assertProperty("stringMinLength")
                .withType("List<String>")
                .toType()
                .assertProperty("stringMaxLength")
                .withType("Set<String>")
                .toType()
                .assertProperty("stringEmail")
                .withType("List<String>")
                .toType()
                .assertProperty("intMinMax")
                .withType("List<Integer>")
                .toType()
                .assertProperty("intMin")
                .withType("List<Integer>")
                .toType()
                .assertProperty("intMax")
                .withType("List<Integer>")
                .toType()
                .assertProperty("numberMinMax")
                .withType("List<BigDecimal>")
                .toType()
                .assertProperty("numberMin")
                .withType("List<BigDecimal>")
                .toType()
                .assertProperty("numberMax")
                .withType("List<BigDecimal>")
                .toType()
                .assertProperty("stringPatternWithMin")
                .withType("JsonNullable<Set<String>>")
                .toType()
                .assertProperty("stringPatternNullable")
                .withType("JsonNullable<Set<String>>")
                .toType()
                .assertProperty("stringMaxMinLengthNullable")
                .withType("JsonNullable<Set<String>>")
                .toType()
                .assertProperty("stringMinLengthNullable")
                .withType("JsonNullable<List<String>>")
                .toType()
                .assertProperty("stringMaxLengthNullable")
                .withType("JsonNullable<Set<String>>")
                .toType()
                .assertProperty("intMinMaxNullable")
                .withType("JsonNullable<List<Integer>>")
                .toType()
                .assertProperty("intMinNullable")
                .withType("JsonNullable<List<Integer>>")
                .toType()
                .assertProperty("intMaxNullable")
                .withType("JsonNullable<List<Integer>>")
                .toType()
                .assertProperty("numberMinMaxNullable")
                .withType("JsonNullable<List<BigDecimal>>")
                .toType()
                .assertProperty("numberMinNullable")
                .withType("JsonNullable<List<BigDecimal>>")
                .toType()
                .assertProperty("numberMaxNullable")
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

    @DataProvider(name = "typeMappings")
    public Object[] typeMappings() {
        return new Object[][]{
                {Collections.emptyMap(), "@Valid MyItem"},
                {Map.of("array", "List"), "@Valid MyItem"},
                {Map.of("array", "Set"), "@Valid MyItem"},
                {Collections.emptyMap(), "@Valid MyItem"},
                {Map.of("MyItem", "com.mycompany.MyItem"), "com.mycompany.@Valid MyItem"},
                {Map.of("MyItem", "com.mycompany.MyContainer<java.lang.String>"), "com.mycompany.@Valid MyContainer<java.lang.String>"}
        };
    }

    @Test(dataProvider = "typeMappings")
    public void typeMappingsForCollections(Map<String, String> typeMappings, String expectedMyItemArgument) throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        final OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/bugs/issue_17472.yaml", null, new ParseOptions()).getOpenAPI();
        final SpringCodegen codegen = new SpringCodegen();
        codegen.setUseTags(true);
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.setOpenAPI(openAPI);
        final GeneratorSettings generatorSettings = GeneratorSettings.newBuilder()
                .withTypeMappings(typeMappings)
                .build();
        ClientOptInput input = new ClientOptInput();
        input.generatorSettings(generatorSettings);
        input.openAPI(openAPI);
        input.config(codegen);
        final DefaultGenerator generator = new DefaultGenerator();
        Map<String, File> files = generator.opts(input).generate().stream()
                .collect(Collectors.toMap(File::getName, Function.identity()));

        String arrayMapping = typeMappings.getOrDefault("array", "List");
        // @Valid@Size(min = 5) is not nice, but not related to this fix
        // adding a space would probably break many other tests
        JavaFileAssert.assertThat(files.get("ListOfPatternsApi.java"))
                .fileContains("ResponseEntity<" + arrayMapping + "<String>>",
                        arrayMapping + "<@Pattern(regexp = \"([a-z]+)\")String> requestBody")
                .fileContainsPattern("@Valid\\s*@Size\\(min = 5\\)\\s*@RequestBody");

        JavaFileAssert.assertThat(files.get("ListOfStringsApi.java"))
                .fileContains(
                        "ResponseEntity<" + arrayMapping + "<String>>",
                        arrayMapping + "<@Size(min = 2, max = 2)String> requestBody")
                .fileContainsPattern("@Valid\\s*@Size\\(min = 5\\)\\s*@RequestBody");

        JavaFileAssert.assertThat(files.get("ListOfObjectsApi.java"))
                .fileContains(
                        "ResponseEntity<" + arrayMapping + "<ListOfObjectsInner>>",
                        arrayMapping + "<@Valid ListOfObjectsInner> listOfObjectsInner")
                .fileContainsPattern("@Valid\\s*@Size\\(min = 5\\)\\s*@RequestBody");

        String myItem = typeMappings.getOrDefault("MyItem", "MyItem");
        JavaFileAssert.assertThat(files.get("ListOfQualifiedItemApi.java"))
                .fileContains(
                        "ResponseEntity<" + arrayMapping + "<" + myItem + ">>",
                        arrayMapping + "<" + expectedMyItemArgument + ">");

        if (!typeMappings.containsKey("array")) {
            // the mapping to Set is done automatically with uniqueItems: true
            JavaFileAssert.assertThat(files.get("ListOfUniqueItemApi.java"))
                    .fileContains(
                            "ResponseEntity<Set<" + myItem + ">>",
                            "Set<" + expectedMyItemArgument + "> ");
        }
    }
}
