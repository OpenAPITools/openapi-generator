package org.openapitools.codegen.kotlin;

import io.swagger.v3.oas.models.Operation;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.ParseTreeWalker;
import org.openapitools.codegen.ClientOptInput;
import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.CodegenOperation;
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.antlr4.KotlinLexer;
import org.openapitools.codegen.antlr4.KotlinParser;
import org.openapitools.codegen.languages.KotlinServerCodegen;
import org.openapitools.codegen.languages.KotlinSpringServerCodegen;
import org.testng.Assert;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

import static org.openapitools.codegen.CodegenConstants.API_PACKAGE;
import static org.openapitools.codegen.CodegenConstants.LIBRARY;
import static org.openapitools.codegen.CodegenConstants.MODEL_PACKAGE;
import static org.openapitools.codegen.CodegenConstants.PACKAGE_NAME;
import static org.openapitools.codegen.TestUtils.assertFileContains;
import static org.openapitools.codegen.TestUtils.assertFileNotContains;
import static org.openapitools.codegen.languages.AbstractKotlinCodegen.USE_JAKARTA_EE;
import static org.openapitools.codegen.languages.KotlinServerCodegen.Constants.INTERFACE_ONLY;
import static org.openapitools.codegen.languages.KotlinServerCodegen.Constants.JAVALIN5;
import static org.openapitools.codegen.languages.KotlinServerCodegen.Constants.JAVALIN6;
import static org.openapitools.codegen.languages.KotlinServerCodegen.Constants.INHERITANCE_MODE;
import static org.openapitools.codegen.languages.KotlinServerCodegen.Constants.INHERITANCE_MODE_ABSTRACT;
import static org.openapitools.codegen.languages.KotlinServerCodegen.Constants.INHERITANCE_MODE_COMPOSITION;
import static org.openapitools.codegen.languages.KotlinServerCodegen.Constants.INHERITANCE_MODE_NONE;
import static org.openapitools.codegen.languages.KotlinServerCodegen.Constants.INHERITANCE_MODE_SEALED;
import static org.openapitools.codegen.languages.KotlinServerCodegen.Constants.JAXRS_SPEC;
import static org.openapitools.codegen.languages.KotlinServerCodegen.Constants.RETURN_RESPONSE;
import static org.openapitools.codegen.languages.KotlinServerCodegen.Constants.USE_TAGS;
import static org.openapitools.codegen.languages.features.BeanValidationFeatures.USE_BEANVALIDATION;
import static org.openapitools.codegen.languages.KotlinServerCodegen.Constants.DELEGATE_PATTERN;
import static org.openapitools.codegen.languages.KotlinServerCodegen.Constants.KTOR;
import static org.openapitools.codegen.languages.KotlinServerCodegen.Constants.RESOURCES;
import static org.openapitools.codegen.languages.KotlinServerCodegen.Constants.X_INHERITANCE_MODE_ABSTRACT;
import static org.openapitools.codegen.languages.KotlinServerCodegen.Constants.X_INHERITANCE_MODE_COMPOSITION;
import static org.openapitools.codegen.languages.KotlinServerCodegen.Constants.X_INHERITANCE_MODE_NONE;
import static org.openapitools.codegen.languages.KotlinServerCodegen.Constants.X_INHERITANCE_MODE_SEALED;

public class KotlinServerCodegenTest {

    @Test
    public void enumDescription() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        KotlinServerCodegen codegen = new KotlinServerCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(LIBRARY, JAXRS_SPEC);

        new DefaultGenerator().opts(new ClientOptInput()
                        .openAPI(TestUtils.parseSpec("src/test/resources/3_0/enum-description.yaml"))
                        .config(codegen))
                .generate();

        String outputPath = output.getAbsolutePath() + "/src/main/kotlin/org/openapitools/server";
        Path petApi = Paths.get(outputPath + "/models/Type.kt");
        assertFileNotContains(
                petApi,
                "import jakarta.ws.rs.*",
                "import jakarta.ws.rs.core.Response",
                "@jakarta.annotation.Generated(value = arrayOf(\"org.openapitools.codegen.languages.KotlinServerCodegen\")"
        );
        // assert, that all enum values have a description comment
        assertFileContains(
                petApi,
                "Pegasi b is a gas giant exoplanet that orbits a G-type star",
                "Mercury is the first planet from the Sun and the smallest in the Solar System",
                "The planet we all live on"
        );
    }

    @Test
    public void javaxImports() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        KotlinServerCodegen codegen = new KotlinServerCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(LIBRARY, JAXRS_SPEC);

        new DefaultGenerator().opts(new ClientOptInput()
                        .openAPI(TestUtils.parseSpec("src/test/resources/3_0/petstore.yaml"))
                        .config(codegen))
                .generate();

        String outputPath = output.getAbsolutePath() + "/src/main/kotlin/org/openapitools/server";
        Path petApi = Paths.get(outputPath + "/apis/PetApi.kt");
        assertFileNotContains(
                petApi,
                "import jakarta.ws.rs.*",
                "import jakarta.ws.rs.core.Response",
                "@jakarta.annotation.Generated(value = arrayOf(\"org.openapitools.codegen.languages.KotlinServerCodegen\")"
        );
        assertFileContains(
                petApi,
                "import javax.ws.rs.*",
                "import javax.ws.rs.core.Response",
                "@javax.annotation.Generated(value = arrayOf(\"org.openapitools.codegen.languages.KotlinServerCodegen\")"
        );
    }

    @Test
    public void jakartaEeImports() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        KotlinServerCodegen codegen = new KotlinServerCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(USE_JAKARTA_EE, true);
        codegen.additionalProperties().put(LIBRARY, JAXRS_SPEC);

        new DefaultGenerator().opts(new ClientOptInput()
                        .openAPI(TestUtils.parseSpec("src/test/resources/3_0/petstore.yaml"))
                        .config(codegen))
                .generate();

        String outputPath = output.getAbsolutePath() + "/src/main/kotlin/org/openapitools/server";
        Path petApi = Paths.get(outputPath + "/apis/PetApi.kt");
        assertFileContains(
                petApi,
                "import jakarta.ws.rs.*",
                "import jakarta.ws.rs.core.Response",
                "@jakarta.annotation.Generated(value = arrayOf(\"org.openapitools.codegen.languages.KotlinServerCodegen\")"
        );
        assertFileNotContains(
                petApi,
                "import javax.ws.rs.*",
                "import javax.ws.rs.core.Response",
                "@javax.annotation.Generated(value = arrayOf(\"org.openapitools.codegen.languages.KotlinServerCodegen\")"
        );
    }

    @Test
    public void beanValidationJavaxImports() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        KotlinServerCodegen codegen = new KotlinServerCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(LIBRARY, JAXRS_SPEC);
        codegen.additionalProperties().put(USE_BEANVALIDATION, true);

        new DefaultGenerator().opts(new ClientOptInput()
                        .openAPI(TestUtils.parseSpec("src/test/resources/3_0/petstore.yaml"))
                        .config(codegen))
                .generate();

        String outputPath = output.getAbsolutePath() + "/src/main/kotlin/org/openapitools/server";
        Path petApi = Paths.get(outputPath + "/apis/PetApi.kt");
        assertFileNotContains(
                petApi,
                "import jakarta.validation.Valid",
                "import jakarta.validation.Valid"
        );
        assertFileContains(
                petApi,
                "import javax.validation.constraints.*",
                "import javax.validation.Valid"
        );

        Path petModel = Paths.get(outputPath + "/models/Pet.kt");
        assertFileNotContains(
                petApi,
                "import jakarta.validation.Valid",
                "import jakarta.validation.Valid"
        );
        assertFileContains(
                petApi,
                "import javax.validation.constraints.*",
                "import javax.validation.Valid"
        );
    }

    @Test
    public void beanValidationJakartaEeImports() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        KotlinServerCodegen codegen = new KotlinServerCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(USE_JAKARTA_EE, true);
        codegen.additionalProperties().put(LIBRARY, JAXRS_SPEC);
        codegen.additionalProperties().put(USE_BEANVALIDATION, true);

        new DefaultGenerator().opts(new ClientOptInput()
                        .openAPI(TestUtils.parseSpec("src/test/resources/3_0/petstore.yaml"))
                        .config(codegen))
                .generate();

        String outputPath = output.getAbsolutePath() + "/src/main/kotlin/org/openapitools/server";
        Path petApi = Paths.get(outputPath + "/apis/PetApi.kt");
        assertFileContains(
                petApi,
                "import jakarta.validation.Valid",
                "import jakarta.validation.Valid"
        );
        assertFileNotContains(
                petApi,
                "import javax.validation.constraints.*",
                "import javax.validation.Valid"
        );

        Path petModel = Paths.get(outputPath + "/models/Pet.kt");
        assertFileContains(
                petModel,
                "import jakarta.validation.Valid",
                "import jakarta.validation.Valid"
        );
        assertFileNotContains(
                petModel,
                "import javax.validation.constraints.*",
                "import javax.validation.Valid"
        );
    }

    @Test
    public void issue18177Arrays() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        KotlinServerCodegen codegen = new KotlinServerCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.setUseBeanValidation(true);
        codegen.additionalProperties().put(INTERFACE_ONLY, true);
        codegen.additionalProperties().put(USE_JAKARTA_EE, true);
        codegen.additionalProperties().put(LIBRARY, JAXRS_SPEC);
        new DefaultGenerator().opts(new ClientOptInput()
                        .openAPI(TestUtils.parseSpec("src/test/resources/3_0/kotlin/issue18177-array.yaml"))
                        .config(codegen))
                .generate();

        String outputPath = output.getAbsolutePath() + "/src/main/kotlin/org/openapitools/server";
        Path stuffApi = Paths.get(outputPath + "/apis/StuffApi.kt");
        assertFileContains(
                stuffApi,
                "fun findStuff(): kotlin.collections.List<Stuff>"
        );
        assertFileNotContains(
                stuffApi,
                "fun findStuff(): Stuff"
        );
        assertFileContains(
                stuffApi,
                "fun findUniqueStuff(): kotlin.collections.Set<Stuff>"
        );
    }

    // to test attributes in the $ref (OpenAPI 3.1 spec)
    @Test
    public void attributesInRef() throws IOException {
        File output = Files.createTempDirectory("test_attributes").toFile().getCanonicalFile();
        output.deleteOnExit();

        KotlinSpringServerCodegen codegen = new KotlinSpringServerCodegen();
        codegen.setOutputDir(output.getAbsolutePath());

        new DefaultGenerator().opts(new ClientOptInput()
                        .openAPI(TestUtils.parseSpec("src/test/resources/3_1/issue_17726.yaml"))
                        .config(codegen))
                .generate();

        String outputPath = output.getAbsolutePath() + "/src/main/kotlin/org/openapitools";
        Path order = Paths.get(outputPath + "/model/Order.kt");
        assertFileContains(
                order,
                "@get:Size(max=50)"
        );
    }

    @DataProvider(name = "dollarEscapeTest")
    private Object[][] createData() {
        return new Object[][]{
                new Object[]{JAXRS_SPEC},
                new Object[]{JAVALIN5},
                new Object[]{JAVALIN6},
        };
    }
    @Test(description = "Issue #20960", dataProvider = "dollarEscapeTest")
    public void givenSchemaObjectPropertyNameContainsDollarSignWhenGenerateThenDollarSignIsProperlyEscapedInAnnotation(String library) throws Exception {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        KotlinServerCodegen codegen = new KotlinServerCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        Map<String, Object> properties = new HashMap<>();
        properties.put(CodegenConstants.LIBRARY, library);
        properties.put(CodegenConstants.ENUM_PROPERTY_NAMING, CodegenConstants.ENUM_PROPERTY_NAMING_TYPE.UPPERCASE.toString());
        properties.put(INTERFACE_ONLY, true);
        properties.put(RETURN_RESPONSE, true);
        properties.put(API_PACKAGE, "com.toasttab.service.scim.api");
        properties.put(MODEL_PACKAGE, "com.toasttab.service.scim.models");
        properties.put(PACKAGE_NAME, "com.toasttab.service.scim");
        codegen.additionalProperties().putAll(properties);

        new DefaultGenerator().opts(new ClientOptInput()
                        .openAPI(TestUtils.parseSpec("src/test/resources/3_1/issue_20960.yaml"))
                        .config(codegen))
                .generate();

        String outputPath = output.getAbsolutePath() + "/src/main/kotlin/com/toasttab/service/scim";
        Path baseGroupModel = Paths.get(outputPath + "/models/BaseGroupMembersInner.kt");
        String baseGroupModelContent = Files.readString(baseGroupModel);
        KotlinLexer kotlinLexer = new KotlinLexer(CharStreams.fromString(baseGroupModelContent));
        KotlinTestUtils.SyntaxErrorListener syntaxErrorListener = new KotlinTestUtils.SyntaxErrorListener();
        kotlinLexer.addErrorListener(syntaxErrorListener);
        CommonTokenStream commonTokenStream = new CommonTokenStream(kotlinLexer);
        KotlinParser kotlinParser = new KotlinParser(commonTokenStream);
        kotlinParser.addErrorListener(syntaxErrorListener);
        ParseTree parseTree = kotlinParser.kotlinFile();
        ParseTreeWalker parseTreeWalker = new ParseTreeWalker();
        KotlinTestUtils.CustomKotlinParseListener customKotlinParseListener = new KotlinTestUtils.CustomKotlinParseListener();
        parseTreeWalker.walk(customKotlinParseListener, parseTree);
        Assert.assertTrue(syntaxErrorListener.getSyntaxErrorCount() == 0);
        Assert.assertTrue(customKotlinParseListener.getStringReferenceCount() == 0);
    }

    // ==================== Polymorphism and Discriminator Tests ====================

    @Test
    public void oneOfWithDiscriminator_generatesSealedClassWithDiscriminatorProperty() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        KotlinServerCodegen codegen = new KotlinServerCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(LIBRARY, JAVALIN6);

        new DefaultGenerator().opts(new ClientOptInput()
                        .openAPI(TestUtils.parseSpec("src/test/resources/3_1/polymorphism-and-discriminator.yaml"))
                        .config(codegen))
                .generate();

        String outputPath = output.getAbsolutePath() + "/src/main/kotlin/org/openapitools/server";
        Path petModel = Paths.get(outputPath + "/models/Pet.kt");

        // Pet should be a sealed class with Jackson polymorphism annotations and discriminator property
        assertFileContains(
                petModel,
                "sealed class Pet(",
                "open val petType: kotlin.String",
                "@com.fasterxml.jackson.annotation.JsonTypeInfo",
                "property = \"petType\"",
                "visible = true",
                "@com.fasterxml.jackson.annotation.JsonSubTypes"
        );
    }

    @Test
    public void oneOfWithDiscriminator_generatesChildrenWithOverrideDiscriminatorProperty() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        KotlinServerCodegen codegen = new KotlinServerCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(LIBRARY, JAVALIN6);

        new DefaultGenerator().opts(new ClientOptInput()
                        .openAPI(TestUtils.parseSpec("src/test/resources/3_1/polymorphism-and-discriminator.yaml"))
                        .config(codegen))
                .generate();

        String outputPath = output.getAbsolutePath() + "/src/main/kotlin/org/openapitools/server";

        // Cat should have petType as overridden non-nullable String with default value
        Path catModel = Paths.get(outputPath + "/models/Cat.kt");
        assertFileContains(
                catModel,
                "data class Cat(",
                "override val petType: kotlin.String = \"cat\"",
                ") : Pet(petType = petType)"
        );
        // Should NOT be nullable
        assertFileNotContains(
                catModel,
                "petType: kotlin.String?",
                "petType: kotlin.Any"
        );

        // Dog should have petType as overridden non-nullable String with default value
        Path dogModel = Paths.get(outputPath + "/models/Dog.kt");
        assertFileContains(
                dogModel,
                "data class Dog(",
                "override val petType: kotlin.String = \"dog\"",
                ") : Pet(petType = petType)"
        );
        // Should NOT be nullable
        assertFileNotContains(
                dogModel,
                "petType: kotlin.String?",
                "petType: kotlin.Any"
        );
    }

    @Test
    public void allOfWithDiscriminator_generatesSealedClassWithProperties() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        KotlinServerCodegen codegen = new KotlinServerCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(LIBRARY, JAVALIN6);

        new DefaultGenerator().opts(new ClientOptInput()
                        .openAPI(TestUtils.parseSpec("src/test/resources/3_1/polymorphism-allof-and-discriminator.yaml"))
                        .config(codegen))
                .generate();

        String outputPath = output.getAbsolutePath() + "/src/main/kotlin/org/openapitools/server";
        Path petModel = Paths.get(outputPath + "/models/Pet.kt");

        // Pet should be a sealed class WITH properties (allOf pattern)
        assertFileContains(
                petModel,
                "sealed class Pet(",
                "open val name: kotlin.String",
                "open val petType: kotlin.String",
                "@com.fasterxml.jackson.annotation.JsonTypeInfo",
                "visible = true"
        );
    }

    @Test
    public void allOfWithDiscriminator_generatesChildrenWithOverrideProperties() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        KotlinServerCodegen codegen = new KotlinServerCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(LIBRARY, JAVALIN6);

        new DefaultGenerator().opts(new ClientOptInput()
                        .openAPI(TestUtils.parseSpec("src/test/resources/3_1/polymorphism-allof-and-discriminator.yaml"))
                        .config(codegen))
                .generate();

        String outputPath = output.getAbsolutePath() + "/src/main/kotlin/org/openapitools/server";

        // Cat should use override for inherited properties and pass them to parent constructor
        Path catModel = Paths.get(outputPath + "/models/Cat.kt");
        assertFileContains(
                catModel,
                "data class Cat(",
                "override val name: kotlin.String",
                "override val petType: kotlin.String",
                ") : Pet(name = name, petType = petType)"
        );

        // Dog should use override for inherited properties and pass them to parent constructor
        Path dogModel = Paths.get(outputPath + "/models/Dog.kt");
        assertFileContains(
                dogModel,
                "data class Dog(",
                "override val name: kotlin.String",
                "override val petType: kotlin.String",
                ") : Pet(name = name, petType = petType)"
        );
    }

    @Test
    public void allOfWithDiscriminator_javalinAbstractMode_generatesAbstractParent() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        KotlinServerCodegen codegen = new KotlinServerCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(LIBRARY, JAVALIN6);
        codegen.additionalProperties().put(INHERITANCE_MODE, INHERITANCE_MODE_ABSTRACT);

        new DefaultGenerator().opts(new ClientOptInput()
                        .openAPI(TestUtils.parseSpec("src/test/resources/3_1/polymorphism-allof-and-discriminator.yaml"))
                        .config(codegen))
                .generate();

        String outputPath = output.getAbsolutePath() + "/src/main/kotlin/org/openapitools/server";
        Path petModel = Paths.get(outputPath + "/models/Pet.kt");

        assertFileContains(
                petModel,
                "abstract class Pet(",
                "open val name: kotlin.String",
                "open val petType: kotlin.String"
        );
        assertFileNotContains(
                petModel,
                "sealed class Pet("
        );
    }

    @Test
    public void allOfWithDiscriminator_javalinNoneMode_generatesFlatModels() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        KotlinServerCodegen codegen = new KotlinServerCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(LIBRARY, JAVALIN6);
        codegen.additionalProperties().put(INHERITANCE_MODE, INHERITANCE_MODE_NONE);

        new DefaultGenerator().opts(new ClientOptInput()
                        .openAPI(TestUtils.parseSpec("src/test/resources/3_1/polymorphism-allof-and-discriminator.yaml"))
                        .config(codegen))
                .generate();

        String outputPath = output.getAbsolutePath() + "/src/main/kotlin/org/openapitools/server";
        Path petModel = Paths.get(outputPath + "/models/Pet.kt");
        Path catModel = Paths.get(outputPath + "/models/Cat.kt");

        assertFileContains(petModel, "data class Pet(");
        assertFileNotContains(
                petModel,
                "sealed class Pet(",
                "abstract class Pet(",
                "@com.fasterxml.jackson.annotation.JsonTypeInfo",
                "@com.fasterxml.jackson.annotation.JsonSubTypes"
        );

        assertFileContains(catModel, "data class Cat(");
        assertFileNotContains(catModel, ") : Pet(");
    }

    @Test
    public void oneOfWithDiscriminator_javalinCompositionMode_generatesWrapperParent() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        KotlinServerCodegen codegen = new KotlinServerCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(LIBRARY, JAVALIN6);
        codegen.additionalProperties().put(INHERITANCE_MODE, INHERITANCE_MODE_COMPOSITION);

        new DefaultGenerator().opts(new ClientOptInput()
                        .openAPI(TestUtils.parseSpec("src/test/resources/3_1/polymorphism-and-discriminator.yaml"))
                        .config(codegen))
                .generate();

        String outputPath = output.getAbsolutePath() + "/src/main/kotlin/org/openapitools/server";
        Path petModel = Paths.get(outputPath + "/models/Pet.kt");
        Path catModel = Paths.get(outputPath + "/models/Cat.kt");

        assertFileContains(
                petModel,
                "data class Pet(",
                "val value: kotlin.Any"
        );
        assertFileNotContains(
                petModel,
                "sealed class Pet",
                "abstract class Pet",
                "@com.fasterxml.jackson.annotation.JsonTypeInfo",
                "@com.fasterxml.jackson.annotation.JsonSubTypes"
        );

        assertFileContains(catModel, "data class Cat(");
        assertFileNotContains(catModel, ") : Pet(");
    }

    @Test
    public void allOfWithDiscriminator_jaxrsAbstractMode_generatesAbstractParentAndOverrideChildren() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        KotlinServerCodegen codegen = new KotlinServerCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(LIBRARY, JAXRS_SPEC);
        codegen.additionalProperties().put(INHERITANCE_MODE, INHERITANCE_MODE_ABSTRACT);

        new DefaultGenerator().opts(new ClientOptInput()
                        .openAPI(TestUtils.parseSpec("src/test/resources/3_1/polymorphism-allof-and-discriminator.yaml"))
                        .config(codegen))
                .generate();

        String outputPath = output.getAbsolutePath() + "/src/main/kotlin/org/openapitools/server";

        Path petModel = Paths.get(outputPath + "/models/Pet.kt");
        assertFileContains(
                petModel,
                "abstract class Pet(",
                "open val name: kotlin.String",
                "open val petType: kotlin.String"
        );
        assertFileNotContains(
                petModel,
                "interface Pet"
        );

        Path catModel = Paths.get(outputPath + "/models/Cat.kt");
        assertFileContains(
                catModel,
                "override val name: kotlin.String",
                "override val petType: kotlin.String",
                ") : Pet(name = name, petType = petType)"
        );
    }

    @Test
    public void polymorphismWithoutDiscriminator_generatesRegularDataClass() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        KotlinServerCodegen codegen = new KotlinServerCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(LIBRARY, JAVALIN6);

        new DefaultGenerator().opts(new ClientOptInput()
                        .openAPI(TestUtils.parseSpec("src/test/resources/3_1/polymorphism.yaml"))
                        .config(codegen))
                .generate();

        String outputPath = output.getAbsolutePath() + "/src/main/kotlin/org/openapitools/server";
        Path petModel = Paths.get(outputPath + "/models/Pet.kt");

        // Without discriminator, Pet should be a regular data class (not sealed)
        assertFileContains(
                petModel,
                "data class Pet("
        );
        assertFileNotContains(
                petModel,
                "sealed class",
                "@com.fasterxml.jackson.annotation.JsonTypeInfo",
                "@com.fasterxml.jackson.annotation.JsonSubTypes"
        );
    }

    @Test
    public void fixJacksonJsonTypeInfoInheritance_canBeDisabled() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        KotlinServerCodegen codegen = new KotlinServerCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(LIBRARY, JAVALIN6);
        codegen.additionalProperties().put(KotlinServerCodegen.Constants.FIX_JACKSON_JSON_TYPE_INFO_INHERITANCE, false);

        new DefaultGenerator().opts(new ClientOptInput()
                        .openAPI(TestUtils.parseSpec("src/test/resources/3_1/polymorphism-and-discriminator.yaml"))
                        .config(codegen))
                .generate();

        String outputPath = output.getAbsolutePath() + "/src/main/kotlin/org/openapitools/server";
        Path petModel = Paths.get(outputPath + "/models/Pet.kt");

        // When fixJacksonJsonTypeInfoInheritance is false and parent has no properties,
        // visible should be false for oneOf pattern
        assertFileContains(
                petModel,
                "visible = false"
        );
    }

    // ==================== useTags for JAXRS-SPEC ====================

    @Test
    public void useTags_false_classNameFromTagsAndRootPathForJaxrsSpecLibrary() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        KotlinServerCodegen codegen = new KotlinServerCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(LIBRARY, JAXRS_SPEC);
        codegen.additionalProperties().put(USE_TAGS, false);

        new DefaultGenerator().opts(new ClientOptInput()
                        .openAPI(TestUtils.parseSpec("src/test/resources/2_0/petstore.yaml"))
                        .config(codegen))
                .generate();

        String outputPath = output.getAbsolutePath() + "/src/main/kotlin/org/openapitools/server";
        Path petApi = Paths.get(outputPath + "/apis/PetApi.kt");

        assertFileContains(petApi,
                "class PetApi",
                "@Path(\"/pet\")",
                "@Path(\"/findByStatus\")",
                "@Path(\"/{petId}\")"
        );
        assertFileNotContains(petApi, "@Path(\"/pet\")".replace("/pet", "/store"));
    }

    @Test
    public void useTags_notSpecified_behavesLikeUseTagsTrueForJaxrsSpecLibrary() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        KotlinServerCodegen codegen = new KotlinServerCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(LIBRARY, JAXRS_SPEC);
        // useTags intentionally NOT set — must default to true

        new DefaultGenerator().opts(new ClientOptInput()
                        .openAPI(TestUtils.parseSpec("src/test/resources/2_0/petstore.yaml"))
                        .config(codegen))
                .generate();

        String outputPath = output.getAbsolutePath() + "/src/main/kotlin/org/openapitools/server";
        Path petApi = Paths.get(outputPath + "/apis/PetApi.kt");

        assertFileContains(petApi,
                "class PetApi",
                "@Path(\"/pet\")",
                "@Path(\"/findByStatus\")",
                "@Path(\"/{petId}\")"
        );
        assertFileNotContains(petApi, "@Path(\"/\")");
        assertFileNotContains(petApi, "@Path(\"/store\")");
    }

    // ==================== useTags for all libraries ====================

    @Test
    public void useTags_false_groupsByFirstPathSegment() {
        KotlinServerCodegen codegen = new KotlinServerCodegen();
        codegen.additionalProperties().put(LIBRARY, JAVALIN6);
        codegen.additionalProperties().put(USE_TAGS, false);
        codegen.processOpts();

        CodegenOperation co = new CodegenOperation();
        co.operationId = "findByStatus";
        Map<String, List<CodegenOperation>> groups = new HashMap<>();

        codegen.addOperationToGroup("Pet", "/pet/findByStatus", new Operation(), co, groups);

        Assert.assertTrue(groups.containsKey("pet"));
        Assert.assertEquals(co.baseName, "pet");
    }

    @Test
    public void useTags_false_rootPath_groupsAsDefault() {
        KotlinServerCodegen codegen = new KotlinServerCodegen();
        codegen.additionalProperties().put(LIBRARY, JAVALIN6);
        codegen.additionalProperties().put(USE_TAGS, false);
        codegen.processOpts();

        CodegenOperation co = new CodegenOperation();
        co.operationId = "getRoot";
        Map<String, List<CodegenOperation>> groups = new HashMap<>();

        codegen.addOperationToGroup("Root", "/", new Operation(), co, groups);

        Assert.assertTrue(groups.containsKey("default"));
        Assert.assertEquals(co.baseName, "default");
    }

    @Test
    public void useTags_false_pathParamOnly_groupsAsDefault() {
        KotlinServerCodegen codegen = new KotlinServerCodegen();
        codegen.additionalProperties().put(LIBRARY, JAVALIN6);
        codegen.additionalProperties().put(USE_TAGS, false);
        codegen.processOpts();

        CodegenOperation co = new CodegenOperation();
        co.operationId = "getById";
        Map<String, List<CodegenOperation>> groups = new HashMap<>();

        codegen.addOperationToGroup("Resource", "/{uuid}", new Operation(), co, groups);

        Assert.assertTrue(groups.containsKey("default"));
        Assert.assertEquals(co.baseName, "default");
    }

    @Test
    public void delegatePattern_canBeEnabled() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        KotlinServerCodegen codegen = new KotlinServerCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(LIBRARY, KTOR);
        codegen.additionalProperties().put(DELEGATE_PATTERN, true);

        new DefaultGenerator().opts(new ClientOptInput()
                        .openAPI(TestUtils.parseSpec("src/test/resources/3_0/petstore.yaml"))
                        .config(codegen))
                .generate();

        String outputPath = output.getAbsolutePath() + "/src/main/kotlin/org/openapitools/server";
        Path petApi = Paths.get(outputPath + "/apis/PetApi.kt");

        // API should use the delegate
        assertFileContains(
                petApi,
                "val petApiDelegate: PetApiDelegate? by call.delegates"
        );

        // Delegate interface should be generated
        Path petApiDelegate = Paths.get(outputPath + "/apis/PetApiDelegate.kt");
        Assert.assertTrue(Files.exists(petApiDelegate));
        assertFileContains(
                petApiDelegate,
                "interface PetApiDelegate"
        );

        // Supporting files should be generated
        String infraPath = output.getAbsolutePath() + "/src/main/kotlin/org/openapitools/server/infrastructure";
        Assert.assertTrue(Files.exists(Paths.get(infraPath + "/Delegates.kt")));
        Assert.assertTrue(Files.exists(Paths.get(infraPath + "/AppDelegates.kt")));
        Assert.assertTrue(Files.exists(Paths.get(infraPath + "/BadParameterException.kt")));
        Assert.assertTrue(Files.exists(Paths.get(infraPath + "/APINotImplementedException.kt")));
    }

    @Test
    public void inheritanceMode_defaultIsSealedForKtor() {
        KotlinServerCodegen codegen = new KotlinServerCodegen();
        codegen.additionalProperties().put(LIBRARY, KTOR);

        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(INHERITANCE_MODE), INHERITANCE_MODE_SEALED);
        Assert.assertEquals(codegen.additionalProperties().get(X_INHERITANCE_MODE_SEALED), Boolean.TRUE);
        Assert.assertEquals(codegen.additionalProperties().get(X_INHERITANCE_MODE_ABSTRACT), Boolean.FALSE);
        Assert.assertEquals(codegen.additionalProperties().get(X_INHERITANCE_MODE_COMPOSITION), Boolean.FALSE);
        Assert.assertEquals(codegen.additionalProperties().get(X_INHERITANCE_MODE_NONE), Boolean.FALSE);
    }

    @Test
    public void inheritanceMode_defaultIsAbstractForJaxrsSpec() {
        KotlinServerCodegen codegen = new KotlinServerCodegen();
        codegen.additionalProperties().put(LIBRARY, JAXRS_SPEC);

        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(INHERITANCE_MODE), INHERITANCE_MODE_ABSTRACT);
        Assert.assertEquals(codegen.additionalProperties().get(X_INHERITANCE_MODE_ABSTRACT), Boolean.TRUE);
        Assert.assertEquals(codegen.additionalProperties().get(X_INHERITANCE_MODE_SEALED), Boolean.FALSE);
    }

    @Test
    public void inheritanceMode_rejectsInvalidValue() {
        KotlinServerCodegen codegen = new KotlinServerCodegen();
        codegen.additionalProperties().put(LIBRARY, KTOR);
        codegen.additionalProperties().put(INHERITANCE_MODE, "invalid-mode");

        IllegalArgumentException ex = Assert.expectThrows(IllegalArgumentException.class, codegen::processOpts);
        Assert.assertTrue(ex.getMessage().contains("Invalid inheritanceMode value 'invalid-mode'"));
    }

    @Test
    public void inheritanceMode_rejectsSealedForJaxrsSpec() {
        KotlinServerCodegen codegen = new KotlinServerCodegen();
        codegen.additionalProperties().put(LIBRARY, JAXRS_SPEC);
        codegen.additionalProperties().put(INHERITANCE_MODE, INHERITANCE_MODE_SEALED);

        IllegalArgumentException ex = Assert.expectThrows(IllegalArgumentException.class, codegen::processOpts);
        Assert.assertTrue(ex.getMessage().contains("does not support inheritanceMode 'sealed'"));
    }

    @Test
    public void inheritanceMode_acceptsCompositionForJaxrsSpec() {
        KotlinServerCodegen codegen = new KotlinServerCodegen();
        codegen.additionalProperties().put(LIBRARY, JAXRS_SPEC);
        codegen.additionalProperties().put(INHERITANCE_MODE, INHERITANCE_MODE_COMPOSITION);

        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(INHERITANCE_MODE), INHERITANCE_MODE_COMPOSITION);
        Assert.assertEquals(codegen.additionalProperties().get(X_INHERITANCE_MODE_COMPOSITION), Boolean.TRUE);
        Assert.assertEquals(codegen.additionalProperties().get(X_INHERITANCE_MODE_ABSTRACT), Boolean.FALSE);
    }

    @Test
    public void inheritanceMode_acceptsNoneForJaxrsSpec() {
        KotlinServerCodegen codegen = new KotlinServerCodegen();
        codegen.additionalProperties().put(LIBRARY, JAXRS_SPEC);
        codegen.additionalProperties().put(INHERITANCE_MODE, INHERITANCE_MODE_NONE);

        codegen.processOpts();

        Assert.assertEquals(codegen.additionalProperties().get(INHERITANCE_MODE), INHERITANCE_MODE_NONE);
        Assert.assertEquals(codegen.additionalProperties().get(X_INHERITANCE_MODE_NONE), Boolean.TRUE);
        Assert.assertEquals(codegen.additionalProperties().get(X_INHERITANCE_MODE_ABSTRACT), Boolean.FALSE);
    }

    @Test
    public void delegatePattern_enumWireValue() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        var codegen = new KotlinServerCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(LIBRARY, KTOR);
        codegen.additionalProperties().put(DELEGATE_PATTERN, true);
        codegen.additionalProperties().put(RESOURCES, false);
        codegen.inlineSchemaOption().put("RESOLVE_INLINE_ENUMS","true");

        new DefaultGenerator().opts(new ClientOptInput()
                        .openAPI(TestUtils.parseSpec("src/test/resources/3_1/enum-wire-value.yaml"))
                        .config(codegen))
                .generate();

        String outputPath = output.getAbsolutePath() + "/src/main/kotlin/org/openapitools/server";


        Path statusFile = Paths.get(outputPath + "/models/TestEnumStatusParameter.kt");

        assertFileContains(
                statusFile,
                "companion object {",
                "fun fromValue(value: kotlin.String): TestEnumStatusParameter =",
                "values().firstOrNull { it.value == value } ?: throw IllegalArgumentException(\"No enum constant TestEnumStatusParameter.$value\")"
        );

        // Check parameter extraction
        // Check Enum definition in the API file (inline enum)
        Path apiPath = Paths.get(outputPath + "/apis/DefaultApi.kt");
        assertFileContains(
                apiPath,
                "val status = call.request.queryParameters[\"status\"]?.let { runCatching { TestEnumStatusParameter.fromValue(it) }.getOrElse { throw BadParameterException(message = \"Invalid enum value for parameter status: $it\", parameterName = \"status\") } }"
        );
    }

    @Test
    public void delegatePattern_headerParamPrimitiveConversion() throws IOException {
        // Regression test for https://github.com/OpenAPITools/openapi-generator/issues/24214
        // Header params were converted with `it.to<fully.qualified.DataType>()`
        // (e.g. `it.tokotlin.Boolean()`, `it.tojava.math.BigDecimal()`), which is not valid Kotlin.
        // The correct String extension is `it.to<SimpleName>()`.
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        KotlinServerCodegen codegen = new KotlinServerCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(LIBRARY, KTOR);
        codegen.additionalProperties().put(DELEGATE_PATTERN, true);

        new DefaultGenerator().opts(new ClientOptInput()
                        .openAPI(TestUtils.parseSpec("src/test/resources/3_0/kotlin/issue24214-ktor-header-param-conversion.yaml"))
                        .config(codegen))
                .generate();

        Path apiPath = Paths.get(output.getAbsolutePath()
                + "/src/main/kotlin/org/openapitools/server/apis/DefaultApi.kt");

        assertFileContains(
                apiPath,
                "val boolHeader = call.request.headers[\"bool-header\"]?.let { runCatching { it.toBoolean() }",
                "val intHeader = call.request.headers[\"int-header\"]?.let { runCatching { it.toInt() }",
                "val longHeader = call.request.headers[\"long-header\"]?.let { runCatching { it.toLong() }",
                "val doubleHeader = call.request.headers[\"double-header\"]?.let { runCatching { it.toDouble() }",
                "val numberHeader = call.request.headers[\"number-header\"]?.let { runCatching { it.toBigDecimal() }"
        );

        // The old, uncompilable fully-qualified conversions must be gone.
        assertFileNotContains(
                apiPath,
                "it.tokotlin.Boolean()",
                "it.tokotlin.Int()",
                "it.tokotlin.Long()",
                "it.tokotlin.Double()",
                "it.tojava.math.BigDecimal()"
        );
    }


    @Test
    public void testFloatingPointMultipleOfValidationUsesTolerance() throws IOException {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        KotlinServerCodegen codegen = new KotlinServerCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(LIBRARY, KTOR);
        codegen.additionalProperties().put(DELEGATE_PATTERN, true);

        new DefaultGenerator().opts(new ClientOptInput()
                        .openAPI(TestUtils.parseSpec("src/test/resources/3_1/kotlin/multiple-of-validation.yaml"))
                        .config(codegen))
                .generate();

        String modelPath = output.getAbsolutePath() + "/src/main/kotlin/org/openapitools/server/models/MultipleOfModel.kt";
        Path multipleOfModel = Paths.get(modelPath);

        Assert.assertTrue(Files.exists(multipleOfModel));

        // Floating-point multipleOf validation must tolerate JVM representation error.
        assertFileContains(
                multipleOfModel,
                "if (kotlin.math.abs((floatVal.toDouble() / 0.1) - kotlin.math.round(floatVal.toDouble() / 0.1)) > 1.0e-6) {",
                "if (kotlin.math.abs((doubleVal.toDouble() / 0.1) - kotlin.math.round(doubleVal.toDouble() / 0.1)) > 1.0e-10) {",
                "if (kotlin.math.abs((it.toString().toDouble() / 0.1) - kotlin.math.round(it.toString().toDouble() / 0.1)) > 1.0e-6) {",
                "if (kotlin.math.abs((value.toString().toDouble() / 0.01) - kotlin.math.round(value.toString().toDouble() / 0.01)) > 1.0e-10) {"
        );

        assertFileNotContains(
                multipleOfModel,
                "if (floatVal % 0.1 != 0) {",
                "if (doubleVal % 0.1 != 0) {"
        );

        // Integral multipleOf validation remains exact.
        assertFileContains(
                multipleOfModel,
                "if (intVal % 2 != 0) {"
        );
    }

    // ==================== Cross-tag path shadowing (issue #23414) ====================

    @Test
    public void testCommonPathDoesNotShadowOtherTags_jaxrsSpec() throws IOException {
        // Regression test for https://github.com/OpenAPITools/openapi-generator/issues/23414
        // tag-one owns /foo/bar/one and /foo/bar/two
        // tag-two owns /foo/bar/three and /baz/bar/four
        // TagOneApi must NOT have @Path("/foo/bar") at class level because that would shadow
        // TagTwoApi's /foo/bar/three route in the JAX-RS runtime.
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        KotlinServerCodegen codegen = new KotlinServerCodegen();
        codegen.setOutputDir(output.getAbsolutePath());
        codegen.additionalProperties().put(LIBRARY, JAXRS_SPEC);
        codegen.additionalProperties().put(USE_TAGS, true);

        new DefaultGenerator().opts(new ClientOptInput()
                        .openAPI(TestUtils.parseSpec("src/test/resources/3_0/issue_23414.yaml"))
                        .config(codegen))
                .generate();

        String outputPath = output.getAbsolutePath() + "/src/main/kotlin/org/openapitools/server";
        Path tagOneApi = Paths.get(outputPath + "/apis/TagOneApi.kt");
        Path tagTwoApi = Paths.get(outputPath + "/apis/TagTwoApi.kt");

        // TagOneApi must NOT have @Path("/foo/bar") — this shadows TagTwoApi's /foo/bar/three
        assertFileNotContains(tagOneApi, "@Path(\"/foo/bar\")");

        // All operations must still be reachable with their full paths
        assertFileContains(tagOneApi, "@Path(\"/foo/bar/one\")", "@Path(\"/foo/bar/two\")");
        assertFileContains(tagTwoApi, "@Path(\"/foo/bar/three\")", "@Path(\"/baz/bar/four\")");
    }
}
