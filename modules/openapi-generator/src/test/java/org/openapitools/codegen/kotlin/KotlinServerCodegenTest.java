package org.openapitools.codegen.kotlin;

import lombok.Getter;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.ParseTreeWalker;
import org.checkerframework.checker.units.qual.C;
import org.openapitools.codegen.ClientOptInput;
import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.antlr4.KotlinLexer;
import org.openapitools.codegen.antlr4.KotlinParser;
import org.openapitools.codegen.antlr4.KotlinParserBaseListener;
import org.openapitools.codegen.languages.KotlinServerCodegen;
import org.openapitools.codegen.languages.KotlinSpringServerCodegen;
import org.testng.Assert;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.Map;

import static org.openapitools.codegen.CodegenConstants.*;
import static org.openapitools.codegen.TestUtils.assertFileContains;
import static org.openapitools.codegen.TestUtils.assertFileNotContains;
import static org.openapitools.codegen.languages.AbstractKotlinCodegen.USE_JAKARTA_EE;
import static org.openapitools.codegen.languages.KotlinServerCodegen.Constants.*;
import static org.openapitools.codegen.languages.features.BeanValidationFeatures.USE_BEANVALIDATION;

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
}
