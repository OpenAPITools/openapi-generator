package io.swagger.codegen.bash;

import io.swagger.codegen.CodegenOperation;
import io.swagger.codegen.CodegenParameter;
import io.swagger.codegen.DefaultCodegen;
import io.swagger.codegen.languages.BashClientCodegen;
import io.swagger.models.Operation;
import io.swagger.models.Swagger;
import io.swagger.parser.SwaggerParser;
import org.testng.Assert;
import org.testng.annotations.Test;

@SuppressWarnings("static-method")
public class BashTest {

    @Test(description = "test basic petstore operation with Bash extensions")
    public void petstoreOperationTest() {

        final Swagger swagger 
            = new SwaggerParser()
                .read("src/test/resources/2_0/petstore-bash.json");
        final DefaultCodegen codegen = new BashClientCodegen();
        final Operation findPetsByStatusOperation
            = swagger.getPath("/pet/findByStatus").getGet();

        final CodegenOperation op
            = codegen.fromOperation(
                "/pet/findByStatus",
                "GET",
                findPetsByStatusOperation,
                swagger.getDefinitions(),
                swagger);

        Assert.assertTrue(
            op.vendorExtensions.containsKey("x-code-samples"));

        Assert.assertEquals(
            op.vendorExtensions.get("x-bash-codegen-description"),
            "Multiple status 'values' can be provided with comma separated strings");

    }

    @Test(description = "test basic petstore operation with example body")
    public void petstoreParameterExampleTest() {

        final Swagger swagger 
            = new SwaggerParser()
                .read("src/test/resources/2_0/petstore-bash.json");
        final DefaultCodegen codegen = new BashClientCodegen();
        final Operation addPetOperation
            = swagger.getPath("/pet").getPost();

        final CodegenOperation op
            = codegen.fromOperation(
                "/pet",
                "POST",
                addPetOperation,
                swagger.getDefinitions(),
                swagger);

        Assert.assertEquals(op.bodyParams.size(), 1);

        CodegenParameter pet = op.bodyParams.get(0);

        Assert.assertTrue(pet.vendorExtensions
                            .containsKey("x-codegen-body-example"));

    }


    @Test(description = "test Bash client codegen escapeText method")
    public void escapeTextTest() {
        final DefaultCodegen codegen = new BashClientCodegen();


        Assert.assertEquals(codegen.escapeText("\\/"), "/");

        Assert.assertEquals(codegen.escapeText("\\"), "\\\\");


        ((BashClientCodegen)codegen).setProcessMarkdown(false);

        Assert.assertEquals(codegen.escapeText("__Bold text__"), 
                            "__Bold text__");

        Assert.assertEquals(codegen.escapeText("**Bold text**"), 
                            "**Bold text**");

        Assert.assertEquals(codegen.escapeText("*Italic text*"), 
                            "*Italic text*");

        Assert.assertEquals(codegen.escapeText("_Italic text_"), 
                            "_Italic text_");


        ((BashClientCodegen)codegen).setProcessMarkdown(true);

        Assert.assertEquals(codegen.escapeText("__Bold text__"), 
                            "$(tput bold) Bold text $(tput sgr0)");

        Assert.assertEquals(codegen.escapeText("**Bold text**"), 
                            "$(tput bold) Bold text $(tput sgr0)");

        Assert.assertEquals(codegen.escapeText("*Italic text*"), 
                            "$(tput dim) Italic text $(tput sgr0)");

        Assert.assertEquals(codegen.escapeText("_Italic text_"), 
                            "$(tput dim) Italic text $(tput sgr0)");

        Assert.assertEquals(codegen.escapeText("# SECTION NAME"), 
            "\n$(tput bold)$(tput setaf 7)SECTION NAME$(tput sgr0)");

        Assert.assertEquals(codegen.escapeText("## SECTION NAME"), 
            "\n$(tput bold)$(tput setaf 7)SECTION NAME$(tput sgr0)");

        Assert.assertEquals(codegen.escapeText("### SECTION NAME"), 
            "\n$(tput bold)$(tput setaf 7)SECTION NAME$(tput sgr0)");

        Assert.assertEquals(codegen.escapeText(
                                "```\nnice -n 100 mvn test\n```"),
                                "\n---\nnice -n 100 mvn test\n---"); 
    }

    @Test(description = "test Bash client codegen escapeUnsafeCharacters method")
    public void escapeUnsafeCharactersTest() {
        final DefaultCodegen codegen = new BashClientCodegen();

        Assert.assertEquals(codegen.escapeUnsafeCharacters("`no backticks`"), 
                                                          "'no backticks'");


    }
    
    @Test(description = "test Bash client codegen escapeReservedWord method")
    public void escapeReservedWordTest() {
        final DefaultCodegen codegen = new BashClientCodegen();

        Assert.assertEquals(codegen.escapeReservedWord("case"), "_case");
    }


}
