package io.swagger.codegen.bash;

import io.swagger.codegen.CodegenOperation;
import io.swagger.codegen.CodegenParameter;
import io.swagger.codegen.DefaultCodegen;
import io.swagger.codegen.languages.BashClientCodegen;
import io.swagger.oas.models.OpenAPI;
import io.swagger.oas.models.Operation;
import io.swagger.parser.v3.OpenAPIV3Parser;
import org.testng.Assert;
import org.testng.annotations.Test;

@SuppressWarnings("static-method")
public class BashTest {

    @Test(description = "test basic petstore operation with Bash extensions")
    public void petstoreOperationTest() {
        // TODO: update test file.
        final OpenAPI openAPI = new OpenAPIV3Parser().read("src/test/resources/2_0/petstore-bash.json");
        final DefaultCodegen codegen = new BashClientCodegen();
        final Operation findPetsByStatusOperation = openAPI.getPaths().get("/pet/findByStatus").getGet();
        final CodegenOperation op = codegen.fromOperation("/pet/findByStatus", "GET", findPetsByStatusOperation, openAPI.getComponents().getSchemas(), openAPI);
        Assert.assertTrue(op.vendorExtensions.containsKey("x-code-samples"));
        Assert.assertEquals(op.vendorExtensions.get("x-bash-codegen-description"), "Multiple status 'values' can be provided with comma separated strings");
    }

    @Test(description = "test basic petstore operation with example body")
    public void petstoreParameterExampleTest() {
        // TODO: update test file.
        final OpenAPI openAPI = new OpenAPIV3Parser().read("src/test/resources/2_0/petstore-bash.json");
        final DefaultCodegen codegen = new BashClientCodegen();
        final Operation addPetOperation = openAPI.getPaths().get("/pet").getPost();
        final CodegenOperation op = codegen.fromOperation("/pet", "POST", addPetOperation, openAPI.getComponents().getSchemas(), openAPI);
        Assert.assertEquals(op.bodyParams.size(), 1);
        CodegenParameter pet = op.bodyParams.get(0);
        Assert.assertTrue(pet.vendorExtensions.containsKey("x-codegen-body-example"));
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
