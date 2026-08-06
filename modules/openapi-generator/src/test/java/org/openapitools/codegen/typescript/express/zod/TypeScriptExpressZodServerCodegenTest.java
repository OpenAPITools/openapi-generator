package org.openapitools.codegen.typescript.express.zod;

import org.openapitools.codegen.CodegenType;
import org.openapitools.codegen.SupportingFile;
import org.openapitools.codegen.languages.TypeScriptExpressZodServerCodegen;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.util.Set;
import java.util.stream.Collectors;

public class TypeScriptExpressZodServerCodegenTest {

    @Test(description = "exposes the expected generator metadata")
    public void generatorMetadata() {
        final TypeScriptExpressZodServerCodegen codegen = new TypeScriptExpressZodServerCodegen();
        Assert.assertEquals(codegen.getName(), "typescript-express-zod-server");
        Assert.assertEquals(codegen.getTag(), CodegenType.SERVER);
    }

    @Test(description = "emits everything as supporting files, no per-model/per-api templates")
    public void processOptsRegistersSupportingFiles() {
        final TypeScriptExpressZodServerCodegen codegen = new TypeScriptExpressZodServerCodegen();
        codegen.processOpts();

        Set<String> outputs = codegen.supportingFiles().stream()
                .map(sf -> sf.getFolder().isEmpty()
                        ? sf.getDestinationFilename()
                        : sf.getFolder() + "/" + sf.getDestinationFilename())
                .collect(Collectors.toSet());
        Assert.assertEquals(outputs, Set.of(
                "types.ts", "schemas.ts", "index.ts",
                "dtos/types.ts", "dtos/mappers.ts", "dtos/index.ts",
                "express/handlers.ts", "express/types.ts", "express/router-factory.ts",
                "express/errors.ts", "express/auth-middleware.ts", "express/index.ts"),
                "unexpected supporting files: " + outputs);
        Assert.assertTrue(codegen.modelTemplateFiles().isEmpty());
        Assert.assertTrue(codegen.apiTemplateFiles().isEmpty());
    }
}
