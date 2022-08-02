package org.openapitools.codegen.typescript.typescriptnode;
import io.swagger.parser.OpenAPIParser;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.media.ObjectSchema;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.media.StringSchema;
import io.swagger.v3.parser.core.models.ParseOptions;
import org.openapitools.codegen.ClientOptInput;
import org.openapitools.codegen.CodegenModel;
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.config.GlobalSettings;
import org.openapitools.codegen.languages.TypeScriptNodeClientCodegen;
import org.openapitools.codegen.model.ModelMap;
import org.openapitools.codegen.model.ModelsMap;
import org.openapitools.codegen.model.OperationMap;
import org.openapitools.codegen.model.OperationsMap;
import org.testng.Assert;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;
import org.openapitools.codegen.CodegenConstants;

public class tryModel {
    public static void main(String[] args) throws IOException {
        String inputPath = "/Users/wechen/workspace/public-api/generated/openapi3/api.json";
        TypeScriptNodeClientCodegen codegen = new TypeScriptNodeClientCodegen();
        GlobalSettings.setProperty(CodegenConstants.SPLIT_RESPONSE_TYPES, "true");
        File output = Files.createTempDirectory("M").toFile().getCanonicalFile();
        OpenAPI openAPI = new OpenAPIParser()
                .readLocation(inputPath, null, new ParseOptions()).getOpenAPI();
        codegen.setOutputDir(output.getAbsolutePath());
        ClientOptInput input = new ClientOptInput()
                .openAPI(openAPI)
                .config(codegen);
        DefaultGenerator generator = new DefaultGenerator();
        Map<String, File> files = generator.opts(input).generate().stream()
                .collect(Collectors.toMap(File::getName, Function.identity()));
    }
}
