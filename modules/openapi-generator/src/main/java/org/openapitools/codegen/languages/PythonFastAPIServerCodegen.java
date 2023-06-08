/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
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

package org.openapitools.codegen.languages;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.JsonSerializer;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializerProvider;
import com.fasterxml.jackson.databind.module.SimpleModule;
import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.Schema;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.*;
import org.openapitools.codegen.meta.GeneratorMetadata;
import org.openapitools.codegen.meta.Stability;
import org.openapitools.codegen.model.ModelMap;
import org.openapitools.codegen.model.ModelsMap;
import org.openapitools.codegen.model.OperationMap;
import org.openapitools.codegen.model.OperationsMap;
import org.openapitools.codegen.utils.ModelUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.IOException;
import java.util.*;

import static org.openapitools.codegen.utils.StringUtils.underscore;

public class PythonFastAPIServerCodegen extends AbstractPythonCodegen {
    private static class SnakeCaseKeySerializer extends JsonSerializer<String> {
        @Override
        public void serialize(String value, JsonGenerator gen, SerializerProvider serializers) throws IOException {
            gen.writeFieldName(underscore(value));
        }
    }

    private static class PythonBooleanSerializer extends JsonSerializer<Boolean> {
        @Override
        public void serialize(Boolean value, JsonGenerator gen, SerializerProvider serializers) throws IOException {
            gen.writeNumber(value ? 1 : 0);
        }
    }

    // An object mapper that is used to convert an example string to
    // a "python-compliant" example string (keys in snake case, boolean as 1/0).
    final ObjectMapper MAPPER = new ObjectMapper();

    final Logger LOGGER = LoggerFactory.getLogger(PythonFastAPIServerCodegen.class);

    protected String sourceFolder;

    private static final String SERVER_PORT = "serverPort";
    private static final String NAME = "python-fastapi";
    private static final int DEFAULT_SERVER_PORT = 8080;
    private static final String DEFAULT_PACKAGE_NAME = "openapi_server";
    private static final String DEFAULT_SOURCE_FOLDER = "src";
    private static final String DEFAULT_PACKAGE_VERSION = "1.0.0";

    @Override
    public CodegenType getTag() {
        return CodegenType.SERVER;
    }

    @Override
    public String getHelp() {
        return "Generates a Python FastAPI server (beta). Models are defined with the pydantic library";
    }

    public PythonFastAPIServerCodegen() {
        super();

        generatorMetadata = GeneratorMetadata.newBuilder(generatorMetadata)
                .stability(Stability.BETA)
                .build();

        SimpleModule simpleModule = new SimpleModule();
        simpleModule.addKeySerializer(String.class, new SnakeCaseKeySerializer());
        simpleModule.addSerializer(Boolean.class, new PythonBooleanSerializer());
        MAPPER.registerModule(simpleModule);

        /*
         * Additional Properties.  These values can be passed to the templates and
         * are available in models, apis, and supporting files
         */
        additionalProperties.put("serverPort", DEFAULT_SERVER_PORT);
        additionalProperties.put(CodegenConstants.SOURCE_FOLDER, DEFAULT_SOURCE_FOLDER);
        additionalProperties.put(CodegenConstants.PACKAGE_NAME, DEFAULT_PACKAGE_NAME);

        languageSpecificPrimitives.add("List");
        languageSpecificPrimitives.add("Dict");
        typeMapping.put("array", "List");
        typeMapping.put("map", "Dict");

        outputFolder = "generated-code" + File.separator + NAME;
        modelTemplateFiles.put("model.mustache", ".py");
        apiTemplateFiles.put("api.mustache", ".py");
        embeddedTemplateDir = templateDir = NAME;
        apiPackage = "apis";
        modelPackage = "models";
        testPackage = "tests";
        apiTestTemplateFiles().put("api_test.mustache", ".py");

        cliOptions.add(new CliOption(CodegenConstants.PACKAGE_NAME, "python package name (convention: snake_case).")
                .defaultValue(DEFAULT_PACKAGE_NAME));
        cliOptions.add(new CliOption(CodegenConstants.PACKAGE_VERSION, "python package version.")
                .defaultValue(DEFAULT_PACKAGE_VERSION));
        cliOptions.add(new CliOption(SERVER_PORT, "TCP port to listen to in app.run")
                .defaultValue(String.valueOf(DEFAULT_SERVER_PORT)));
        cliOptions.add(new CliOption(CodegenConstants.SOURCE_FOLDER, "directory for generated python source code")
                .defaultValue(DEFAULT_SOURCE_FOLDER));

    }

    @Override
    public void processOpts() {
        super.processOpts();

        if (additionalProperties.containsKey(CodegenConstants.PACKAGE_NAME)) {
            setPackageName((String) additionalProperties.get(CodegenConstants.PACKAGE_NAME));
        }

        if (additionalProperties.containsKey(CodegenConstants.SOURCE_FOLDER)) {
            this.sourceFolder = ((String) additionalProperties.get(CodegenConstants.SOURCE_FOLDER));
        }

        modelPackage = packageName + "." + modelPackage;
        apiPackage = packageName + "." + apiPackage;

        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("openapi.mustache", "", "openapi.yaml"));
        supportingFiles.add(new SupportingFile("main.mustache", String.join(File.separator, new String[]{sourceFolder, packageName.replace('.', File.separatorChar)}), "main.py"));
        supportingFiles.add(new SupportingFile("docker-compose.mustache", "", "docker-compose.yaml"));
        supportingFiles.add(new SupportingFile("Dockerfile.mustache", "", "Dockerfile"));
        supportingFiles.add(new SupportingFile("requirements.mustache", "", "requirements.txt"));
        supportingFiles.add(new SupportingFile("security_api.mustache", String.join(File.separator, new String[]{sourceFolder, packageName.replace('.', File.separatorChar)}), "security_api.py"));
        supportingFiles.add(new SupportingFile("extra_models.mustache", StringUtils.substringAfter(modelFileFolder(), outputFolder), "extra_models.py"));

        // Add __init__.py to all sub-folders under namespace pkg
        StringBuilder namespacePackagePath = new StringBuilder(String.join(File.separator, new String[]{sourceFolder, StringUtils.substringBefore(packageName, ".")}));
        for (String tmp : StringUtils.split(StringUtils.substringAfter(packageName, "."), '.')) {
            namespacePackagePath.append(File.separator).append(tmp);
            supportingFiles.add(new SupportingFile("__init__.mustache", namespacePackagePath.toString(), "__init__.py"));
        }
        supportingFiles.add(new SupportingFile("__init__.mustache", StringUtils.substringAfter(modelFileFolder(), outputFolder), "__init__.py"));
        supportingFiles.add(new SupportingFile("__init__.mustache", StringUtils.substringAfter(apiFileFolder(), outputFolder), "__init__.py"));

        supportingFiles.add(new SupportingFile("conftest.mustache", testPackage.replace('.', File.separatorChar), "conftest.py"));

        supportingFiles.add(new SupportingFile("gitignore.mustache", "", ".gitignore"));
        supportingFiles.add(new SupportingFile("pyproject_toml.mustache", "", "pyproject.toml"));
        supportingFiles.add(new SupportingFile("setup_cfg.mustache", "", "setup.cfg"));
        supportingFiles.add(new SupportingFile(".flake8.mustache", "", ".flake8"));
    }

    @Override
    public String getName() {
        return NAME;
    }

    @Override
    public String toModelImport(String name) {
        String modelImport;
        if (StringUtils.startsWithAny(name, "import", "from")) {
            modelImport = name;
        } else {
            modelImport = "from ";
            if (!"".equals(modelPackage())) {
                modelImport += modelPackage() + ".";
            }
            modelImport += toModelFilename(name) + " import " + name;
        }
        return modelImport;
    }

    @Override
    public String getTypeDeclaration(Schema p) {
        if (ModelUtils.isArraySchema(p)) {
            ArraySchema ap = (ArraySchema) p;
            Schema inner = ap.getItems();
            return getSchemaType(p) + "[" + getTypeDeclaration(inner) + "]";
        } else if (ModelUtils.isMapSchema(p)) {
            Schema inner = getAdditionalProperties(p);
            return getSchemaType(p) + "[str, " + getTypeDeclaration(inner) + "]";
        }
        return super.getTypeDeclaration(p);
    }

    @Override
    public OperationsMap postProcessOperationsWithModels(OperationsMap objs, List<ModelMap> allModels) {
        OperationMap operations = objs.getOperations();
        // Set will make sure that no duplicated items are used.
        Set<String> securityImports = new HashSet<>();
        if (operations != null) {
            List<CodegenOperation> ops = operations.getOperation();
            for (final CodegenOperation operation : ops) {
                List<CodegenResponse> responses = operation.responses;
                if (responses != null) {
                    for (final CodegenResponse resp : responses) {
                        // Convert "default" value (0) to OK (200).
                        if ("0".equals(resp.code)) {
                            resp.code = "200";
                        }
                    }
                }
                List<CodegenSecurity> securityMethods = operation.authMethods;
                if (securityMethods != null) {
                    for (final CodegenSecurity securityMethod : securityMethods) {
                        securityImports.add(securityMethod.name);
                    }
                }

                if (operation.requestBodyExamples != null) {
                    for (Map<String, String> example : operation.requestBodyExamples) {
                        if (example.get("contentType") != null && example.get("contentType").equals("application/json")) {
                            // Make an example dictionary more python-like (snake-case, etc.).
                            // If fails, use the original string.
                            try {
                                Map<String, Object> result = MAPPER.readValue(example.get("example"),
                                        new TypeReference<Map<String, Object>>() {
                                        });
                                operation.bodyParam.example = MAPPER.writeValueAsString(result);
                            } catch (IOException e) {
                                operation.bodyParam.example = example.get("example");
                            }
                        }
                    }
                }
            }
        }

        objs.put("securityImports", new ArrayList<>(securityImports));

        return objs;
    }

    @Override
    public Map<String, ModelsMap> postProcessAllModels(Map<String, ModelsMap> objs) {
        Map<String, ModelsMap> result = super.postProcessAllModels(objs);
        for (Map.Entry<String, ModelsMap> entry : result.entrySet()) {
            for (ModelMap mo : entry.getValue().getModels()) {
                CodegenModel cm = mo.getModel();
                // Add additional filename information for imports
                mo.put("pyImports", toPyImports(cm, cm.imports));
            }
        }
        return result;
    }

    private List<Map<String, String>> toPyImports(CodegenModel cm, Set<String> imports) {
        List<Map<String, String>> pyImports = new ArrayList<>();
        for (String im : imports) {
            if (!im.equals(cm.classname)) {
                HashMap<String, String> pyImport = new HashMap<>();
                pyImport.put("import", toModelImport(im));
                pyImports.add(pyImport);
            }
        }
        return pyImports;
    }

    @Override
    public Map<String, Object> postProcessSupportingFileData(Map<String, Object> objs) {
        generateYAMLSpecFile(objs);
        return objs;
    }

    @Override
    public String apiFileFolder() {
        return String.join(File.separator, new String[]{outputFolder, sourceFolder, apiPackage().replace('.', File.separatorChar)});
    }

    @Override
    public String modelFileFolder() {
        return String.join(File.separator, new String[]{outputFolder, sourceFolder, modelPackage().replace('.', File.separatorChar)});
    }

    @Override
    public void postProcess() {
        System.out.println("################################################################################");
        System.out.println("# Thanks for using OpenAPI Generator.                                          #");
        System.out.println("# Please consider donation to help us maintain this project \uD83D\uDE4F                 #");
        System.out.println("# https://opencollective.com/openapi_generator/donate                          #");
        System.out.println("#                                                                              #");
        System.out.println("# This generator's contributed by Nikita Vakula (https://github.com/krjakbrjak)#");
        System.out.println("# Please support his work directly via https://paypal.me/krjakbrjak  \uD83D\uDE4F        #");
        System.out.println("################################################################################");
    }

    @Override
    public String toRegularExpression(String pattern) {
        String regex = super.toRegularExpression(pattern);
        return StringUtils.substring(regex, 1, -1);
    }

    @Override
    public String generatorLanguageVersion() { return "3.7"; };
}
