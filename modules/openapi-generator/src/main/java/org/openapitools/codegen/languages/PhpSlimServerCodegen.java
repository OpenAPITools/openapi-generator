/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 * Copyright 2018 SmartBear Software
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.openapitools.codegen.languages;

import org.openapitools.codegen.CliOption;
import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.CodegenOperation;
import org.openapitools.codegen.CodegenType;
import org.openapitools.codegen.SupportingFile;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class PhpSlimServerCodegen extends AbstractPhpCodegen {
    private static final Logger LOGGER = LoggerFactory.getLogger(PhpSlimServerCodegen.class);

    public static final String PHPCS_STANDARD = "phpcsStandard";

    protected String groupId = "org.openapitools";
    protected String artifactId = "openapi-server";
    protected String phpcsStandard = "PSR12";

    public PhpSlimServerCodegen() {
        super();

        // clear import mapping (from default generator) as slim does not use it
        // at the moment
        importMapping.clear();

        variableNamingConvention = "camelCase";
        artifactVersion = "1.0.0";
        setInvokerPackage("OpenAPIServer");
        apiPackage = invokerPackage + "\\" + apiDirName;
        modelPackage = invokerPackage + "\\" + modelDirName;
        outputFolder = "generated-code" + File.separator + "slim";

        modelTestTemplateFiles.put("model_test.mustache", ".php");
        // no doc files
        modelDocTemplateFiles.clear();
        apiDocTemplateFiles.clear();

        embeddedTemplateDir = templateDir = "php-slim-server";

        additionalProperties.put(CodegenConstants.GROUP_ID, groupId);
        additionalProperties.put(CodegenConstants.ARTIFACT_ID, artifactId);

        // override cliOptions from AbstractPhpCodegen
        for (CliOption co : cliOptions) {
            if (co.getOpt().equals(AbstractPhpCodegen.VARIABLE_NAMING_CONVENTION)) {
                co.setDescription("naming convention of variable name, e.g. camelCase.");
                co.setDefault("camelCase");
                break;
            }
        }

        cliOptions.add(new CliOption(PHPCS_STANDARD, "PHP CodeSniffer <standard> option. Accepts name or path of the coding standard to use.")
                .defaultValue("PSR12"));
    }

    @Override
    public CodegenType getTag() {
        return CodegenType.SERVER;
    }

    @Override
    public String getName() {
        return "php-slim";
    }

    @Override
    public String getHelp() {
        return "Generates a PHP Slim Framework server library.";
    }

    @Override
    public String apiFileFolder() {
        if (apiPackage.matches("^" + invokerPackage + "\\\\*(.+)")) {
            // need to strip out invokerPackage from path
            return (outputFolder + File.separator + toSrcPath(apiPackage.replaceFirst("^" + invokerPackage + "\\\\*(.+)", "$1"), srcBasePath));
        }
        return (outputFolder + File.separator + toSrcPath(apiPackage, srcBasePath));
    }

    @Override
    public String modelFileFolder() {
        if (modelPackage.matches("^" + invokerPackage + "\\\\*(.+)")) {
            // need to strip out invokerPackage from path
            return (outputFolder + File.separator + toSrcPath(modelPackage.replaceFirst("^" + invokerPackage + "\\\\*(.+)", "$1"), srcBasePath));
        }
        return (outputFolder + File.separator + toSrcPath(modelPackage, srcBasePath));
    }

    @Override
    public void processOpts() {
        super.processOpts();

        if (additionalProperties.containsKey(PHPCS_STANDARD)) {
            this.setPhpcsStandard((String) additionalProperties.get(PHPCS_STANDARD));
        } else {
            additionalProperties.put(PHPCS_STANDARD, phpcsStandard);
        }

        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("composer.mustache", "", "composer.json"));
        supportingFiles.add(new SupportingFile("index.mustache", "", "index.php"));
        supportingFiles.add(new SupportingFile(".htaccess", "", ".htaccess"));
        supportingFiles.add(new SupportingFile("AbstractApiController.mustache", toSrcPath(invokerPackage, srcBasePath), toAbstractName("ApiController") + ".php"));
        supportingFiles.add(new SupportingFile("SlimRouter.mustache", toSrcPath(invokerPackage, srcBasePath), "SlimRouter.php"));
        supportingFiles.add(new SupportingFile("phpunit.xml.mustache", "", "phpunit.xml.dist"));
    }

    @Override
    public Map<String, Object> postProcessOperationsWithModels(Map<String, Object> objs, List<Object> allModels) {
        Map<String, Object> operations = (Map<String, Object>) objs.get("operations");
        List<CodegenOperation> operationList = (List<CodegenOperation>) operations.get("operation");
        escapeMediaType(operationList);
        return objs;
    }

    @Override
    public Map<String, Object> postProcessSupportingFileData(Map<String, Object> objs) {
        Map<String, Object> apiInfo = (Map<String, Object>) objs.get("apiInfo");
        List<HashMap<String, Object>> apiList = (List<HashMap<String, Object>>) apiInfo.get("apis");
        for (HashMap<String, Object> api : apiList) {
            HashMap<String, Object> operations = (HashMap<String, Object>) api.get("operations");
            List<CodegenOperation> operationList = (List<CodegenOperation>) operations.get("operation");

            // Sort operations to avoid static routes shadowing
            // ref: https://github.com/nikic/FastRoute/blob/master/src/DataGenerator/RegexBasedAbstract.php#L92-L101
            Collections.sort(operationList, new Comparator<CodegenOperation>() {
                @Override
                public int compare(CodegenOperation one, CodegenOperation another) {
                    if (one.getHasPathParams() && !another.getHasPathParams()) return 1;
                    if (!one.getHasPathParams() && another.getHasPathParams()) return -1;
                    return 0;
                }
            });
        }
        return objs;
    }

    public void setPhpcsStandard(String phpcsStandard) {
        this.phpcsStandard = phpcsStandard;
    }

}
