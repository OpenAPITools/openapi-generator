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

import io.swagger.models.properties.*;
import io.swagger.v3.oas.models.*;
import io.swagger.v3.oas.models.media.*;
import io.swagger.v3.oas.models.parameters.*;
import org.openapitools.codegen.*;

import java.io.File;
import java.util.*;

import io.swagger.v3.oas.models.media.*;
import io.swagger.v3.oas.models.PathItem;
import io.swagger.v3.oas.models.PathItem.HttpMethod;
import io.swagger.v3.oas.models.*;
import io.swagger.v3.oas.models.parameters.*;
import io.swagger.v3.core.util.Yaml;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class PhpLaravelServerCodegen extends AbstractPhpCodegen implements CodegenConfig {
    @SuppressWarnings("hiding")
    protected String apiVersion = "1.0.0";
    protected String variableNamingConvention = "camelCase";

    /**
     * Configures the type of generator.
     *
     * @return the CodegenType for this generator
     * @see org.openapitools.codegen.CodegenType
     */
    public CodegenType getTag() {
        return CodegenType.SERVER;
    }

    /**
     * Configures a friendly name for the generator.  This will be used by the generator
     * to select the library with the -g flag.
     *
     * @return the friendly name for the generator
     */
    public String getName() {
        return "php-laravel";
    }

    /**
     * Returns human-friendly help for the generator.  Provide the consumer with help
     * tips, parameters here
     *
     * @return A string value for the help message
     */
    public String getHelp() {
        return "Generates a PHP laravel server library.";
    }

    /**
     * Class constructor
     */
    public PhpLaravelServerCodegen() {
        super();

        embeddedTemplateDir = templateDir = "php-laravel";

        /*
         * packPath
         */
        invokerPackage = "php-laravel";
        packagePath = "";
        outputFolder = packagePath + File.separator + srcBasePath;

        /*
         * Api Package.  Optional, if needed, this can be used in templates
         */
        apiPackage = "app.Http.Controllers";

        /*
         * Model Package.  Optional, if needed, this can be used in templates
         */
        modelPackage = "models";

        // template files want to be ignored
        modelTemplateFiles.clear();
        apiTestTemplateFiles.clear();
        apiDocTemplateFiles.clear();
        modelDocTemplateFiles.clear();

        /*
         * Additional Properties.  These values can be passed to the templates and
         * are available in models, apis, and supporting files
         */
        additionalProperties.put("apiVersion", apiVersion);

        /*
         * Supporting Files.  You can write single files for the generator with the
         * entire object tree available.  If the input file has a suffix of `.mustache
         * it will be processed by the template engine.  Otherwise, it will be copied
         */
        supportingFiles.add(new SupportingFile("composer.mustache", outputFolder, "composer.json"));
        supportingFiles.add(new SupportingFile("README.md", outputFolder, "README.md"));
        supportingFiles.add(new SupportingFile("artisan", outputFolder, "artisan"));

        supportingFiles.add(new SupportingFile("bootstrap/cache/.gitignore", outputFolder + File.separator + "bootstrap" + File.separator + "cache", ".gitignore"));
        supportingFiles.add(new SupportingFile("bootstrap/app.php", outputFolder + File.separator + "bootstrap", "app.php"));
        supportingFiles.add(new SupportingFile("bootstrap/testingAutoload.php", outputFolder + File.separator + "bootstrap", "testingAutoload.php"));

        /* /public/ */
        supportingFiles.add(new SupportingFile("public/.htaccess", outputFolder + File.separator + "public", ".htaccess"));
        supportingFiles.add(new SupportingFile("public/favicon.ico", outputFolder + File.separator + "public", "favicon.ico"));
        supportingFiles.add(new SupportingFile("public/index.php", outputFolder + File.separator + "public", "index.php"));
        supportingFiles.add(new SupportingFile("public/robots.txt", outputFolder + File.separator + "public", "robots.txt"));

        /* routes */
        supportingFiles.add(new SupportingFile("routes/api.mustache", outputFolder + File.separator + "routes", "api.php"));
        supportingFiles.add(new SupportingFile("routes/web.mustache", outputFolder + File.separator + "routes", "web.php"));

        /* /app/Http/Controllers/ */
        supportingFiles.add(new SupportingFile("app/Http/Controllers/Controller.php", outputFolder + File.separator + "app" + File.separator + "Http" + File.separator + "Controllers", "Controller.php"));
    }

    // override with any special post-processing
    @Override
    public Map<String, Object> postProcessOperations(Map<String, Object> objs) {
        @SuppressWarnings("unchecked")
        Map<String, Object> objectMap = (Map<String, Object>) objs.get("operations");
        @SuppressWarnings("unchecked")
        List<CodegenOperation> operations = (List<CodegenOperation>) objectMap.get("operation");

        for (CodegenOperation op : operations) {
            op.httpMethod = op.httpMethod.toLowerCase();
            // check to see if the path contains ".", which is not supported by PHP laravel
            // ref: https://github.com/swagger-api/swagger-codegen/issues/6897
            if (op.path != null && op.path.contains(".")) {
                throw new IllegalArgumentException("'.' (dot) is not supported by PHP laravel. Please refer to https://github.com/swagger-api/swagger-codegen/issues/6897 for more info.");
            }
        }

        // sort the endpoints in ascending to avoid the route priority issure.
        // https://github.com/swagger-api/swagger-codegen/issues/2643
        Collections.sort(operations, new Comparator<CodegenOperation>() {
            @Override
            public int compare(CodegenOperation lhs, CodegenOperation rhs) {
                return lhs.path.compareTo(rhs.path);
            }
        });

        return objs;
    }

    @Override
    public String toApiName(String name) {
        if (name.isEmpty()) {
            return "DefaultController";
        }

        return camelize(name, false) + "Controller";
    }

    protected String controllerFileFolder() {
        return (outputFolder + File.separator + srcBasePath + File.separator + "app" + File.separator + "Http" + File.separator + "Controllers");
    }

    @Override
    public String apiFilename(String templateName, String tag) {
        String suffix = apiTemplateFiles().get(templateName);
        if (templateName.equals("api.mustache")) {
            return controllerFileFolder() + '/' + toControllerName(tag) + suffix;
        }

        return apiFileFolder() + '/' + toApiFilename(tag) + suffix;
    }

    protected String toControllerName(String name) {
        if (name.isEmpty()) {
            return "DefaultController";
        }

        return camelize(name, false) + "Controller";
    }
}
