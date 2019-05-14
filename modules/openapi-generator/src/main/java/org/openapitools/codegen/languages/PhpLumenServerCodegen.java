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

import org.openapitools.codegen.CodegenOperation;
import org.openapitools.codegen.CodegenType;
import org.openapitools.codegen.SupportingFile;

import java.io.File;
import java.util.*;

public class PhpLumenServerCodegen extends AbstractPhpCodegen {
    @SuppressWarnings("hiding")
    protected String apiVersion = "1.0.0";

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
        return "php-lumen";
    }

    /**
     * Returns human-friendly help for the generator.  Provide the consumer with help
     * tips, parameters here
     *
     * @return A string value for the help message
     */
    public String getHelp() {
        return "Generates a PHP Lumen server library.";
    }

    public PhpLumenServerCodegen() {
        super();

        embeddedTemplateDir = templateDir = "php-lumen";

        /*
         * packPath
         */
        invokerPackage = "lumen";

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
        supportingFiles.add(new SupportingFile(".env.example", srcBasePath, ".env.example"));
        supportingFiles.add(new SupportingFile("storage_logs_.gitignore", srcBasePath + File.separator + "storage" + File.separator + "logs", ".gitignore"));
        supportingFiles.add(new SupportingFile("artisan", srcBasePath, "artisan"));
        supportingFiles.add(new SupportingFile("composer.mustache", srcBasePath, "composer.json"));
        supportingFiles.add(new SupportingFile("readme.md", srcBasePath, "readme.md"));
        supportingFiles.add(new SupportingFile("User.php", srcBasePath + File.separator + "app", "User.php"));
        supportingFiles.add(new SupportingFile("Kernel.php", srcBasePath + File.separator + "app" + File.separator + "Console", "Kernel.php"));
        supportingFiles.add(new SupportingFile(".gitkeep", srcBasePath + File.separator + "app" + File.separator + "Console" + File.separator + "Commands", ".gitkeep"));
        supportingFiles.add(new SupportingFile("Event.php", srcBasePath + File.separator + "app" + File.separator + "Events", "Event.php"));
        supportingFiles.add(new SupportingFile("ExampleEvent.php", srcBasePath + File.separator + "app" + File.separator + "Events", "ExampleEvent.php"));
        supportingFiles.add(new SupportingFile("Handler.php", srcBasePath + File.separator + "app" + File.separator + "Exceptions", "Handler.php"));
        supportingFiles.add(new SupportingFile("Controller.php", srcBasePath + File.separator + "app" + File.separator + "Http" + File.separator + "Controllers" + File.separator, "Controller.php"));
        supportingFiles.add(new SupportingFile("ExampleController.php", srcBasePath + File.separator + "app" + File.separator + "Http" + File.separator + "Controllers" + File.separator, "ExampleController.php"));
        supportingFiles.add(new SupportingFile("Authenticate.php", srcBasePath + File.separator + "app" + File.separator + "Http" + File.separator + "Middleware" + File.separator, "Authenticate.php"));
        supportingFiles.add(new SupportingFile("ExampleMiddleware.php", srcBasePath + File.separator + "app" + File.separator + "Http" + File.separator + "Middleware" + File.separator, "ExampleMiddleware.php"));
        supportingFiles.add(new SupportingFile("ExampleJob.php", srcBasePath + File.separator + "app" + File.separator + "Jobs", "ExampleJob.php"));
        supportingFiles.add(new SupportingFile("Job.php", srcBasePath + File.separator + "app" + File.separator + "Jobs", "Job.php"));
        supportingFiles.add(new SupportingFile("ExampleListener.php", srcBasePath + File.separator + "app" + File.separator + "Listeners", "ExampleListener.php"));
        supportingFiles.add(new SupportingFile("AppServiceProvider.php", srcBasePath + File.separator + "app" + File.separator + "Providers", "AppServiceProvider.php"));
        supportingFiles.add(new SupportingFile("AuthServiceProvider.php", srcBasePath + File.separator + "app" + File.separator + "Providers", "AuthServiceProvider.php"));
        supportingFiles.add(new SupportingFile("EventServiceProvider.php", srcBasePath + File.separator + "app" + File.separator + "Providers", "EventServiceProvider.php"));
        supportingFiles.add(new SupportingFile("app.php", srcBasePath + File.separator + "bootstrap", "app.php"));
        supportingFiles.add(new SupportingFile("ModelFactory.php", srcBasePath + File.separator + "database" + File.separator + "factories", "ModelFactory.php"));
        supportingFiles.add(new SupportingFile(".gitkeep", srcBasePath + File.separator + "database" + File.separator + "migrations", ".gitkeep"));
        supportingFiles.add(new SupportingFile("DatabaseSeeder.php", srcBasePath + File.separator + "database" + File.separator + "seeds", "DatabaseSeeder.php"));
        supportingFiles.add(new SupportingFile(".htaccess", srcBasePath + File.separator + "public", ".htaccess"));
        supportingFiles.add(new SupportingFile("index.php", srcBasePath + File.separator + "public", "index.php"));
        supportingFiles.add(new SupportingFile(".gitkeep", srcBasePath + File.separator + "resources" + File.separator + "views", ".gitkeep"));
        supportingFiles.add(new SupportingFile("routes.mustache", srcBasePath + File.separator + "routes", "web.php"));
        supportingFiles.add(new SupportingFile("ExampleTest.php", srcBasePath + File.separator + "tests", "ExampleTest.php"));
        supportingFiles.add(new SupportingFile("TestCase.php", srcBasePath + File.separator + "tests", "TestCase.php"));
    }

    // override with any special post-processing
    @Override
    public Map<String, Object> postProcessOperationsWithModels(Map<String, Object> objs, List<Object> allModels) {
        @SuppressWarnings("unchecked")
        Map<String, Object> objectMap = (Map<String, Object>) objs.get("operations");
        @SuppressWarnings("unchecked")
        List<CodegenOperation> operations = (List<CodegenOperation>) objectMap.get("operation");

        for (CodegenOperation op : operations) {
            op.httpMethod = op.httpMethod.toLowerCase(Locale.ROOT);
        }

        // sort the endpoints in ascending to avoid the route priority issure. 
        Collections.sort(operations, new Comparator<CodegenOperation>() {
            @Override
            public int compare(CodegenOperation lhs, CodegenOperation rhs) {
                return lhs.path.compareTo(rhs.path);
            }
        });

        escapeMediaType(operations);

        return objs;
    }
}
