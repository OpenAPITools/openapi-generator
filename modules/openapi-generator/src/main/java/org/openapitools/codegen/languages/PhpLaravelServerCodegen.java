/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 * Copyright 2018 SmartBear Software
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

import org.openapitools.codegen.CodegenOperation;
import org.openapitools.codegen.CodegenType;
import org.openapitools.codegen.SupportingFile;
import org.openapitools.codegen.meta.features.*;

import java.io.File;
import java.util.*;

import static org.openapitools.codegen.utils.StringUtils.camelize;

public class PhpLaravelServerCodegen extends AbstractPhpCodegen {
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

        modifyFeatureSet(features -> features
                .includeDocumentationFeatures(DocumentationFeature.Readme)
                .wireFormatFeatures(EnumSet.of(WireFormatFeature.JSON, WireFormatFeature.XML))
                .securityFeatures(EnumSet.noneOf(SecurityFeature.class))
                .excludeGlobalFeatures(
                        GlobalFeature.XMLStructureDefinitions,
                        GlobalFeature.Callbacks,
                        GlobalFeature.LinkObjects,
                        GlobalFeature.ParameterStyling
                )
                .excludeSchemaSupportFeatures(
                        SchemaSupportFeature.Polymorphism
                )
        );

        embeddedTemplateDir = templateDir = "php-laravel";
        variableNamingConvention = "camelCase";

        /*
         * packPath
         */
        invokerPackage = "php-laravel";
        outputFolder = srcBasePath;

        /*
         * Api Package.  Optional, if needed, this can be used in templates
         */
        apiPackage = "app.Http.Controllers";

        /*
         * Model Package.  Optional, if needed, this can be used in templates
         */
        modelPackage = "app\\Models";

        // template files want to be ignored
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
        supportingFiles.add(new SupportingFile("package.json", outputFolder, "package.json"));
        supportingFiles.add(new SupportingFile("phpunit.xml", outputFolder, "phpunit.xml"));
        supportingFiles.add(new SupportingFile("webpack.mix.js", outputFolder, "webpack.mix.js"));
        supportingFiles.add(new SupportingFile(".env.example", outputFolder, ".env.example"));
        supportingFiles.add(new SupportingFile("server.php", outputFolder, "server.php"));

        supportingFiles.add(new SupportingFile("bootstrap/cache/gitignore", outputFolder + File.separator + "bootstrap" + File.separator + "cache", ".gitignore"));
        supportingFiles.add(new SupportingFile("bootstrap/app.php", outputFolder + File.separator + "bootstrap", "app.php"));
        supportingFiles.add(new SupportingFile("bootstrap/testingAutoload.php", outputFolder + File.separator + "bootstrap", "testingAutoload.php"));

        /* /public/ */
        supportingFiles.add(new SupportingFile("public/css/app.css", outputFolder + File.separator + "public" + File.separator + "css", "app.css"));
        supportingFiles.add(new SupportingFile("public/js/app.js", outputFolder + File.separator + "public" + File.separator + "js", "app.js"));
        supportingFiles.add(new SupportingFile("public/.htaccess", outputFolder + File.separator + "public", ".htaccess"));
        supportingFiles.add(new SupportingFile("public/favicon.ico", outputFolder + File.separator + "public", "favicon.ico"));
        supportingFiles.add(new SupportingFile("public/index.php", outputFolder + File.separator + "public", "index.php"));
        supportingFiles.add(new SupportingFile("public/robots.txt", outputFolder + File.separator + "public", "robots.txt"));
        supportingFiles.add(new SupportingFile("public/web.config", outputFolder + File.separator + "public", "web.config"));

        /* /routes/ */
        supportingFiles.add(new SupportingFile("routes/api.mustache", outputFolder + File.separator + "routes", "api.php"));
        supportingFiles.add(new SupportingFile("routes/web.mustache", outputFolder + File.separator + "routes", "web.php"));
        supportingFiles.add(new SupportingFile("routes/channels.mustache", outputFolder + File.separator + "routes", "channels.php"));
        supportingFiles.add(new SupportingFile("routes/console.mustache", outputFolder + File.separator + "routes", "console.php"));

        /* /app/Http/Controllers/ */
        supportingFiles.add(new SupportingFile("app/Http/Kernel.php", outputFolder + File.separator + "app" + File.separator + "Http", "Kernel.php"));
        supportingFiles.add(new SupportingFile("app/Http/Controllers/Controller.php", outputFolder + File.separator + "app" + File.separator + "Http" + File.separator + "Controllers", "Controller.php"));
        supportingFiles.add(new SupportingFile("app/Http/Middleware/EncryptCookies.php", outputFolder + File.separator + "app" + File.separator + "Http" + File.separator + "Middleware", "EncryptCookies.php"));
        supportingFiles.add(new SupportingFile("app/Http/Middleware/RedirectIfAuthenticated.php", outputFolder + File.separator + "app" + File.separator + "Http" + File.separator + "Middleware", "RedirectIfAuthenticated.php"));
        supportingFiles.add(new SupportingFile("app/Http/Middleware/TrimStrings.php", outputFolder + File.separator + "app" + File.separator + "Http" + File.separator + "Middleware", "TrimStrings.php"));
        supportingFiles.add(new SupportingFile("app/Http/Middleware/TrustProxies.php", outputFolder + File.separator + "app" + File.separator + "Http" + File.separator + "Middleware", "TrustProxies.php"));
        supportingFiles.add(new SupportingFile("app/Http/Middleware/VerifyCsrfToken.php", outputFolder + File.separator + "app" + File.separator + "Http" + File.separator + "Middleware", "VerifyCsrfToken.php"));

        // /app/Console
        supportingFiles.add(new SupportingFile("app/Console/Kernel.php", outputFolder + File.separator + "app" + File.separator + "Console", "Kernel.php"));
        // /app/Exceptions
        supportingFiles.add(new SupportingFile("app/Exceptions/Handler.php", outputFolder + File.separator + "app" + File.separator + "Exceptions", "Handler.php"));
        // /app/Providers
        supportingFiles.add(new SupportingFile("app/Providers/AppServiceProvider.php", outputFolder + File.separator + "app" + File.separator + "Providers", "AppServiceProvider.php"));
        supportingFiles.add(new SupportingFile("app/Providers/AuthServiceProvider.php", outputFolder + File.separator + "app" + File.separator + "Providers", "AuthServiceProvider.php"));
        supportingFiles.add(new SupportingFile("app/Providers/BroadcastServiceProvider.php", outputFolder + File.separator + "app" + File.separator + "Providers", "BroadcastServiceProvider.php"));
        supportingFiles.add(new SupportingFile("app/Providers/EventServiceProvider.php", outputFolder + File.separator + "app" + File.separator + "Providers", "EventServiceProvider.php"));
        supportingFiles.add(new SupportingFile("app/Providers/RouteServiceProvider.php", outputFolder + File.separator + "app" + File.separator + "Providers", "RouteServiceProvider.php"));

        // /database/
        supportingFiles.add(new SupportingFile("database/factories/UserFactory.php", outputFolder + File.separator + "database" + File.separator + "factories", "UserFactory.php"));
        supportingFiles.add(new SupportingFile("database/migrations/2014_10_12_000000_create_users_table.php", outputFolder + File.separator + "database" + File.separator + "migrations", "2014_10_12_000000_create_users_table.php"));
        supportingFiles.add(new SupportingFile("database/migrations/2014_10_12_100000_create_password_resets_table.php", outputFolder + File.separator + "database" + File.separator + "migrations", "2014_10_12_100000_create_password_resets_table.php"));
        supportingFiles.add(new SupportingFile("database/seeds/DatabaseSeeder.php", outputFolder + File.separator + "database" + File.separator + "seeds", "DatabaseSeeder.php"));
        supportingFiles.add(new SupportingFile("database/gitignore", outputFolder + File.separator + "database", ".gitignore"));

        // /config/
        supportingFiles.add(new SupportingFile("config/app.php", outputFolder + File.separator + "config", "app.php"));
        supportingFiles.add(new SupportingFile("config/auth.php", outputFolder + File.separator + "config", "auth.php"));
        supportingFiles.add(new SupportingFile("config/broadcasting.php", outputFolder + File.separator + "config", "broadcasting.php"));
        supportingFiles.add(new SupportingFile("config/cache.php", outputFolder + File.separator + "config", "cache.php"));
        supportingFiles.add(new SupportingFile("config/database.php", outputFolder + File.separator + "config", "database.php"));
        supportingFiles.add(new SupportingFile("config/filesystems.php", outputFolder + File.separator + "config", "filesystems.php"));
        supportingFiles.add(new SupportingFile("config/hashing.php", outputFolder + File.separator + "config", "hashing.php"));
        supportingFiles.add(new SupportingFile("config/logging.php", outputFolder + File.separator + "config", "logging.php"));
        supportingFiles.add(new SupportingFile("config/mail.php", outputFolder + File.separator + "config", "mail.php"));
        supportingFiles.add(new SupportingFile("config/queue.php", outputFolder + File.separator + "config", "queue.php"));
        supportingFiles.add(new SupportingFile("config/services.php", outputFolder + File.separator + "config", "services.php"));
        supportingFiles.add(new SupportingFile("config/session.php", outputFolder + File.separator + "config", "session.php"));
        supportingFiles.add(new SupportingFile("config/view.php", outputFolder + File.separator + "config", "view.php"));

        // /resources/
        supportingFiles.add(new SupportingFile("resources/assets/js/components/ExampleComponent.vue", outputFolder + File.separator + "resources" + File.separator + "assets" + File.separator + "js" + File.separator + "components", "ExampleComponent.vue"));
        supportingFiles.add(new SupportingFile("resources/assets/js/app.js", outputFolder + File.separator + "resources" + File.separator + "assets" + File.separator + "js", "app.js"));
        supportingFiles.add(new SupportingFile("resources/assets/js/bootstrap.js", outputFolder + File.separator + "resources" + File.separator + "assets" + File.separator + "js", "bootstrap.js"));
        supportingFiles.add(new SupportingFile("resources/assets/sass/_variables.scss", outputFolder + File.separator + "resources" + File.separator + "assets" + File.separator + "sass", "_variables.scss"));
        supportingFiles.add(new SupportingFile("resources/assets/sass/app.scss", outputFolder + File.separator + "resources" + File.separator + "assets" + File.separator + "sass", "app.scss"));
        supportingFiles.add(new SupportingFile("resources/lang/en/auth.php", outputFolder + File.separator + "resources" + File.separator + "lang" + File.separator + "en", "auth.php"));
        supportingFiles.add(new SupportingFile("resources/lang/en/pagination.php", outputFolder + File.separator + "resources" + File.separator + "lang" + File.separator + "en", "pagination.php"));
        supportingFiles.add(new SupportingFile("resources/lang/en/passwords.php", outputFolder + File.separator + "resources" + File.separator + "lang" + File.separator + "en", "passwords.php"));
        supportingFiles.add(new SupportingFile("resources/lang/en/validation.php", outputFolder + File.separator + "resources" + File.separator + "lang" + File.separator + "en", "validation.php"));
        supportingFiles.add(new SupportingFile("resources/views/welcome.blade.php", outputFolder + File.separator + "resources" + File.separator + "views", "welcome.blade.php"));

        // /storage/
        supportingFiles.add(new SupportingFile("storage/app/gitignore", outputFolder + File.separator + "storage" + File.separator + "app", ".gitignore"));
        supportingFiles.add(new SupportingFile("storage/app/public/gitignore", outputFolder + File.separator + "storage" + File.separator + "app" + File.separator + "public", ".gitignore"));
        supportingFiles.add(new SupportingFile("storage/framework/gitignore", outputFolder + File.separator + "storage" + File.separator + "framework", ".gitignore"));
        supportingFiles.add(new SupportingFile("storage/framework/cache/gitignore", outputFolder + File.separator + "storage" + File.separator + "framework" + File.separator + "cache", ".gitignore"));
        supportingFiles.add(new SupportingFile("storage/framework/sessions/gitignore", outputFolder + File.separator + "storage" + File.separator + "framework" + File.separator + "sessions", ".gitignore"));
        supportingFiles.add(new SupportingFile("storage/framework/testing/gitignore", outputFolder + File.separator + "storage" + File.separator + "framework" + File.separator + "testing", ".gitignore"));
        supportingFiles.add(new SupportingFile("storage/framework/views/gitignore", outputFolder + File.separator + "storage" + File.separator + "framework" + File.separator + "views", ".gitignore"));
        supportingFiles.add(new SupportingFile("storage/logs/gitignore", outputFolder + File.separator + "storage" + File.separator + "logs", ".gitignore"));

        // /tests/
        supportingFiles.add(new SupportingFile("tests/Feature/ExampleTest.php", outputFolder + File.separator + "tests" + File.separator + "Feature", "ExampleTest.php"));
        supportingFiles.add(new SupportingFile("tests/Unit/ExampleTest.php", outputFolder + File.separator + "tests" + File.separator + "Unit", "ExampleTest.php"));
        supportingFiles.add(new SupportingFile("tests/CreatesApplication.php", outputFolder + File.separator + "tests", "CreatesApplication.php"));
        supportingFiles.add(new SupportingFile("tests/TestCase.php", outputFolder + File.separator + "tests", "TestCase.php"));
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
            // check to see if the path contains ".", which is not supported by PHP laravel
            // ref: https://github.com/swagger-api/swagger-codegen/issues/6897
            if (op.path != null && op.path.contains(".")) {
                throw new IllegalArgumentException("'.' (dot) is not supported by PHP laravel. Please refer to https://github.com/swagger-api/swagger-codegen/issues/6897 for more info.");
            }

            if (op.hasProduces) {
                // need to escape */* values because they breakes current mustaches
                List<Map<String, String>> c = op.produces;
                for (Map<String, String> mediaType : c) {
                    if ("*/*".equals(mediaType.get("mediaType"))) {
                        mediaType.put("mediaType", "*_/_*");
                    }
                }
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
