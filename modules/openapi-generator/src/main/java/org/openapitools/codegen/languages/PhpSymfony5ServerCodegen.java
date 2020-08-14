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

import org.openapitools.codegen.SupportingFile;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import java.io.File;

public class PhpSymfony5ServerCodegen extends PhpSymfonyServerCodegen {
    private static final Logger LOGGER = LoggerFactory.getLogger(PhpSymfony5ServerCodegen.class);

    public PhpSymfony5ServerCodegen() {
        super();

        embeddedTemplateDir = templateDir = "php-symfony5-server";

        // remove phpLegacySupport option because Symfony5 requires PHP >= 7.2.5
        removeOption(PHP_LEGACY_SUPPORT);
    }

    @Override
    public String getName() {
        return "php-symfony5";
    }

    @Override
    public String getHelp() {
        return "Generates a PHP Symfony 5 server bundle.";
    }

    @Override
    public void processOpts() {
        super.processOpts();

        final String configDir = "Resources" + File.separator + "config";
        final String dependencyInjectionDir = "DependencyInjection";

        supportingFiles.clear();
        // supportingFiles.add(new SupportingFile("Controller.mustache", toSrcPath(controllerPackage, srcBasePath), "Controller.php"));
        // supportingFiles.add(new SupportingFile("Bundle.mustache", "", bundleClassName + ".php"));
        // supportingFiles.add(new SupportingFile("Extension.mustache", dependencyInjectionDir, bundleExtensionName + ".php"));
        // supportingFiles.add(new SupportingFile("ApiPass.mustache", dependencyInjectionDir + File.separator + "Compiler", bundleName + "ApiPass.php"));
        // supportingFiles.add(new SupportingFile("ApiServer.mustache", toSrcPath(apiPackage, srcBasePath), "ApiServer.php"));

        // Serialization components
        // supportingFiles.add(new SupportingFile("serialization/SerializerInterface.mustache", toSrcPath(servicePackage, srcBasePath), "SerializerInterface.php"));
        // supportingFiles.add(new SupportingFile("serialization/JmsSerializer.mustache", toSrcPath(servicePackage, srcBasePath), "JmsSerializer.php"));
        // supportingFiles.add(new SupportingFile("serialization/StrictJsonDeserializationVisitor.mustache", toSrcPath(servicePackage, srcBasePath), "StrictJsonDeserializationVisitor.php"));
        // supportingFiles.add(new SupportingFile("serialization/TypeMismatchException.mustache", toSrcPath(servicePackage, srcBasePath), "TypeMismatchException.php"));
        // Validation components
        // supportingFiles.add(new SupportingFile("validation/ValidatorInterface.mustache", toSrcPath(servicePackage, srcBasePath), "ValidatorInterface.php"));
        // supportingFiles.add(new SupportingFile("validation/SymfonyValidator.mustache", toSrcPath(servicePackage, srcBasePath), "SymfonyValidator.php"));

        // Testing components
        // supportingFiles.add(new SupportingFile("testing/phpunit.xml.mustache", "", "phpunit.xml.dist"));
        // supportingFiles.add(new SupportingFile("testing/pom.xml", "", "pom.xml"));
        // supportingFiles.add(new SupportingFile("testing/AppKernel.mustache", toSrcPath(testsPackage, srcBasePath), "AppKernel.php"));
        // supportingFiles.add(new SupportingFile("testing/ControllerTest.mustache", toSrcPath(controllerTestsPackage, srcBasePath), "ControllerTest.php"));
        // supportingFiles.add(new SupportingFile("testing/test_config.yml", toSrcPath(testsPackage, srcBasePath), "test_config.yml"));

        // supportingFiles.add(new SupportingFile("routing.mustache", configDir, "routing.yml"));
        // supportingFiles.add(new SupportingFile("services.mustache", configDir, "services.yml"));
        supportingFiles.add(new SupportingFile("composer.mustache", "", "composer.json"));
        // supportingFiles.add(new SupportingFile("autoload.mustache", "", "autoload.php"));
        // supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));

        // supportingFiles.add(new SupportingFile(".travis.yml", "", ".travis.yml"));
        // supportingFiles.add(new SupportingFile(".php_cs.dist", "", ".php_cs.dist"));
        // supportingFiles.add(new SupportingFile(".coveralls.yml", "", ".coveralls.yml"));
        // supportingFiles.add(new SupportingFile("git_push.sh.mustache", "", "git_push.sh"));
    }
}
