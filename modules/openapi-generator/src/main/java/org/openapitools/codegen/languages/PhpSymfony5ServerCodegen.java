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

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

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
}
