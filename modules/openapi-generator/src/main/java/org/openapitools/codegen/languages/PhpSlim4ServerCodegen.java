/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
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

import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.security.SecurityScheme;
import io.swagger.v3.oas.models.servers.Server;
import org.apache.commons.lang3.StringEscapeUtils;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;
import java.util.*;

import static org.openapitools.codegen.utils.StringUtils.*;

public class PhpSlim4ServerCodegen extends PhpSlimServerCodegen {
    private static final Logger LOGGER = LoggerFactory.getLogger(PhpSlim4ServerCodegen.class);

    public static final String PSR7_IMPLEMENTATION = "psr7Implementation";

    protected String psr7Implementation = "slim-psr7";
    protected List<Map<String, String>> composerPackages = new ArrayList<Map<String, String>>();
    protected List<Map<String, String>> composerDevPackages = new ArrayList<Map<String, String>>();

    public PhpSlim4ServerCodegen() {
        super();

        outputFolder = "generated-code" + File.separator + "slim4";
        embeddedTemplateDir = templateDir = "php-slim4-server";

        // override cliOptions from AbstractPhpCodegen
        updateOption(AbstractPhpCodegen.VARIABLE_NAMING_CONVENTION, "camelCase");

        // Slim 4 can use any PSR-7 implementation
        // https://www.slimframework.com/docs/v4/concepts/value-objects.html
        CliOption psr7Option = new CliOption(PSR7_IMPLEMENTATION,
                "Slim 4 provides its own PSR-7 implementation so that it works out of the box. However, you are free to replace Slim’s default PSR-7 objects with a third-party implementation. Ref: https://www.slimframework.com/docs/v4/concepts/value-objects.html");

        psr7Option.addEnum("slim-psr7", "Slim PSR-7 Message implementation")
                .addEnum("nyholm-psr7", "Nyholm PSR-7 Message implementation")
                .addEnum("guzzle-psr7", "Guzzle PSR-7 Message implementation")
                .addEnum("zend-diactoros", "Zend Diactoros PSR-7 Message implementation")
                .setDefault("slim-psr7");

        cliOptions.add(psr7Option);
    }

    @Override
    public String getName() {
        return "php-slim4";
    }

    @Override
    public String getHelp() {
        return "Generates a PHP Slim 4 Framework server library.";
    }

    @Override
    public void processOpts() {
        super.processOpts();

        if (additionalProperties.containsKey(PSR7_IMPLEMENTATION)) {
            this.setPsr7Implementation((String) additionalProperties.get(PSR7_IMPLEMENTATION));
        }

        additionalProperties.put(PSR7_IMPLEMENTATION, getPsr7Implementation());

        // init Composer dependencies
        addComposerPackage("php", "^7.1");
        addComposerPackage("slim/slim", "^4.0");
        addComposerPackage("dyorg/slim-token-authentication", "dev-slim4");

        // add PSR-7 implementation to Composer dependencies
        switch (getPsr7Implementation()) {
            case "slim-psr7":
                addComposerPackage("slim/psr7", "^0.4.0");
                break;
            case "nyholm-psr7":
                addComposerPackage("nyholm/psr7", "^1.1.0");
                addComposerPackage("nyholm/psr7-server", "^0.3.0");
                break;
            case "guzzle-psr7":
                addComposerPackage("guzzlehttp/psr7", "^1.6.1");
                addComposerPackage("http-interop/http-factory-guzzle", "^1.0.0");
                break;
            case "zend-diactoros":
                addComposerPackage("zendframework/zend-diactoros", "^2.1.3");
                break;
            default:
                addComposerPackage("slim/psr7", "^0.4.0");
                LOGGER.warn("\"" + getPsr7Implementation() + "\" is invalid \"psr7Implementation\" codegen option. Default \"slim-psr7\" used instead.");
        }

        // init Composer dev dependencies
        addComposerPackage("phpunit/phpunit", "^6.0 || ^7.0", true);
        addComposerPackage("overtrue/phplint", "^1.0", true);
        addComposerPackage("squizlabs/php_codesniffer", "^3.0", true);

        additionalProperties.put("composerPackages", composerPackages);
        additionalProperties.put("composerDevPackages", composerDevPackages);
    }

    /**
     * Set PSR-7 implementation package.
     * Ref: https://www.slimframework.com/docs/v4/concepts/value-objects.html
     *
     * @param psr7Implementation PSR-7 implementation package
     */
    public void setPsr7Implementation(String psr7Implementation) {
        switch (psr7Implementation) {
            case "slim-psr7":
            case "nyholm-psr7":
            case "guzzle-psr7":
            case "zend-diactoros":
                this.psr7Implementation = psr7Implementation;
                break;
            default:
                this.psr7Implementation = "slim-psr7";
                LOGGER.warn("\"" + (String) psr7Implementation + "\" is invalid \"psr7Implementation\" argument. Default \"slim-psr7\" used instead.");
        }
    }

    /**
     * Returns PSR-7 implementation package.
     *
     * @return PSR-7 implementation package
     */
    public String getPsr7Implementation() {
        return this.psr7Implementation;
    }
}
