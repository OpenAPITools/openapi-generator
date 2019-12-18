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
import org.openapitools.codegen.meta.GeneratorMetadata;
import org.openapitools.codegen.meta.Stability;
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

        generatorMetadata = GeneratorMetadata.newBuilder(generatorMetadata)
                .stability(Stability.STABLE)
                .build();

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

        // reset implementation flags
        additionalProperties.put("isSlimPsr7", Boolean.FALSE);
        additionalProperties.put("isNyholmPsr7", Boolean.FALSE);
        additionalProperties.put("isGuzzlePsr7", Boolean.FALSE);
        additionalProperties.put("isZendDiactoros", Boolean.FALSE);

        // set specific PSR-7 implementation flag
        switch (getPsr7Implementation()) {
            case "slim-psr7":
                additionalProperties.put("isSlimPsr7", Boolean.TRUE);
                break;
            case "nyholm-psr7":
                additionalProperties.put("isNyholmPsr7", Boolean.TRUE);
                break;
            case "guzzle-psr7":
                additionalProperties.put("isGuzzlePsr7", Boolean.TRUE);
                break;
            case "zend-diactoros":
                additionalProperties.put("isZendDiactoros", Boolean.TRUE);
                break;
            default:
                LOGGER.warn("\"" + getPsr7Implementation() + "\" is invalid \"psr7Implementation\" codegen option. Default \"slim-psr7\" used instead.");
                additionalProperties.put("isSlimPsr7", Boolean.TRUE);
        }

        // Slim 4 doesn't parse JSON body anymore we need to add suggested middleware
        // ref: https://www.slimframework.com/docs/v4/objects/request.html#the-request-body
        supportingFiles.add(new SupportingFile("json_body_parser_middleware.mustache", toSrcPath(invokerPackage + "\\Middleware", srcBasePath), "JsonBodyParserMiddleware.php"));
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
