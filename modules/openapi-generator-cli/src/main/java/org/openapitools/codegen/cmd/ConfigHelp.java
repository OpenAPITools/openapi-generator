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

package org.openapitools.codegen.cmd;

import io.airlift.airline.Command;
import io.airlift.airline.Option;
import org.openapitools.codegen.CliOption;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.CodegenConfigLoader;
import org.openapitools.codegen.GeneratorNotFoundException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import static org.apache.commons.lang3.StringUtils.isEmpty;
import static org.apache.commons.lang3.StringUtils.isNotEmpty;

@Command(name = "config-help", description = "Config help for chosen generator")
public class ConfigHelp implements Runnable {

    private static final Logger LOGGER = LoggerFactory.getLogger(Generate.class);

    @Option(name = {"-lang"}, title = "language",
            description = "language to get config help for")
    private String lang;

    @Option(name = {"-framework"}, title = "framework",
            description = "framework to get config help for")
    private String framework;

    @Option(name = {"-type"}, title = "type",
            description = "generator type to get config help for",
            allowedValues = {"server", "client", "documentation"})
    private String type;

    @Option(name = {"-g", "--generator-name"}, title = "generator name",
            description = "generator to get config help for")
    private String generatorName;

    @Override
    public void run() {

        if (isEmpty(generatorName) && isEmpty(lang) && isEmpty(framework) && isEmpty(type)) {
            System.err.println("[error] A generator name (--generator-name / -g) ");
            System.err.println("[error] or a language|framework|type triplet (-language -framework -type)");
            System.err.println("[error] is required");
            System.exit(1);
        }

        try {
            CodegenConfig config = CodegenConfigLoader.forKey(generatorName, lang, framework, type);

            System.out.println();
            System.out.println("CONFIG OPTIONS");
            for (CliOption langCliOption : config.cliOptions()) {
                System.out.println("\t" + langCliOption.getOpt());
                System.out.println("\t    "
                        + langCliOption.getOptionHelp().replaceAll("\n", System.lineSeparator() + "\t    "));
                System.out.println();
            }
        } catch (GeneratorNotFoundException e) {
            System.err.println(e.getMessage());
            System.err.println("[error] Check the spelling of the generator's name and try again.");
            System.exit(1);
        }
    }
}
