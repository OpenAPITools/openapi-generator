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

@Command(name = "config-help", description = "Config help for chosen lang")
public class ConfigHelp implements Runnable {

    @Option(name = {"-l", "--lang"}, title = "language", required = true,
            description = "language to get config help for")
    private String lang;

    @Override
    public void run() {
        try {
            CodegenConfig config = CodegenConfigLoader.forName(lang);
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
