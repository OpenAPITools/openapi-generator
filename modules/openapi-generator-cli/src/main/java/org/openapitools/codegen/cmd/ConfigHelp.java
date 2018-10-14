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

import java.io.*;
import java.nio.charset.StandardCharsets;

import static org.apache.commons.lang3.StringUtils.isEmpty;
import static org.apache.commons.lang3.StringUtils.isNotEmpty;

@Command(name = "config-help", description = "Config help for chosen lang")
public class ConfigHelp implements Runnable {

    private static final Logger LOGGER = LoggerFactory.getLogger(Generate.class);

    @Option(name = {"-l", "--lang"}, title = "language",
            description = "language to get config help for")
    private String lang;

    @Option(name = {"-g", "--generator-name"}, title = "generator name",
            description = "generator to get config help for")
    private String generatorName;

    @Option(name = {"--named-header"}, title = "named header",
            description = "Header includes the generator name, for clarity in output")
    private Boolean namedHeader;

    @Option(name = {"-o", "--output"}, title = "output location",
            description = "Optionally write help to this location, otherwise default is standard output")
    private String outputFile;

    private String newline = System.lineSeparator();

    @Override
    public void run() {

        // TODO: After 3.0.0 release (maybe for 3.1.0): Fully deprecate lang.
        if (isEmpty(generatorName)) {
            if (isNotEmpty(lang)) {
                LOGGER.warn("The '--lang' and '-l' are deprecated and may reference language names only in the next major release (4.0). Please use --generator-name /-g instead.");
                generatorName = lang;
            } else {
                System.err.println("[error] A generator name (--generator-name / -g) is required.");
                System.exit(1);
            }
        }

        try {
            StringBuilder sb = new StringBuilder();
            CodegenConfig config = CodegenConfigLoader.forName(generatorName);

            generatePlainTextHelp(sb, config);

            if (!isEmpty(outputFile)) {
                File out = new File(outputFile);
                //noinspection ResultOfMethodCallIgnored
                out.mkdirs();

                Writer writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(out), StandardCharsets.UTF_8));

                writer.write(sb.toString());
                writer.close();
            } else {
                System.out.print(sb.toString());
            }
        } catch (GeneratorNotFoundException e) {
            System.err.println(e.getMessage());
            System.err.println("[error] Check the spelling of the generator's name and try again.");
            System.exit(1);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private void generatePlainTextHelp(StringBuilder sb, CodegenConfig config) {
        sb.append(newline);
        sb.append("CONFIG OPTIONS");
        if (Boolean.TRUE.equals(namedHeader)) {
            sb.append(" for ").append(generatorName).append(newline);
        }

        sb.append(newline);

        for (CliOption langCliOption : config.cliOptions()) {
            sb.append("\t").append(langCliOption.getOpt());
            sb.append(newline);
            sb.append("\t    ").append(langCliOption.getOptionHelp().replaceAll("\n", System.lineSeparator() + "\t    "));
            sb.append(newline);
            sb.append(newline);
        }
    }
}
