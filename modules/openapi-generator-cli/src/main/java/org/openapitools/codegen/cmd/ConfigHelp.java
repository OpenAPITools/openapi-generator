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
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.CliOption;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.CodegenConfigLoader;
import org.openapitools.codegen.GeneratorNotFoundException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.*;
import java.nio.charset.StandardCharsets;
import java.nio.file.Paths;
import java.util.Map;

import static org.apache.commons.lang3.StringEscapeUtils.escapeHtml4;
import static org.apache.commons.lang3.StringUtils.isEmpty;

@Command(name = "config-help", description = "Config help for chosen lang")
public class ConfigHelp implements Runnable {

    private static final Logger LOGGER = LoggerFactory.getLogger(Generate.class);

    public static final String FORMAT_TEXT = "text";
    public static final String FORMAT_MARKDOWN = "markdown";
    public static final String FORMAT_YAMLSAMPLE = "yamlsample";

    @Option(name = {"-g",
            "--generator-name"}, title = "generator name", description = "generator to get config help for")
    private String generatorName;

    @Option(name = {
            "--named-header"}, title = "named header", description = "Header includes the generator name, for clarity in output")
    private Boolean namedHeader;

    @Option(name = {"-o",
            "--output"}, title = "output location", description = "Optionally write help to this location, otherwise default is standard output")
    private String outputFile;

    @Option(name = {"-f",
            "--format"}, title = "output format", description = "Write output files in the desired format. Options are 'text', 'markdown' or 'yamlsample'. Default is 'text'.", allowedValues = {
            FORMAT_TEXT, FORMAT_MARKDOWN, FORMAT_YAMLSAMPLE})
    private String format;

    @Option(name = {
            "--markdown-header"}, title = "markdown header", description = "When format=markdown, include this option to write out markdown headers (e.g. for docusaurus).")
    private Boolean markdownHeader;

    private String newline = System.lineSeparator();

    @Override public void run() {
        if (isEmpty(generatorName)) {
            LOGGER.error("[error] A generator name (--generator-name / -g) is required.");
            System.exit(1);
        }

        try {
            StringBuilder sb = new StringBuilder();
            CodegenConfig config = CodegenConfigLoader.forName(generatorName);

            String desiredFormat = StringUtils.defaultIfBlank(format, FORMAT_TEXT);

            switch (desiredFormat) {
                case FORMAT_MARKDOWN:
                    generateMarkdownHelp(sb, config);
                    break;
                case FORMAT_YAMLSAMPLE:
                    generateYamlSample(sb, config);
                    break;
                case FORMAT_TEXT:
                    generatePlainTextHelp(sb, config);
                    break;
                default:
                    LOGGER.warn("[warning] Unrecognized format option: {}", format);
                    break;
            }


            if (!isEmpty(outputFile)) {
                File out = Paths.get(outputFile).toFile();
                //noinspection ResultOfMethodCallIgnored
                File parentFolder = out.getParentFile();
                if (parentFolder != null && parentFolder.isDirectory()) {
                    parentFolder.mkdirs();
                }

                try (Writer writer = new BufferedWriter(
                        new OutputStreamWriter(new FileOutputStream(out), StandardCharsets.UTF_8))) {
                    writer.write(sb.toString());
                }
            } else {
                System.out.print(sb.toString());
            }
        } catch (GeneratorNotFoundException e) {
            LOGGER.error(e.getMessage());
            LOGGER.error("[error] Check the spelling of the generator's name and try again.");
            System.exit(1);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private void generateYamlSample(StringBuilder sb, CodegenConfig config) {

        for (CliOption langCliOption : config.cliOptions()) {

            sb.append("# Description: ").append(langCliOption.getDescription()).append(newline);

            Map<String, String> enums = langCliOption.getEnum();
            if (enums != null) {
                sb.append("# Available Values:").append(newline);

                for (Map.Entry<String, String> entry : enums.entrySet()) {
                    sb.append("#    ").append(entry.getKey()).append(newline);
                    sb.append("#         ").append(entry.getValue()).append(newline);
                }
            }

            String defaultValue = langCliOption.getDefault();

            if (defaultValue != null) {
                sb.append(langCliOption.getOpt()).append(": ").append(defaultValue).append(newline);
            } else {
                sb.append("# ").append(langCliOption.getOpt()).append(": ").append(newline);
            }

            sb.append(newline);
        }
    }

    private void generateMarkdownHelp(StringBuilder sb, CodegenConfig config) {
        sb.append(newline);

        if (Boolean.TRUE.equals(markdownHeader)) {
            sb.append("---").append(newline);
            sb.append("id: generator-opts-").append(config.getTag().toValue()).append("-")
                    .append(config.getName()).append(newline);
            sb.append("title: Config Options for ").append(generatorName).append(newline);
            sb.append("sidebar_label: ").append(generatorName).append(newline);
            sb.append("---").append(newline);
        } else {
            sb.append("## CONFIG OPTIONS");

            if (Boolean.TRUE.equals(namedHeader)) {
                sb.append(" for <em>").append(generatorName).append("</em>").append(newline);
            }
        }

        sb.append(newline);

        sb.append("| Option | Description | Values | Default |").append(newline);
        sb.append("| ------ | ----------- | ------ | ------- |").append(newline);

        for (CliOption langCliOption : config.cliOptions()) {
            // start
            sb.append("|");

            // option
            sb.append(escapeHtml4(langCliOption.getOpt())).append("|");
            // description
            sb.append(escapeHtml4(langCliOption.getDescription())).append("|");

            // values
            Map<String, String> enums = langCliOption.getEnum();
            if (enums != null) {
                sb.append("<dl>");

                for (Map.Entry<String, String> entry : enums.entrySet()) {
                    sb.append("<dt>**").append(escapeHtml4(entry.getKey())).append("**</dt>");
                    sb.append("<dd>").append(escapeHtml4(entry.getValue())).append("</dd>");
                }

                sb.append("<dl>");
            } else {
                sb.append(" ");
            }
            sb.append("|");

            // default
            sb.append(escapeHtml4(langCliOption.getDefault())).append("|");

            sb.append(newline);
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
            sb.append("\t    ").append(langCliOption.getOptionHelp()
                    .replaceAll("\n", System.lineSeparator() + "\t    "));
            sb.append(newline);
            sb.append(newline);
        }
    }
}
