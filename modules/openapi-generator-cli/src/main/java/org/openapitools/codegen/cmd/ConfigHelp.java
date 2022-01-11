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

package org.openapitools.codegen.cmd;

import io.airlift.airline.Command;
import io.airlift.airline.Option;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.CliOption;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.CodegenConfigLoader;
import org.openapitools.codegen.GeneratorNotFoundException;
import org.openapitools.codegen.meta.FeatureSet;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.*;
import java.nio.charset.StandardCharsets;
import java.nio.file.Paths;
import java.util.*;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Function;
import java.util.stream.Collectors;

import static org.apache.commons.lang3.StringEscapeUtils.escapeHtml4;
import static org.apache.commons.lang3.StringUtils.isEmpty;

@SuppressWarnings({"unused","java:S106", "java:S1192"})
@Command(name = "config-help", description = "Config help for chosen lang")
public class ConfigHelp extends OpenApiGeneratorCommand {

    private final Logger LOGGER = LoggerFactory.getLogger(ConfigHelp.class);

    private static final String FORMAT_TEXT = "text";
    private static final String FORMAT_MARKDOWN = "markdown";
    private static final String FORMAT_YAMLSAMPLE = "yamlsample";
    private static final int FEATURE_SET_DISPLAY_WIDTH= 20;

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

    @Option(name = {"--import-mappings"}, title = "import mappings", description = "displays the default import mappings (types and aliases, and what imports they will pull into the template)")
    private Boolean importMappings;

    @Option(name = {"--metadata"}, title = "metadata", description = "displays the generator metadata like the help txt for the generator and generator type etc")
    private Boolean metadata;

    @Option(name = {"--language-specific-primitive"}, title = "language specific primitives", description = "displays the language specific primitives (types which require no additional imports, or which may conflict with user defined model names)")
    private Boolean languageSpecificPrimitives;

    @Option(name = {"--reserved-words"}, title = "language specific reserved words", description = "displays the reserved words which may result in renamed model or property names")
    private Boolean reservedWords;

    @Option(name = {"--instantiation-types"}, title = "instantiation types", description = "displays types used to instantiate simple type/alias names")
    private Boolean instantiationTypes;

    @Option(name = {"--feature-set"}, title = "feature set", description = "displays feature set as supported by the generator")
    private Boolean featureSets;

    @Option(name = {
            "--markdown-header"}, title = "markdown header", description = "When format=markdown, include this option to write out markdown headers (e.g. for docusaurus).")
    private Boolean markdownHeader;

    @Option(name = {"--full-details"}, title = "full generator details", description = "displays CLI options as well as other configs/mappings (implies --instantiation-types, --reserved-words, --language-specific-primitives, --import-mappings, --feature-set)")
    private Boolean fullDetails;

    private String newline = System.lineSeparator();

    @Override
    public void execute() {
        if (isEmpty(generatorName)) {
            LOGGER.error("[error] A generator name (--generator-name / -g) is required.");
            System.exit(1);
        }

        if (Boolean.TRUE.equals(fullDetails)) {
            instantiationTypes = Boolean.TRUE;
            reservedWords = Boolean.TRUE;
            languageSpecificPrimitives = Boolean.TRUE;
            importMappings = Boolean.TRUE;
            featureSets = Boolean.TRUE;
            metadata = Boolean.TRUE;
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
                File parentFolder = out.getParentFile();
                if (parentFolder != null && parentFolder.isDirectory()) {
                    //noinspection ResultOfMethodCallIgnored
                    parentFolder.mkdirs();
                }

                try (FileOutputStream fileOutputStream = new FileOutputStream(out);
                     OutputStreamWriter outputStreamWriter = new OutputStreamWriter(fileOutputStream, StandardCharsets.UTF_8);
                     Writer writer = new BufferedWriter(outputStreamWriter)) {
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
            LOGGER.error("Unexpected error", e);
        }
    }

    private void generateMdConfigOptions(StringBuilder sb, CodegenConfig config) {
        sb.append(newline);
        sb.append("| Option | Description | Values | Default |").append(newline);
        sb.append("| ------ | ----------- | ------ | ------- |").append(newline);

        Map<String, CliOption> langCliOptions = config.cliOptions()
                .stream()
                .collect(Collectors.toMap(CliOption::getOpt, Function.identity(), (a, b) -> {
                    throw new IllegalStateException(String.format(Locale.ROOT, "Duplicated options! %s and %s", a.getOpt(), b.getOpt()));
                }, TreeMap::new));

        langCliOptions.forEach((key, langCliOption) -> {
            // start
            sb.append("|");

            // option
            sb.append(escapeHtml4(key)).append("|");
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

                sb.append("</dl>");
            } else {
                sb.append(" ");
            }
            sb.append("|");

            // default
            sb.append(escapeHtml4(langCliOption.getDefault())).append("|").append(newline);
        });
    }

    private void generateMdImportMappings(StringBuilder sb, CodegenConfig config) {
        sb.append(newline).append("## IMPORT MAPPING").append(newline).append(newline);

        sb.append("| Type/Alias | Imports |").append(newline);
        sb.append("| ---------- | ------- |").append(newline);

        config.importMapping()
                .entrySet()
                .stream()
                .sorted(Map.Entry.comparingByKey())
                .forEachOrdered(kvp -> {
                    sb.append("|").append(escapeHtml4(kvp.getKey())).append("|").append(escapeHtml4(kvp.getValue())).append("|");
                    sb.append(newline);
                });

        sb.append(newline);
    }

    private void generateMdInstantiationTypes(StringBuilder sb, CodegenConfig config) {
        sb.append(newline).append("## INSTANTIATION TYPES").append(newline).append(newline);

        sb.append("| Type/Alias | Instantiated By |").append(newline);
        sb.append("| ---------- | --------------- |").append(newline);

        config.instantiationTypes()
                .entrySet()
                .stream()
                .sorted(Map.Entry.comparingByKey())
                .forEachOrdered(kvp -> {
                    sb.append("|").append(escapeHtml4(kvp.getKey())).append("|").append(escapeHtml4(kvp.getValue())).append("|");
                    sb.append(newline);
                });

        sb.append(newline);
    }

    private void generateMdLanguageSpecificPrimitives(StringBuilder sb, CodegenConfig config) {
        sb.append(newline).append("## LANGUAGE PRIMITIVES").append(newline).append(newline);

        sb.append("<ul class=\"column-ul\">").append(newline);
        config.languageSpecificPrimitives()
                .stream()
                .sorted(String::compareTo)
                .forEach(s -> sb.append("<li>").append(escapeHtml4(s)).append("</li>").append(newline));
        sb.append("</ul>").append(newline);
    }

    private void generateMdReservedWords(StringBuilder sb, CodegenConfig config) {
        sb.append(newline).append("## RESERVED WORDS").append(newline).append(newline);

        sb.append("<ul class=\"column-ul\">").append(newline);
        config.reservedWords()
                .stream()
                .sorted(String::compareTo)
                .forEach(s -> sb.append("<li>").append(escapeHtml4(s)).append("</li>").append(newline));
        sb.append("</ul>").append(newline);
    }

    private void generateMdFeatureSets(StringBuilder sb, CodegenConfig config) {
        sb.append(newline).append("## FEATURE SET").append(newline).append(newline);

        List<FeatureSet.FeatureSetFlattened> flattened = config.getGeneratorMetadata().getFeatureSet().flatten();
        flattened.sort(Comparator.comparing(FeatureSet.FeatureSetFlattened::getFeatureCategory));

        AtomicReference<String> lastCategory = new AtomicReference<>();
        flattened.forEach(featureSet -> {
            if (!featureSet.getFeatureCategory().equals(lastCategory.get())) {
                lastCategory.set(featureSet.getFeatureCategory());

                String[] header = StringUtils.splitByCharacterTypeCamelCase(featureSet.getFeatureCategory());
                sb.append(newline).append("### ").append(StringUtils.join(header, " ")).append(newline);

                sb.append("| Name | Supported | Defined By |").append(newline);
                sb.append("| ---- | --------- | ---------- |").append(newline);
            }

            // Appends a ✓ or ✗ for support
            sb.append("|").append(featureSet.getFeatureName())
                    .append("|").append(featureSet.isSupported() ? "✓" : "✗")
                    .append("|").append(StringUtils.join(featureSet.getSource(), ","))
                    .append(newline);
        });
    }

    private void generateMdConfigOptionsHeader(StringBuilder sb, CodegenConfig config) {
        if (Boolean.TRUE.equals(markdownHeader)) {
            sb.append("## CONFIG OPTIONS").append(newline);
            sb.append("These options may be applied as additional-properties (cli) or configOptions (plugins). Refer to [configuration docs](https://openapi-generator.tech/docs/configuration) for more details.");
            sb.append(newline);
        } else {
            sb.append(newline);
            sb.append("## CONFIG OPTIONS");

            if (Boolean.TRUE.equals(namedHeader)) {
                sb.append(" for <em>").append(generatorName).append("</em>").append(newline);
            }
        }
    }

    private void generateMdMetadata(StringBuilder sb, CodegenConfig config) {
        sb.append("## METADATA").append(newline).append(newline);

        sb.append("| Property | Value | Notes |").append(newline);
        sb.append("| -------- | ----- | ----- |").append(newline);
        sb.append("| generator name | "+config.getName()+" | pass this to the generate command after -g |").append(newline);
        sb.append("| generator stability | "+config.getGeneratorMetadata().getStability()+" | |").append(newline);
        sb.append("| generator type | "+config.getTag()+" | |").append(newline);
        if (config.generatorLanguage() != null) {
            sb.append("| generator language | "+config.generatorLanguage().toString()+" | |").append(newline);
        }
        if (config.generatorLanguageVersion() != null) {
            sb.append("| generator language version | "+config.generatorLanguageVersion()+" | |").append(newline);
        }
        sb.append("| helpTxt | "+config.getHelp()+" | |").append(newline);

        sb.append(newline);
    }

    private void generateMarkdownHelp(StringBuilder sb, CodegenConfig config) {
        if (Boolean.TRUE.equals(markdownHeader)) {
            sb.append("---").append(newline);
            sb.append("title: Documentation for the " + generatorName + " Generator").append(newline);
            sb.append("---").append(newline);
            sb.append(newline);
        }

        if (Boolean.TRUE.equals(metadata)) {
            generateMdMetadata(sb, config);
        }

        generateMdConfigOptionsHeader(sb, config);
        generateMdConfigOptions(sb, config);

        if (Boolean.TRUE.equals(importMappings)) {
            generateMdImportMappings(sb, config);
        }

        if (Boolean.TRUE.equals(instantiationTypes)) {
            generateMdInstantiationTypes(sb, config);
        }

        if (Boolean.TRUE.equals(languageSpecificPrimitives)) {
            generateMdLanguageSpecificPrimitives(sb, config);
        }

        if (Boolean.TRUE.equals(reservedWords)) {
            generateMdReservedWords(sb, config);
        }

        if (Boolean.TRUE.equals(featureSets)) {
            generateMdFeatureSets(sb, config);
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

    @SuppressWarnings({"java:S1117"})
    private void generatePlainTextHelp(StringBuilder sb, CodegenConfig config) {
        sb.append(newline).append("CONFIG OPTIONS");
        if (Boolean.TRUE.equals(namedHeader)) {
            sb.append(" for ").append(generatorName).append(newline);
        }

        sb.append(newline).append(newline);

        String optIndent = "\t";
        String optNestedIndent = "\t    ";

        Map<String, CliOption> langCliOptions = config.cliOptions()
                .stream()
                .collect(Collectors.toMap(CliOption::getOpt, Function.identity(), (a, b) -> {
                    throw new IllegalStateException(String.format(Locale.ROOT, "Duplicated options! %s and %s", a.getOpt(), b.getOpt()));
                }, TreeMap::new));

        langCliOptions.forEach((key, langCliOption) -> {
            sb.append(optIndent).append(key).append(newline);
            sb.append(optNestedIndent).append(langCliOption.getOptionHelp()
                    .replaceAll("\n", System.lineSeparator() + optNestedIndent));
            sb.append(newline).append(newline);
        });

        if (Boolean.TRUE.equals(importMappings)) {
            sb.append(newline).append("IMPORT MAPPING").append(newline).append(newline);
            Map<String, String> map = config.importMapping()
                    .entrySet()
                    .stream()
                    .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue, (a, b) -> {
                        throw new IllegalStateException(String.format(Locale.ROOT, "Duplicated options! %s and %s", a, b));
                    }, TreeMap::new));
            writePlainTextFromMap(sb, map, optIndent, optNestedIndent, "Type/Alias", "Imports");
            sb.append(newline);
        }

        if (Boolean.TRUE.equals(instantiationTypes)) {
            sb.append(newline).append("INSTANTIATION TYPES").append(newline).append(newline);
            Map<String, String> map = config.instantiationTypes()
                    .entrySet()
                    .stream()
                    .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue, (a, b) -> {
                        throw new IllegalStateException(String.format(Locale.ROOT, "Duplicated options! %s and %s", a, b));
                    }, TreeMap::new));
            writePlainTextFromMap(sb, map, optIndent, optNestedIndent, "Type/Alias", "Instantiated By");
            sb.append(newline);
        }

        if (Boolean.TRUE.equals(languageSpecificPrimitives)) {
            sb.append(newline).append("LANGUAGE PRIMITIVES").append(newline).append(newline);
            String[] arr = config.languageSpecificPrimitives().stream().sorted().toArray(String[]::new);
            writePlainTextFromArray(sb, arr, optIndent);
            sb.append(newline);
        }

        if (Boolean.TRUE.equals(reservedWords)) {
            sb.append(newline).append("RESERVED WORDS").append(newline).append(newline);
            String[] arr = config.reservedWords().stream().sorted().toArray(String[]::new);
            writePlainTextFromArray(sb, arr, optIndent);
            sb.append(newline);
        }

        if (Boolean.TRUE.equals(featureSets)) {
            sb.append(newline).append("FEATURE SET").append(newline);

            List<FeatureSet.FeatureSetFlattened> flattened = config.getGeneratorMetadata().getFeatureSet().flatten();
            flattened.sort(Comparator.comparing(FeatureSet.FeatureSetFlattened::getFeatureCategory));

            AtomicReference<String> lastCategory = new AtomicReference<>();

            String nameKey = "Name";
            String supportedKey = "Supported";
            String definedByKey = "Defined By";
            int maxNameLength = flattened.stream().map(FeatureSet.FeatureSetFlattened::getFeatureName).mapToInt(String::length).max().orElse(nameKey.length());
            int maxSupportedLength = supportedKey.length();
            int definedInLength = FEATURE_SET_DISPLAY_WIDTH;
            String format = "%-" + maxNameLength + "s\t%-" + maxSupportedLength + "s\t%-" + definedInLength + "s";

            flattened.forEach(featureSet -> {
                if (!featureSet.getFeatureCategory().equals(lastCategory.get())) {
                    lastCategory.set(featureSet.getFeatureCategory());
                    sb.append(newline).append(newline).append("  ").append(featureSet.getFeatureCategory()).append(":");
                    sb.append(newline).append(newline);
                    sb.append(optIndent).append(String.format(Locale.ROOT, format, nameKey, supportedKey, definedByKey)).append(newline);
                    sb.append(optIndent).append(String.format(Locale.ROOT, format,
                            StringUtils.repeat("-", maxNameLength),
                            StringUtils.repeat("-", maxSupportedLength),
                            StringUtils.repeat("-", definedInLength)));
                }

                String mark = featureSet.isSupported() ? "✓" : "x";
                String centeredMark = StringUtils.center(mark, maxSupportedLength);
                String definedByCsv = StringUtils.join(featureSet.getSource(), ",");
                sb.append(newline).append(optIndent).append(String.format(Locale.ROOT, format, featureSet.getFeatureName(), centeredMark, definedByCsv));
            });
        }
    }

    @SuppressWarnings({"java:S1117"})
    private void writePlainTextFromMap(
            StringBuilder sb,
            Map<String, String> map,
            String optIndent,
            String optNestedIndent,
            @SuppressWarnings("SameParameterValue") String keyHeader,
            String valueHeader) {
        if (map != null && map.size() > 0) {
            int maxKey = keyHeader.length();
            int maxValue = valueHeader.length();

            for (Map.Entry<String, String> entry : map.entrySet()) {
                String k = entry.getKey();
                String v = entry.getValue();
                maxKey = Math.max(maxKey, k.length());
                maxValue = Math.max(maxValue, v.length());
            }

            String format = "%-" + maxKey + "s\t%-" + maxValue + "s";
            sb.append(optIndent).append(String.format(Locale.ROOT, format, keyHeader, valueHeader));
            sb.append(newline);
            sb.append(optIndent).append(String.format(Locale.ROOT, format, StringUtils.repeat("-", maxKey), StringUtils.repeat("-", maxValue)));
            map.forEach((key, value) -> {
                sb.append(newline);
                sb.append(optIndent).append(String.format(Locale.ROOT, format, key, value));
            });
        } else {
            sb.append(optIndent).append("None");
        }
    }

    @SuppressWarnings({"java:S1117"})
    private void writePlainTextFromArray(StringBuilder sb, String[] arr, String optIndent) {
        if (arr.length > 0) {
            // target a width of 20, then take the max up to 40.
            int width = 20;
            for (String s : arr) {
                width = Math.max(width, Math.min(40, s.length()));
            }

            // do three columns if possible (assume terminal width ~90), otherwise do two columns.
            int columns = width < 30 ? 3 : 2;
            String format = "%-" + (90 / columns) + "s";
            for (int i = 0; i < arr.length; i++) {
                String current = arr[i];
                sb.append(optIndent).append(String.format(Locale.ROOT, format, current));
                if ((i + 1) % columns == 0) {
                    sb.append(newline);
                }
            }
        } else {
            sb.append(optIndent).append("None");
        }
    }
}
