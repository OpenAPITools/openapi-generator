package org.openapitools.codegen.cmd;

import com.google.common.base.Objects;

import io.airlift.airline.Command;
import io.airlift.airline.Option;

import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.CodegenConfigLoader;
import org.openapitools.codegen.CodegenType;
import org.openapitools.codegen.meta.GeneratorMetadata;
import org.openapitools.codegen.meta.Stability;

import java.util.*;
import java.util.stream.Collectors;

// NOTE: List can later have subcommands such as list languages, list types, list frameworks, etc.
@SuppressWarnings({"java:S106"})
@Command(name = "list", description = "Lists the available generators")
public class ListGenerators extends OpenApiGeneratorCommand {

    @Option(name = {"-s", "--short" }, description = "shortened output (suitable for scripting)")
    private Boolean shortened = false;

    @Option(name = {"-d", "--docsite" }, description = "format for docusaurus site output", hidden = true)
    private Boolean docusaurus = false;

    @Option(name = {"--github-nested-index" }, description = "format for github index at docs/generators/README.md", hidden = true)
    private Boolean  githubNestedIndex = false;

    @Option(name = {"-i", "--include" },
            description = "comma-separated list of stability indexes to include (value: all,beta,stable,experimental,deprecated). Excludes deprecated by default.",
            allowedValues = { "all", "beta", "stable", "experimental", "deprecated" })
    private String include = "stable,beta,experimental";

    @Override
    public void execute() {
        List<CodegenConfig> generators = new ArrayList<>();
        List<Stability> stabilities = Arrays.asList(Stability.values());

        if (!StringUtils.isEmpty(include)) {
            List<String> includes = Arrays.asList(include.split(","));
            if (includes.size() != 0 && !includes.contains("all")) {
                stabilities = includes.stream()
                        .map(Stability::forDescription)
                        .collect(Collectors.toList());
            }
        }

        for (CodegenConfig codegenConfig : CodegenConfigLoader.getAll()) {
            GeneratorMetadata meta = codegenConfig.getGeneratorMetadata();
            if (meta != null && stabilities.contains(meta.getStability())) {
                generators.add(codegenConfig);
            }
        }

        StringBuilder sb = new StringBuilder();

        if (shortened) {
            for (int i = 0; i < generators.size(); i++) {
                CodegenConfig generator = generators.get(i);
                if (i != 0) {
                    sb.append(",");
                }
                sb.append(generator.getName());
            }
        } else {
            CodegenType[] types = CodegenType.values();

            sb.append("The following generators are available:");

            sb.append(System.lineSeparator());
            sb.append(System.lineSeparator());

            for (CodegenType type : types) {
                appendForType(sb, type, type.name(), generators);
            }
            appendForType(sb, null, "UNSPECIFIED", generators);
        }

        System.out.printf(Locale.ROOT, "%s%n", sb.toString());
    }

    private void appendForType(StringBuilder sb, CodegenType type, String typeName, List<CodegenConfig> generators) {
        List<CodegenConfig> list = generators.stream()
                .filter(g -> Objects.equal(type, g.getTag()))
                .sorted(Comparator.comparing(CodegenConfig::getName))
                .collect(Collectors.toList());

        if(!list.isEmpty()) {
            if (docusaurus || githubNestedIndex) {
                sb.append("## ").append(typeName).append(" generators");
            } else {
                sb.append(typeName).append(" generators:");
            }
            sb.append(System.lineSeparator());

            list.forEach(generator -> {
                GeneratorMetadata meta = generator.getGeneratorMetadata();
                if (docusaurus || githubNestedIndex) {
                    sb.append("* ");
                    String idPrefix = docusaurus ? "generators/" : "";
                    String id = idPrefix + generator.getName() + ".md";
                    sb.append("[").append(generator.getName());

                    if (meta != null && meta.getStability() != null && meta.getStability() != Stability.STABLE) {
                        sb.append(" (").append(meta.getStability().value()).append(")");
                    }

                    sb.append("](").append(id).append(")");

                    // trailing space is important for markdown list formatting
                    sb.append("  ");
                } else {
                    sb.append("    - ");
                    sb.append(generator.getName());

                    if (meta != null && meta.getStability() != null && meta.getStability() != Stability.STABLE) {
                        sb.append(" (").append(meta.getStability().value()).append(")");
                    }
                }
                sb.append(System.lineSeparator());
            });

            sb.append(System.lineSeparator());
            sb.append(System.lineSeparator());
        }
    }
}
