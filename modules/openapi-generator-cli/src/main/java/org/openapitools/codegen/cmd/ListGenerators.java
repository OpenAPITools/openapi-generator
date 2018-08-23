package org.openapitools.codegen.cmd;

import io.airlift.airline.Command;
import io.airlift.airline.Option;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.CodegenConfigLoader;
import org.openapitools.codegen.CodegenType;

import java.util.Arrays;
import java.util.Comparator;
import java.util.List;
import java.util.Locale;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.apache.commons.lang3.StringUtils.isEmpty;
import static org.apache.commons.lang3.StringUtils.isNotEmpty;

// NOTE: List can later have subcommands such as list languages, list types, list frameworks, etc.
@Command(name = "list", description = "Lists the available generators")
public class ListGenerators implements Runnable {

    @Option(name = {"-s", "--short" }, description = "shortened output (suitable for scripting)")
    private Boolean shortened = false;

    @Option(name = {"-lang"}, title = "language",
            description = "Only show generators with that language")
    private String lang;

    @Option(name = {"-framework"}, title = "framework",
            description = "Only show generators using that framework")
    private String framework;

    @Option(name = {"-type"}, title = "type",
            description = "Only show generators of that type")
    private CodegenType type;

    @Override
    public void run() {
        List<CodegenConfig> generators = CodegenConfigLoader.getAll();

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
            List<CodegenType> types = Arrays.asList(CodegenType.values());

            if (type!=null || isNotEmpty(lang) || isNotEmpty(framework)) {
                if (type != null ) {
                    sb.append("Filtering by type: ").append(type.name()).append(System.lineSeparator());
                }
                if (isNotEmpty(lang)) {
                    sb.append("Filtering by language: ").append(lang).append(System.lineSeparator());
                }
                if (isNotEmpty(framework)) {
                    sb.append("Filtering by framework: ").append(framework).append(System.lineSeparator());
                }

                sb.append("The following generators are available:");
                sb.append(System.lineSeparator());

                for (CodegenType currentType : types) {
                    generators.stream()
                            .filter(g -> g.getTag().equals(currentType))
                            .filter(g -> type == null || type.equals(g.getTag()))
                            .filter(g -> isEmpty(lang) || lang.equals(g.getLanguage()))
                            .filter(g -> isEmpty(framework) || framework.equals(g.getFramework()))
                            .sorted(Comparator.comparing(CodegenConfig::getName))
                            .forEach(generator -> sb.append("    - ").append(generator.getName()).append(System.lineSeparator()));
                }
            } else {

                sb.append("The following generators are available:");

                sb.append(System.lineSeparator());
                sb.append(System.lineSeparator());

                for (CodegenType type : types) {
                    sb.append(type.name()).append(" generators:");
                    sb.append(System.lineSeparator());

                    generators.stream()
                            .sorted(Comparator.comparing(CodegenConfig::getName))
                            .forEach(generator -> sb.append("    - ").append(generator.getName()).append(System.lineSeparator()));
                    sb.append(System.lineSeparator());
                    sb.append(System.lineSeparator());
                }
            }
        }

        System.out.printf(Locale.ROOT,"%s%n", sb.toString());
    }
}
