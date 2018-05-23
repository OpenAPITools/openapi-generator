package org.openapitools.codegen.cmd;

import io.airlift.airline.Command;
import io.airlift.airline.Option;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.CodegenConfigLoader;
import org.openapitools.codegen.CodegenType;

import java.util.Arrays;
import java.util.Comparator;
import java.util.List;

// NOTE: List can later have subcommands such as list languages, list types, list frameworks, etc.
@Command(name = "list", description = "Lists the available generators")
public class ListGenerators implements Runnable {

    @Option(name = {"-s", "--short" }, description = "shortened output (suitable for scripting)")
    private Boolean shortened = false;

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

            sb.append("The following generators are available:");

            sb.append(System.lineSeparator());
            sb.append(System.lineSeparator());

            for (CodegenType type : types) {
                sb.append(type.name()).append(" generators:");
                sb.append(System.lineSeparator());

                generators.stream()
                        .filter(g -> g.getTag().equals(type))
                        .sorted(Comparator.comparing(CodegenConfig::getName))
                        .forEach(generator -> sb.append("    - ").append(generator.getName()).append(System.lineSeparator()));

                sb.append(System.lineSeparator());
                sb.append(System.lineSeparator());
            }
        }

        System.out.printf("%s%n", sb.toString());
    }
}
