package org.openapitools.codegen.cmd;

import com.google.common.base.Objects;

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

        if(list.size() > 0) {
            sb.append(typeName).append(" generators:");
            sb.append(System.lineSeparator());

            list.stream()
                    .forEach(generator -> sb.append("    - ").append(generator.getName()).append(System.lineSeparator()));

            sb.append(System.lineSeparator());
            sb.append(System.lineSeparator());
        }
    }
}
