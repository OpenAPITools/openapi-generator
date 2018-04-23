package org.openapitools.codegen;

import static java.util.ServiceLoader.load;

import java.util.ArrayList;
import java.util.List;
import java.util.ServiceLoader;

public class CodegenConfigLoader {
    /**
     * Tries to load config class with SPI first, then with class name directly from classpath
     *
     * @param name name of config, or full qualified class name in classpath
     * @return config class
     */
    public static CodegenConfig forName(String name) {
        ServiceLoader<CodegenConfig> loader = load(CodegenConfig.class);

        StringBuilder availableConfigs = new StringBuilder();

        for (CodegenConfig config : loader) {
            if (config.getName().equals(name)) {
                return config;
            }

            availableConfigs.append(config.getName()).append("\n");
        }

        // else try to load directly
        try {
            return (CodegenConfig) Class.forName(name).newInstance();
        } catch (Exception e) {
            throw new RuntimeException("Can't load config class with name '".concat(name) + "'\nAvailable:\n" + availableConfigs.toString());
        }
    }

    public static List<CodegenConfig> getAll() {
        ServiceLoader<CodegenConfig> loader = ServiceLoader.load(CodegenConfig.class);
        List<CodegenConfig> output = new ArrayList<CodegenConfig>();
        for (CodegenConfig aLoader : loader) {
            output.add(aLoader);
        }
        return output;
    }
}
