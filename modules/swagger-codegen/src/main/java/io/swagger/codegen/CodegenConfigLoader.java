package io.swagger.codegen;

import java.util.ServiceLoader;

import static java.util.ServiceLoader.load;

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
            throw new RuntimeException("Can't load config class with name ".concat(name) + " Available: " + availableConfigs.toString(), e);
        }
    }
}
