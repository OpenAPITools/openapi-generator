package io.swagger.codegen.cmd;

import io.airlift.airline.Command;
import io.airlift.airline.Option;
import io.swagger.codegen.CliOption;
import io.swagger.codegen.CodegenConfig;

import java.util.ServiceLoader;

import static java.util.ServiceLoader.load;

@Command(name = "library-help", description = "Library help for chosen lang")
public class LibraryHelp implements Runnable {

    @Option(name = {"-l", "--lang"}, title = "language", required = true,
            description = "language to get library help for")
    private String lang;

    /**
     * Tries to load config class with SPI first, then with class name directly from classpath
     *
     * @param name name of config, or full qualified class name in classpath
     * @return config class
     */
    private static CodegenConfig forName(String name) {
        ServiceLoader<CodegenConfig> loader = load(CodegenConfig.class);
        for (CodegenConfig config : loader) {
            if (config.getName().equals(name)) {
                return config;
            }
        }

        // else try to load directly
        try {
            return (CodegenConfig) Class.forName(name).newInstance();
        } catch (Exception e) {
            throw new RuntimeException("Can't load config class with name ".concat(name), e);
        }
    }

    @Override
    public void run() {
        System.out.println();
        CodegenConfig config = forName(lang);
        System.out.println("LIBRARY OPTIONS");
        for (String library : config.supportedLibraries().keySet()) {
            String description = config.supportedLibraries().get(library);
            if ("".equals(library))
                library = "<default>";
            System.out.println("\t" + library);
            System.out.println("\t    " + description);
            System.out.println();
        }
    }
}
