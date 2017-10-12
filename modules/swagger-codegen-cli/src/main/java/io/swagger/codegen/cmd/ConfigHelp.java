package io.swagger.codegen.cmd;

import io.swagger.codegen.CliOption;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.CodegenConfigLoader;

public class ConfigHelp implements Runnable {
    private String lang;

    @Override
    public void run() {
        System.out.println();
        CodegenConfig config = CodegenConfigLoader.forName(lang);
        System.out.println("CONFIG OPTIONS");
        for (CliOption langCliOption : config.cliOptions()) {
            System.out.println("\t" + langCliOption.getOpt());
            System.out.println("\t    "
                    + langCliOption.getOptionHelp().replaceAll("\n", "\n\t    "));
            System.out.println();
        }
    }
}
