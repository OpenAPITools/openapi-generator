package org.openapitools.codegen.cmd;

import io.airlift.airline.Help;
import io.airlift.airline.model.GlobalMetadata;

import javax.inject.Inject;

@SuppressWarnings({"java:S106"})
public abstract class OpenApiGeneratorCommand implements Runnable {
    @Inject
    public GlobalOptions globalOptions = new GlobalOptions();

    @Inject
    public GlobalMetadata global;

    protected BuildInfo buildInfo = new BuildInfo();

    @Override
    public void run() {
        if (globalOptions.version) {
            System.out.println(buildInfo.versionDisplayText());
            return;
        }

        if (globalOptions.help) {
            Help help = new Help();
            help.global = global;
            help.call();
            return;
        }

        execute();
    }

    /**
     * Logic to be executed by implementing commands
     */
    abstract void execute();
}
