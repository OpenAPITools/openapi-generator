package org.openapitools.codegen.cmd;

import io.airlift.airline.Help;
import io.airlift.airline.model.GlobalMetadata;

import javax.inject.Inject;
import java.time.format.DateTimeFormatter;

import static org.openapitools.codegen.Constants.*;

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
            StringBuilder sb = new StringBuilder(CLI_NAME);
            sb.append(" ").append(buildInfo.getVersion()).append(System.lineSeparator());
            sb.append("  commit : ").append(buildInfo.getSha()).append(System.lineSeparator());
            sb.append("  built  : ").append(buildInfo.getBuildTime().format(DateTimeFormatter.ISO_OFFSET_DATE_TIME)).append(System.lineSeparator());
            sb.append("  source : ").append(GIT_REPO).append(System.lineSeparator());
            sb.append("  docs   : ").append(SITE).append(System.lineSeparator());
            System.out.println(sb.toString());
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
