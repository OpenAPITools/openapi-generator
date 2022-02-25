package org.openapitools.codegen.cmd;

import io.airlift.airline.Option;

import static io.airlift.airline.OptionType.GLOBAL;

public class GlobalOptions {
    @Option(type = GLOBAL, name = "--version", description = "Display full version output", hidden = true)
    public boolean version;

    @Option(type = GLOBAL, name = "--help", description = "Display help about the tool", hidden = true)
    public boolean help;
}
