package org.openapitools.codegen.cmd;

import static io.airlift.airline.OptionType.GLOBAL;

import io.airlift.airline.Option;

public class GlobalOptions {
    @Option(
            type = GLOBAL,
            name = "--version",
            description = "Display full version output",
            hidden = true)
    public boolean version;

    @Option(
            type = GLOBAL,
            name = "--help",
            description = "Display help about the tool",
            hidden = true)
    public boolean help;
}
