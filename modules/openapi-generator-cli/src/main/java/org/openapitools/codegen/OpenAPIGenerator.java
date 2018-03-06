package org.openapitools.codegen;

import io.airlift.airline.Cli;
import io.airlift.airline.Help;
import org.openapitools.codegen.cmd.ConfigHelp;
import org.openapitools.codegen.cmd.Generate;
import org.openapitools.codegen.cmd.Langs;
import org.openapitools.codegen.cmd.Meta;
import org.openapitools.codegen.cmd.Validate;
import org.openapitools.codegen.cmd.Version;

/**
 * User: lanwen Date: 24.03.15 Time: 17:56
 * <p>
 * Command line interface for openapi generator use `openapi-generator-cli.jar help` for more info
 *
 * @since 2.1.3-M1
 */
public class OpenAPIGenerator {


    public static void main(String[] args) {
        String version = Version.readVersionFromResources();
        @SuppressWarnings("unchecked")
        Cli.CliBuilder<Runnable> builder =
                Cli.<Runnable>builder("openapi-generator-cli")
                        .withDescription(
                                String.format(
                                        "OpenAPI generator CLI (version %s).",
                                        version))
                        .withDefaultCommand(Langs.class)
                        .withCommands(Generate.class, Meta.class, Langs.class, Help.class,
                                ConfigHelp.class, Validate.class, Version.class);

        builder.build().parse(args).run();
    }
}
