package io.swagger.codegen;

import io.airlift.airline.Cli;
import io.airlift.airline.Help;
import io.swagger.codegen.cmd.ConfigHelp;
import io.swagger.codegen.cmd.Generate;
import io.swagger.codegen.cmd.Langs;
import io.swagger.codegen.cmd.Meta;
import io.swagger.codegen.cmd.Validate;
import io.swagger.codegen.cmd.Version;

/**
 * User: lanwen Date: 24.03.15 Time: 17:56
 * <p>
 * Command line interface for swagger codegen use `swagger-codegen-cli.jar help` for more info
 *
 * @since 2.1.3-M1
 */
public class SwaggerCodegen {


    public static void main(String[] args) {
        String version = Version.readVersionFromResources();
        @SuppressWarnings("unchecked")
        Cli.CliBuilder<Runnable> builder =
                Cli.<Runnable>builder("swagger-codegen-cli")
                        .withDescription(
                                String.format(
                                        "Swagger code generator CLI (version %s). More info on swagger.io",
                                        version))
                        .withDefaultCommand(Langs.class)
                        .withCommands(Generate.class, Meta.class, Langs.class, Help.class,
                                ConfigHelp.class, Validate.class, Version.class);

        builder.build().parse(args).run();
    }
}
