package com.wordnik.swagger.codegen;

import com.wordnik.swagger.codegen.cmd.Generate;
import com.wordnik.swagger.codegen.cmd.Langs;
import com.wordnik.swagger.codegen.cmd.Meta;
import io.airlift.airline.Cli;
import io.airlift.airline.Help;

/**
 * User: lanwen
 * Date: 24.03.15
 * Time: 17:56
 *
 * Command line interface for swagger codegen
 * use `swagger-codegen-cli.jar help` for more info
 *
 * @since 2.1.3-M1
 */
public class SwaggerCodegen {


    public static void main(String[] args) {
        Cli.CliBuilder<Runnable> builder = Cli.<Runnable>builder("swagger")
                .withDescription("Swagger code generator CLI. More info on swagger.io")
                .withDefaultCommand(Langs.class)
                .withCommands(
                        Generate.class,
                        Meta.class,
                        Langs.class,
                        Help.class
                );

        builder.build().parse(args).run();
    }
}
