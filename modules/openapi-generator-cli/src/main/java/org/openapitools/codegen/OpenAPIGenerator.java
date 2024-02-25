/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 * Copyright 2018 SmartBear Software
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.openapitools.codegen;

import io.airlift.airline.Cli;
import io.airlift.airline.ParseArgumentsUnexpectedException;
import io.airlift.airline.ParseOptionMissingException;
import io.airlift.airline.ParseOptionMissingValueException;
import org.openapitools.codegen.cmd.*;

import java.util.Locale;

import static org.openapitools.codegen.Constants.CLI_NAME;

/**
 * User: lanwen Date: 24.03.15 Time: 17:56
 * <p>
 * Command line interface for OpenAPI Generator use `openapi-generator-cli.jar help` for more info
 */
public class OpenAPIGenerator {

    public static void main(String[] args) {
        BuildInfo buildInfo = new BuildInfo();
        Cli.CliBuilder<OpenApiGeneratorCommand> builder =
                Cli.<OpenApiGeneratorCommand>builder(CLI_NAME)
                        .withDescription(
                                String.format(
                                        Locale.ROOT,
                                        "OpenAPI Generator CLI %s (%s).",
                                        buildInfo.getVersion(),
                                        buildInfo.getSha()))
                        .withDefaultCommand(HelpCommand.class)
                        .withCommands(
                                ListGenerators.class,
                                Generate.class,
                                Meta.class,
                                HelpCommand.class,
                                ConfigHelp.class,
                                Validate.class,
                                Version.class,
                                CompletionCommand.class,
                                GenerateBatch.class
                        );

        builder.withGroup("author")
                .withDescription("Utilities for authoring generators or customizing templates.")
                .withDefaultCommand(HelpCommand.class)
                .withCommands(AuthorTemplate.class);

        try {
            builder.build().parse(args).run();

            // If CLI runs without a command, consider this an error. This exists after initial parse/run
            // so we can present the configured "default command".
            // We can check against empty args because unrecognized arguments/commands result in an exception.
            // This is useful to exit with status 1, for example, so that misconfigured scripts fail fast.
            // We don't want the default command to exit internally with status 1 because when the default command is something like "list",
            // it would prevent scripting using the command directly. Example:
            //     java -jar cli.jar list --short | tr ',' '\n' | xargs -I{} echo "Doing something with {}"
            if (args.length == 0) {
                System.exit(1);
            }
        } catch (ParseArgumentsUnexpectedException e) {
            System.err.printf(Locale.ROOT, "[error] %s%n%nSee '%s help' for usage.%n", e.getMessage(), CLI_NAME);
            System.exit(1);
        } catch (ParseOptionMissingException | ParseOptionMissingValueException e) {
            System.err.printf(Locale.ROOT, "[error] %s%n", e.getMessage());
            System.exit(1);
        }
    }
}
