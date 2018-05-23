/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 * Copyright 2018 SmartBear Software
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.openapitools.codegen;

import io.airlift.airline.Cli;
import io.airlift.airline.Help;
import org.openapitools.codegen.cmd.*;

import java.util.Arrays;

/**
 * User: lanwen Date: 24.03.15 Time: 17:56
 * <p>
 * Command line interface for OpenAPI Generator use `openapi-generator-cli.jar help` for more info
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
                        .withDefaultCommand(ListGenerators.class)
                        .withCommands(
                                ListGenerators.class,
                                Generate.class,
                                Meta.class,
                                Langs.class,
                                Help.class,
                                ConfigHelp.class,
                                Validate.class,
                                Version.class
                        );

        builder.build().parse(args).run();

        // If CLI is run without a command, consider this an error.
        // We can check against empty args because unrecognized arguments/commands result in an exception.
        // This is useful to exit with status 1, for example, so that misconfigured scripts fail fast.
        // We don't want the default command to exit internally with status 1 because when the default command is something like "list",
        // it would prevent scripting using the command directly. Example:
        //     java -jar cli.jar list --short | tr ',' '\n' | xargs -I{} echo "Doing something with {}"
        if (args.length == 0) {
            System.exit(1);
        }
    }
}
