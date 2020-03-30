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

package org.openapitools.codegen.cmd;

import io.airlift.airline.Command;
import io.airlift.airline.Option;

@SuppressWarnings({"unused", "java:S106"})
@Command(name = "version", description = "Show version information used in tooling")
public class Version extends OpenApiGeneratorCommand {

    @Option(name = {"--sha"}, description = "Git commit SHA version")
    private Boolean sha;

    @Option(name = {"--full"}, description = "Full version details")
    private Boolean full;

    @Override
    public void execute() {
        String version;

        if (Boolean.TRUE.equals(full)) {
            version = buildInfo.versionDisplayText();
        } else if (Boolean.TRUE.equals(sha)) {
            version = buildInfo.getSha();
        } else {
            version = buildInfo.getVersion();
        }
        System.out.println(version);
    }

}
