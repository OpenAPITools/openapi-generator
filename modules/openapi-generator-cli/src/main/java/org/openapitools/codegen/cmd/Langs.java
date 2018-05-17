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

package org.openapitools.codegen.cmd;

import ch.lambdaj.collection.LambdaIterable;
import io.airlift.airline.Command;
import org.openapitools.codegen.CodegenConfig;

import static ch.lambdaj.Lambda.on;
import static ch.lambdaj.collection.LambdaCollections.with;
import static java.util.ServiceLoader.load;

/**
 * User: lanwen Date: 24.03.15 Time: 20:25
 */
@Command(name = "langs", description = "Shows available languages (deprecated. use 'list' instead)")
public class Langs implements Runnable {
    @Override
    public void run() {
        LambdaIterable<String> langs =
                with(load(CodegenConfig.class)).extract(on(CodegenConfig.class).getName());
        System.out.printf("Available languages (generators): %s%n", langs);
    }
}
