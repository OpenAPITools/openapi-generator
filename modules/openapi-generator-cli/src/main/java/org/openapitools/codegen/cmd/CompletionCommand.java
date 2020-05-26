/*
 * Copyright (C) 2010 the original author or authors.
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
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
/*
 * NOTICE: File originally taken from:
 * https://github.com/airlift/airline/blob/fc7a55e34b6361cb97235de5a1b21cba9b508f4b/src/main/java/io/airlift/airline/SuggestCommand.java#L1
 * Modifications have been made to fit the needs of OpenAPI Tools CLI.
 */
package org.openapitools.codegen.cmd;

import com.google.common.annotations.VisibleForTesting;
import com.google.common.base.Joiner;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import io.airlift.airline.*;
import io.airlift.airline.model.*;

import javax.inject.Inject;
import java.util.List;
import java.util.Map;
import java.util.concurrent.Callable;

import static com.google.common.collect.Lists.newArrayList;
import static io.airlift.airline.ParserUtil.createInstance;

@SuppressWarnings({"java:S106"})
@Command(name = "completion", description = "Complete commands (for using in tooling such as Bash Completions).", hidden = true)
public class CompletionCommand extends OpenApiGeneratorCommand
        implements Runnable, Callable<Void> {
    private static final Map<Context, Class<? extends Suggester>> BUILTIN_SUGGESTERS = ImmutableMap.<Context, Class<? extends Suggester>>builder()
            .put(Context.GLOBAL, GlobalSuggester.class)
            .put(Context.GROUP, GroupSuggester.class)
            .put(Context.COMMAND, CommandSuggester.class)
            .build();

    @Inject
    public GlobalMetadata metadata;

    @Arguments
    public List<String> arguments = newArrayList();

    @Override
    public Void call() {
        run();
        return null;
    }

    @VisibleForTesting
    public Iterable<String> generateSuggestions() {
        Parser parser = new Parser();
        ParseState state = parser.parse(metadata, arguments);

        Class<? extends Suggester> suggesterClass = BUILTIN_SUGGESTERS.get(state.getLocation());
        if (suggesterClass != null) {
            SuggesterMetadata suggesterMetadata = MetadataLoader.loadSuggester(suggesterClass);

            if (suggesterMetadata != null) {
                ImmutableMap.Builder<Class<?>, Object> bindings = ImmutableMap.<Class<?>, Object>builder()
                        .put(GlobalMetadata.class, metadata);

                if (state.getGroup() != null) {
                    bindings.put(CommandGroupMetadata.class, state.getGroup());
                }

                if (state.getCommand() != null) {
                    bindings.put(CommandMetadata.class, state.getCommand());
                }

                Suggester suggester = createInstance(suggesterMetadata.getSuggesterClass(),
                        ImmutableList.<OptionMetadata>of(),
                        null,
                        null,
                        null,
                        suggesterMetadata.getMetadataInjections(),
                        bindings.build(),
                        new DefaultCommandFactory<Suggester>());

                return suggester.suggest();
            }
        }

        return ImmutableList.of();
    }

    @Override
    void execute() {
        System.out.println(Joiner.on("\n").join(generateSuggestions()));
    }
}