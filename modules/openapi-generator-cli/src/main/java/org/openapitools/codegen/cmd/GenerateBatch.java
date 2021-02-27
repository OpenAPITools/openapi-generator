/*
 * Copyright 2019 OpenAPI-Generator Contributors (https://openapi-generator.tech)
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

import ch.qos.logback.classic.Level;
import ch.qos.logback.classic.LoggerContext;

import com.fasterxml.jackson.core.JsonFactory;
import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonToken;
import com.fasterxml.jackson.core.TreeNode;
import com.fasterxml.jackson.databind.*;
import com.fasterxml.jackson.databind.deser.BeanDeserializerModifier;
import com.fasterxml.jackson.databind.deser.std.DelegatingDeserializer;
import com.fasterxml.jackson.databind.module.SimpleModule;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.fasterxml.jackson.databind.util.TokenBuffer;

import io.airlift.airline.Arguments;
import io.airlift.airline.Command;
import io.airlift.airline.Option;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.ClientOptInput;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.openapitools.codegen.config.DynamicSettings;
import org.openapitools.codegen.config.GlobalSettings;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;
import java.util.Locale;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;
import java.util.stream.Stream;

@SuppressWarnings({"unused", "MismatchedQueryAndUpdateOfCollection", "java:S106"})
@Command(name = "batch", description = "Generate code in batch via external configs.")
public class GenerateBatch extends OpenApiGeneratorCommand {
    private static AtomicInteger failures = new AtomicInteger(0);
    private static AtomicInteger successes = new AtomicInteger(0);
    private final Logger LOGGER = LoggerFactory.getLogger(GenerateBatch.class);

    @Option(name = {"-v", "--verbose"}, description = "verbose mode")
    private Boolean verbose;

    @Option(name = {"-r", "--threads"}, description = "thread count")
    private Integer threads;

    @Arguments(description = "Generator configuration files.", required = true)
    private List<String> configs;

    @Option(name = {"--fail-fast"}, description = "fail fast on any errors")
    private Boolean failFast;

    @Option(name = {"--clean"}, description = "clean output of previously written files before generation")
    private Boolean clean;

    @Option(name = {"--timeout"}, description = "execution timeout (minutes)")
    private Integer timeout;

    @Option(name = {"--includes-base-dir"}, description = "base directory used for includes")
    private String includes;

    @Option(name = {"--root-dir"}, description = "root directory used output/includes (includes can be overridden)")
    private String root;

    /**
     * When an object implementing interface <code>Runnable</code> is used
     * to create a thread, starting the thread causes the object's
     * <code>run</code> method to be called in that separately executing
     * thread.
     * <p>
     * The general contract of the method <code>run</code> is that it may
     * take any action whatsoever.
     *
     * @see Thread#run()
     */
    @Override
    public void execute() {
        if (configs.size() < 1) {
            LOGGER.error("No configuration file inputs specified");
            System.exit(1);
        }

        int cores = Runtime.getRuntime().availableProcessors();
        int numThreads = 2 * cores;
        if (null != threads && (threads > 0 && threads < Thread.activeCount())) {
            numThreads = threads;
        }

        Path rootDir;
        if (root != null) {
            rootDir = Paths.get(root);
        } else {
            rootDir = Paths.get(System.getProperty("user.dir"));
        }

        // This allows us to put meta-configs in a different file from referenced configs.
        // If not specified, we'll assume it's the parent directory of the first file.
        File includesDir;
        if (includes != null) {
            includesDir = new File(includes);
        } else {
            Path first = Paths.get(configs.get(0));
            if (Files.isRegularFile(first) && !Files.isSymbolicLink(first)) {
                includesDir = first.toAbsolutePath().getParent().toFile();
            } else {
                // Not traversing symbolic links for includes. Falling back to rooted working directory.
                includesDir = rootDir.toFile();
            }
        }

        LOGGER.info(String.format(Locale.ROOT, "Batch generation using up to %d threads.\nIncludes: %s\nRoot: %s", numThreads, includesDir.getAbsolutePath(), rootDir.toAbsolutePath().toString()));

        // Create a module which loads our config files, but supports a special "!include" key which can point to an existing config file.
        // This allows us to create a sort of meta-config which holds configs which are otherwise required at CLI time (via generate task).
        // That is, this allows us to create a wrapper config for generatorName, inputSpec, outputDir, etc.
        SimpleModule module = getCustomDeserializationModel(includesDir);
        List<CodegenConfigurator> configurators = configs.stream().map(config -> CodegenConfigurator.fromFile(config, module)).collect(Collectors.toList());

        // it doesn't make sense to interleave INFO level logs, so limit these to only ERROR.
        LoggerContext lc = (LoggerContext) LoggerFactory.getILoggerFactory();
        Stream.of(Logger.ROOT_LOGGER_NAME, "io.swagger", "org.openapitools")
                .map(lc::getLogger)
                .forEach(logger -> logger.setLevel(Level.ERROR));

        ExecutorService executor = Executors.newFixedThreadPool(numThreads);

        // Execute each configurator on a separate pooled thread.
        configurators.forEach(configurator -> {
            GenerationRunner runner = new GenerationRunner(configurator, rootDir, Boolean.TRUE.equals(failFast), Boolean.TRUE.equals(clean));
            executor.execute(runner);
        });

        executor.shutdown();

        try {
            // Allow the batch job to terminate, never running for more than 30 minutes (defaulted to max 10 minutes)
            if (timeout == null) timeout = 10;
            int awaitFor = Math.min(Math.max(timeout, 1), 30);

            executor.awaitTermination(awaitFor, TimeUnit.MINUTES);

            int failCount = failures.intValue();
            if (failCount > 0) {
                System.err.println(String.format(Locale.ROOT, "[FAIL] Completed with %d failures, %d successes", failCount, successes.intValue()));
                System.exit(1);
            } else {
                System.out.println(String.format(Locale.ROOT, "[SUCCESS] Batch generation finished %d generators successfully.", successes.intValue()));
            }
        } catch (InterruptedException e) {
            e.printStackTrace();
            // re-interrupt
            Thread.currentThread().interrupt();
        }
    }

    private static class GenerationRunner implements Runnable {
        private final CodegenConfigurator configurator;
        private final Path rootDir;
        private final boolean exitOnError;
        private final boolean clean;

        private GenerationRunner(CodegenConfigurator configurator, Path rootDir, boolean failFast, boolean clean) {
            this.configurator = configurator;
            this.rootDir = rootDir;
            this.exitOnError = failFast;
            this.clean = clean;
        }

        /**
         * When an object implementing interface <code>Runnable</code> is used
         * to create a thread, starting the thread causes the object's
         * <code>run</code> method to be called in that separately executing
         * thread.
         * <p>
         * The general contract of the method <code>run</code> is that it may
         * take any action whatsoever.
         *
         * @see Thread#run()
         */
        @Override
        public void run() {
            String name = null;
            try {
                GlobalSettings.reset();

                ClientOptInput opts = configurator.toClientOptInput();
                CodegenConfig config = opts.getConfig();
                name = config.getName();
                
                Path target = Paths.get(config.getOutputDir());
                Path updated = rootDir.resolve(target);
                config.setOutputDir(updated.toString());

                if (this.clean) {
                    cleanPreviousFiles(name, updated);
                }

                System.out.printf(Locale.ROOT, "[%s] Generating %s (outputs to %s)…%n", Thread.currentThread().getName(), name, updated.toString());

                DefaultGenerator defaultGenerator = new DefaultGenerator();
                defaultGenerator.opts(opts);

                defaultGenerator.generate();

                System.out.printf(Locale.ROOT, "[%s] Finished generating %s…%n", Thread.currentThread().getName(), name);
                successes.incrementAndGet();
            } catch (Throwable e) {
                failures.incrementAndGet();
                String failedOn = name;
                if (StringUtils.isEmpty(failedOn)) {
                    failedOn = "unspecified";
                }
                System.err.printf(Locale.ROOT, "[%s] Generation failed for %s: (%s) %s%n", Thread.currentThread().getName(), failedOn, e.getClass().getSimpleName(), e.getMessage());
                e.printStackTrace(System.err);
                if (exitOnError) {
                    System.exit(1);
                }
            } finally {
                GlobalSettings.reset();
            }
        }

        private void cleanPreviousFiles(final String name, Path outDir) throws IOException {
            System.out.printf(Locale.ROOT, "[%s] Cleaning previous contents for %s in %s…%n", Thread.currentThread().getName(), name, outDir.toString());
            Path filesMeta = Paths.get(outDir.toAbsolutePath().toString(), ".openapi-generator", "FILES");
            if (filesMeta.toFile().exists()) {
                FileUtils.readLines(filesMeta.toFile(), StandardCharsets.UTF_8).forEach(relativePath -> {
                    if (!StringUtils.startsWith(relativePath, ".")) {
                        Path file = outDir.resolve(relativePath).toAbsolutePath();
                        // hack: disallow directory traversal outside of output directory. we don't want to delete wrong files.
                        if (file.toString().startsWith(outDir.toAbsolutePath().toString())) {
                            try {
                                Files.delete(file);
                            } catch (Throwable e) {
                                System.out.printf(Locale.ROOT, "[%s] Generator %s failed to clean file %s…%n", Thread.currentThread().getName(), name, file);
                            }
                        }
                    } else {
                        System.out.printf(Locale.ROOT, "[%s] Generator %s skip cleaning special filename %s…%n", Thread.currentThread().getName(), name, relativePath);
                    }
                });
            }
        }
    }

    static SimpleModule getCustomDeserializationModel(final File includesDir) {
        // Create a module which loads our config files, but supports a special "!include" key which can point to an existing config file.
        // This allows us to create a sort of meta-config which holds configs which are otherwise required at CLI time (via generate task).
        // That is, this allows us to create a wrapper config for generatorName, inputSpec, outputDir, etc.
        SimpleModule module = new SimpleModule("GenerateBatch");
        module.setDeserializerModifier(new BeanDeserializerModifier() {
            @Override
            public JsonDeserializer<?> modifyDeserializer(DeserializationConfig config,
                                                          BeanDescription bd, JsonDeserializer<?> original) {
                JsonDeserializer<?> result;
                if (bd.getBeanClass() == DynamicSettings.class) {
                    result = new DynamicSettingsRefSupport(original, includesDir);
                } else {
                    result = original;
                }
                return result;
            }
        });

        return module;
    }

    static class DynamicSettingsRefSupport extends DelegatingDeserializer {
        private static final String INCLUDE = "!include";
        private File scanDir;

        DynamicSettingsRefSupport(JsonDeserializer<?> delegate, File scanDir) {
            super(delegate);
            this.scanDir = scanDir;
        }

        @Override
        protected JsonDeserializer<?> newDelegatingInstance(JsonDeserializer<?> newDelegatee) {
            return new DynamicSettingsRefSupport(newDelegatee, null);
        }

        @Override
        public Object deserialize(JsonParser p, DeserializationContext ctx) throws IOException {
            ObjectMapper codec = (ObjectMapper) ctx.getParser().getCodec();
            TokenBuffer buffer = new TokenBuffer(p);
            
            recurse(buffer, p, codec, false);
            
            JsonParser newParser = buffer.asParser(codec);
            newParser.nextToken();
            
            return super.deserialize(newParser, ctx);
        }
        
        private void recurse(TokenBuffer buffer, JsonParser p, ObjectMapper codec, boolean skipOuterbraces) throws IOException {
            boolean firstToken = true;
            JsonToken token; 
            
            while ((token = p.nextToken()) != null) {
                String name = p.currentName();
                
                if (skipOuterbraces && firstToken && JsonToken.START_OBJECT.equals(token)) {
                    continue;
                }
                
                if (skipOuterbraces && p.getParsingContext().inRoot() && JsonToken.END_OBJECT.equals(token)) {
                    continue;
                }
                
                if (JsonToken.VALUE_NULL.equals(token)) {
                    continue;
                }
                
                if (name != null && JsonToken.FIELD_NAME.equals(token) && name.startsWith(INCLUDE)) {
                    p.nextToken();
                    String fileName = p.getText();
                    if (fileName != null) {
                        File includeFile = scanDir != null ? new File(scanDir, fileName) : new File(fileName);
                        if (includeFile.exists()) {
                            recurse(buffer, codec.getFactory().createParser(includeFile), codec, true);
                        }
                    }
                } else {
                    buffer.copyCurrentEvent(p);
                }
                
                firstToken = false;
            }
        }
    }
}
