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

import static ch.lambdaj.collection.LambdaCollections.with;
import static com.google.common.base.Joiner.on;

import com.google.common.base.CaseFormat;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import com.samskivert.mustache.Mustache;
import io.airlift.airline.Command;
import io.airlift.airline.Option;
import org.apache.commons.io.FileUtils;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.SupportingFile;
import org.openapitools.codegen.TemplateManager;
import org.openapitools.codegen.api.TemplatePathLocator;
import org.openapitools.codegen.templating.MustacheEngineAdapter;
import org.openapitools.codegen.templating.TemplateManagerOptions;
import org.openapitools.codegen.templating.CommonTemplateContentLocator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.IOException;
import java.io.Reader;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.Map;

import ch.lambdaj.function.convert.Converter;

/**
 * User: lanwen Date: 24.03.15 Time: 20:22
 */

@Command(name = "meta", description = "MetaGenerator. Generator for creating a new template set "
        + "and configuration for Codegen.  The output will be based on the language you "
        + "specify, and includes default templates to include.")
public class Meta extends OpenApiGeneratorCommand {

    private static final Logger LOGGER = LoggerFactory.getLogger(Meta.class);

    private static final String TEMPLATE_DIR_CLASSPATH = "codegen";
    private static final String MUSTACHE_EXTENSION = ".mustache";

    @Option(name = {"-o", "--output"}, title = "output directory",
            description = "where to write the generated files (current dir by default)")
    private String outputFolder = "";

    @Option(name = {"-n", "--name"}, title = "name",
            description = "the human-readable name of the generator")
    private String name = "default";

    @Option(name = {"-p", "--package"}, title = "package",
            description = "the package to put the main class into (defaults to org.openapitools.codegen)")
    private String targetPackage = "org.openapitools.codegen";

    @Option(name = {"-t", "--type"}, title = "type",
            description = "the type of generator that is created",
            allowedValues = {"CLIENT", "SERVER", "DOCUMENTATION", "CONFIG", "OTHER"})
    private String type = "OTHER";

    @Option(name = {"-l", "--language"}, title = "language",
            description = "the implementation language for the generator class",
            allowedValues = {"java", "kotlin"}
    )
    private String language = "java";

    @Override
    public void execute() {
        final File targetDir = new File(outputFolder);
        LOGGER.info("writing to folder [{}]", targetDir.getAbsolutePath());

        String mainClass = CaseFormat.LOWER_HYPHEN.to(CaseFormat.UPPER_CAMEL, name) + "Generator";
        String kebabName = CaseFormat.UPPER_CAMEL.to(CaseFormat.LOWER_HYPHEN, name);

        List<SupportingFile> supportingFiles = "kotlin".equals(language) ?
                ImmutableList.of(
                        new SupportingFile("kotlin/build_gradle.mustache", "", "build.gradle.kts"),
                        new SupportingFile("kotlin/gradle.properties", "", "gradle.properties"),
                        new SupportingFile("kotlin/settings.mustache", "", "settings.gradle"),
                        new SupportingFile("kotlin/generatorClass.mustache", on(File.separator).join("src/main/kotlin", asPath(targetPackage)), mainClass.concat(".kt")),
                        new SupportingFile("kotlin/generatorClassTest.mustache", on(File.separator).join("src/test/kotlin", asPath(targetPackage)), mainClass.concat("Test.kt")),
                        new SupportingFile("kotlin/README.mustache", "", "README.md"),

                        new SupportingFile("api.template", "src/main/resources" + File.separator + name,"api.mustache"),
                        new SupportingFile("model.template", "src/main/resources" + File.separator + name,"model.mustache"),
                        new SupportingFile("myFile.template", String.join(File.separator, "src", "main", "resources", name), "myFile.mustache"),
                        new SupportingFile("services.mustache", "src/main/resources/META-INF/services", CodegenConfig.class.getCanonicalName()))
                : ImmutableList.of(
                        new SupportingFile("pom.mustache", "", "pom.xml"),
                        new SupportingFile("generatorClass.mustache", on(File.separator).join("src/main/java", asPath(targetPackage)), mainClass.concat(".java")),
                        new SupportingFile("generatorClassTest.mustache", on(File.separator).join("src/test/java", asPath(targetPackage)), mainClass.concat("Test.java")),
                        new SupportingFile("README.mustache", "", "README.md"),
                        new SupportingFile("api.template", "src/main/resources" + File.separator + name,"api.mustache"),
                        new SupportingFile("model.template", "src/main/resources" + File.separator + name,"model.mustache"),
                        new SupportingFile("myFile.template", String.join(File.separator, "src", "main", "resources", name), "myFile.mustache"),
                        new SupportingFile("services.mustache", "src/main/resources/META-INF/services", CodegenConfig.class.getCanonicalName()));

        String currentVersion = buildInfo.getVersion();

        Map<String, Object> data =
                new ImmutableMap.Builder<String, Object>()
                        .put("generatorPackage", targetPackage)
                        .put("generatorClass", mainClass)
                        .put("name", name)
                        .put("kebabName", kebabName)
                        .put("generatorType", type)
                        .put("fullyQualifiedGeneratorClass", targetPackage + "." + mainClass)
                        .put("openapiGeneratorVersion", currentVersion).build();


        with(supportingFiles).convert(processFiles(targetDir, data));
    }

    /**
     * Converter method to process supporting files: execute with mustache, or simply copy to
     * destination directory
     *
     * @param targetDir - destination directory
     * @param data - map with additional params needed to process templates
     * @return converter object to pass to lambdaj
     */
    private static Converter<SupportingFile, File> processFiles(final File targetDir,
            final Map<String, Object> data) {
        return support -> {
            try {
                File destinationFolder =
                        new File(new File(targetDir.getAbsolutePath()), support.folder);
                File outputFile = new File(destinationFolder, support.destinationFilename);

                TemplateManager templateProcessor = new TemplateManager(
                        new TemplateManagerOptions(false, false),
                        new MustacheEngineAdapter(),
                        new TemplatePathLocator[]{ new CommonTemplateContentLocator("codegen") }
                );

                String template = templateProcessor.readTemplate(new File(TEMPLATE_DIR_CLASSPATH, support.templateFile).getPath());

                String formatted = template;

                Mustache.TemplateLoader loader = name -> templateProcessor.getTemplateReader(name.concat(MUSTACHE_EXTENSION));

                if (support.templateFile.endsWith(MUSTACHE_EXTENSION)) {
                    LOGGER.info("writing file to {}", outputFile.getAbsolutePath());
                    formatted =
                            Mustache.compiler().withLoader(loader).defaultValue("")
                                    .compile(template).execute(data);
                } else {
                    LOGGER.info("copying file to {}", outputFile.getAbsolutePath());
                }

                FileUtils.writeStringToFile(outputFile, formatted, StandardCharsets.UTF_8);
                return outputFile;

            } catch (IOException e) {
                throw new RuntimeException("Can't generate project", e);
            }
        };
    }

    /**
     * Converts package name to path on file system
     *
     * @param packageName - package name to convert
     * @return relative path
     */
    private static String asPath(String packageName) {
        return packageName.replace(".", File.separator);
    }
}
