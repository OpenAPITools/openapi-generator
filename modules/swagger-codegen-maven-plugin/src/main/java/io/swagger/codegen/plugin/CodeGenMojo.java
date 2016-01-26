package io.swagger.codegen.plugin;

/*
 * Copyright 2001-2005 The Apache Software Foundation.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

import config.Config;
import config.ConfigParser;
import io.swagger.codegen.*;
import io.swagger.codegen.utils.OptionUtils;
import io.swagger.models.Swagger;
import io.swagger.parser.SwaggerParser;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.project.MavenProject;

import java.io.File;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static io.swagger.codegen.plugin.AdditionalParams.*;

/**
 * Goal which generates client/server code from a swagger json/yaml definition.
 */
@Mojo(name = "generate", defaultPhase = LifecyclePhase.GENERATE_SOURCES)
public class CodeGenMojo extends AbstractMojo {
    /**
     * Location of the output directory.
     */
    @Parameter(name = "output",
            property = "swagger.codegen.maven.plugin.output",
            defaultValue = "${project.build.directory}/generated-sources/swagger")
    private File output;

    /**
     * Location of the swagger spec, as URL or file.
     */
    @Parameter(name = "inputSpec", required = true)
    private String inputSpec;

    /**
     * Folder containing the template files.
     */
    @Parameter(name = "templateDirectory")
    private File templateDirectory;

    /**
     * The package to use for generated model objects/classes
     */
    @Parameter(name = "modelPackage")
    private String modelPackage;

    /**
     * The package to use for generated api objects/classes
     */
    @Parameter(name = "apiPackage")
    private String apiPackage;

    /**
     * The package to use for the generated invoker objects
     */
    @Parameter(name = "invokerPackage")
    private String invokerPackage;

    /**
     * Client language to generate.
     */
    @Parameter(name = "language", required = true)
    private String language;

    /**
     * Path to separate json configuration file.
     */
    @Parameter(name = "configurationFile", required = false)
    private String configurationFile;

    /**
     * Sets the library
     */
    @Parameter(name = "library", required = false)
    private String library;

    /**
     * A map of language-specific parameters as passed with the -c option to the command line
     */
    @Parameter(name = "configOptions")
    private Map<?, ?> configOptions;

    /**
     * Add the output directory to the project as a source root, so that the
     * generated java types are compiled and included in the project artifact.
     */
    @Parameter(defaultValue = "true")
    private boolean addCompileSourceRoot = true;

    @Parameter
    protected Map<String, String> environmentVariables = new HashMap<String, String>();

    @Parameter
    private boolean configHelp = false;

    /**
     * The project being built.
     */
    @Parameter(readonly = true, required = true, defaultValue = "${project}")
    private MavenProject project;

    @Override
    public void execute() throws MojoExecutionException {
        Swagger swagger = new SwaggerParser().read(inputSpec);

        CodegenConfig config = CodegenConfigLoader.forName(language);
        config.setOutputDir(output.getAbsolutePath());

        if (environmentVariables != null) {
            for(String key : environmentVariables.keySet()) {
                String value = environmentVariables.get(key);
                if(value == null) {
                    // don't put null values
                    value = "";
                }
                System.setProperty(key, value);
            }
        }
        if (null != library) {
            config.setLibrary(library);
        }
        if (null != templateDirectory) {
            config.additionalProperties().put(TEMPLATE_DIR_PARAM, templateDirectory.getAbsolutePath());
        }
        if (null != modelPackage) {
            config.additionalProperties().put(MODEL_PACKAGE_PARAM, modelPackage);
        }
        if (null != apiPackage) {
            config.additionalProperties().put(API_PACKAGE_PARAM, apiPackage);
        }
        if (null != invokerPackage) {
            config.additionalProperties().put(INVOKER_PACKAGE_PARAM, invokerPackage);
        }

        if (configOptions != null) {
            for (CliOption langCliOption : config.cliOptions()) {
                if (configOptions.containsKey(langCliOption.getOpt())) {
                    config.additionalProperties().put(langCliOption.getOpt(),
                            configOptions.get(langCliOption.getOpt()));
                }
            }
            if(configOptions.containsKey("import-mappings")) {
                Map<String, String> mappings = createMapFromKeyValuePairs(configOptions.get("import-mappings").toString());
                config.importMapping().putAll(mappings);
            }

            if(configOptions.containsKey("type-mappings")) {
                Map<String, String> mappings = createMapFromKeyValuePairs(configOptions.get("type-mappings").toString());
                config.typeMapping().putAll(mappings);
            }

            if(configOptions.containsKey("instantiation-types")) {
                Map<String, String> mappings = createMapFromKeyValuePairs(configOptions.get("instantiation-types").toString());
                config.instantiationTypes().putAll(mappings);
            }
        }

        if (null != configurationFile) {
            Config genConfig = ConfigParser.read(configurationFile);
            if (null != genConfig) {
                for (CliOption langCliOption : config.cliOptions()) {
                    if (genConfig.hasOption(langCliOption.getOpt())) {
                        config.additionalProperties().put(langCliOption.getOpt(), genConfig.getOption(langCliOption.getOpt()));
                    }
                }
            } else {
            	throw new RuntimeException("Unable to read configuration file");
            }
        }
        
        ClientOptInput input = new ClientOptInput().opts(new ClientOpts()).swagger(swagger);
        input.setConfig(config);

        if(configHelp) {
            for (CliOption langCliOption : config.cliOptions()) {
                System.out.println("\t" + langCliOption.getOpt());
                System.out.println("\t    " + langCliOption.getOptionHelp().replaceAll("\n", "\n\t    "));
                System.out.println();
            }
            return;
        }
        try {
            new DefaultGenerator().opts(input).generate();
        } catch (Exception e) {
            // Maven logs exceptions thrown by plugins only if invoked with -e
        	// I find it annoying to jump through hoops to get basic diagnostic information,
        	// so let's log it in any case:
            getLog().error(e); 
            throw new MojoExecutionException("Code generation failed. See above for the full exception.");
        }

        if (addCompileSourceRoot) {
            project.addCompileSourceRoot(output.toString());
        }
    }

    private static Map<String, String> createMapFromKeyValuePairs(String commaSeparatedKVPairs) {
        final List<Pair<String, String>> pairs = OptionUtils.parseCommaSeparatedTuples(commaSeparatedKVPairs);

        Map<String, String> result = new HashMap<String, String>();

        for (Pair<String, String> pair : pairs) {
            result.put(pair.getLeft(), pair.getRight());
        }

        return result;
    }
}
