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

import io.swagger.codegen.CliOption;
import io.swagger.codegen.ClientOptInput;
import io.swagger.codegen.ClientOpts;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.DefaultGenerator;
import io.swagger.models.Swagger;
import io.swagger.parser.SwaggerParser;
import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.project.MavenProject;

import java.io.File;
import java.util.Map;
import java.util.ServiceLoader;

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
     * Folder containing the template files.
     */
    @Parameter(name = "modelPackage")
    private String modelPackage;

    /**
     * Folder containing the template files.
     */
    @Parameter(name = "apiPackage")
    private String apiPackage;

    /**
     * Folder containing the template files.
     */
    @Parameter(name = "invokerPackage")
    private String invokerPackage;

    /**
     * Folder containing the template files.
     */
    @Parameter(name = "implFolder")
    private String implFolder;

    /**
     * Folder containing the template files.
     */
    @Parameter(name = "sourceFolder")
    private File sourceFolder;

    /**
     * Client language to generate.
     */
    @Parameter(name = "language", required = true)
    private String language;

    /**
     * A map of language-specific properties as passed with the -c option to the command line
     */
    @Parameter(name ="configOptions")
    private Map configOptions;

    /**
     * Add the output directory to the project as a source root, so that the
     * generated java types are compiled and included in the project artifact.
     */
    @Parameter(defaultValue = "true")
    private boolean addCompileSourceRoot = true;

    /**
     * The project being built.
     */
    @Parameter(readonly = true, required = true, defaultValue = "${project}")
    private MavenProject project;

    @Override
    public void execute() throws MojoExecutionException {
        Swagger swagger = new SwaggerParser().read(inputSpec);

        CodegenConfig config = forName(language);
        config.setOutputDir(output.getAbsolutePath());

        if (null != templateDirectory) {
            config.additionalProperties().put(TEMPLATE_DIR_PARAM, templateDirectory.getAbsolutePath());
        }
        if (null != modelPackage) {
            config.additionalProperties().put(MODEL_PACKAGE_PARAM, modelPackage );
        }
        if (null != apiPackage) {
            config.additionalProperties().put(API_PACKAGE_PARAM, apiPackage);
        }
        if (null != invokerPackage) {
            config.additionalProperties().put(INVOKER_PACKAGE_PARAM, invokerPackage);
        }
        if (null != sourceFolder) {
            config.additionalProperties().put(SOURCE_FOLDER_PARAM, sourceFolder.getAbsolutePath());
        }
        if (null != implFolder) {
            config.additionalProperties().put(IMPL_FOLDER_PARAM, implFolder);
        }

        ClientOpts clientOpts = new ClientOpts();
        if( configOptions != null ){
            for (CliOption langCliOption : config.cliOptions()) {
                if (configOptions.containsKey(langCliOption.getOpt())) {
                    config.additionalProperties().put(langCliOption.getOpt(),
                            configOptions.get(langCliOption.getOpt()));
                }
            }
        }

        ClientOptInput input = new ClientOptInput().opts(clientOpts).swagger(swagger);
        input.setConfig(config);
        new DefaultGenerator().opts(input).generate();

        if (addCompileSourceRoot) {
            project.addCompileSourceRoot(output.toString());
        }
    }

    private CodegenConfig forName(String name) {
        ServiceLoader<CodegenConfig> loader = ServiceLoader.load(CodegenConfig.class);
        for (CodegenConfig config : loader) {
            if (config.getName().equals(name)) {
                return config;
            }
        }

        // else try to load directly
        try {
            return (CodegenConfig) Class.forName(name).newInstance();
        } catch (Exception e) {
            throw new RuntimeException("Can't load config class with name ".concat(name), e);
        }
    }
}
