package com.wordnik.swagger.codegen.cmd;

import com.wordnik.swagger.codegen.ClientOptInput;
import com.wordnik.swagger.codegen.ClientOpts;
import com.wordnik.swagger.codegen.CodegenConfig;
import com.wordnik.swagger.codegen.DefaultGenerator;
import com.wordnik.swagger.models.Swagger;
import io.airlift.airline.Command;
import io.airlift.airline.Option;
import io.swagger.parser.SwaggerParser;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.ServiceLoader;

import static java.util.ServiceLoader.load;
import static org.apache.commons.lang3.StringUtils.isNotEmpty;

/**
 * User: lanwen
 * Date: 24.03.15
 * Time: 20:22
 */

@Command(name = "generate", description = "Generate code with chosen lang")
public class Generate implements Runnable {

    public static final Logger LOG = LoggerFactory.getLogger(Generate.class);

    public static final String TEMPLATE_DIR_PARAM = "templateDir";

    @Option(name = {"-v", "--verbose"}, description = "verbose mode")
    private boolean verbose;

    @Option(name = {"-l", "--lang"}, title = "language", required = true,
            description = "client language to generate (maybe class name in classpath, required)")
    private String lang;

    @Option(name = {"-o", "--output"}, title = "output directory",
            description = "where to write the generated files (current dir by default)")
    private String output = "";

    @Option(name = {"-i", "--input-spec"}, title = "spec file", required = true,
            description = "location of the swagger spec, as URL or file (required)")
    private String spec;

    @Option(name = {"-t", "--template-dir"}, title = "template directory",
            description = "folder containing the template files")
    private String templateDir;

    @Option(name = {"-a", "--auth"}, title = "authorization",
            description = "adds authorization headers when fetching the swagger definitions remotely. " +
                    "Pass in a URL-encoded string of name:header with a comma separating multiple values")
    private String auth;

    @Override
    public void run() {
        verbosed(verbose);

        ClientOptInput input = new ClientOptInput();

        if (isNotEmpty(auth)) {
            input.setAuth(auth);
        }

        CodegenConfig config = forName(lang);
        config.setOutputDir(new File(output).getAbsolutePath());

        if (null != templateDir) {
            config.additionalProperties().put(TEMPLATE_DIR_PARAM, new File(templateDir).getAbsolutePath());
        }

        input.setConfig(config);

        Swagger swagger = new SwaggerParser().read(spec, input.getAuthorizationValues(), true);
        new DefaultGenerator().opts(input.opts(new ClientOpts()).swagger(swagger)).generate();
    }

    /**
     * If true parameter, adds system properties which enables debug mode in generator
     * @param verbose - if true, enables debug mode
     */
    private void verbosed(boolean verbose) {
        if (!verbose) {
            return;
        }
        LOG.info("\nVERBOSE MODE: ON. Additional debug options are injected" +
                "\n - [debugSwagger] prints the swagger specification as interpreted by the codegen" +
                "\n - [debugModels] prints models passed to the template engine" +
                "\n - [debugOperations] prints operations passed to the template engine" +
                "\n - [debugSupportingFiles] prints additional data passed to the template engine");

        System.setProperty("debugSwagger", "");
        System.setProperty("debugModels", "");
        System.setProperty("debugOperations", "");
        System.setProperty("debugSupportingFiles", "");
    }

    /**
     * Tries to load config class with SPI first, then with class name directly from classpath
     * @param name name of config, or full qualified class name in classpath
     * @return config class
     */
    private static CodegenConfig forName(String name) {
        ServiceLoader<CodegenConfig> loader = load(CodegenConfig.class);
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
