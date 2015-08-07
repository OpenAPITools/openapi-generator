package io.swagger.codegen.cmd;

import config.Config;
import config.ConfigParser;
import io.airlift.airline.Command;
import io.airlift.airline.Option;
import io.swagger.codegen.CliOption;
import io.swagger.codegen.ClientOptInput;
import io.swagger.codegen.ClientOpts;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.DefaultGenerator;
import io.swagger.models.Swagger;
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

    @Option(name = {"-D"}, title = "system properties", description = "sets specified system properties in " +
            "the format of name=value,name=value")
    private String systemProperties;

    @Option(name = {"-c", "--config"}, title = "configuration file", description = "Path to json configuration file. " +
            "File content should be in a json format {\"optionKey\":\"optionValue\", \"optionKey1\":\"optionValue1\"...} " +
            "Supported options can be different for each language. Run config-help -l {lang} command for language specific config options.")
    private String configFile;

    @Option(name = {"-s", "--skip-overwrite"}, title = "skip overwrite", description = "specifies if the existing files should be " +
            "overwritten during the generation.")
    private boolean skipOverwrite;

    /**
     * Tries to load config class with SPI first, then with class name directly from classpath
     *
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

    @Override
    public void run() {
        verbosed(verbose);

        setSystemProperties();

        ClientOptInput input = new ClientOptInput();

        if (isNotEmpty(auth)) {
            input.setAuth(auth);
        }

        CodegenConfig config = forName(lang);
        config.setOutputDir(new File(output).getAbsolutePath());

        if (null != templateDir) {
            config.additionalProperties().put(TEMPLATE_DIR_PARAM, new File(templateDir).getAbsolutePath());
        }

        if (null != configFile) {
            Config genConfig = ConfigParser.read(configFile);
            if (null != genConfig) {
                for (CliOption langCliOption : config.cliOptions()) {
                    String opt = langCliOption.getOpt();
                    if (genConfig.hasOption(opt)) {
                        config.additionalProperties().put(opt, genConfig.getOption(opt));
                        // the "library" config option is for library template (sub-template)
                        if ("library".equals(opt)) {
                            config.setLibrary(genConfig.getOption(opt));
                        }
                    }
                }
            }
        }

        config.setSkipOverwrite(skipOverwrite);
        input.setConfig(config);

        Swagger swagger = new SwaggerParser().read(spec, input.getAuthorizationValues(), true);
        new DefaultGenerator().opts(input.opts(new ClientOpts()).swagger(swagger)).generate();
    }

    private void setSystemProperties() {
        if (systemProperties != null && systemProperties.length() > 0) {
            for (String property : systemProperties.split(",")) {
                int ix = property.indexOf('=');
                if (ix > 0 && ix < property.length() - 1) {
                    System.setProperty(property.substring(0, ix), property.substring(ix + 1));
                }
            }
        }
    }

    /**
     * If true parameter, adds system properties which enables debug mode in generator
     *
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
}
