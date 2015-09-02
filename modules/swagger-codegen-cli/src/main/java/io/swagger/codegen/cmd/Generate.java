package io.swagger.codegen.cmd;

import config.Config;
import config.ConfigParser;
import io.airlift.airline.Command;
import io.airlift.airline.Option;
import io.swagger.codegen.CliOption;
import io.swagger.codegen.ClientOptInput;
import io.swagger.codegen.ClientOpts;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.CodegenConfigLoader;
import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.DefaultGenerator;
import io.swagger.codegen.cmd.utils.OptionUtils;
import io.swagger.models.Swagger;
import io.swagger.parser.SwaggerParser;
import org.apache.commons.lang3.tuple.Pair;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.List;
import java.util.Map;
import java.util.Set;

import static org.apache.commons.lang3.StringUtils.isNotEmpty;

/**
 * User: lanwen
 * Date: 24.03.15
 * Time: 20:22
 */

@Command(name = "generate", description = "Generate code with chosen lang")
public class Generate implements Runnable {

    public static final Logger LOG = LoggerFactory.getLogger(Generate.class);

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

    @Option(name = {"--api-package"}, title = "api package", description = CodegenConstants.API_PACKAGE_DESC)
    private String apiPackage;

    @Option(name = {"--model-package"}, title = "model package", description = CodegenConstants.MODEL_PACKAGE_DESC)
    private String modelPackage;

    @Option(name = {"--instantiation-types"}, title = "instantiation types", description = "sets instantiation type mappings in the format of type=instantiatedType,type=instantiatedType." +
            "For example (in Java): array=ArrayList,map=HashMap. In other words array types will get instantiated as ArrayList in generated code.")
    private String instantiationTypes;

    @Option(name = {"--type-mappings"}, title = "type mappings", description = "sets mappings between swagger spec types and generated code types " +
            "in the format of swaggerType=generatedType,swaggerType=generatedType. For example: array=List,map=Map,string=String")
    private String typeMappings;

    @Option(name = {"--additional-properties"}, title = "additional properties", description = "sets additional properties that can be referenced by the mustache templates in the format of name=value,name=value")
    private String additionalProperties;

    @Option(name = {"--language-specific-primitives"}, title = "language specific primitives",
            description = "specifies additional language specific primitive types in the format of type1,type2,type3,type3. For example: String,boolean,Boolean,Double")
    private String languageSpecificPrimitives;

    @Option(name = {"--import-mappings"}, title = "import mappings",
            description = "specifies mappings between a given class and the import that should be used for that class in the format of type=import,type=import")
    private String importMappings;

    @Option(name = {"--invoker-package"}, title = "invoker package", description = CodegenConstants.INVOKER_PACKAGE_DESC)
    private String invokerPackage;

    @Option(name = {"--group-id"}, title = "group id", description = CodegenConstants.GROUP_ID_DESC)
    private String groupId;

    @Option(name = {"--artifact-id"}, title = "artifact id", description = CodegenConstants.ARTIFACT_ID_DESC)
    private String artifactId;

    @Option(name = {"--artifact-version"}, title = "artifact version", description = CodegenConstants.ARTIFACT_VERSION_DESC)
    private String artifactVersion;

    @Override
    public void run() {
        verbosed(verbose);

        setSystemProperties();

        CodegenConfig config = CodegenConfigLoader.forName(lang);

        config.setOutputDir(new File(output).getAbsolutePath());
        config.setSkipOverwrite(skipOverwrite);

        putKeyValuePairsInMap(config.instantiationTypes(), instantiationTypes);
        putKeyValuePairsInMap(config.typeMapping(), typeMappings);
        putKeyValuePairsInMap(config.additionalProperties(), additionalProperties);
        putKeyValuePairsInMap(config.importMapping(), importMappings);

        addValuesToSet(config.languageSpecificPrimitives(), languageSpecificPrimitives);

        checkAndSetAdditionalProperty(config, apiPackage, CodegenConstants.API_PACKAGE);
        checkAndSetAdditionalProperty(config, modelPackage, CodegenConstants.MODEL_PACKAGE);

        if(isNotEmpty(templateDir)) {
            config.additionalProperties().put(CodegenConstants.TEMPLATE_DIR, new File(templateDir).getAbsolutePath());
        }

        checkAndSetAdditionalProperty(config, invokerPackage, CodegenConstants.INVOKER_PACKAGE);
        checkAndSetAdditionalProperty(config, groupId, CodegenConstants.GROUP_ID);
        checkAndSetAdditionalProperty(config, artifactId, CodegenConstants.ARTIFACT_ID);
        checkAndSetAdditionalProperty(config, artifactVersion, CodegenConstants.ARTIFACT_VERSION);

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

        ClientOptInput input = new ClientOptInput().config(config);

        if (isNotEmpty(auth)) {
            input.setAuth(auth);
        }

        Swagger swagger = new SwaggerParser().read(spec, input.getAuthorizationValues(), true);
        new DefaultGenerator().opts(input.opts(new ClientOpts()).swagger(swagger)).generate();
    }

    private void addValuesToSet(Set<String> set, String csvProperty) {
        final List<String> values = OptionUtils.splitCommaSeparatedList(csvProperty);

        for (String value : values) {
            set.add(value);
        }
    }

    private void checkAndSetAdditionalProperty(CodegenConfig config, String property, String propertyKey) {
        checkAndSetAdditionalProperty(config, property, property, propertyKey);
    }

    private void checkAndSetAdditionalProperty(CodegenConfig config, String property, String valueToSet, String propertyKey) {
        if(isNotEmpty(property)) {
            config.additionalProperties().put(propertyKey, valueToSet);
        }
    }

    private void setSystemProperties() {

        final List<Pair<String, String>> systemPropertyPairs = OptionUtils.parseCommaSeparatedTuples(systemProperties);

        for (Pair<String, String> pair : systemPropertyPairs) {
            System.setProperty(pair.getLeft(), pair.getRight());
        }
    }

    private void putKeyValuePairsInMap(Map map, String commaSeparatedKVPairs) {
        final List<Pair<String, String>> pairs = OptionUtils.parseCommaSeparatedTuples(commaSeparatedKVPairs);

        for (Pair<String, String> pair : pairs) {
            map.put(pair.getLeft(), pair.getRight());
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
