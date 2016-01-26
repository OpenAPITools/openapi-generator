package io.swagger.codegen.cmd;

import io.airlift.airline.Command;
import io.airlift.airline.Option;
import io.swagger.codegen.ClientOptInput;
import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.DefaultGenerator;
import io.swagger.codegen.utils.OptionUtils;
import io.swagger.codegen.config.CodegenConfigurator;
import org.apache.commons.lang3.tuple.Pair;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.HashMap;
import java.util.HashSet;
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
    private Boolean verbose;

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
    private Boolean skipOverwrite;

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

    @Option(name = {"--library"}, title = "library", description = CodegenConstants.LIBRARY_DESC)
    private String library;

    @Override
    public void run() {

        //attempt to read from config file
        CodegenConfigurator configurator = CodegenConfigurator.fromFile(configFile);

        //if a config file wasn't specified or we were unable to read it
        if(configurator == null) {
            //createa a fresh configurator
            configurator = new CodegenConfigurator();
        }

        //now override with any specified parameters
        if (verbose != null) {
            configurator.setVerbose(verbose);
        }

        if(skipOverwrite != null) {
            configurator.setSkipOverwrite(skipOverwrite);
        }

        if(isNotEmpty(spec)) {
            configurator.setInputSpec(spec);
        }

        if(isNotEmpty(lang)) {
            configurator.setLang(lang);
        }

        if(isNotEmpty(output)) {
            configurator.setOutputDir(output);
        }

        if(isNotEmpty(auth)) {
            configurator.setAuth(auth);
        }

        if(isNotEmpty(templateDir)) {
            configurator.setTemplateDir(templateDir);
        }

        if(isNotEmpty(apiPackage)) {
            configurator.setApiPackage(apiPackage);
        }

        if(isNotEmpty(modelPackage)) {
            configurator.setModelPackage(modelPackage);
        }

        if(isNotEmpty(invokerPackage)) {
            configurator.setInvokerPackage(invokerPackage);
        }

        if(isNotEmpty(groupId)) {
            configurator.setGroupId(groupId);
        }

        if(isNotEmpty(artifactId)) {
            configurator.setArtifactId(artifactId);
        }

        if(isNotEmpty(artifactVersion)) {
            configurator.setArtifactVersion(artifactVersion);
        }

        if(isNotEmpty(library)) {
            configurator.setLibrary(library);
        }

        setSystemProperties(configurator);
        setInstantiationTypes(configurator);
        setImportMappings(configurator);
        setTypeMappings(configurator);
        setAdditionalProperties(configurator);
        setLanguageSpecificPrimitives(configurator);

        final ClientOptInput clientOptInput = configurator.toClientOptInput();

        new DefaultGenerator().opts(clientOptInput).generate();
    }

    private void setSystemProperties(CodegenConfigurator configurator) {
        final Map<String, String> map = createMapFromKeyValuePairs(systemProperties);
        for (Map.Entry<String, String> entry : map.entrySet()) {
            configurator.addSystemProperty(entry.getKey(), entry.getValue());
        }
    }

    private void setInstantiationTypes(CodegenConfigurator configurator) {
        final Map<String, String> map = createMapFromKeyValuePairs(instantiationTypes);
        for (Map.Entry<String, String> entry : map.entrySet()) {
            configurator.addInstantiationType(entry.getKey(), entry.getValue());
        }
    }

    private void setImportMappings(CodegenConfigurator configurator) {
        final Map<String, String> map = createMapFromKeyValuePairs(importMappings);
        for (Map.Entry<String, String> entry : map.entrySet()) {
            configurator.addImportMapping(entry.getKey(), entry.getValue());
        }
    }

    private void setTypeMappings(CodegenConfigurator configurator) {
        final Map<String, String> map = createMapFromKeyValuePairs(typeMappings);
        for (Map.Entry<String, String> entry : map.entrySet()) {
            configurator.addTypeMapping(entry.getKey(), entry.getValue());
        }
    }

    private void setAdditionalProperties(CodegenConfigurator configurator) {
        final Map<String, String> map = createMapFromKeyValuePairs(additionalProperties);
        for (Map.Entry<String, String> entry : map.entrySet()) {
            configurator.addAdditionalProperty(entry.getKey(), entry.getValue());
        }
    }

    private void setLanguageSpecificPrimitives(CodegenConfigurator configurator) {
        final Set<String> set = createSetFromCsvList(languageSpecificPrimitives);
        for (String item : set) {
            configurator.addLanguageSpecificPrimitive(item);
        }
    }

    private static Set<String> createSetFromCsvList(String csvProperty) {
        final List<String> values = OptionUtils.splitCommaSeparatedList(csvProperty);
        return new HashSet<String>(values);
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
