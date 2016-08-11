package io.swagger.codegen;

import com.samskivert.mustache.Mustache;
import com.samskivert.mustache.Template;
import org.apache.commons.cli.BasicParser;
import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.HelpFormatter;
import org.apache.commons.cli.Options;
import org.apache.commons.io.FileUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.IOException;
import java.io.Reader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.ServiceLoader;

/**
 * @deprecated use instead {@link io.swagger.codegen.DefaultGenerator}
 * or cli interface from https://github.com/swagger-api/swagger-codegen/pull/547
 */
@Deprecated
public class MetaGenerator extends AbstractGenerator {

    private static final Logger LOGGER = LoggerFactory.getLogger(MetaGenerator.class);

    static Map<String, CodegenConfig> configs = new HashMap<String, CodegenConfig>();
    static String configString;

    public static void main(String[] args) {
        new MetaGenerator().generate(args);
    }

    public static List<CodegenConfig> getExtensions() {
        ServiceLoader<CodegenConfig> loader = ServiceLoader.load(CodegenConfig.class);
        List<CodegenConfig> output = new ArrayList<CodegenConfig>();
        Iterator<CodegenConfig> itr = loader.iterator();
        while (itr.hasNext()) {
            output.add(itr.next());
        }
        return output;
    }

    static void usage(Options options) {
        HelpFormatter formatter = new HelpFormatter();
        formatter.printHelp("MetaGenerator. Generator for creating a new template set " +
                "and configuration for Codegen.  The output will be based on the language you " +
                "specify, and includes default templates to include.", options);
    }

    public static CodegenConfig getConfig(String name) {
        if (configs.containsKey(name)) {
            return configs.get(name);
        }
        return null;
    }

    protected void generate(String[] args) {
        String outputFolder = null;
        String name = null;
        String targetPackage = "io.swagger.codegen";
        final String templateDir = "codegen";

        Options options = new Options();
        options.addOption("h", "help", false, "shows this message");
        options.addOption("l", "lang", false, "client language to generate.\nAvailable languages include:\n\t[" + configString + "]");
        options.addOption("o", "output", true, "where to write the generated files");
        options.addOption("n", "name", true, "the human-readable name of the generator");
        options.addOption("p", "package", true, "the package to put the main class into (defaults to io.swagger.codegen");

        CommandLine cmd = null;
        try {
            CommandLineParser parser = new BasicParser();
            cmd = parser.parse(options, args);
            if (cmd.hasOption("h")) {
                usage(options);
                return;
            }
            if (cmd.hasOption("n")) {
                name = cmd.getOptionValue("n");
            } else {
                System.out.println("name is required"); //FIXME replace by LOGGER
                usage(options);
                return;
            }
            if (cmd.hasOption("l")) {
            }
            if (cmd.hasOption("p")) {
                targetPackage = cmd.getOptionValue("p");
            }
            if (cmd.hasOption("o")) {
                outputFolder = cmd.getOptionValue("o");
            } else {
                System.out.println("output folder is required"); // FIXME replace by LOGGER
                usage(options);
                return;
            }
        } catch (Exception e) {
            usage(options);
            return;
        }
        LOGGER.info("writing to folder " + outputFolder);
        File outputFolderLocation = new File(outputFolder);
        if (!outputFolderLocation.exists()) {
            outputFolderLocation.mkdirs();
        }
        File sourceFolder = new File(outputFolder + File.separator + "src/main/java/" + targetPackage.replace('.', File.separatorChar));
        if (!sourceFolder.exists()) {
            sourceFolder.mkdirs();
        }
        File resourcesFolder = new File(outputFolder + File.separator + "src/main/resources/META-INF/services");
        if (!resourcesFolder.exists()) {
            resourcesFolder.mkdirs();
        }

        String mainClass = Character.toUpperCase(name.charAt(0)) + name.substring(1) + "Generator";

        List<SupportingFile> supportingFiles = new ArrayList<SupportingFile>();
        supportingFiles.add(new SupportingFile("pom.mustache", "", "pom.xml"));
        supportingFiles.add(new SupportingFile("generatorClass.mustache",
                "src/main/java/" + File.separator + targetPackage.replace('.', File.separatorChar),
                mainClass + ".java"));
        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("api.template", "src/main/resources" + File.separator + name, "api.mustache"));
        supportingFiles.add(new SupportingFile("model.template", "src/main/resources" + File.separator + name, "model.mustache"));

        supportingFiles.add(new SupportingFile("services.mustache", "src/main/resources/META-INF/services", "io.swagger.codegen.CodegenConfig"));

        List<File> files = new ArrayList<File>();

        Map<String, Object> data = new HashMap<String, Object>();
        data.put("generatorPackage", targetPackage);
        data.put("generatorClass", mainClass);
        data.put("name", name);
        data.put("fullyQualifiedGeneratorClass", targetPackage + "." + mainClass);

        for (SupportingFile support : supportingFiles) {
            try {
                String destinationFolder = outputFolder;
                if (support.folder != null && !"".equals(support.folder)) {
                    destinationFolder += File.separator + support.folder;
                }
                File of = new File(destinationFolder);
                if (!of.isDirectory()) {
                    of.mkdirs();
                }
                String outputFilename = destinationFolder + File.separator + support.destinationFilename;

                if (support.templateFile.endsWith("mustache")) {
                    String template = readTemplate(templateDir + File.separator + support.templateFile);
                    Template tmpl = Mustache.compiler()
                            .withLoader(new Mustache.TemplateLoader() {
                                @Override
                                public Reader getTemplate(String name) {
                                    return getTemplateReader(templateDir + File.separator + name + ".mustache");
                                }
                            })
                            .defaultValue("")
                            .compile(template);

                    writeToFile(outputFilename, tmpl.execute(data));
                    files.add(new File(outputFilename));
                } else {
                    String template = readTemplate(templateDir + File.separator + support.templateFile);
                    FileUtils.writeStringToFile(new File(outputFilename), template);
                    LOGGER.info("copying file to " + outputFilename);
                    files.add(new File(outputFilename));
                }
            } catch (IOException e) {
                LOGGER.error(e.getMessage(), e);
            }
        }
    }

    static {
        List<CodegenConfig> extensions = getExtensions();
        StringBuilder sb = new StringBuilder();

        for (CodegenConfig config : extensions) {
            if (sb.toString().length() != 0) {
                sb.append(", ");
            }
            sb.append(config.getName());
            configs.put(config.getName(), config);
            configString = sb.toString();
        }
    }
}
