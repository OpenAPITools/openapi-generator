package io.swagger.codegen;

import io.swagger.oas.models.OpenAPI;
import io.swagger.oas.models.media.ArraySchema;
import io.swagger.oas.models.media.BooleanSchema;
import io.swagger.oas.models.media.Schema;
import io.swagger.parser.v3.OpenAPIV3Parser;
import net.sourceforge.argparse4j.ArgumentParsers;
import net.sourceforge.argparse4j.inf.Argument;
import net.sourceforge.argparse4j.inf.ArgumentParser;
import net.sourceforge.argparse4j.inf.ArgumentParserException;
import net.sourceforge.argparse4j.inf.Subparser;
import net.sourceforge.argparse4j.inf.Subparsers;
import org.apache.commons.beanutils.BeanUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.lang.reflect.InvocationTargetException;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

/**
 * User: lanwen Date: 24.03.15 Time: 17:56
 * <p>
 * Command line interface for swagger codegen use `swagger-codegen-cli.jar help` for more info
 *
 * @since 2.1.3-M1
 */
public class SwaggerCodegen {


    private static Logger LOGGER = LoggerFactory.getLogger(SwaggerCodegen.class);

    public static void main(String[] args) {
        final String oas3 = CLIHelper.loadResourceOAS3File();
        if(StringUtils.isBlank(oas3)) {
            LOGGER.error("Could not load resource file.");
            return;
        }
        final OpenAPI openAPI = new OpenAPIV3Parser().readContents(oas3, null, null).getOpenAPI();
        final Map<String, Schema> schemaMap = openAPI.getComponents().getSchemas();
        final Set<String> schemaNames = schemaMap.keySet();

        final ArgumentParser codegenParser = ArgumentParsers.newFor("swagger-codegen").build();
        final Subparsers subparsers = codegenParser.addSubparsers()
                .title("commands")
                .help("additional help")
                .metavar("Command");

        final Map<String, Schema> commandMap = new HashMap<>();

        for(String schemaName : schemaNames) {
            final Schema schema = schemaMap.get(schemaName);
            final String command = CLIHelper.getCommand(schemaName, schema);
            final Map<String, Schema> schemaProperties = schema.getProperties();
            if(schemaProperties == null || schemaProperties.isEmpty()) {
                LOGGER.warn(String.format("there are not options for command '%s'", command));
                continue;
            }

            commandMap.put(command, schema);

            final Subparser parser = subparsers.addParser(command).help(command);

            for (String propertyName : schemaProperties.keySet()) {
                final Schema property = schemaProperties.get(propertyName);
                final Map<String, Object> extensions = property.getExtensions();
                if(!CLIHelper.containsOptionExtensions(extensions)) {
                    LOGGER.warn(String.format("there are not option extensions for property '%s?", propertyName));
                    continue;
                }
                String[] arguments = CLIHelper.getArguments(extensions);
                final Argument argument = parser.addArgument(arguments)
                        .type(CLIHelper.getClass(property))
                        .help(property.getDescription())
                        .metavar(StringUtils.EMPTY);

                if(property instanceof BooleanSchema) {
                    argument.nargs("?").setConst(true);
                } else if(property instanceof ArraySchema) {
                    argument.nargs("*");
                }
            }
        }
        final Map<String, Object> inputArgs = new HashMap<>();
        try {
            codegenParser.parseArgs(args, inputArgs);
        } catch (ArgumentParserException e) {
            codegenParser.handleError(e);
            return;
        }
        final String userInputCommand = CLIHelper.detectCommand(args);
        if(userInputCommand == null) {
            LOGGER.error("No command found.");
            return;
        }
        final Schema commandSchema = commandMap.get(userInputCommand);
        if(commandSchema == null) {
            LOGGER.error(String.format("There are not schema related to command '%s'", userInputCommand));
            return;
        }
        final Map<String, Object> extensions = commandSchema.getExtensions();
        if(extensions == null || extensions.isEmpty() || extensions.get("x-class-name") == null) {
            LOGGER.error("Extensions are required to run command. i.e: 'x-class-name'");
            return;
        }
        final String className = extensions.get("x-class-name").toString();
        try {
            final Class clazz = Class.forName(className);
            final Object commandObject = clazz.newInstance();
            final Map<String, Object> optionValueMap = CLIHelper.createOptionValueMap(commandSchema, inputArgs);

            BeanUtils.populate(commandObject, optionValueMap);
            if(commandObject instanceof Runnable) {
                System.out.println("time to run boy...");
                ((Runnable) commandObject).run();
            }

        } catch (ClassNotFoundException e) {
            LOGGER.error(String.format("Could not load class '%s' for command '%s'", className, userInputCommand));
        } catch (IllegalAccessException e) {
            e.printStackTrace();
        } catch (InstantiationException e) {
            e.printStackTrace();
        } catch (InvocationTargetException e) {
            e.printStackTrace();
        }
    }
}
