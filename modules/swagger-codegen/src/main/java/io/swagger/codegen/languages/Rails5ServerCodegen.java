package io.swagger.codegen.languages;

import java.text.SimpleDateFormat;
import java.util.Date;
import com.fasterxml.jackson.core.JsonProcessingException;

import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.CodegenType;
import io.swagger.codegen.DefaultCodegen;
import io.swagger.codegen.SupportingFile;
import io.swagger.models.properties.ArrayProperty;
import io.swagger.models.properties.MapProperty;
import io.swagger.models.properties.Property;
import io.swagger.models.Swagger;
import io.swagger.util.Yaml;

import java.io.File;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Map;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class Rails5ServerCodegen extends DefaultCodegen implements CodegenConfig {

    private static final Logger LOGGER = LoggerFactory.getLogger(Rails5ServerCodegen.class);
    private static final SimpleDateFormat MIGRATE_FILE_NAME_FORMAT = new SimpleDateFormat("yyyyMMddHHmmss");

    protected String gemName;
    protected String moduleName;
    protected String gemVersion = "1.0.0";
    protected String appFolder = "app";
    protected String channelsFolder = appFolder + File.separator + "channels";
    protected String applicationCableFolder = channelsFolder + File.separator + "application_cable";
    protected String controllersFolder = appFolder + File.separator + "controllers";
    protected String jobsFolder = appFolder + File.separator + "jobs";
    protected String mailersFolder = appFolder + File.separator + "mailers";
    protected String modelsFolder = appFolder + File.separator + "models";
    protected String viewsFolder = appFolder + File.separator + "views";
    protected String layoutsFolder = viewsFolder + File.separator + "layouts";
    protected String binFolder = "bin";
    protected String configFolder = "config";
    protected String environmentsFolder = configFolder + File.separator + "config";
    protected String initializersFolder = configFolder + File.separator + "initializers";
    protected String localesFolder = configFolder + File.separator + "locales";
    protected String dbFolder = "db";
    protected String migrateFolder = dbFolder + File.separator + "migrate";
    protected String libFolder = "lib";
    protected String tasksFolder = libFolder + File.separator + "tasks";
    protected String logFolder = "log";
    protected String publicFolder = "public";
    protected String testFolder = "test";
    protected String tmpFolder = "tmp";
    protected String cacheFolder = tmpFolder + File.separator + "cache";
    protected String pidFolder = tmpFolder + File.separator + "pids";
    protected String socketsFolder = tmpFolder + File.separator + "sockets";
    protected String vendorFolder = "vendor";

    public Rails5ServerCodegen() {
        super();
        outputFolder = "generated-code" + File.separator + "rails5";
        apiPackage = "app/controllers";
        apiTemplateFiles.put("controller.mustache", ".rb");

        modelPackage = "app/models";
        modelTemplateFiles.put("model.mustache", ".rb");

        embeddedTemplateDir = templateDir = "rails5";

        typeMapping.clear();
        languageSpecificPrimitives.clear();

        setReservedWordsLowerCase(
                Arrays.asList(
                        "__FILE__", "and", "def", "end", "in", "or", "self", "unless", "__LINE__",
                        "begin", "defined?", "ensure", "module", "redo", "super", "until", "BEGIN",
                        "break", "do", "false", "next", "rescue", "then", "when", "END", "case",
                        "else", "for", "nil", "retry", "true", "while", "alias", "class", "elsif",
                        "if", "not", "return", "undef", "yield")
        );

        typeMapping.put("string", "string");
        typeMapping.put("char", "string");
        typeMapping.put("int", "integer");
        typeMapping.put("integer", "integer");
        typeMapping.put("long", "integer");
        typeMapping.put("short", "integer");
        typeMapping.put("float", "float");
        typeMapping.put("double", "decimal");
        typeMapping.put("number", "float");
        typeMapping.put("date", "date");
        typeMapping.put("DateTime", "datetime");
        typeMapping.put("boolean", "boolean");
        typeMapping.put("binary", "string");
        typeMapping.put("ByteArray", "string");
        typeMapping.put("UUID", "string");

        // remove modelPackage and apiPackage added by default
        cliOptions.clear();
    }

    @Override
    public void processOpts() {
        super.processOpts();

        // use constant model/api package (folder path)
        //setModelPackage("models");
        setApiPackage("app/controllers");

        supportingFiles.add(new SupportingFile("Gemfile", "", "Gemfile"));
        supportingFiles.add(new SupportingFile("README.md", "", "README.md"));
        supportingFiles.add(new SupportingFile("Rakefile", "", "Rakefile"));
        supportingFiles.add(new SupportingFile("config.ru", "", "config.ru"));
        supportingFiles.add(new SupportingFile("channel.rb", applicationCableFolder, "channel.rb"));
        supportingFiles.add(new SupportingFile("connection.rb", applicationCableFolder, "connection.rb"));
        supportingFiles.add(new SupportingFile("application_controller.rb", controllersFolder, "application_controller.rb"));
        supportingFiles.add(new SupportingFile("application_job.rb", jobsFolder, "application_job.rb"));
        supportingFiles.add(new SupportingFile("application_mailer.rb", mailersFolder, "application_mailer.rb"));
        supportingFiles.add(new SupportingFile("application_record.rb", modelsFolder, "application_record.rb"));
        supportingFiles.add(new SupportingFile("mailer.html.erb", layoutsFolder, "mailer.html.erb"));
        supportingFiles.add(new SupportingFile("mailer.text.erb", layoutsFolder, "mailer.text.erb"));
        supportingFiles.add(new SupportingFile("bundle", binFolder, "bundle"));
        supportingFiles.add(new SupportingFile("rails", binFolder, "rails"));
        supportingFiles.add(new SupportingFile("rake", binFolder, "rake"));
        supportingFiles.add(new SupportingFile("setup", binFolder, "setup"));
        supportingFiles.add(new SupportingFile("update", binFolder, "update"));
        supportingFiles.add(new SupportingFile("development.rb", environmentsFolder, "development.rb"));
        supportingFiles.add(new SupportingFile("production.rb", environmentsFolder, "production.rb"));
        supportingFiles.add(new SupportingFile("active_record_belongs_to_required_by_default.rb", initializersFolder, "active_record_belongs_to_required_by_default.rb"));
        supportingFiles.add(new SupportingFile("application_controller_renderer.rb", initializersFolder, "application_controller_renderer.rb"));
        supportingFiles.add(new SupportingFile("backtrace_silencers.rb", initializersFolder, "backtrace_silencers.rb"));
        supportingFiles.add(new SupportingFile("callback_terminator.rb", initializersFolder, "callback_terminator.rb"));
        supportingFiles.add(new SupportingFile("cors.rb", initializersFolder, "cors.rb"));
        supportingFiles.add(new SupportingFile("filter_parameter_logging.rb", initializersFolder, "filter_parameter_logging.rb"));
        supportingFiles.add(new SupportingFile("inflections.rb", initializersFolder, "inflections.rb"));
        supportingFiles.add(new SupportingFile("mime_types.rb", initializersFolder, "mime_types.rb"));
        supportingFiles.add(new SupportingFile("ssl_options.rb", initializersFolder, "ssl_options.rb"));
        supportingFiles.add(new SupportingFile("to_time_preserves_timezone.rb", initializersFolder, "to_time_preserves_timezone.rb"));
        supportingFiles.add(new SupportingFile("en.yml", localesFolder, "en.yml"));
        supportingFiles.add(new SupportingFile("application.rb", configFolder, "application.rb"));
        supportingFiles.add(new SupportingFile("boot.rb", configFolder, "boot.rb"));
        supportingFiles.add(new SupportingFile("cable.yml", configFolder, "cable.yml"));
        supportingFiles.add(new SupportingFile("database.yml", configFolder, "database.yml"));
        supportingFiles.add(new SupportingFile("environment.rb", configFolder, "environment.rb"));
        supportingFiles.add(new SupportingFile("puma.rb", configFolder, "puma.rb"));
        supportingFiles.add(new SupportingFile("routes.mustache", configFolder, "routes.rb"));
        supportingFiles.add(new SupportingFile("secrets.yml", configFolder, "secrets.yml"));
        supportingFiles.add(new SupportingFile("spring.rb", configFolder, "spring.rb"));
        supportingFiles.add(new SupportingFile(".keep", migrateFolder, ".keep"));
        supportingFiles.add(new SupportingFile("migrate.mustache", migrateFolder, "0_init_tables.rb"));
        supportingFiles.add(new SupportingFile("schema.rb", dbFolder, "schema.rb"));
        supportingFiles.add(new SupportingFile("seeds.rb", dbFolder, "seeds.rb"));
        supportingFiles.add(new SupportingFile(".keep", tasksFolder, ".keep"));
        supportingFiles.add(new SupportingFile(".keep", logFolder, ".keep"));
        supportingFiles.add(new SupportingFile("404.html", publicFolder, "404.html"));
        supportingFiles.add(new SupportingFile("422.html", publicFolder, "422.html"));
        supportingFiles.add(new SupportingFile("500.html", publicFolder, "500.html"));
        supportingFiles.add(new SupportingFile("apple-touch-icon-precomposed.png", publicFolder, "apple-touch-icon-precomposed.png"));
        supportingFiles.add(new SupportingFile("apple-touch-icon.png", publicFolder, "apple-touch-icon.png"));
        supportingFiles.add(new SupportingFile("favicon.ico", publicFolder, "favicon.ico"));
        supportingFiles.add(new SupportingFile("robots.txt", publicFolder, "robots.txt"));
        supportingFiles.add(new SupportingFile("robots.txt", publicFolder, "robots.txt"));
        supportingFiles.add(new SupportingFile("test_helper.rb", testFolder, "test_helper.rb"));
        supportingFiles.add(new SupportingFile(".keep", cacheFolder, ".keep"));
        supportingFiles.add(new SupportingFile(".keep", pidFolder, ".keep"));
        supportingFiles.add(new SupportingFile(".keep", socketsFolder, ".keep"));
        supportingFiles.add(new SupportingFile("restart.txt", tmpFolder, "restart.txt"));
        supportingFiles.add(new SupportingFile(".keep", vendorFolder, ".keep"));
    }

    @Override
    public CodegenType getTag() {
        return CodegenType.SERVER;
    }

    @Override
    public String getName() {
        return "rails5";
    }

    @Override
    public String getHelp() {
        return "Generates a Rails5 server library.";
    }

    @Override
    public String escapeReservedWord(String name) {
        return "_" + name;
    }

    @Override
    public String apiFileFolder() {
        return outputFolder + File.separator + apiPackage.replace("/", File.separator);
    }

    @Override
    public String getTypeDeclaration(Property p) {
        if (p instanceof ArrayProperty) {
            ArrayProperty ap = (ArrayProperty) p;
            Property inner = ap.getItems();
            return getSwaggerType(p) + "[" + getTypeDeclaration(inner) + "]";
        } else if (p instanceof MapProperty) {
            MapProperty mp = (MapProperty) p;
            Property inner = mp.getAdditionalProperties();
            return getSwaggerType(p) + "[string," + getTypeDeclaration(inner) + "]";
        }
        return super.getTypeDeclaration(p);
    }

    @Override
    public String toDefaultValue(Property p) {
        return "null";
    }

    @Override
    public String toVarName(String name) {
        // replace - with _ e.g. created-at => created_at
        name = name.replaceAll("-", "_"); // FIXME: a parameter should not be assigned. Also declare the methods parameters as 'final'.

        // if it's all uppper case, convert to lower case
        if (name.matches("^[A-Z_]*$")) {
            name = name.toLowerCase();
        }

        // camelize (lower first character) the variable name
        // petId => pet_id
        name = underscore(name);

        // for reserved word or word starting with number, append _
        if (isReservedWord(name) || name.matches("^\\d.*")) {
            name = escapeReservedWord(name);
        }

        return name;
    }

    @Override
    public String getSwaggerType(Property p) {
        String swaggerType = super.getSwaggerType(p);
        String type = null;
        if (typeMapping.containsKey(swaggerType)) {
            return typeMapping.get(swaggerType);
        }
        return "string";
    }

    @Override
    public String toParamName(String name) {
        // should be the same as variable name
        return toVarName(name);
    }

    @Override
    public String toModelName(String name) {
        // model name cannot use reserved keyword, e.g. return
        if (isReservedWord(name)) {
            throw new RuntimeException(name + " (reserved word) cannot be used as a model name");
        }

        // camelize the model name
        // phone_number => PhoneNumber
        return camelize(name);
    }

    @Override
    public String toModelFilename(String name) {
        // model name cannot use reserved keyword, e.g. return
        if (isReservedWord(name)) {
            throw new RuntimeException(name + " (reserved word) cannot be used as a model name");
        }

        // underscore the model file name
        // PhoneNumber.rb => phone_number.rb
        return underscore(name);
    }

    @Override
    public String toApiFilename(String name) {
        // replace - with _ e.g. created-at => created_at
        name = name.replaceAll("-", "_"); // FIXME: a parameter should not be assigned. Also declare the methods parameters as 'final'.

        // e.g. DefaultController => defaults_controller.rb
        return underscore(name) + "s_controller";
    }

    @Override
    public String toApiName(String name) {
        if (name.length() == 0) {
            return "ApiController";
        }
        // e.g. phone_number_api => PhoneNumberApi
        return camelize(name) + "Controller";
    }

    @Override
    public String toOperationId(String operationId) {
        // method name cannot use reserved keyword, e.g. return
        if (isReservedWord(operationId)) {
            throw new RuntimeException(operationId + " (reserved word) cannot be used as method name");
        }

        return underscore(operationId);
    }

    @Override
    public Map<String, Object> postProcessSupportingFileData(Map<String, Object> objs) {
        Swagger swagger = (Swagger)objs.get("swagger");
        if(swagger != null) {
            try {
                objs.put("swagger-yaml", Yaml.mapper().writeValueAsString(swagger));
            } catch (JsonProcessingException e) {
                LOGGER.error(e.getMessage(), e);
            }
        }
        return super.postProcessSupportingFileData(objs);
    }

    @Override
    public String escapeQuotationMark(String input) {
        // remove ' to avoid code injection
        return input.replace("'", "");
    }

    @Override
    public String escapeUnsafeCharacters(String input) {
        return input.replace("=end", "=_end").replace("=begin", "=_begin");
    }
}
