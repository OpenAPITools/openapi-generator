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

package org.openapitools.codegen.languages;

import io.swagger.v3.oas.models.media.Schema;
import org.openapitools.codegen.CliOption;
import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.CodegenType;
import org.openapitools.codegen.SupportingFile;
import org.openapitools.codegen.meta.features.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.text.SimpleDateFormat;
import java.util.EnumSet;
import java.util.Locale;
import java.util.Map;

import static org.openapitools.codegen.utils.StringUtils.camelize;
import static org.openapitools.codegen.utils.StringUtils.underscore;

public class RubyOnRailsServerCodegen extends AbstractRubyCodegen {

    private static final Logger LOGGER = LoggerFactory.getLogger(RubyOnRailsServerCodegen.class);

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
    protected String environmentsFolder = configFolder + File.separator + "environments";
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
    protected String databaseAdapter = "sqlite";


    public RubyOnRailsServerCodegen() {
        super();

        modifyFeatureSet(features -> features
                .includeDocumentationFeatures(DocumentationFeature.Readme)
                .wireFormatFeatures(EnumSet.of(WireFormatFeature.JSON, WireFormatFeature.XML, WireFormatFeature.Custom))
                .securityFeatures(EnumSet.noneOf(SecurityFeature.class))
                .excludeGlobalFeatures(
                        GlobalFeature.XMLStructureDefinitions,
                        GlobalFeature.Callbacks,
                        GlobalFeature.LinkObjects,
                        GlobalFeature.ParameterStyling
                )
                .excludeSchemaSupportFeatures(
                        SchemaSupportFeature.Polymorphism
                )
                .excludeParameterFeatures(
                        ParameterFeature.Cookie
                )
        );

        outputFolder = "generated-code" + File.separator + "rails5";
        apiPackage = "app/controllers";
        apiTemplateFiles.put("controller.mustache", ".rb");

        modelPackage = "app/models";
        modelTemplateFiles.put("model.mustache", ".rb");

        embeddedTemplateDir = templateDir = "ruby-on-rails-server";

        // In order to adapt to DB migrate script, overwrite typeMapping
        typeMapping.put("string", "string");
        typeMapping.put("int", "integer");
        typeMapping.put("integer", "integer");
        typeMapping.put("long", "integer");
        typeMapping.put("short", "integer");
        typeMapping.put("DateTime", "datetime");
        typeMapping.put("boolean", "boolean");

        // remove modelPackage and apiPackage added by default
        cliOptions.clear();

        cliOptions.add(new CliOption(CodegenConstants.DATABASE_ADAPTER, CodegenConstants.DATABASE_ADAPTER_DESC).
                defaultValue("sqlite"));
    }

    @Override
    public void processOpts() {
        super.processOpts();

        // use constant model/api package (folder path)
        //setModelPackage("models");
        setApiPackage("app/controllers");

        // determine which db adapter to use
        if (additionalProperties.containsKey(CodegenConstants.DATABASE_ADAPTER)) {
            setDatabaseAdapter((String) additionalProperties.get(CodegenConstants.DATABASE_ADAPTER));
        } else {
            // not set, pass the default value to template
            additionalProperties.put(CodegenConstants.DATABASE_ADAPTER, databaseAdapter);
        }

        if ("sqlite".equals(databaseAdapter)) {
            additionalProperties.put("isDBSQLite", Boolean.TRUE);
        } else if ("mysql".equals(databaseAdapter)) {
            additionalProperties.put("isDBMySQL", Boolean.TRUE);
        } else {
            LOGGER.warn("Unknown database {}. Defaul to 'sqlite'.", databaseAdapter);
            additionalProperties.put("isDBSQLite", Boolean.TRUE);
        }

        supportingFiles.add(new SupportingFile("Gemfile.mustache", "", "Gemfile"));
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
        supportingFiles.add(new SupportingFile("database.mustache", configFolder, "database.yml"));
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
        supportingFiles.add(new SupportingFile("Dockerfile", "", "Dockerfile"));
        supportingFiles.add(new SupportingFile("docker-entrypoint.sh", "", "docker-entrypoint.sh"));
    }

    @Override
    public CodegenType getTag() {
        return CodegenType.SERVER;
    }

    @Override
    public String getName() {
        return "ruby-on-rails";
    }

    @Override
    public String getHelp() {
        return "Generates a Ruby on Rails (v5) server library.";
    }

    @Override
    public String apiFileFolder() {
        return outputFolder + File.separator + apiPackage.replace("/", File.separator);
    }

    @Override
    public String getSchemaType(Schema p) {
        String openAPIType = super.getSchemaType(p);
        String type = null;
        if (typeMapping.containsKey(openAPIType)) {
            return typeMapping.get(openAPIType);
        }
        return "string";
    }

    @Override
    public String toModelName(String name) {
        // model name cannot use reserved keyword, e.g. return
        if (isReservedWord(name)) {
            String modelName = camelize("Model" + name);
            LOGGER.warn(name + " (reserved word) cannot be used as model name. Renamed to " + modelName);
            return modelName;
        }

        // camelize the model name
        // phone_number => PhoneNumber
        return camelize(name);
    }

    @Override
    public String toModelFilename(String name) {
        // model name cannot use reserved keyword, e.g. return
        if (isReservedWord(name)) {
            String filename = underscore("model_" + name);
            LOGGER.warn(name + " (reserved word) cannot be used as model filename. Renamed to " + filename);
            return filename;
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
        return underscore(name) + "_controller";
    }

    @Override
    public String toApiVarName(String name) {
        if (name.length() == 0) {
            return "api";
        }

        // e.g. PhoneNumber => phone_number
        return underscore(sanitizeName(name));
    }

    @Override
    public String toApiName(String name) {
        if (name.length() == 0) {
            return "ApiController";
        }
        // e.g. phone_number_controller => PhoneNumberController
        return camelize(name) + "Controller";
    }

    @Override
    public Map<String, Object> postProcessSupportingFileData(Map<String, Object> objs) {
        generateYAMLSpecFile(objs);
        return super.postProcessSupportingFileData(objs);
    }

    public void setDatabaseAdapter(String databaseAdapter) {
        this.databaseAdapter = databaseAdapter;
    }
}
