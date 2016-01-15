package io.swagger.codegen.languages;

import io.swagger.codegen.CliOption;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.CodegenType;
import io.swagger.codegen.DefaultCodegen;
import io.swagger.codegen.SupportingFile;
import io.swagger.models.properties.*;

import java.io.File;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Iterator;

import org.apache.commons.lang.StringUtils;

public class RubyClientCodegen extends DefaultCodegen implements CodegenConfig {
    public static final String GEM_NAME = "gemName";
    public static final String MODULE_NAME = "moduleName";
    public static final String GEM_VERSION = "gemVersion";
    public static final String GEM_LICENSE = "gemLicense";
    public static final String GEM_HOMEPAGE = "gemHomepage";
    public static final String GEM_SUMMARY = "gemSummary";
    public static final String GEM_DESCRIPTION = "gemDescription";
    public static final String GEM_AUTHOR = "gemAuthor";
    public static final String GEM_AUTHOR_EMAIL = "gemAuthorEmail";

    protected String gemName;
    protected String moduleName;
    protected String gemVersion = "1.0.0";
    protected String libFolder = "lib";
    protected String gemLicense = "Apache-2.0";
    protected String gemHomepage = "http://swagger.io";
    protected String gemSummary = "A ruby wrapper for the swagger APIs";
    protected String gemDescription = "This gem maps to a swagger API";
    protected String gemAuthor = "";
    protected String gemAuthorEmail = "";


    public RubyClientCodegen() {
        super();
        modelPackage = "models";
        apiPackage = "api";
        outputFolder = "generated-code" + File.separator + "ruby";
        modelTemplateFiles.put("model.mustache", ".rb");
        apiTemplateFiles.put("api.mustache", ".rb");
        embeddedTemplateDir = templateDir = "ruby";

        typeMapping.clear();
        languageSpecificPrimitives.clear();

        reservedWords = new HashSet<String>(
                Arrays.asList(
                    // local variable names used in API methods (endpoints)
                    "path", "query_params", "header_params", "_header_accept", "_header_accept_result",
                    "_header_content_type", "form_params", "post_body", "auth_names",
                    // ruby reserved keywords
                    "__FILE__", "and", "def", "end", "in", "or", "self", "unless", "__LINE__",
                    "begin", "defined?", "ensure", "module", "redo", "super", "until", "BEGIN",
                    "break", "do", "false", "next", "rescue", "then", "when", "END", "case",
                    "else", "for", "nil", "retry", "true", "while", "alias", "class", "elsif",
                    "if", "not", "return", "undef", "yield")
        );

        languageSpecificPrimitives.add("int");
        languageSpecificPrimitives.add("array");
        languageSpecificPrimitives.add("map");
        languageSpecificPrimitives.add("string");
        languageSpecificPrimitives.add("DateTime");

        typeMapping.put("string", "String");
        typeMapping.put("char", "String");
        typeMapping.put("int", "Integer");
        typeMapping.put("integer", "Integer");
        typeMapping.put("long", "Integer");
        typeMapping.put("short", "Integer");
        typeMapping.put("float", "Float");
        typeMapping.put("double", "Float");
        typeMapping.put("number", "Float");
        typeMapping.put("date", "Date");
        typeMapping.put("DateTime", "DateTime");
        typeMapping.put("boolean", "BOOLEAN");
        typeMapping.put("array", "Array");
        typeMapping.put("List", "Array");
        typeMapping.put("map", "Hash");
        typeMapping.put("object", "Object");
        typeMapping.put("file", "File");

        // remove modelPackage and apiPackage added by default
        Iterator<CliOption> itr = cliOptions.iterator();
        while (itr.hasNext()) {
            CliOption opt = itr.next();
            if (CodegenConstants.MODEL_PACKAGE.equals(opt.getOpt()) ||
                    CodegenConstants.API_PACKAGE.equals(opt.getOpt())) {
                itr.remove();
            }
        }
        cliOptions.add(new CliOption(GEM_NAME, "gem name (convention: underscore_case).").
                defaultValue("swagger_client"));
        cliOptions.add(new CliOption(MODULE_NAME, "top module name (convention: CamelCase, usually corresponding" +
                " to gem name).").defaultValue("SwaggerClient"));
        cliOptions.add(new CliOption(GEM_VERSION, "gem version.").defaultValue("1.0.0"));

        cliOptions.add(new CliOption(GEM_LICENSE, "gem license. ").
                defaultValue("Apache-2.0"));

        cliOptions.add(new CliOption(GEM_HOMEPAGE, "gem homepage. ").
                defaultValue("http://swagger.io"));

        cliOptions.add(new CliOption(GEM_SUMMARY, "gem summary. ").
                defaultValue("A ruby wrapper for the swagger APIs"));

        cliOptions.add(new CliOption(GEM_DESCRIPTION, "gem description. ").
                defaultValue("This gem maps to a swagger API"));

        cliOptions.add(new CliOption(GEM_AUTHOR, "gem author (only one is supported)."));

        cliOptions.add(new CliOption(GEM_AUTHOR_EMAIL, "gem author email (only one is supported)."));

    }

    @Override
    public void processOpts() {
        super.processOpts();

        if (additionalProperties.containsKey(GEM_NAME)) {
            setGemName((String) additionalProperties.get(GEM_NAME));
        }
        if (additionalProperties.containsKey(MODULE_NAME)) {
            setModuleName((String) additionalProperties.get(MODULE_NAME));
        }

        if (gemName == null && moduleName == null) {
            setGemName("swagger_client");
            setModuleName(generateModuleName(gemName));
        } else if (gemName == null) {
            setGemName(generateGemName(moduleName));
        } else if (moduleName == null) {
            setModuleName(generateModuleName(gemName));
        }

        additionalProperties.put(GEM_NAME, gemName);
        additionalProperties.put(MODULE_NAME, moduleName);

        if (additionalProperties.containsKey(GEM_VERSION)) {
            setGemVersion((String) additionalProperties.get(GEM_VERSION));
        }else {
            // not set, pass the default value to template
            additionalProperties.put(GEM_VERSION, gemVersion);
        }

        if (additionalProperties.containsKey(GEM_LICENSE)) {
            setGemLicense((String) additionalProperties.get(GEM_LICENSE));
        }

        if (additionalProperties.containsKey(GEM_HOMEPAGE)) {
            setGemHomepage((String) additionalProperties.get(GEM_HOMEPAGE));
        }

        if (additionalProperties.containsKey(GEM_SUMMARY)) {
            setGemSummary((String) additionalProperties.get(GEM_SUMMARY));
        }

        if (additionalProperties.containsKey(GEM_DESCRIPTION)) {
            setGemDescription((String) additionalProperties.get(GEM_DESCRIPTION));
        }

        if (additionalProperties.containsKey(GEM_AUTHOR)) {
            setGemAuthor((String) additionalProperties.get(GEM_AUTHOR));
        }

        if (additionalProperties.containsKey(GEM_AUTHOR_EMAIL)) {
            setGemAuthorEmail((String) additionalProperties.get(GEM_AUTHOR_EMAIL));
        }


        // use constant model/api package (folder path)
        setModelPackage("models");
        setApiPackage("api");

        supportingFiles.add(new SupportingFile("gemspec.mustache", "", gemName + ".gemspec"));
        supportingFiles.add(new SupportingFile("gem.mustache", libFolder, gemName + ".rb"));
        String gemFolder = libFolder + File.separator + gemName;
        supportingFiles.add(new SupportingFile("api_client.mustache", gemFolder, "api_client.rb"));
        supportingFiles.add(new SupportingFile("api_error.mustache", gemFolder, "api_error.rb"));
        supportingFiles.add(new SupportingFile("configuration.mustache", gemFolder, "configuration.rb"));
        supportingFiles.add(new SupportingFile("version.mustache", gemFolder, "version.rb"));
    }


    @Override
    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    @Override
    public String getName() {
        return "ruby";
    }

    @Override
    public String getHelp() {
        return "Generates a Ruby client library.";
    }

    /**
     * Generate Ruby module name from the gem name, e.g. use "SwaggerClient" for "swagger_client".
     */
    @SuppressWarnings("static-method")
    public String generateModuleName(String gemName) {
        return camelize(gemName.replaceAll("[^\\w]+", "_"));
    }

    /**
     * Generate Ruby gem name from the module name, e.g. use "swagger_client" for "SwaggerClient".
     */
    @SuppressWarnings("static-method")
    public String generateGemName(String moduleName) {
        return underscore(moduleName.replaceAll("[^\\w]+", ""));
    }

    @Override
    public String escapeReservedWord(String name) {
        return "_" + name;
    }

    @Override
    public String apiFileFolder() {
        return outputFolder + File.separator + libFolder + File.separator + gemName + File.separator + apiPackage.replace("/", File.separator);
    }

    @Override
    public String modelFileFolder() {
        return outputFolder + File.separator + libFolder + File.separator + gemName + File.separator + modelPackage.replace("/", File.separator);
    }

    @Override
    public String getTypeDeclaration(Property p) {
        if (p instanceof ArrayProperty) {
            ArrayProperty ap = (ArrayProperty) p;
            Property inner = ap.getItems();
            return getSwaggerType(p) + "<" + getTypeDeclaration(inner) + ">";
        } else if (p instanceof MapProperty) {
            MapProperty mp = (MapProperty) p;
            Property inner = mp.getAdditionalProperties();
            return getSwaggerType(p) + "<String, " + getTypeDeclaration(inner) + ">";
        }
        return super.getTypeDeclaration(p);
    }

    @Override
    public String toDefaultValue(Property p) {
        if (p instanceof IntegerProperty) {
            IntegerProperty dp = (IntegerProperty) p;
            if (dp.getDefault() != null) {
                return dp.getDefault().toString();
            }
        } else if (p instanceof LongProperty) {
            LongProperty dp = (LongProperty) p;
            if (dp.getDefault() != null) {
                return dp.getDefault().toString();
            }
        } else if (p instanceof DoubleProperty) {
            DoubleProperty dp = (DoubleProperty) p;
            if (dp.getDefault() != null) {
                return dp.getDefault().toString();
            }
        } else if (p instanceof FloatProperty) {
            FloatProperty dp = (FloatProperty) p;
            if (dp.getDefault() != null) {
                return dp.getDefault().toString();
            }
        } else if (p instanceof BooleanProperty) {
            BooleanProperty bp = (BooleanProperty) p;
            if (bp.getDefault() != null) {
                return bp.getDefault().toString();
            }
        } else if (p instanceof StringProperty) {
            StringProperty sp = (StringProperty) p;
            if (sp.getDefault() != null) {
                return "\"" + escapeText(sp.getDefault()) + "\"";
            }
        }

        return null;
    }

    @Override
    public String getSwaggerType(Property p) {
        String swaggerType = super.getSwaggerType(p);
        String type = null;
        if (typeMapping.containsKey(swaggerType)) {
            type = typeMapping.get(swaggerType);
            if (languageSpecificPrimitives.contains(type)) {
                return type;
            }
        } else {
            type = swaggerType;
        }
        if (type == null) {
            return null;
        }
        return type;
    }

    @Override
    public String toVarName(String name) {
        // sanitize name
        name = sanitizeName(name); // FIXME: a parameter should not be assigned. Also declare the methods parameters as 'final'.

        // if it's all uppper case, convert to lower case
        if (name.matches("^[A-Z_]*$")) {
            name = name.toLowerCase();
        }

        // camelize (lower first character) the variable name
        // petId => pet_id
        name = underscore(name);

        // for reserved word or word starting with number, append _
        if (reservedWords.contains(name) || name.matches("^\\d.*")) {
            name = escapeReservedWord(name);
        }

        return name;
    }

    @Override
    public String toParamName(String name) {
        // should be the same as variable name
        return toVarName(name);
    }

    @Override
    public String toModelName(String name) {
        name = sanitizeName(name); // FIXME: a parameter should not be assigned. Also declare the methods parameters as 'final'.

        // model name cannot use reserved keyword, e.g. return
        if (reservedWords.contains(name)) {
            throw new RuntimeException(name + " (reserved word) cannot be used as a model name");
        }

        // camelize the model name
        // phone_number => PhoneNumber
        return camelize(name);
    }

    @Override
    public String toModelFilename(String name) {
        // model name cannot use reserved keyword, e.g. return
        if (reservedWords.contains(name)) {
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

        // e.g. PhoneNumberApi.rb => phone_number_api.rb
        return underscore(name) + "_api";
    }

    @Override
    public String toApiName(String name) {
        if (name.length() == 0) {
            return "DefaultApi";
        }
        // e.g. phone_number_api => PhoneNumberApi
        return camelize(name) + "Api";
    }

    @Override
    public String toOperationId(String operationId) {
        // throw exception if method name is empty
        if (StringUtils.isEmpty(operationId)) {
            throw new RuntimeException("Empty method name (operationId) not allowed");
        }

        // method name cannot use reserved keyword, e.g. return
        if (reservedWords.contains(operationId)) {
            throw new RuntimeException(operationId + " (reserved word) cannot be used as method name");
        }

        return underscore(sanitizeName(operationId));
    }

    @Override
    public String toModelImport(String name) {
        return gemName + "/" + modelPackage() + "/" + toModelFilename(name);
    }

    @Override
    public String toApiImport(String name) {
        return gemName + "/" + apiPackage() + "/" + toApiFilename(name);
    }

    public void setGemName(String gemName) {
        this.gemName = gemName;
    }

    public void setModuleName(String moduleName) {
        this.moduleName = moduleName;
    }

    public void setGemVersion(String gemVersion) {
        this.gemVersion = gemVersion;
    }

    public void setGemDescription(String gemDescription) {
        this.gemDescription = gemDescription;
    }

    public void setGemSummary(String gemSummary) {
        this.gemSummary = gemSummary;
    }

    public void setGemLicense(String gemLicense) {
        this.gemLicense = gemLicense;
    }

    public void setGemHomepage(String gemHomepage) {
        this.gemHomepage = gemHomepage;
    }

    public void setGemAuthor(String gemAuthor) {
        this.gemAuthor = gemAuthor;
    }

    public void setGemAuthorEmail(String gemAuthorEmail) {
        this.gemAuthorEmail = gemAuthorEmail;
    }
}
