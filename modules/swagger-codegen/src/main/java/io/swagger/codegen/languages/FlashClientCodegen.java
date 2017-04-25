package io.swagger.codegen.languages;

import io.swagger.codegen.CliOption;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.CodegenType;
import io.swagger.codegen.DefaultCodegen;
import io.swagger.codegen.SupportingFile;
import io.swagger.models.properties.ArrayProperty;
import io.swagger.models.properties.BooleanProperty;
import io.swagger.models.properties.DateProperty;
import io.swagger.models.properties.DateTimeProperty;
import io.swagger.models.properties.DoubleProperty;
import io.swagger.models.properties.FloatProperty;
import io.swagger.models.properties.IntegerProperty;
import io.swagger.models.properties.LongProperty;
import io.swagger.models.properties.MapProperty;
import io.swagger.models.properties.Property;
import io.swagger.models.properties.StringProperty;
import org.apache.commons.lang3.StringUtils;

import java.io.File;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;

public class FlashClientCodegen extends DefaultCodegen implements CodegenConfig {
    protected String packageName = "io.swagger";
    protected String packageVersion;

    protected String invokerPackage = "io.swagger";
    protected String sourceFolder = "flash";

    public FlashClientCodegen() {
        super();

        modelPackage = "io.swagger.client.model";
        apiPackage = "io.swagger.client.api";
        outputFolder = "generated-code" + File.separatorChar + "flash";
        modelTemplateFiles.put("model.mustache", ".as");
        modelTemplateFiles.put("modelList.mustache", "List.as");
        apiTemplateFiles.put("api.mustache", ".as");
        embeddedTemplateDir = templateDir = "flash";

        languageSpecificPrimitives.clear();
        languageSpecificPrimitives.add("Number");
        languageSpecificPrimitives.add("Boolean");
        languageSpecificPrimitives.add("String");
        languageSpecificPrimitives.add("Date");
        languageSpecificPrimitives.add("Array");
        languageSpecificPrimitives.add("Dictionary");

        typeMapping.clear();
        typeMapping.put("integer", "Number");
        typeMapping.put("float", "Number");
        typeMapping.put("long", "Number");
        typeMapping.put("double", "Number");
        typeMapping.put("array", "Array");
        typeMapping.put("map", "Dictionary");
        typeMapping.put("boolean", "Boolean");
        typeMapping.put("string", "String");
        typeMapping.put("date", "Date");
        typeMapping.put("DateTime", "Date");
        typeMapping.put("object", "Object");
        typeMapping.put("file", "File");
        typeMapping.put("UUID", "String");
        //TODO binary should be mapped to byte array
        // mapped to String as a workaround
        typeMapping.put("binary", "String");

        importMapping = new HashMap<String, String>();
        importMapping.put("File", "flash.filesystem.File");

        // from
        setReservedWordsLowerCase(Arrays.asList("add", "for", "lt", "tellTarget", "and",
                "function", "ne", "this", "break", "ge", "new", "typeof", "continue", "gt", "not",
                "var", "delete", "if", "on", "void", "do", "ifFrameLoaded", "onClipEvent", "while",
                "else", "in", "or", "with", "eq", "le", "return"));

        cliOptions.clear();
        cliOptions.add(new CliOption(CodegenConstants.PACKAGE_NAME, "flash package name (convention:" +
                " package.name)").defaultValue("io.swagger"));
        cliOptions.add(new CliOption(CodegenConstants.PACKAGE_VERSION, "flash package version")
                .defaultValue("1.0.0"));
        cliOptions.add(new CliOption(CodegenConstants.INVOKER_PACKAGE, CodegenConstants.INVOKER_PACKAGE_DESC));
        cliOptions.add(new CliOption(CodegenConstants.SOURCE_FOLDER, "source folder for generated " +
                "code. e.g. flash"));

    }

    @Override
    public void processOpts() {
        super.processOpts();

        if (additionalProperties.containsKey(CodegenConstants.INVOKER_PACKAGE)) {
            this.setInvokerPackage((String) additionalProperties.get(CodegenConstants.INVOKER_PACKAGE));
        } else {
            //not set, use default to be passed to template
            additionalProperties.put(CodegenConstants.INVOKER_PACKAGE, invokerPackage);
        }

        if (additionalProperties.containsKey(CodegenConstants.SOURCE_FOLDER)) {
            this.setSourceFolder((String) additionalProperties.get(CodegenConstants.SOURCE_FOLDER));
        }

        if (additionalProperties.containsKey(CodegenConstants.PACKAGE_NAME)) {
            setPackageName((String) additionalProperties.get(CodegenConstants.PACKAGE_NAME));
            apiPackage = packageName + ".client.api";
            modelPackage = packageName + ".client.model";
        }
        else {
            setPackageName("io.swagger");
        }

        if (additionalProperties.containsKey(CodegenConstants.PACKAGE_VERSION)) {
            setPackageVersion((String) additionalProperties.get(CodegenConstants.PACKAGE_VERSION));
        }
        else {
            setPackageVersion("1.0.0");
        }

        additionalProperties.put(CodegenConstants.PACKAGE_NAME, packageName);
        additionalProperties.put(CodegenConstants.PACKAGE_VERSION, packageVersion);

        //modelPackage = invokerPackage + File.separatorChar + "client" + File.separatorChar + "model";
        //apiPackage = invokerPackage + File.separatorChar + "client" + File.separatorChar + "api";

        final String invokerFolder = (sourceFolder + File.separator + "src/" + invokerPackage + File.separator).replace(".", File.separator).replace('.', File.separatorChar);

        supportingFiles.add(new SupportingFile("ApiInvoker.as", invokerFolder + "common", "ApiInvoker.as"));
        supportingFiles.add(new SupportingFile("ApiUrlHelper.as", invokerFolder + "common", "ApiUrlHelper.as"));
        supportingFiles.add(new SupportingFile("ApiUserCredentials.as", invokerFolder + "common", "ApiUserCredentials.as"));
        supportingFiles.add(new SupportingFile("ListWrapper.as", invokerFolder + "common", "ListWrapper.as"));
        supportingFiles.add(new SupportingFile("SwaggerApi.as", invokerFolder + "common", "SwaggerApi.as"));
        supportingFiles.add(new SupportingFile("XMLWriter.as", invokerFolder + "common", "XMLWriter.as"));
        supportingFiles.add(new SupportingFile("ApiError.as", invokerFolder + "exception", "ApiError.as"));
        supportingFiles.add(new SupportingFile("ApiErrorCodes.as", invokerFolder + "exception", "ApiErrorCodes.as"));
        supportingFiles.add(new SupportingFile("ApiClientEvent.as", invokerFolder + "event", "ApiClientEvent.as"));
        supportingFiles.add(new SupportingFile("Response.as", invokerFolder + "event", "Response.as"));
        supportingFiles.add(new SupportingFile("build.properties", sourceFolder, "build.properties"));
        supportingFiles.add(new SupportingFile("build.xml", sourceFolder, "build.xml"));
        supportingFiles.add(new SupportingFile("README.txt", sourceFolder, "README.txt"));
        //supportingFiles.add(new SupportingFile("AirExecutorApp-app.xml", sourceFolder + File.separatorChar
        //        + "bin", "AirExecutorApp-app.xml"));
        supportingFiles.add(new SupportingFile("ASAXB-0.1.1.swc", sourceFolder + File.separatorChar
                + "lib", "ASAXB-0.1.1.swc"));
        supportingFiles.add(new SupportingFile("as3corelib.swc", sourceFolder + File.separatorChar
                + "lib", "as3corelib.swc"));
        supportingFiles.add(new SupportingFile("flexunit-4.1.0_RC2-28-flex_3.5.0.12683.swc", sourceFolder
                + File.separator + "lib" + File.separator + "ext", "flexunit-4.1.0_RC2-28-flex_3.5.0.12683.swc"));
        supportingFiles.add(new SupportingFile("flexunit-aircilistener-4.1.0_RC2-28-3.5.0.12683.swc", sourceFolder
                + File.separator + "lib" + File.separator + "ext", "flexunit-aircilistener-4.1.0_RC2-28-3.5.0.12683.swc"));
        supportingFiles.add(new SupportingFile("flexunit-cilistener-4.1.0_RC2-28-3.5.0.12683.swc", sourceFolder
                + File.separator + "lib" + File.separator + "ext", "flexunit-cilistener-4.1.0_RC2-28-3.5.0.12683.swc"));
        supportingFiles.add(new SupportingFile("flexunit-core-flex-4.0.0.2-sdk3.5.0.12683.swc", sourceFolder
                + File.separator + "lib" + File.separator + "ext", "flexunit-core-flex-4.0.0.2-sdk3.5.0.12683.swc"));
        supportingFiles.add(new SupportingFile("git_push.sh.mustache", "", "git_push.sh"));
        supportingFiles.add(new SupportingFile("gitignore.mustache", "", ".gitignore"));
    }

    private static String dropDots(String str) {
        return str.replaceAll("\\.", "_");
    }

    @Override
    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    @Override
    public String getName() {
        return "flash";
    }

    @Override
    public String getHelp() {
        return "Generates a Flash client library.";
    }

    @Override
    public String escapeReservedWord(String name) {           
        if(this.reservedWordsMappings().containsKey(name)) {
            return this.reservedWordsMappings().get(name);
        }
        return "_" + name;
    }

    @Override
    public String apiFileFolder() {
        return (outputFolder + File.separatorChar + sourceFolder + File.separatorChar + "src/"
                + apiPackage().replace('.', File.separatorChar)).replace('/', File.separatorChar);
    }

    @Override
    public String modelFileFolder() {
        return (outputFolder + File.separatorChar + sourceFolder + File.separatorChar + "src/"
                + modelPackage().replace('.', File.separatorChar)).replace('/', File.separatorChar);
    }

    @Override
    public String getTypeDeclaration(Property p) {
        if (p instanceof ArrayProperty || p instanceof MapProperty) {
            return getSwaggerType(p);
        }
        return super.getTypeDeclaration(p);
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
            type = toModelName(swaggerType);
        }
        return type;
    }

    @Override
    public String toDefaultValue(Property p) {
        if (p instanceof StringProperty) {
            return "null";
        } else if (p instanceof BooleanProperty) {
            return "false";
        } else if (p instanceof DateProperty) {
            return "null";
        } else if (p instanceof DateTimeProperty) {
            return "null";
        } else if (p instanceof DoubleProperty) {
            DoubleProperty dp = (DoubleProperty) p;
            if (dp.getDefault() != null) {
                return dp.getDefault().toString();
            }
            return "0.0";
        } else if (p instanceof FloatProperty) {
            FloatProperty dp = (FloatProperty) p;
            if (dp.getDefault() != null) {
                return dp.getDefault().toString();
            }
            return "0.0";
        } else if (p instanceof IntegerProperty) {
            IntegerProperty dp = (IntegerProperty) p;
            if (dp.getDefault() != null) {
                return dp.getDefault().toString();
            }
            return "0";
        } else if (p instanceof LongProperty) {
            LongProperty dp = (LongProperty) p;
            if (dp.getDefault() != null) {
                return dp.getDefault().toString();
            }
            return "0";
        } else if (p instanceof MapProperty) {
            return "new Dictionary()";
        } else if (p instanceof ArrayProperty) {
            return "new Array()";
        } else {
            return "NaN";
        }
    }

    @Override
    public String toVarName(String name) {
        // replace - with _ e.g. created-at => created_at
        name = name.replaceAll("-", "_"); // FIXME: a parameter should not be assigned. Also declare the methods parameters as 'final'.

        // if it's all uppper case, convert to lower case
        if (name.matches("^[A-Z_]*$")) {
            name = name.toLowerCase();
        }

        // underscore the variable name
        // petId => pet_id
        name = camelize(dropDots(name), true);

        // for reserved word or word starting with number, append _
        if (isReservedWord(name) || name.matches("^\\d.*")) {
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
        if (!StringUtils.isEmpty(modelNamePrefix)) {
            name = modelNamePrefix + "_" + name;
        }

        if (!StringUtils.isEmpty(modelNameSuffix)) {
            name = name + "_" + modelNameSuffix;
        }

        name = sanitizeName(name);

        // model name cannot use reserved keyword, e.g. return
        if (isReservedWord(name)) {
            LOGGER.warn(name + " (reserved word) cannot be used as model name. Renamed to " + camelize("model_" + name));
            name = "model_" + name; // e.g. return => ModelReturn (after camelize)
        }

        // camelize the model name
        // phone_number => PhoneNumber
        return camelize(name);
    }

    @Override
    public String toModelFilename(String name) {
        // leverage toModelName
        return dropDots(toModelName(name));
    }

    @Override
    public String toApiFilename(String name) {
        // replace - with _ e.g. created-at => created_at
        name = name.replaceAll("-", "_"); // FIXME: a parameter should not be assigned. Also declare the methods parameters as 'final'.

        // e.g. PhoneNumberApi.rb => phone_number_api.rb
        return camelize(name) + "Api";
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
    public String toApiVarName(String name) {
        if (name.length() == 0) {
            return "DefaultApi";
        }
        return camelize(name) + "Api";
    }

    @Override
    public String toOperationId(String operationId) {
        // throw exception if method name is empty
        if (StringUtils.isEmpty(operationId)) {
            throw new RuntimeException("Empty method name (operationId) not allowed");
        }

        // method name cannot use reserved keyword, e.g. return
        if (isReservedWord(operationId)) {
            LOGGER.warn(operationId + " (reserved word) cannot be used as method name. Renamed to " + underscore(sanitizeName("call_" + operationId)));
            operationId = "call_" + operationId;
        }

        return underscore(operationId);
    }

    public void setPackageName(String packageName) {
        this.packageName = packageName;
    }

    public void setPackageVersion(String packageVersion) {
        this.packageVersion = packageVersion;
    }

    public void setInvokerPackage(String invokerPackage) {
        this.invokerPackage = invokerPackage;
    }

    public void setSourceFolder(String sourceFolder) {
        this.sourceFolder = sourceFolder;
    }

    @Override
    public String escapeQuotationMark(String input) {
        // remove " to avoid code injection
        return input.replace("\"", "");
    }

    @Override
    public String escapeUnsafeCharacters(String input) {
        return input.replace("*/", "*_/").replace("/*", "/_*");
    }
}
