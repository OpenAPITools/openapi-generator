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

import com.google.common.collect.ImmutableMap;
import com.samskivert.mustache.Mustache.Lambda;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.servers.Server;
import io.swagger.v3.oas.models.servers.ServerVariables;
import io.swagger.v3.oas.models.servers.ServerVariable;
import org.openapitools.codegen.*;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.templating.mustache.IndentedLambda;
import org.openapitools.codegen.utils.ModelUtils;
import org.openapitools.codegen.utils.URLPathUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

abstract public class AbstractCppCodegen extends DefaultCodegen implements CodegenConfig {
    private final Logger LOGGER = LoggerFactory.getLogger(AbstractCppCodegen.class);

    protected static final String RESERVED_WORD_PREFIX_OPTION = "reservedWordPrefix";
    protected static final String RESERVED_WORD_PREFIX_DESC = "Prefix to prepend to reserved words in order to avoid conflicts";
    protected String reservedWordPrefix = "r_";
    protected static final String VARIABLE_NAME_FIRST_CHARACTER_UPPERCASE_OPTION = "variableNameFirstCharacterUppercase";
    protected static final String VARIABLE_NAME_FIRST_CHARACTER_UPPERCASE_DESC = "Make first character of variable name uppercase (eg. value -> Value)";
    protected boolean variableNameFirstCharacterUppercase = true;

    public AbstractCppCodegen() {
        super();

        /*
         * Reserved words.  Override this with reserved words specific to your language
         */
        setReservedWordsLowerCase(
                Arrays.asList(
                        "alignas",
                        "alignof",
                        "and",
                        "and_eq",
                        "asm",
                        "auto",
                        "bitand",
                        "bitor",
                        "bool",
                        "break",
                        "case",
                        "catch",
                        "char",
                        "char16_t",
                        "char32_t",
                        "class",
                        "compl",
                        "concept",
                        "const",
                        "constexpr",
                        "const_cast",
                        "continue",
                        "decltype",
                        "default",
                        "delete",
                        "do",
                        "double",
                        "dynamic_cast",
                        "else",
                        "enum",
                        "explicit",
                        "export",
                        "extern",
                        "false",
                        "float",
                        "for",
                        "friend",
                        "goto",
                        "if",
                        "inline",
                        "int",
                        "linux",
                        "long",
                        "mutable",
                        "namespace",
                        "new",
                        "noexcept",
                        "not",
                        "not_eq",
                        "nullptr",
                        "operator",
                        "or",
                        "or_eq",
                        "private",
                        "protected",
                        "public",
                        "register",
                        "reinterpret_cast",
                        "requires",
                        "return",
                        "short",
                        "signed",
                        "sizeof",
                        "static",
                        "static_assert",
                        "static_cast",
                        "struct",
                        "switch",
                        "template",
                        "this",
                        "thread_local",
                        "throw",
                        "true",
                        "try",
                        "typedef",
                        "typeid",
                        "typename",
                        "union",
                        "unsigned",
                        "using",
                        "virtual",
                        "void",
                        "volatile",
                        "wchar_t",
                        "while",
                        "xor",
                        "xor_eq")
        );

        addOption(RESERVED_WORD_PREFIX_OPTION,
                RESERVED_WORD_PREFIX_DESC,
                this.reservedWordPrefix);
        addOption(VARIABLE_NAME_FIRST_CHARACTER_UPPERCASE_OPTION,
                  VARIABLE_NAME_FIRST_CHARACTER_UPPERCASE_DESC,
                  Boolean.toString(this.variableNameFirstCharacterUppercase));
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

    @Override
    public String toApiName(String type) {
        return sanitizeName(modelNamePrefix + super.toApiName(type));
    }

    @Override
    public String toModelName(String type) {
        if (type == null) {
            LOGGER.warn("Model name can't be null. Default to 'UnknownModel'.");
            type = "UnknownModel";
        }

        if (typeMapping.keySet().contains(type) || typeMapping.values().contains(type)
                || importMapping.values().contains(type) || defaultIncludes.contains(type)
                || languageSpecificPrimitives.contains(type)) {
            return type;
        } else {
            return sanitizeName(modelNamePrefix + Character.toUpperCase(type.charAt(0)) + type.substring(1));
        }
    }

    @Override
    public String toEnumValue(String value, String datatype) {
        return escapeText(value);
    }

    @Override
    public String toVarName(String name) {
        if (typeMapping.keySet().contains(name) || typeMapping.values().contains(name)
                || importMapping.values().contains(name) || defaultIncludes.contains(name)
                || languageSpecificPrimitives.contains(name)) {
            return sanitizeName(name);
        }

        if (isReservedWord(name) || name.matches("^\\d.*")) {
            return escapeReservedWord(name);
        }

        if (variableNameFirstCharacterUppercase && name.length() > 1) {
            return sanitizeName(Character.toUpperCase(name.charAt(0)) + name.substring(1));
        }

        return sanitizeName(name);
    }

    /**
     * Escapes a reserved word as defined in the `reservedWords` array. Handle
     * escaping those terms here. This logic is only called if a variable
     * matches the reserved words
     *
     * @return the escaped term
     */
    @Override
    public String escapeReservedWord(String name) {
        if (this.reservedWordsMappings().containsKey(name)) {
            return this.reservedWordsMappings().get(name);
        }
        return sanitizeName(reservedWordPrefix + name);
    }

    @Override
    public String toOperationId(String operationId) {
        if (isReservedWord(operationId)) {
            LOGGER.warn("{} (reserved word) cannot be used as method name. Renamed to {}", operationId, escapeReservedWord(operationId));
            return escapeReservedWord(operationId);
        }
        return sanitizeName(super.toOperationId(operationId));
    }

    @Override
    public String toParamName(String name) {
        if (isReservedWord(name) || name.matches("^\\d.*")) {
            return escapeReservedWord(name);
        }

        return sanitizeName(super.toParamName(name));
    }

    @SuppressWarnings("rawtypes")
    @Override
    public CodegenProperty fromProperty(String name, Schema p) {
        CodegenProperty property = super.fromProperty(name, p);
        String nameInCamelCase = property.nameInCamelCase;
        if (nameInCamelCase.length() > 1) {
            nameInCamelCase = sanitizeName(Character.toLowerCase(nameInCamelCase.charAt(0)) + nameInCamelCase.substring(1));
        } else {
            nameInCamelCase = sanitizeName(nameInCamelCase);
        }
        if (isReservedWord(nameInCamelCase) || nameInCamelCase.matches("^\\d.*")) {
            nameInCamelCase = escapeReservedWord(nameInCamelCase);
        }
        property.nameInCamelCase = nameInCamelCase;
        return property;
    }

    /**
     * Output the Getter name for boolean property, e.g. isActive
     *
     * @param name the name of the property
     * @return getter name based on naming convention
     */
    @Override
    public String toBooleanGetter(String name) {
        return "is" + getterAndSetterCapitalize(name);
    }

    @Override
    public String getTypeDeclaration(String str) {
        return "std::shared_ptr<" + toModelName(str) + ">";
    }

    @Override
    public void processOpts() {
        super.processOpts();

        if (StringUtils.isEmpty(System.getenv("CPP_POST_PROCESS_FILE"))) {
            LOGGER.info("Environment variable CPP_POST_PROCESS_FILE not defined so the C++ code may not be properly formatted. To define it, try 'export CPP_POST_PROCESS_FILE=\"/usr/local/bin/clang-format -i\"' (Linux/Mac)");
            LOGGER.info("NOTE: To enable file post-processing, 'enablePostProcessFile' must be set to `true` (--enable-post-process-file for CLI).");
        }

        if (additionalProperties.containsKey(RESERVED_WORD_PREFIX_OPTION)) {
            reservedWordPrefix = (String) additionalProperties.get(RESERVED_WORD_PREFIX_OPTION);
        }

        additionalProperties.put(RESERVED_WORD_PREFIX_OPTION, reservedWordPrefix);

        if (additionalProperties.containsKey(VARIABLE_NAME_FIRST_CHARACTER_UPPERCASE_OPTION))
            variableNameFirstCharacterUppercase =
                    convertPropertyToBooleanAndWriteBack(VARIABLE_NAME_FIRST_CHARACTER_UPPERCASE_OPTION);
        additionalProperties.put(VARIABLE_NAME_FIRST_CHARACTER_UPPERCASE_OPTION, variableNameFirstCharacterUppercase);
    }

    @Override
    protected ImmutableMap.Builder<String, Lambda> addMustacheLambdas() {
        return super.addMustacheLambdas()
                .put("multiline_comment_4", new IndentedLambda(4, " ", "///"));
    }

    @Override
    public void postProcessFile(File file, String fileType) {
        if (file == null) {
            return;
        }
        String cppPostProcessFile = System.getenv("CPP_POST_PROCESS_FILE");
        if (StringUtils.isEmpty(cppPostProcessFile)) {
            return; // skip if CPP_POST_PROCESS_FILE env variable is not defined
        }
        // only process files with cpp extension
        if ("cpp".equals(FilenameUtils.getExtension(file.toString())) || "h".equals(FilenameUtils.getExtension(file.toString()))) {
            String command = cppPostProcessFile + " " + file;
            try {
                Process p = Runtime.getRuntime().exec(command);
                p.waitFor();
                int exitValue = p.exitValue();
                if (exitValue != 0) {
                    LOGGER.error("Error running the command ({}). Exit value: {}", command, exitValue);
                } else {
                    LOGGER.info("Successfully executed: {}", command);
                }
            } catch (InterruptedException | IOException e) {
                LOGGER.error("Error running the command ({}). Exception: {}", command, e.getMessage());
                // Restore interrupted state
                Thread.currentThread().interrupt();
            }
        }
    }

    @Override
    public void preprocessOpenAPI(OpenAPI openAPI) {
        List<Server> serverList = openAPI.getServers();
        List<CodegenServer> CodegenServerList = new ArrayList<CodegenServer>();
        URL url = URLPathUtils.getServerURL(openAPI, serverVariableOverrides());
        String port = URLPathUtils.getPort(url, "");
        String host = url.getHost();
        String scheme = url.getProtocol();

        if (!port.isEmpty()) {
            this.additionalProperties.put("serverPort", port);
        }
        if (!host.isEmpty()) {
            this.additionalProperties.put("serverHost", host);
        }
        if (!scheme.isEmpty()) {
            this.additionalProperties.put("scheme", scheme);
        }
        if (!serverList.isEmpty()) {
            for (Server server : serverList) {
                CodegenServer s = new CodegenServer();
                s.description = server.getDescription();
                s.url = server.getUrl();
                s.variables = new ArrayList<CodegenServerVariable>();
                ServerVariables serverVars = server.getVariables();
                if(serverVars != null){
                serverVars.forEach((key,value) -> {
                    CodegenServerVariable codegenServerVar= new CodegenServerVariable();
                    ServerVariable ServerVar = value;
                    codegenServerVar.name = key;
                    codegenServerVar.description = ServerVar.getDescription();
                    codegenServerVar.defaultValue = ServerVar.getDefault();
                    codegenServerVar.enumValues = ServerVar.getEnum();
                    s.variables.add(codegenServerVar);
                    });
                }
                CodegenServerList.add(s);
            }
            this.vendorExtensions.put("x-cpp-global-server-list", CodegenServerList);
        }
    }

    @Override
    @SuppressWarnings("unchecked")
    public Map<String, Object> postProcessModels(Map<String, Object> objs) {
        List<Object> models = (List<Object>) objs.get("models");
        for (Object _mo : models) {
            Map<String, Object> mo = (Map<String, Object>) _mo;
            CodegenModel cm = (CodegenModel) mo.get("model");
            // cannot handle inheritance from maps and arrays in C++
            if((cm.isArray || cm.isMap ) && (cm.parentModel == null)) {
                cm.parent = null;
            }
        }
        return postProcessModelsEnum(objs);
    }

    @Override
    public Map<String, Object> postProcessAllModels(Map<String, Object> objs){
        Map<String, Object> models = super.postProcessAllModels(objs);
        for (final Entry<String, Object> model : models.entrySet()) {
            CodegenModel mo = ModelUtils.getModelByName(model.getKey(), models);
            addForwardDeclarations(mo, models);
        }
        return models;
    }

    private void addForwardDeclarations(CodegenModel parentModel, Map<String, Object> objs) {
        List<String> forwardDeclarations = new ArrayList<String>();
        if(!parentModel.hasVars) {
            return;
        }
        for(CodegenProperty property : parentModel.vars){
            if(!( (property.isContainer && property.mostInnerItems.isModel) || (property.isModel) ) ){
                continue;
            }
            String childPropertyType = property.isContainer? property.mostInnerItems.baseType : property.baseType;
            for(final Entry<String, Object> mo : objs.entrySet()) {
                CodegenModel childModel = ModelUtils.getModelByName(mo.getKey(), objs);
                if( !childPropertyType.equals(childModel.classname) || childPropertyType.equals(parentModel.classname) || !childModel.hasVars ){
                    continue;
                }
                for(CodegenProperty p : childModel.vars) {
                    if(((p.isModel && p.dataType.equals(parentModel.classname)) || (p.isContainer && p.mostInnerItems.baseType.equals(parentModel.classname)))) {
                        String forwardDecl = "class " + childModel.classname + ";";
                        if(!forwardDeclarations.contains(forwardDecl)) {
                            forwardDeclarations.add(forwardDecl);
                        }
                    }
                }
            }
        }
        if(!forwardDeclarations.isEmpty()){
            parentModel.vendorExtensions.put("x-has-forward-declarations", true);
            parentModel.vendorExtensions.put("x-forward-declarations", forwardDeclarations);
        }
        return;
    }

    @Override
    public GeneratorLanguage generatorLanguage() { return GeneratorLanguage.C_PLUS_PLUS; }
}
