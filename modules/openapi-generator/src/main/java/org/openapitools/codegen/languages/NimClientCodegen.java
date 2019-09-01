package org.openapitools.codegen.languages;

import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.Schema;
import org.openapitools.codegen.*;
import org.openapitools.codegen.utils.ModelUtils;
import org.openapitools.codegen.utils.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Map;

import static org.openapitools.codegen.utils.StringUtils.camelize;

public class NimClientCodegen extends DefaultCodegen implements CodegenConfig {
    public static final String PROJECT_NAME = "projectName";

    static Logger LOGGER = LoggerFactory.getLogger(NimClientCodegen.class);

    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    public String getName() {
        return "nim";
    }

    public String getHelp() {
        return "Generates a nim client.";
    }

    public NimClientCodegen() {
        super();

        outputFolder = "generated-code" + File.separator + "nim";
        modelTemplateFiles.put("model.mustache", ".nim");
        apiTemplateFiles.put("api.mustache", ".nim");
        embeddedTemplateDir = templateDir = "nim-client";
        apiPackage = File.separator + "apis";
        modelPackage = File.separator + "models";
        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));

        setReservedWordsLowerCase(
                Arrays.asList(
                        "addr", "and", "as", "asm",
                        "bind", "block", "break",
                        "case", "cast", "concept", "const", "continue", "converter",
                        "defer", "discard", "distinct", "div", "do",
                        "elif", "else", "end", "enum", "except", "export",
                        "finally", "for", "from", "func",
                        "if", "import", "in", "include", "interface", "is", "isnot", "iterator",
                        "let",
                        "macro", "method", "mixin", "mod",
                        "nil", "not", "notin",
                        "object", "of", "or", "out",
                        "proc", "ptr",
                        "raise", "ref", "return",
                        "shl", "shr", "static",
                        "template", "try", "tuple", "type",
                        "using",
                        "var",
                        "when", "while",
                        "xor",
                        "yield"
                )
        );

        defaultIncludes = new HashSet<String>(
                Arrays.asList(
                        "array"
                )
        );

        languageSpecificPrimitives = new HashSet<String>(
                Arrays.asList(
                        "int",
                        "int8",
                        "int16",
                        "int32",
                        "int64",
                        "uint",
                        "uint8",
                        "uint16",
                        "uint32",
                        "uint64",
                        "float",
                        "float32",
                        "float64",
                        "bool",
                        "char",
                        "string",
                        "cstring",
                        "pointer")
        );

        typeMapping.clear();
        typeMapping.put("integer", "int");
        typeMapping.put("long", "int64");
        typeMapping.put("number", "float");
        typeMapping.put("float", "float");
        typeMapping.put("double", "float64");
        typeMapping.put("boolean", "bool");
        typeMapping.put("UUID", "string");
        typeMapping.put("URI", "string");
        typeMapping.put("date", "string");
        typeMapping.put("DateTime", "string");
        typeMapping.put("password", "string");
    }

    @Override
    public Map<String, Object> postProcessModels(Map<String, Object> objs) {
        return postProcessModelsEnum(objs);
    }

    @Override
    public String escapeReservedWord(String name) {
        if (this.reservedWordsMappings().containsKey(name)) {
            return this.reservedWordsMappings().get(name);
        }
        return "var" + StringUtils.camelize(name, false);
    }

    @Override
    public String toModelImport(String name) {
        if (importMapping.containsKey(name)) {
            return StringUtils.camelize(importMapping.get(name), true);
        } else {
            return StringUtils.camelize(name, true);
        }
    }

    @Override
    public String toModelFilename(String name) {
        return StringUtils.camelize(name, true);
    }

    @Override
    public String getTypeDeclaration(Schema p) {
        if (ModelUtils.isArraySchema(p)) {
            ArraySchema ap = (ArraySchema) p;
            Schema inner = ap.getItems();
            if (inner == null) {
                return null;
            }
            return "seq[" + getTypeDeclaration(inner) + "]";
        }

        String schemaType = getSchemaType(p);
        if (typeMapping.containsKey(schemaType)) {
            return typeMapping.get(schemaType);
        }

        return schemaType;
    }

    @Override
    protected boolean needToImport(String type) {
        if (defaultIncludes.contains(type)) {
            return false;
        } else if (languageSpecificPrimitives.contains(type)) {
            return false;
        } else if (typeMapping.containsKey(type) && languageSpecificPrimitives.contains(typeMapping.get(type))) {
            return false;
        }

        return true;
    }

    @Override
    public String toEnumName(CodegenProperty property) {
        return StringUtils.camelize(property.name, false);
    }

    @Override
    public String toEnumVarName(String name, String datatype) {
        return StringUtils.camelize(name, false);
    }
}
