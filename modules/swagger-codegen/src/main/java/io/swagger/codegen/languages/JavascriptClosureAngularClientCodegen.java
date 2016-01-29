package io.swagger.codegen.languages;

import io.swagger.codegen.CodegenModel;
import io.swagger.codegen.*;
import io.swagger.models.properties.*;

import java.util.TreeSet;
import java.util.*;
import java.io.File;

public class JavascriptClosureAngularClientCodegen extends DefaultCodegen implements CodegenConfig {
    public JavascriptClosureAngularClientCodegen() {
        super();

        supportsInheritance = false;
        reservedWords = new HashSet<String>(Arrays.asList("abstract",
            "continue", "for", "new", "switch", "assert", "default", "if",
            "package", "synchronized", "do", "goto", "private",
            "this", "break", "double", "implements", "protected", "throw",
            "byte", "else", "import", "public", "throws", "case", "enum",
            "instanceof", "return", "transient", "catch", "extends", "int",
            "short", "try", "char", "final", "interface", "static", "void",
            "class", "finally", "const", "super", "while"));

        languageSpecificPrimitives = new HashSet<String>(Arrays.asList(
            "string",
            "boolean",
            "number",
            "Object",
            "Blob",
            "Date"));
        instantiationTypes.put("array", "Array");

        typeMapping = new HashMap<String, String>();
        typeMapping.put("Array", "Array");
        typeMapping.put("array", "Array");
        typeMapping.put("List", "Array");
        typeMapping.put("boolean", "boolean");
        typeMapping.put("string", "string");
        typeMapping.put("int", "number");
        typeMapping.put("float", "number");
        typeMapping.put("number", "number");
        typeMapping.put("long", "number");
        typeMapping.put("short", "number");
        typeMapping.put("char", "string");
        typeMapping.put("double", "number");
        typeMapping.put("object", "Object");
        typeMapping.put("Object", "Object");
        typeMapping.put("File", "Blob");
        typeMapping.put("file", "Blob");
        typeMapping.put("integer", "number");
        typeMapping.put("Map", "Object");
        typeMapping.put("map", "Object");
        typeMapping.put("DateTime", "Date");

        importMapping = new HashMap<String, String>();
        defaultIncludes = new HashSet<String>(Arrays.asList(
            "Object",
            "Array",
            "Blob"
        ));

        typeMapping.put("binary", "string");

        outputFolder = "generated-code/javascript-closure-angular";
        modelTemplateFiles.put("model.mustache", ".js");
        apiTemplateFiles.put("api.mustache", ".js");
        embeddedTemplateDir = templateDir = "Javascript-Closure-Angular";
        apiPackage = "API.Client";
        modelPackage = "API.Client";
    }

    @Override
    public String getName() {
        return "javascript-closure-angular";
    }

    @Override
    public String getHelp() {
        return "Generates a Javascript AngularJS client library annotated with Google Closure Compiler annotations" +
            "(https://developers.google.com/closure/compiler/docs/js-for-compiler?hl=en)";
    }

    @Override
    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    @Override
    public String escapeReservedWord(String name) {
        return "_" + name;
    }

    @Override
    public String apiFileFolder() {
        return outputFolder + "/" + apiPackage().replace('.', File.separatorChar);
    }

    public String modelFileFolder() {
        return outputFolder + "/" + modelPackage().replace('.', File.separatorChar);
    }

    @Override
    public String toVarName(String name) {
        // replace - with _ e.g. created-at => created_at
        name = name.replaceAll("-", "_");

        // if it's all uppper case, do nothing
        if (name.matches("^[A-Z_]*$"))
            return name;

        // camelize the variable name
        // pet_id => PetId
        name = camelize(name, true);

        // for reserved word or word starting with number, append _
        if (reservedWords.contains(name) || name.matches("^\\d.*"))
            name = escapeReservedWord(name);

        return name;
    }

    @Override
    public String toParamName(String name) {
        // should be the same as variable name
        return toVarName(name);
    }

    @Override
    public String toModelName(String name) {
        // model name cannot use reserved keyword, e.g. return
        if (reservedWords.contains(name))
            throw new RuntimeException(name
                    + " (reserved word) cannot be used as a model name");

        // camelize the model name
        // phone_number => PhoneNumber
        return camelize(name);
    }

    @Override
    public String toModelFilename(String name) {
        // should be the same as the model name
        return toModelName(name);
    }

    @Override
    public String getTypeDeclaration(Property p) {
        if (p instanceof ArrayProperty) {
            ArrayProperty ap = (ArrayProperty) p;
            Property inner = ap.getItems();
            return getSwaggerType(p) + "<!" + getTypeDeclaration(inner) + ">";
        } else if (p instanceof MapProperty) {
            MapProperty mp = (MapProperty) p;
            Property inner = mp.getAdditionalProperties();
            return "Object<!string, "+ getTypeDeclaration(inner) + ">";
        } else if (p instanceof FileProperty) {
            return "Object";
        }
        String type = super.getTypeDeclaration(p);
        if (type.equals("boolean") ||
                type.equals("Date") ||
                type.equals("number") ||
                type.equals("string")) {
            return type;
                }
        return apiPackage + "." + type;
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
        } else
            type = swaggerType;
        return type;
    }

    @Override
    public Map<String, Object> postProcessModels(Map<String, Object> objs) {

        List<Object> models = (List<Object>) objs.get("models");
        for (Object _mo : models) {
            Map<String, Object> mo = (Map<String, Object>) _mo;
            CodegenModel cm = (CodegenModel) mo.get("model");
            cm.imports = new TreeSet(cm.imports);
            for (CodegenProperty var : cm.vars) {
                // handle default value for enum, e.g. available => StatusEnum.available
                if (var.isEnum && var.defaultValue != null && !"null".equals(var.defaultValue)) {
                    var.defaultValue = var.datatypeWithEnum + "." + var.defaultValue;
                }
            }
        }
        return objs;
    }

    @Override
    public Map<String, Object> postProcessOperations(Map<String, Object> objs) {
        if (objs.get("imports") instanceof List) {
            List<Map<String, String>> imports = (ArrayList<Map<String, String>>)objs.get("imports");
            Collections.sort(imports, new Comparator<Map<String, String>>() {
                public int compare(Map<String, String> o1, Map<String, String> o2) {
                    return o1.get("import").compareTo(o2.get("import"));
                }
            });
            objs.put("imports", imports);
        }
        return objs;
    }

}
