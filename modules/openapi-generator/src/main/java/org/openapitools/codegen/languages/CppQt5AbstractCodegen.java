package org.openapitools.codegen.languages;

import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.parser.util.SchemaTypeUtil;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.CodegenModel;
import org.openapitools.codegen.CodegenOperation;
import org.openapitools.codegen.CodegenParameter;
import org.openapitools.codegen.utils.ModelUtils;

import java.io.File;
import java.util.*;

public class CppQt5AbstractCodegen extends AbstractCppCodegen implements CodegenConfig {

    protected final String PREFIX = "OAI";
    protected String apiVersion = "1.0.0";
    protected static final String CPP_NAMESPACE = "cppNamespace";
    protected static final String CPP_NAMESPACE_DESC = "C++ namespace (convention: name::space::for::api).";
    protected Set<String> foundationClasses = new HashSet<String>();
    protected String cppNamespace = "OpenAPI";
    protected Map<String, String> namespaces = new HashMap<String, String>();
    protected Set<String> systemIncludes = new HashSet<String>();

    protected Set<String> nonFrameworkPrimitives = new HashSet<String>();
    
    public CppQt5AbstractCodegen() {
        super();
        // set modelNamePrefix as default for QHttpEngine Server
        if (StringUtils.isEmpty(modelNamePrefix)) {
            modelNamePrefix = PREFIX;
        }
        // CLI options
        addOption(CPP_NAMESPACE, CPP_NAMESPACE_DESC, this.cppNamespace);

        /*
         * Additional Properties.  These values can be passed to the templates and
         * are available in models, apis, and supporting files
         */
        additionalProperties.put("apiVersion", apiVersion);
        additionalProperties().put("prefix", PREFIX);

        // Write defaults namespace in properties so that it can be accessible in templates.
        // At this point command line has not been parsed so if value is given
        // in command line it will supersede this content
        additionalProperties.put("cppNamespace", cppNamespace);   
        // CLI options
        addOption(CPP_NAMESPACE, CPP_NAMESPACE_DESC, this.cppNamespace);
        /*
         * Language Specific Primitives.  These types will not trigger imports by
         * the client generator
         */
        languageSpecificPrimitives = new HashSet<String>(
                Arrays.asList(
                        "bool",
                        "qint32",
                        "qint64",
                        "float",
                        "double")
        );
        nonFrameworkPrimitives.addAll(languageSpecificPrimitives);
        
        foundationClasses.addAll(
                Arrays.asList(
                        "QString",                          
                        "QDate",
                        "QDateTime",
                        "QByteArray")
        );
        languageSpecificPrimitives.addAll(foundationClasses);
        super.typeMapping = new HashMap<String, String>();

        typeMapping.put("date", "QDate");
        typeMapping.put("DateTime", "QDateTime");
        typeMapping.put("string", "QString");
        typeMapping.put("integer", "qint32");
        typeMapping.put("long", "qint64");
        typeMapping.put("boolean", "bool");
        typeMapping.put("array", "QList");
        typeMapping.put("map", "QMap");
        typeMapping.put("object", PREFIX + "Object");
        // mapped as "file" type for OAS 3.0
        typeMapping.put("ByteArray", "QByteArray");
        //   UUID support - possible enhancement : use QUuid instead of QString.
        //   beware though that Serialization/de-serialization of QUuid does not
        //   come out of the box and will need to be sorted out (at least imply
        //   modifications on multiple templates)
        typeMapping.put("UUID", "QString");
        typeMapping.put("file", "QIODevice");
        typeMapping.put("binary", "QIODevice");
        importMapping = new HashMap<String, String>();
        namespaces = new HashMap<String, String>();

        systemIncludes.add("QString");
        systemIncludes.add("QList");
        systemIncludes.add("QMap");
        systemIncludes.add("QDate");
        systemIncludes.add("QDateTime");
        systemIncludes.add("QByteArray");
        systemIncludes.add("QIODevice");
    }
    @Override
    public void processOpts() {
        super.processOpts();

        if (additionalProperties.containsKey("cppNamespace")) {
            cppNamespace = (String) additionalProperties.get("cppNamespace");
        }

        additionalProperties.put("cppNamespaceDeclarations", cppNamespace.split("\\::"));
        if (additionalProperties.containsKey("modelNamePrefix")) {
            typeMapping.put("object", modelNamePrefix + "Object");
            additionalProperties().put("prefix", modelNamePrefix);
        }
    }
    
    @Override
    public String toModelImport(String name) {
        if( name.isEmpty() ) {
            return null;
        }

        if (namespaces.containsKey(name)) {
            return "using " + namespaces.get(name) + ";";
        } else if (systemIncludes.contains(name)) {
            return "#include <" + name + ">";
        } else if(importMapping.containsKey(name)){
            return importMapping.get(name);
        }

        String folder = modelPackage().replace("::", File.separator);
        if (!folder.isEmpty())
            folder += File.separator;

        return "#include \"" + folder + name + ".h\"";
    }
    
    /**
     * Optional - type declaration.  This is a String which is used by the templates to instantiate your
     * types.  There is typically special handling for different property types
     *
     * @return a string value used as the `dataType` field for model templates, `returnType` for api templates
     */
    @Override
    @SuppressWarnings("rawtypes")
    public String getTypeDeclaration(Schema p) {
        String openAPIType = getSchemaType(p);

        if (ModelUtils.isArraySchema(p)) {
            ArraySchema ap = (ArraySchema) p;
            Schema inner = ap.getItems();
            return getSchemaType(p) + "<" + getTypeDeclaration(inner) + ">";
        } else if (ModelUtils.isMapSchema(p)) {
            Schema inner = ModelUtils.getAdditionalProperties(p);
            return getSchemaType(p) + "<QString, " + getTypeDeclaration(inner) + ">";
        } else if (ModelUtils.isBinarySchema(p)) {
            return getSchemaType(p) + "*";
        } else if (ModelUtils.isFileSchema(p)) {
            return getSchemaType(p) + "*";
        }
        if (foundationClasses.contains(openAPIType)) {
            return openAPIType;
        } else if (languageSpecificPrimitives.contains(openAPIType)) {
            return toModelName(openAPIType);
        } else {
            return openAPIType;
        }
    }

    @Override
    @SuppressWarnings("rawtypes")    
    public String toDefaultValue(Schema p) {
        if (ModelUtils.isBooleanSchema(p)) {
            return "false";
        } else if (ModelUtils.isDateSchema(p)) {
            return "NULL";
        } else if (ModelUtils.isDateTimeSchema(p)) {
            return "NULL";
        } else if (ModelUtils.isNumberSchema(p)) {
            if (SchemaTypeUtil.FLOAT_FORMAT.equals(p.getFormat())) {
                return "0.0f";
            }
            return "0.0";
        } else if (ModelUtils.isIntegerSchema(p)) {
            if (SchemaTypeUtil.INTEGER64_FORMAT.equals(p.getFormat())) {
                return "0L";
            }
            return "0";
        } else if (ModelUtils.isMapSchema(p)) {
            Schema inner = ModelUtils.getAdditionalProperties(p);
            return "QMap<QString, " + getTypeDeclaration(inner) + ">()";
        } else if (ModelUtils.isArraySchema(p)) {
            ArraySchema ap = (ArraySchema) p;
            Schema inner = ap.getItems();
            return "QList<" + getTypeDeclaration(inner) + ">()";
        } else if (ModelUtils.isStringSchema(p)) {
            return "QString(\"\")";
        } else if (!StringUtils.isEmpty(p.get$ref())) {
            return toModelName(ModelUtils.getSimpleRef(p.get$ref())) + "()";
        }
        return "NULL";
    }

    @Override
    public String toModelFilename(String name) {
        return toModelName(name);
    }
    
    /**
     * Optional - OpenAPI type conversion.  This is used to map OpenAPI types in a `Schema` into
     * either language specific types via `typeMapping` or into complex models if there is not a mapping.
     *
     * @return a string value of the type or complex model for this property
     */
    @Override
    @SuppressWarnings("rawtypes")    
    public String getSchemaType(Schema p) {
        String openAPIType = super.getSchemaType(p);

        String type = null;
        if (typeMapping.containsKey(openAPIType)) {
            type = typeMapping.get(openAPIType);
            if (languageSpecificPrimitives.contains(type)) {
                return toModelName(type);
            }
            if (foundationClasses.contains(type)) {
                return type;
            }
        } else {
            type = openAPIType;
        }
        return toModelName(type);
    }

    @Override
    public String toVarName(String name) {
        // sanitize name
        String varName = name;
        varName = sanitizeName(name); 

        // if it's all uppper case, convert to lower case
        if (varName.matches("^[A-Z_]*$")) {
            varName = varName.toLowerCase(Locale.ROOT);
        }

        // camelize (lower first character) the variable name
        // petId => pet_id
        varName = org.openapitools.codegen.utils.StringUtils.underscore(varName);

        // for reserved word or word starting with number, append _
        if (isReservedWord(varName) || varName.matches("^\\d.*")) {
            varName = escapeReservedWord(varName);
        }

        return varName;
    }

    @Override
    public String toParamName(String name) {
        return toVarName(name);
    }

    @Override
    public String getTypeDeclaration(String str) {
        return str;
    }
      
    @Override
    protected boolean needToImport(String type) {
        return StringUtils.isNotBlank(type) && !defaultIncludes.contains(type)
                && !nonFrameworkPrimitives.contains(type);
    }


    @Override
    @SuppressWarnings("unchecked")
    public Map<String, Object> postProcessOperationsWithModels(Map<String, Object> objs, List<Object> allModels) {
        Map<String, Object> objectMap = (Map<String, Object>) objs.get("operations");
        List<CodegenOperation> operations = (List<CodegenOperation>) objectMap.get("operation");
        
        List<Map<String, String>> imports = (List<Map<String, String>>) objs.get("imports");
        Map<String, CodegenModel> codegenModels = new HashMap<String, CodegenModel> ();
        for(Object moObj : allModels) {
            CodegenModel mo = ((Map<String, CodegenModel>) moObj).get("model");
            if(mo.isEnum) {
                codegenModels.put(mo.classname, mo);
            }
        }
        for (CodegenOperation operation : operations) {
            if(operation.returnType != null) {
                if(codegenModels.containsKey(operation.returnType)){
                    operation.vendorExtensions.put("returnsEnum", true);
                }
            }
            // Check all return parameter baseType if there is a necessity to include, include it if not 
            // already done
            if (operation.returnBaseType != null && needToImport(operation.returnBaseType)) {
                if(!isIncluded(operation.returnBaseType, imports)) {
                    imports.add(createMapping("import", operation.returnBaseType));
                }
            }
            List<CodegenParameter> params = new ArrayList<CodegenParameter>();
            if (operation.allParams != null)params.addAll(operation.allParams);

            // Check all parameter baseType if there is a necessity to include, include it if not 
            // already done
            for(CodegenParameter param : params) {
                if(param.isPrimitiveType && needToImport(param.baseType)) {
                    if(!isIncluded(param.baseType, imports)) {
                        imports.add(createMapping("import", param.baseType));
                    }
                }
            }
            if (operation.pathParams != null) {
                // We use QString to pass path params, add it to include
                if(!isIncluded("QString", imports)) {
                    imports.add(createMapping("import", "QString"));
                }               
            }
        }
        if(isIncluded("QMap", imports)) {
            // Maps uses QString as key
            if(!isIncluded("QString", imports)) {
                imports.add(createMapping("import", "QString"));
            }
        }
        return objs;
    }
    
    @Override
    public Map<String, Object> postProcessModels(Map<String, Object> objs) {
        return postProcessModelsEnum(objs);
    }

    @Override
    public String toEnumValue(String value, String datatype) {
        return escapeText(value);
    }
    
    @Override
    public boolean isDataTypeString(String dataType) {
        return "QString".equals(dataType);
    }
    
    private Map<String, String> createMapping(String key, String value) {
        Map<String, String> customImport = new HashMap<String, String>();
        customImport.put(key, toModelImport(value));
        return customImport;
    }
    
    private boolean isIncluded(String type, List<Map<String, String>> imports) {
        boolean included = false;
        String inclStr = toModelImport(type);
        for (Map<String, String> importItem : imports) {
            if(importItem.containsValue(inclStr)) {
                included = true;
                break;
            }
        }
        return included;
    }
}
