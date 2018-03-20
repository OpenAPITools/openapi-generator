package io.swagger.codegen.languages;

import io.swagger.codegen.CliOption;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.DefaultCodegen;
import io.swagger.models.properties.ArrayProperty;
import io.swagger.models.properties.MapProperty;
import io.swagger.models.properties.Property;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;

public abstract class AbstractKotlinCodegen extends DefaultCodegen implements CodegenConfig {
    static Logger LOGGER = LoggerFactory.getLogger(AbstractKotlinCodegen.class);

    protected String artifactId;
    protected String artifactVersion = "1.0.0";
    protected String groupId = "io.swagger";
    protected String packageName;

    protected String sourceFolder = "src/main/kotlin";

    protected String apiDocPath = "docs/";
    protected String modelDocPath = "docs/";

    protected CodegenConstants.ENUM_PROPERTY_NAMING_TYPE enumPropertyNaming = CodegenConstants.ENUM_PROPERTY_NAMING_TYPE.camelCase;

    public AbstractKotlinCodegen() {
        super();
        supportsInheritance = true;

        languageSpecificPrimitives = new HashSet<String>(Arrays.asList(
                "kotlin.Byte",
                "kotlin.Short",
                "kotlin.Int",
                "kotlin.Long",
                "kotlin.Float",
                "kotlin.Double",
                "kotlin.Boolean",
                "kotlin.Char",
                "kotlin.String",
                "kotlin.Array",
                "kotlin.collections.List",
                "kotlin.collections.Map",
                "kotlin.collections.Set"
        ));

        // this includes hard reserved words defined by https://github.com/JetBrains/kotlin/blob/master/core/descriptors/src/org/jetbrains/kotlin/renderer/KeywordStringsGenerated.java
        // as well as keywords from https://kotlinlang.org/docs/reference/keyword-reference.html
        reservedWords = new HashSet<String>(Arrays.asList(
                "abstract",
                "annotation",
                "as",
                "break",
                "case",
                "catch",
                "class",
                "companion",
                "const",
                "constructor",
                "continue",
                "crossinline",
                "data",
                "delegate",
                "do",
                "else",
                "enum",
                "external",
                "false",
                "final",
                "finally",
                "for",
                "fun",
                "if",
                "in",
                "infix",
                "init",
                "inline",
                "inner",
                "interface",
                "internal",
                "is",
                "it",
                "lateinit",
                "lazy",
                "noinline",
                "null",
                "object",
                "open",
                "operator",
                "out",
                "override",
                "package",
                "private",
                "protected",
                "public",
                "reified",
                "return",
                "sealed",
                "super",
                "suspend",
                "tailrec",
                "this",
                "throw",
                "true",
                "try",
                "typealias",
                "typeof",
                "val",
                "var",
                "vararg",
                "when",
                "while"
        ));

        defaultIncludes = new HashSet<String>(Arrays.asList(
                "kotlin.Byte",
                "kotlin.Short",
                "kotlin.Int",
                "kotlin.Long",
                "kotlin.Float",
                "kotlin.Double",
                "kotlin.Boolean",
                "kotlin.Char",
                "kotlin.Array",
                "kotlin.collections.List",
                "kotlin.collections.Set",
                "kotlin.collections.Map"
        ));

        typeMapping = new HashMap<String, String>();
        typeMapping.put("string", "kotlin.String");
        typeMapping.put("boolean", "kotlin.Boolean");
        typeMapping.put("integer", "kotlin.Int");
        typeMapping.put("float", "kotlin.Float");
        typeMapping.put("long", "kotlin.Long");
        typeMapping.put("double", "kotlin.Double");
        typeMapping.put("number", "java.math.BigDecimal");
        typeMapping.put("date-time", "java.time.LocalDateTime");
        typeMapping.put("date", "java.time.LocalDateTime");
        typeMapping.put("file", "java.io.File");
        typeMapping.put("array", "kotlin.Array");
        typeMapping.put("list", "kotlin.Array");
        typeMapping.put("map", "kotlin.collections.Map");
        typeMapping.put("object", "kotlin.Any");
        typeMapping.put("binary", "kotlin.Array<kotlin.Byte>");
        typeMapping.put("Date", "java.time.LocalDateTime");
        typeMapping.put("DateTime", "java.time.LocalDateTime");

        instantiationTypes.put("array", "arrayOf");
        instantiationTypes.put("list", "arrayOf");
        instantiationTypes.put("map", "mapOf");

        importMapping = new HashMap<String, String>();
        importMapping.put("BigDecimal", "java.math.BigDecimal");
        importMapping.put("UUID", "java.util.UUID");
        importMapping.put("File", "java.io.File");
        importMapping.put("Date", "java.util.Date");
        importMapping.put("Timestamp", "java.sql.Timestamp");
        importMapping.put("DateTime", "java.time.LocalDateTime");
        importMapping.put("LocalDateTime", "java.time.LocalDateTime");
        importMapping.put("LocalDate", "java.time.LocalDate");
        importMapping.put("LocalTime", "java.time.LocalTime");

        specialCharReplacements.put(";", "Semicolon");

        cliOptions.clear();
        addOption(CodegenConstants.SOURCE_FOLDER, CodegenConstants.SOURCE_FOLDER_DESC, sourceFolder);
        addOption(CodegenConstants.PACKAGE_NAME, "Generated artifact package name (e.g. io.swagger).", packageName);
        addOption(CodegenConstants.GROUP_ID, "Generated artifact package's organization (i.e. maven groupId).", groupId);
        addOption(CodegenConstants.ARTIFACT_ID, "Generated artifact id (name of jar).", artifactId);
        addOption(CodegenConstants.ARTIFACT_VERSION, "Generated artifact's package version.", artifactVersion);

        CliOption enumPropertyNamingOpt = new CliOption(CodegenConstants.ENUM_PROPERTY_NAMING, CodegenConstants.ENUM_PROPERTY_NAMING_DESC);
        cliOptions.add(enumPropertyNamingOpt.defaultValue(enumPropertyNaming.name()));
    }

    @Override
    public String apiDocFileFolder() {
        return (outputFolder + "/" + apiDocPath).replace('/', File.separatorChar);
    }

    @Override
    public String apiFileFolder() {
        return outputFolder + File.separator + sourceFolder + File.separator + apiPackage().replace('.', File.separatorChar);
    }

    @Override
    public String escapeQuotationMark(String input) {
        // remove " to avoid code injection
        return input.replace("\"", "");
    }

    @Override
    public String escapeReservedWord(String name) {
        // TODO: Allow enum escaping as an option (e.g. backticks vs append/prepend underscore vs match model property escaping).
        return String.format("`%s`", name);
    }

    @Override
    public String escapeUnsafeCharacters(String input) {
        return input.replace("*/", "*_/").replace("/*", "/_*");
    }

    public CodegenConstants.ENUM_PROPERTY_NAMING_TYPE getEnumPropertyNaming() {
        return this.enumPropertyNaming;
    }

    /**
     * Sets the naming convention for Kotlin enum properties
     *
     * @param enumPropertyNamingType The string representation of the naming convention, as defined by {@link CodegenConstants.ENUM_PROPERTY_NAMING_TYPE}
     */
    public void setEnumPropertyNaming(final String enumPropertyNamingType) {
        try {
            this.enumPropertyNaming = CodegenConstants.ENUM_PROPERTY_NAMING_TYPE.valueOf(enumPropertyNamingType);
        } catch (IllegalArgumentException ex) {
            StringBuilder sb = new StringBuilder(enumPropertyNamingType + " is an invalid enum property naming option. Please choose from:");
            for (CodegenConstants.ENUM_PROPERTY_NAMING_TYPE t : CodegenConstants.ENUM_PROPERTY_NAMING_TYPE.values()) {
                sb.append("\n  ").append(t.name());
            }
            throw new RuntimeException(sb.toString());
        }
    }

    /**
     * returns the swagger type for the property
     *
     * @param p Swagger property object
     * @return string presentation of the type
     **/
    @Override
    public String getSwaggerType(Property p) {
        String swaggerType = super.getSwaggerType(p);
        String type;
        // This maps, for example, long -> kotlin.Long based on hashes in this type's constructor
        if (typeMapping.containsKey(swaggerType)) {
            type = typeMapping.get(swaggerType);
            if (languageSpecificPrimitives.contains(type)) {
                return toModelName(type);
            }
        } else {
            type = swaggerType;
        }
        return toModelName(type);
    }

    /**
     * Output the type declaration of the property
     *
     * @param p Swagger Property object
     * @return a string presentation of the property type
     */
    @Override
    public String getTypeDeclaration(Property p) {
        if (p instanceof ArrayProperty) {
            return getArrayTypeDeclaration((ArrayProperty) p);
        } else if (p instanceof MapProperty) {
            MapProperty mp = (MapProperty) p;
            Property inner = mp.getAdditionalProperties();

            // Maps will be keyed only by primitive Kotlin string
            return getSwaggerType(p) + "<kotlin.String, " + getTypeDeclaration(inner) + ">";
        }
        return super.getTypeDeclaration(p);
    }

    @Override
    public String modelDocFileFolder() {
        return (outputFolder + "/" + modelDocPath).replace('/', File.separatorChar);
    }

    @Override
    public String modelFileFolder() {
        return outputFolder + File.separator + sourceFolder + File.separator + modelPackage().replace('.', File.separatorChar);
    }

    @Override
    public Map<String, Object> postProcessModels(Map<String, Object> objs) {
        return postProcessModelsEnum(super.postProcessModels(objs));
    }

    @Override
    public void processOpts() {
        super.processOpts();

        if (additionalProperties.containsKey(CodegenConstants.ENUM_PROPERTY_NAMING)) {
            setEnumPropertyNaming((String) additionalProperties.get(CodegenConstants.ENUM_PROPERTY_NAMING));
        }

        if (additionalProperties.containsKey(CodegenConstants.SOURCE_FOLDER)) {
            this.setSourceFolder((String) additionalProperties.get(CodegenConstants.SOURCE_FOLDER));
        } else {
            additionalProperties.put(CodegenConstants.SOURCE_FOLDER, sourceFolder);
        }

        if (additionalProperties.containsKey(CodegenConstants.PACKAGE_NAME)) {
            this.setPackageName((String) additionalProperties.get(CodegenConstants.PACKAGE_NAME));
            if (!additionalProperties.containsKey(CodegenConstants.MODEL_PACKAGE))
                this.setModelPackage(packageName + ".models");
            if (!additionalProperties.containsKey(CodegenConstants.API_PACKAGE))
                this.setApiPackage(packageName + ".apis");
        } else {
            additionalProperties.put(CodegenConstants.PACKAGE_NAME, packageName);
        }

        if (additionalProperties.containsKey(CodegenConstants.ARTIFACT_ID)) {
            this.setArtifactId((String) additionalProperties.get(CodegenConstants.ARTIFACT_ID));
        } else {
            additionalProperties.put(CodegenConstants.ARTIFACT_ID, artifactId);
        }

        if (additionalProperties.containsKey(CodegenConstants.GROUP_ID)) {
            this.setGroupId((String) additionalProperties.get(CodegenConstants.GROUP_ID));
        } else {
            additionalProperties.put(CodegenConstants.GROUP_ID, groupId);
        }

        if (additionalProperties.containsKey(CodegenConstants.ARTIFACT_VERSION)) {
            this.setArtifactVersion((String) additionalProperties.get(CodegenConstants.ARTIFACT_VERSION));
        } else {
            additionalProperties.put(CodegenConstants.ARTIFACT_VERSION, artifactVersion);
        }

        if (additionalProperties.containsKey(CodegenConstants.INVOKER_PACKAGE)) {
            LOGGER.warn(CodegenConstants.INVOKER_PACKAGE + " with " + this.getName() + " generator is ignored. Use " + CodegenConstants.PACKAGE_NAME + ".");
        }

        additionalProperties.put(CodegenConstants.API_PACKAGE, apiPackage());
        additionalProperties.put(CodegenConstants.MODEL_PACKAGE, modelPackage());

        additionalProperties.put("apiDocPath", apiDocPath);
        additionalProperties.put("modelDocPath", modelDocPath);
    }

    public void setArtifactId(String artifactId) {
        this.artifactId = artifactId;
    }

    public void setArtifactVersion(String artifactVersion) {
        this.artifactVersion = artifactVersion;
    }

    public void setGroupId(String groupId) {
        this.groupId = groupId;
    }

    public void setPackageName(String packageName) {
        this.packageName = packageName;
    }

    public void setSourceFolder(String sourceFolder) {
        this.sourceFolder = sourceFolder;
    }

    /**
     * Return the sanitized variable name for enum
     *
     * @param value    enum variable name
     * @param datatype data type
     * @return the sanitized variable name for enum
     */
    @Override
    public String toEnumVarName(String value, String datatype) {
        String modified;
        if (value.length() == 0) {
            modified = "EMPTY";
        } else {
            modified = value;
            modified = sanitizeKotlinSpecificNames(modified);
        }

        switch (getEnumPropertyNaming()) {
            case original:
                // NOTE: This is provided as a last-case allowance, but will still result in reserved words being escaped.
                modified = value;
                break;
            case camelCase:
                // NOTE: Removes hyphens and underscores
                modified = camelize(modified, true);
                break;
            case PascalCase:
                // NOTE: Removes hyphens and underscores
                String result = camelize(modified);
                modified = titleCase(result);
                break;
            case snake_case:
                // NOTE: Removes hyphens
                modified = underscore(modified);
                break;
            case UPPERCASE:
                modified = modified.toUpperCase();
                break;
        }

        if (reservedWords.contains(modified)) {
            return escapeReservedWord(modified);
        }

        return modified;
    }

    @Override
    public String toInstantiationType(Property p) {
        if (p instanceof ArrayProperty) {
            return getArrayTypeDeclaration((ArrayProperty) p);
        }
        return super.toInstantiationType(p);
    }

    /**
     * Return the fully-qualified "Model" name for import
     *
     * @param name the name of the "Model"
     * @return the fully-qualified "Model" name for import
     */
    @Override
    public String toModelImport(String name) {
        // toModelImport is called while processing operations, but DefaultCodegen doesn't
        // define imports correctly with fully qualified primitives and models as defined in this generator.
        if (needToImport(name)) {
            return super.toModelImport(name);
        }

        return name;
    }

    /**
     * Output the proper model name (capitalized).
     * In case the name belongs to the TypeSystem it won't be renamed.
     *
     * @param name the name of the model
     * @return capitalized model name
     */
    @Override
    public String toModelName(final String name) {
        // Allow for explicitly configured kotlin.* and java.* types
        if (name.startsWith("kotlin.") || name.startsWith("java.")) {
            return name;
        }

        // If importMapping contains name, assume this is a legitimate model name.
        if (importMapping.containsKey(name)) {
            return importMapping.get(name);
        }

        String modifiedName = name.replaceAll("\\.", "");
        modifiedName = sanitizeKotlinSpecificNames(modifiedName);

        if (reservedWords.contains(modifiedName)) {
            modifiedName = escapeReservedWord(modifiedName);
        }

        return titleCase(modifiedName);
    }

    /**
     * Provides a strongly typed declaration for simple arrays of some type and arrays of arrays of some type.
     *
     * @param arr
     * @return
     */
    private String getArrayTypeDeclaration(ArrayProperty arr) {
        // TODO: collection type here should be fully qualified namespace to avoid model conflicts
        // This supports arrays of arrays.
        String arrayType = typeMapping.get("array");
        StringBuilder instantiationType = new StringBuilder(arrayType);
        Property items = arr.getItems();
        String nestedType = getTypeDeclaration(items);
        // TODO: We may want to differentiate here between generics and primitive arrays.
        instantiationType.append("<").append(nestedType).append(">");
        return instantiationType.toString();
    }

    /**
     * Sanitize against Kotlin specific naming conventions, which may differ from those required by {@link DefaultCodegen#sanitizeName}.
     *
     * @param name string to be sanitize
     * @return sanitized string
     */
    private String sanitizeKotlinSpecificNames(final String name) {
        String word = name;
        for (Map.Entry<String, String> specialCharacters : specialCharReplacements.entrySet()) {
            // Underscore is the only special character we'll allow
            if (!specialCharacters.getKey().equals("_")) {
                word = word.replaceAll("\\Q" + specialCharacters.getKey() + "\\E", specialCharacters.getValue());
            }
        }

        // Fallback, replace unknowns with underscore.
        word = word.replaceAll("\\W+", "_");
        if (word.matches("\\d.*")) {
            word = "_" + word;
        }

        // _, __, and ___ are reserved in Kotlin. Treat all names with only underscores consistently, regardless of count.
        if (word.matches("^_*$")) {
            word = word.replaceAll("\\Q_\\E", "Underscore");
        }

        return word;
    }

    private String titleCase(final String input) {
        return input.substring(0, 1).toUpperCase() + input.substring(1);
    }

    @Override
    protected boolean isReservedWord(String word) {
        // We want case-sensitive escaping, to avoid unnecessary backtick-escaping.
        return reservedWords.contains(word);
    }

    /**
     * Check the type to see if it needs import the library/module/package
     *
     * @param type name of the type
     * @return true if the library/module/package of the corresponding type needs to be imported
     */
    @Override
    protected boolean needToImport(String type) {
        // provides extra protection against improperly trying to import language primitives and java types
        boolean imports = !type.startsWith("kotlin.") && !type.startsWith("java.") && !defaultIncludes.contains(type) && !languageSpecificPrimitives.contains(type);
        return imports;
    }
}
