package org.openapitools.codegen.languages;

import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.media.StringSchema;
import org.openapitools.codegen.*;
import org.openapitools.codegen.api.TemplatingEngineAdapter;
import org.openapitools.codegen.meta.features.*;
import org.openapitools.codegen.model.ModelsMap;
import org.openapitools.codegen.templating.HandlebarsEngineAdapter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

import static org.openapitools.codegen.utils.StringUtils.camelize;


public class GdscriptClientCodegen extends DefaultCodegen implements CodegenConfig {

    // All generated core classes will use this affixes both in class_name and file name.
    public static final String CORE_NAME_PREFIX = "coreNamePrefix";
    public static final String CORE_NAME_PREFIX_VALUE = "";
    public static final String CORE_NAME_SUFFIX = "coreNameSuffix";
    public static final String CORE_NAME_SUFFIX_VALUE = "";
    // Affixes added to reserved words ; do include the _ for snake_case.
    public static final String ANTICOLLISION_PREFIX = "anticollisionPrefix";
    public static final String ANTICOLLISION_PREFIX_VALUE = "some_";
    public static final String ANTICOLLISION_SUFFIX = "anticollisionSuffix";
    public static final String ANTICOLLISION_SUFFIX_VALUE = "";

    @SuppressWarnings("FieldCanBeLocal")
    private final Logger LOGGER = LoggerFactory.getLogger(GdscriptClientCodegen.class);

    protected String coreNamePrefix = CORE_NAME_PREFIX_VALUE;
    protected String coreNameSuffix = CORE_NAME_SUFFIX_VALUE;

    protected String anticollisionPrefix = ANTICOLLISION_PREFIX_VALUE;
    protected String anticollisionSuffix = ANTICOLLISION_SUFFIX_VALUE;

    // We're putting the doc files right next their target code file
    protected String apiDocPath = "apis/";
    protected String modelDocPath = "models/";

    public void setCoreNamePrefix(String prefix) {
        coreNamePrefix = prefix;
    }

    public String getCoreNamePrefix() {
        return coreNamePrefix;
    }

    public void setCoreNameSuffix(String coreNameSuffix) {
        this.coreNameSuffix = coreNameSuffix;
    }

    public String getCoreNameSuffix() {
        return coreNameSuffix;
    }

    public void setAnticollisionPrefix(String anticollisionPrefix) {
        this.anticollisionPrefix = anticollisionPrefix;
    }

    public String getAnticollisionPrefix() {
        return anticollisionPrefix;
    }

    public void setAnticollisionSuffix(String anticollisionSuffix) {
        this.anticollisionSuffix = anticollisionSuffix;
    }

    public String getAnticollisionSuffix() {
        return anticollisionSuffix;
    }

    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    public String getName() {
        return "gdscript";
    }

    public String getHelp() {
        return "Generates a GDScript client (Godot 4+).";
    }

    public GdscriptClientCodegen() {
        super();

        modifyFeatureSet(features -> features
                .includeSchemaSupportFeatures(
                        SchemaSupportFeature.Simple,
                        SchemaSupportFeature.Composite
                )
                .excludeSchemaSupportFeatures(
                        SchemaSupportFeature.Polymorphism,
                        SchemaSupportFeature.Union,
                        SchemaSupportFeature.allOf,
                        SchemaSupportFeature.anyOf,
                        SchemaSupportFeature.oneOf,
                        SchemaSupportFeature.not
                )
                .includeDocumentationFeatures(
                        DocumentationFeature.Readme
                )
                .includeWireFormatFeatures(
                        WireFormatFeature.JSON
                )
                .excludeWireFormatFeatures(
                        WireFormatFeature.Custom,
                        WireFormatFeature.XML,
                        WireFormatFeature.PROTOBUF
                )
                .includeSecurityFeatures(
                        SecurityFeature.BearerToken,
                        SecurityFeature.ApiKey,
                        SecurityFeature.OAuth2_Implicit // untested
                )
                .excludeSecurityFeatures(
                        SecurityFeature.BasicAuth // desired
                )
                .includeDataTypeFeatures(
                        DataTypeFeature.Array,
                        DataTypeFeature.ArrayOfModel,
                        DataTypeFeature.Binary,
                        DataTypeFeature.Boolean,
                        DataTypeFeature.Byte,
                        DataTypeFeature.Custom,  // help (?)
                        DataTypeFeature.Date,
                        DataTypeFeature.DateTime,
                        DataTypeFeature.Decimal,
                        DataTypeFeature.Double,
                        DataTypeFeature.Enum,
                        DataTypeFeature.File,  // untested
                        DataTypeFeature.Float,
                        DataTypeFeature.Int32,
                        DataTypeFeature.Int64,
                        DataTypeFeature.Null,
                        DataTypeFeature.Object,
                        DataTypeFeature.Password,
                        DataTypeFeature.String,
                        DataTypeFeature.Uuid
                )
                .excludeDataTypeFeatures(
                        DataTypeFeature.AnyType, // Variant will be there eventually
                        DataTypeFeature.ArrayOfCollectionOfEnum,
                        DataTypeFeature.ArrayOfCollectionOfModel,
                        DataTypeFeature.ArrayOfCollectionOfPrimitives,
                        DataTypeFeature.ArrayOfEnum,
                        DataTypeFeature.CollectionFormat,
                        DataTypeFeature.CollectionFormatMulti,
                        DataTypeFeature.MapOfCollectionOfEnum,
                        DataTypeFeature.MapOfCollectionOfModel,
                        DataTypeFeature.MapOfCollectionOfPrimitives,
                        DataTypeFeature.MapOfEnum,
                        DataTypeFeature.MapOfModel,
                        DataTypeFeature.Maps
                )
                .includeGlobalFeatures(
                        GlobalFeature.BasePath,
                        GlobalFeature.Examples,
                        GlobalFeature.ExternalDocumentation,
                        GlobalFeature.Host,
                        GlobalFeature.Info,
                        GlobalFeature.Produces,
                        GlobalFeature.Consumes,
                        GlobalFeature.Schemes,
                        GlobalFeature.ParameterStyling
                )
                .excludeGlobalFeatures(
                        GlobalFeature.Callbacks,
                        GlobalFeature.ParameterizedServer,
                        GlobalFeature.MultiServer,
                        GlobalFeature.LinkObjects,
                        GlobalFeature.XMLStructureDefinitions
                )
                .includeParameterFeatures(
                        ParameterFeature.Body,
                        ParameterFeature.Path,
                        ParameterFeature.Header,
                        ParameterFeature.FormUnencoded,
                        ParameterFeature.Query
                )
                .excludeParameterFeatures(
                        ParameterFeature.FormMultipart,
                        ParameterFeature.Cookie
                )
        );

        // It makes sense to package the generated code like a Godot addon, for ease of installation.
        // Since most users will set the output dir, we perhaps ought to document that fact.
        String addonName = "goas.client";
        outputFolder = "generated-code"
                + File.separator + "gdscript"
                + File.separator + "addons"
                + File.separator + addonName;
        embeddedTemplateDir = templateDir = "gdscript";
        apiPackage = "apis";
        modelPackage = "models";

        modelTemplateFiles.put("model.handlebars", ".gd");
        apiTemplateFiles.put("api.handlebars", ".gd");
        apiDocTemplateFiles.put("api_doc.handlebars", ".md");
        // more supported files are defined in processOpts() (they use coreNamePrefix)

        setReservedWordsLowerCase(getReservedWords());

        typeMapping.clear();
        typeMapping.put("array", "Array");
        typeMapping.put("List", "Array");
        typeMapping.put("boolean", "bool");
        typeMapping.put("string", "String");
        typeMapping.put("int", "int");
        typeMapping.put("float", "float");
        typeMapping.put("number", "float");
        typeMapping.put("long", "float");
        typeMapping.put("short", "float");
        typeMapping.put("char", "String");
        typeMapping.put("double", "int");
        typeMapping.put("object", "Object");
        typeMapping.put("integer", "int");
        typeMapping.put("map", "Dictionary");
        typeMapping.put("set", "Array");
        // No timezone support in Godot so I'm skipping Datetimes for now
        // We might at some point enable datetime support for UTC (non-timezoned)
        typeMapping.put("date", "String");
        typeMapping.put("datetime", "String");
        typeMapping.put("DateTime", "String");  // case matters (format: date-time)
        typeMapping.put("date-time", "String");  // possibly useless
        //typeMapping.put("binary", "?");
        typeMapping.put("file", "String");  // untested
        typeMapping.put("ByteArray", "Array");
        typeMapping.put("UUID", "String");
        //typeMapping.put("Error", "?");
        //typeMapping.put("AnyType", "Variant");

        cliOptions.add(new CliOption(CORE_NAME_PREFIX, "PascalCase prefix added to all core classes")
                .defaultValue(CORE_NAME_PREFIX_VALUE));
        cliOptions.add(new CliOption(CORE_NAME_SUFFIX, "PascalCase suffix added to all core classes")
                .defaultValue(CORE_NAME_SUFFIX_VALUE));
        cliOptions.add(new CliOption(ANTICOLLISION_PREFIX, "Prefix added at the beginning of reserved words")
                .defaultValue(ANTICOLLISION_PREFIX_VALUE));
        cliOptions.add(new CliOption(ANTICOLLISION_SUFFIX, "Suffix added at the ending of reserved words")
                .defaultValue(ANTICOLLISION_SUFFIX_VALUE));

        // Alas, I have not taken care of escaping things properly in the templates.
        // I'm not sure how to handle the different escaping strategies required.
        // FIXME: we can't log like this here → too verbose
        //LOGGER.warn("---- THE GENERATED CODE MAY BE UNSAFE AND MALICIOUS OAS FILES MAY HURT YOU ----");
        //LOGGER.warn("PLEASE READ *CAREFULLY* THE OAS FILE YOU ARE USING BEFORE YOU TRUST IT.");
    }

    @Override
    public String defaultTemplatingEngine() {
        return "handlebars";
    }

    public void processAdditionalProperties() {
        additionalProperties.put("apiDocPath", apiDocPath);
        additionalProperties.put("modelDocPath", modelDocPath);
        additionalProperties.put("modelNameSuffix", modelNameSuffix);

        if (additionalProperties.containsKey(CORE_NAME_PREFIX)) {
            setCoreNamePrefix((String) additionalProperties.get(CORE_NAME_PREFIX));
        }
        writePropertyBack(CORE_NAME_PREFIX, getCoreNamePrefix());

        if (additionalProperties.containsKey(CORE_NAME_SUFFIX)) {
            setCoreNameSuffix((String) additionalProperties.get(CORE_NAME_SUFFIX));
        }
        writePropertyBack(CORE_NAME_SUFFIX, getCoreNameSuffix());

        if (additionalProperties.containsKey(ANTICOLLISION_PREFIX)) {
            setAnticollisionPrefix((String) additionalProperties.get(ANTICOLLISION_PREFIX));
        }
        writePropertyBack(ANTICOLLISION_PREFIX, getAnticollisionPrefix());

        if (additionalProperties.containsKey(ANTICOLLISION_SUFFIX)) {
            setAnticollisionSuffix((String) additionalProperties.get(ANTICOLLISION_SUFFIX));
        }
        writePropertyBack(ANTICOLLISION_SUFFIX, getAnticollisionSuffix());
    }

    @Override
    public void processOpts() {
        super.processOpts();

        processAdditionalProperties();
        supportingFiles.add(new SupportingFile("core/ApiBee.handlebars", "core", toCoreFilename("ApiBee") + ".gd"));
        supportingFiles.add(new SupportingFile("core/ApiError.handlebars", "core", toCoreFilename("ApiError") + ".gd"));
        supportingFiles.add(new SupportingFile("core/ApiConfig.handlebars", "core", toCoreFilename("ApiConfig") + ".gd"));
        supportingFiles.add(new SupportingFile("core/ApiResponse.handlebars", "core", toCoreFilename("ApiResponse") + ".gd"));
        supportingFiles.add(new SupportingFile("README.handlebars", "", "README.md"));

        // Ensure we're using the appropriate template engine, and configure it while we're at it.
        // We had to use handlebars because the truthy values of mustache include `""` and `"null"`,
        // and things went south for default values and examples (but descriptions were OK, somehow)
        TemplatingEngineAdapter templatingEngine = getTemplatingEngine();
        if (templatingEngine instanceof HandlebarsEngineAdapter) {
            HandlebarsEngineAdapter handlebars = (HandlebarsEngineAdapter) templatingEngine;
            //handlebars.infiniteLoops(true); // will we want this eventually?
            handlebars.setPrettyPrint(true);  // removes blank lines left by flow control tags
        } else {
            throw new RuntimeException("Only the HandlebarsEngineAdapter is supported for this generator");
        }
    }

    @Override
    public String apiDocFileFolder() {
        return (outputFolder + File.separator + apiDocPath);
    }

    @Override
    public String modelDocFileFolder() {
        return (outputFolder + File.separator + modelDocPath);
    }

    @Override
    public String escapeUnsafeCharacters(String input) {
        // There might be ways to inject code in Gdscript, but I don't see any for now.  (no /* */ comments)
        // TODO: review this can of worms with someone knowledgeable
        return input;
    }

    @Override
    public String escapeQuotationMark(String input) {
        // I've seen some other targets REMOVE the quotation marks altogether.
        // We might need to do that as well ?
        // TODO: review this with someone knowledgeable
        return input
                .replace("\"", "\\\"")
                .replace("'", "\\'")
                ;
    }

    @Override
    public String escapeReservedWord(String name) {
        return getAnticollisionPrefix() + name + getAnticollisionSuffix();
    }

    public String escapeStringLiteral(String input) {
        return input
                .replace("\"", "\\\"")  // escape double quotes
                .replaceAll("[\\\\]+$", "")  // remove trailing backslash(es)
                // issue: "foo\<control char>" will perhaps still wreak havoc
                ;
    }

    // In GDScript we want our file names to match our class names.
    // This ensures we get the (optional) prefix and suffix added to the file name.
    // Perhaps we'll even do (optional) snake_case in here later on.
    @Override
    public String toModelFilename(String name) {
        return toModelName(name);
    }

    public String toCoreFilename(String name) {
        return camelize(getCoreNamePrefix() + '_' + name + '_' + getCoreNameSuffix());
    }

    @Override
    public String toExampleValue(Schema schema) {
        if (schema.getExample() != null) {
            return escapeStringLiteral(super.toExampleValue(schema));
        }

        return "";
    }

    @Override
    public String toDefaultValue(Schema schema) {
        if (schema.getDefault() != null) {
            if (schema instanceof StringSchema) {
                return "\"" + escapeStringLiteral(schema.getDefault().toString()) + "\"";
            }
            return schema.getDefault().toString();
        }

        if (schema instanceof StringSchema) {
            return "\"\"";
        }

        return null;  // if instead we call super here, we get "null" where we should not
    }

    @Override
    public String getTypeDeclaration(Schema schema) {
        String type = super.getTypeDeclaration(schema);

        // purpose: camelize the inline response models dataType (requests are already fine)
        // Possibly not the correct way to do it but I've tried many, many things.
        if (type.contains("_")) {
            type = camelize(type);
        }

        return type;
    }

    @Override
    public Map<String, ModelsMap> updateAllModels(Map<String, ModelsMap> objs) {
        objs = super.updateAllModels(objs);

        String[] names = objs.keySet().toArray(new String[0]);
        for (String modelName : names) {
            // Skip models with jsonld, we don't support them atm
            // eg: Announcement.jsonld
            if (modelName.contains("jsonld")) {
                objs.remove(modelName);
                LOGGER.warn("Skipped model " + modelName + " (jsonld not supported)");
            }
            // Also skip Hydra models, we're not supporting them atm
            if (modelName.contains("Hydra") || modelName.contains("hydra")) {
                objs.remove(modelName);
                LOGGER.warn("Skipped model " + modelName + " (hydra not supported)");
            }
        }

        return objs;
    }

    protected List<String> getReservedWords() {
        return Arrays.asList(
                // Local method names used in base API class
                "_bzz_connect_client_if_needed", "_bzz_request", "_bzz_request_text", "_bzz_do_request_text",
                "_bzz_convert_http_method", "_bzz_urlize_path_param", "_bzz_escape_path_param",
                "_bzz_next_loop_iteration", "_bzz_get_content_type", "_bzz_format_error_response",
                // Local properties used in base API class
                "_bzz_config", "_bzz_client", "_bzz_name",
                // Local variable names used in API methods (endpoints)
                "bzz_method", "bzz_path", "bzz_query",
                "bzz_result", "bzz_code", "bzz_headers",
                "bzz_error",
                // Hmmm…  Will be removed eventually.  Promises !  ;)
                "on_success", "on_failure",

                // Godot's Global Scope
                // https://github.com/godotengine/godot/blob/master/doc/classes/%40GlobalScope.xml
                // List generated from modules/openapi-generator/src/main/resources/gdscript/utils/extract_reserved_words.py
                // Godot's global functions
                "abs", "absf", "absi", "acos", "asin", "atan", "atan2", "bezier_interpolate", "bytes_to_var",
                "bytes_to_var_with_objects", "ceil", "ceilf", "ceili", "clamp", "clampf", "clampi", "cos",
                "cosh", "cubic_interpolate", "cubic_interpolate_angle", "cubic_interpolate_angle_in_time",
                "cubic_interpolate_in_time", "db_to_linear", "deg_to_rad", "ease", "error_string", "exp",
                "floor", "floorf", "floori", "fmod", "fposmod", "hash", "instance_from_id", "inverse_lerp",
                "is_equal_approx", "is_finite", "is_inf", "is_instance_id_valid", "is_instance_valid",
                "is_nan", "is_zero_approx", "lerp", "lerp_angle", "lerpf", "linear_to_db", "log", "max",
                "maxf", "maxi", "min", "minf", "mini", "move_toward", "nearest_po2", "pingpong", "posmod",
                "pow", "print", "print_rich", "print_verbose", "printerr", "printraw", "prints", "printt",
                "push_error", "push_warning", "rad_to_deg", "rand_from_seed", "randf", "randf_range",
                "randfn", "randi", "randi_range", "randomize", "remap", "rid_allocate_id", "rid_from_int64",
                "round", "roundf", "roundi", "seed", "sign", "signf", "signi", "sin", "sinh", "smoothstep",
                "snapped", "sqrt", "step_decimals", "str", "str_to_var", "tan", "tanh", "typeof",
                "var_to_bytes", "var_to_bytes_with_objects", "var_to_str", "weakref", "wrap", "wrapf", "wrapi",
                // Godot's global constants
                "SIDE_LEFT", "SIDE_TOP", "SIDE_RIGHT", "SIDE_BOTTOM", "CORNER_TOP_LEFT",
                "CORNER_TOP_RIGHT", "CORNER_BOTTOM_RIGHT", "CORNER_BOTTOM_LEFT", "VERTICAL", "HORIZONTAL",
                "CLOCKWISE", "COUNTERCLOCKWISE", "HORIZONTAL_ALIGNMENT_LEFT", "HORIZONTAL_ALIGNMENT_CENTER",
                "HORIZONTAL_ALIGNMENT_RIGHT", "HORIZONTAL_ALIGNMENT_FILL", "VERTICAL_ALIGNMENT_TOP",
                "VERTICAL_ALIGNMENT_CENTER", "VERTICAL_ALIGNMENT_BOTTOM", "VERTICAL_ALIGNMENT_FILL",
                "INLINE_ALIGNMENT_TOP_TO", "INLINE_ALIGNMENT_CENTER_TO", "INLINE_ALIGNMENT_BOTTOM_TO",
                "INLINE_ALIGNMENT_TO_TOP", "INLINE_ALIGNMENT_TO_CENTER", "INLINE_ALIGNMENT_TO_BASELINE",
                "INLINE_ALIGNMENT_TO_BOTTOM", "INLINE_ALIGNMENT_TOP", "INLINE_ALIGNMENT_CENTER",
                "INLINE_ALIGNMENT_BOTTOM", "INLINE_ALIGNMENT_IMAGE_MASK", "INLINE_ALIGNMENT_TEXT_MASK",
                "KEY_NONE", "KEY_SPECIAL", "KEY_ESCAPE", "KEY_TAB", "KEY_BACKTAB", "KEY_BACKSPACE",
                "KEY_ENTER", "KEY_KP_ENTER", "KEY_INSERT", "KEY_DELETE", "KEY_PAUSE", "KEY_PRINT",
                "KEY_SYSREQ", "KEY_CLEAR", "KEY_HOME", "KEY_END", "KEY_LEFT", "KEY_UP", "KEY_RIGHT",
                "KEY_DOWN", "KEY_PAGEUP", "KEY_PAGEDOWN", "KEY_SHIFT", "KEY_CTRL", "KEY_META", "KEY_ALT",
                "KEY_CAPSLOCK", "KEY_NUMLOCK", "KEY_SCROLLLOCK", "KEY_F1", "KEY_F2", "KEY_F3", "KEY_F4",
                "KEY_F5", "KEY_F6", "KEY_F7", "KEY_F8", "KEY_F9", "KEY_F10", "KEY_F11", "KEY_F12", "KEY_F13",
                "KEY_F14", "KEY_F15", "KEY_F16", "KEY_F17", "KEY_F18", "KEY_F19", "KEY_F20", "KEY_F21",
                "KEY_F22", "KEY_F23", "KEY_F24", "KEY_F25", "KEY_F26", "KEY_F27", "KEY_F28", "KEY_F29",
                "KEY_F30", "KEY_F31", "KEY_F32", "KEY_F33", "KEY_F34", "KEY_F35", "KEY_KP_MULTIPLY",
                "KEY_KP_DIVIDE", "KEY_KP_SUBTRACT", "KEY_KP_PERIOD", "KEY_KP_ADD", "KEY_KP_0", "KEY_KP_1",
                "KEY_KP_2", "KEY_KP_3", "KEY_KP_4", "KEY_KP_5", "KEY_KP_6", "KEY_KP_7", "KEY_KP_8",
                "KEY_KP_9", "KEY_SUPER_L", "KEY_SUPER_R", "KEY_MENU", "KEY_HYPER_L", "KEY_HYPER_R",
                "KEY_HELP", "KEY_DIRECTION_L", "KEY_DIRECTION_R", "KEY_BACK", "KEY_FORWARD", "KEY_STOP",
                "KEY_REFRESH", "KEY_VOLUMEDOWN", "KEY_VOLUMEMUTE", "KEY_VOLUMEUP", "KEY_BASSBOOST",
                "KEY_BASSUP", "KEY_BASSDOWN", "KEY_TREBLEUP", "KEY_TREBLEDOWN", "KEY_MEDIAPLAY",
                "KEY_MEDIASTOP", "KEY_MEDIAPREVIOUS", "KEY_MEDIANEXT", "KEY_MEDIARECORD", "KEY_HOMEPAGE",
                "KEY_FAVORITES", "KEY_SEARCH", "KEY_STANDBY", "KEY_OPENURL", "KEY_LAUNCHMAIL",
                "KEY_LAUNCHMEDIA", "KEY_LAUNCH0", "KEY_LAUNCH1", "KEY_LAUNCH2", "KEY_LAUNCH3", "KEY_LAUNCH4",
                "KEY_LAUNCH5", "KEY_LAUNCH6", "KEY_LAUNCH7", "KEY_LAUNCH8", "KEY_LAUNCH9", "KEY_LAUNCHA",
                "KEY_LAUNCHB", "KEY_LAUNCHC", "KEY_LAUNCHD", "KEY_LAUNCHE", "KEY_LAUNCHF", "KEY_UNKNOWN",
                "KEY_SPACE", "KEY_EXCLAM", "KEY_QUOTEDBL", "KEY_NUMBERSIGN", "KEY_DOLLAR", "KEY_PERCENT",
                "KEY_AMPERSAND", "KEY_APOSTROPHE", "KEY_PARENLEFT", "KEY_PARENRIGHT", "KEY_ASTERISK",
                "KEY_PLUS", "KEY_COMMA", "KEY_MINUS", "KEY_PERIOD", "KEY_SLASH", "KEY_0", "KEY_1", "KEY_2",
                "KEY_3", "KEY_4", "KEY_5", "KEY_6", "KEY_7", "KEY_8", "KEY_9", "KEY_COLON", "KEY_SEMICOLON",
                "KEY_LESS", "KEY_EQUAL", "KEY_GREATER", "KEY_QUESTION", "KEY_AT", "KEY_A", "KEY_B", "KEY_C",
                "KEY_D", "KEY_E", "KEY_F", "KEY_G", "KEY_H", "KEY_I", "KEY_J", "KEY_K", "KEY_L", "KEY_M",
                "KEY_N", "KEY_O", "KEY_P", "KEY_Q", "KEY_R", "KEY_S", "KEY_T", "KEY_U", "KEY_V", "KEY_W",
                "KEY_X", "KEY_Y", "KEY_Z", "KEY_BRACKETLEFT", "KEY_BACKSLASH", "KEY_BRACKETRIGHT",
                "KEY_ASCIICIRCUM", "KEY_UNDERSCORE", "KEY_QUOTELEFT", "KEY_BRACELEFT", "KEY_BAR",
                "KEY_BRACERIGHT", "KEY_ASCIITILDE", "KEY_NOBREAKSPACE", "KEY_EXCLAMDOWN", "KEY_CENT",
                "KEY_STERLING", "KEY_CURRENCY", "KEY_YEN", "KEY_BROKENBAR", "KEY_SECTION", "KEY_DIAERESIS",
                "KEY_COPYRIGHT", "KEY_ORDFEMININE", "KEY_GUILLEMOTLEFT", "KEY_NOTSIGN", "KEY_HYPHEN",
                "KEY_REGISTERED", "KEY_MACRON", "KEY_DEGREE", "KEY_PLUSMINUS", "KEY_TWOSUPERIOR",
                "KEY_THREESUPERIOR", "KEY_ACUTE", "KEY_MU", "KEY_PARAGRAPH", "KEY_PERIODCENTERED",
                "KEY_CEDILLA", "KEY_ONESUPERIOR", "KEY_MASCULINE", "KEY_GUILLEMOTRIGHT", "KEY_ONEQUARTER",
                "KEY_ONEHALF", "KEY_THREEQUARTERS", "KEY_QUESTIONDOWN", "KEY_AGRAVE", "KEY_AACUTE",
                "KEY_ACIRCUMFLEX", "KEY_ATILDE", "KEY_ADIAERESIS", "KEY_ARING", "KEY_AE", "KEY_CCEDILLA",
                "KEY_EGRAVE", "KEY_EACUTE", "KEY_ECIRCUMFLEX", "KEY_EDIAERESIS", "KEY_IGRAVE", "KEY_IACUTE",
                "KEY_ICIRCUMFLEX", "KEY_IDIAERESIS", "KEY_ETH", "KEY_NTILDE", "KEY_OGRAVE", "KEY_OACUTE",
                "KEY_OCIRCUMFLEX", "KEY_OTILDE", "KEY_ODIAERESIS", "KEY_MULTIPLY", "KEY_OOBLIQUE",
                "KEY_UGRAVE", "KEY_UACUTE", "KEY_UCIRCUMFLEX", "KEY_UDIAERESIS", "KEY_YACUTE", "KEY_THORN",
                "KEY_SSHARP", "KEY_DIVISION", "KEY_YDIAERESIS", "KEY_CODE_MASK", "KEY_MODIFIER_MASK",
                "KEY_MASK_CMD_OR_CTRL", "KEY_MASK_SHIFT", "KEY_MASK_ALT", "KEY_MASK_META", "KEY_MASK_CTRL",
                "KEY_MASK_KPAD", "KEY_MASK_GROUP_SWITCH", "MOUSE_BUTTON_NONE", "MOUSE_BUTTON_LEFT",
                "MOUSE_BUTTON_RIGHT", "MOUSE_BUTTON_MIDDLE", "MOUSE_BUTTON_WHEEL_UP",
                "MOUSE_BUTTON_WHEEL_DOWN", "MOUSE_BUTTON_WHEEL_LEFT", "MOUSE_BUTTON_WHEEL_RIGHT",
                "MOUSE_BUTTON_XBUTTON1", "MOUSE_BUTTON_XBUTTON2", "MOUSE_BUTTON_MASK_LEFT",
                "MOUSE_BUTTON_MASK_RIGHT", "MOUSE_BUTTON_MASK_MIDDLE", "MOUSE_BUTTON_MASK_XBUTTON1",
                "MOUSE_BUTTON_MASK_XBUTTON2", "JOY_BUTTON_INVALID", "JOY_BUTTON_A", "JOY_BUTTON_B",
                "JOY_BUTTON_X", "JOY_BUTTON_Y", "JOY_BUTTON_BACK", "JOY_BUTTON_GUIDE", "JOY_BUTTON_START",
                "JOY_BUTTON_LEFT_STICK", "JOY_BUTTON_RIGHT_STICK", "JOY_BUTTON_LEFT_SHOULDER",
                "JOY_BUTTON_RIGHT_SHOULDER", "JOY_BUTTON_DPAD_UP", "JOY_BUTTON_DPAD_DOWN",
                "JOY_BUTTON_DPAD_LEFT", "JOY_BUTTON_DPAD_RIGHT", "JOY_BUTTON_MISC1", "JOY_BUTTON_PADDLE1",
                "JOY_BUTTON_PADDLE2", "JOY_BUTTON_PADDLE3", "JOY_BUTTON_PADDLE4", "JOY_BUTTON_TOUCHPAD",
                "JOY_BUTTON_SDL_MAX", "JOY_BUTTON_MAX", "JOY_AXIS_INVALID", "JOY_AXIS_LEFT_X",
                "JOY_AXIS_LEFT_Y", "JOY_AXIS_RIGHT_X", "JOY_AXIS_RIGHT_Y", "JOY_AXIS_TRIGGER_LEFT",
                "JOY_AXIS_TRIGGER_RIGHT", "JOY_AXIS_SDL_MAX", "JOY_AXIS_MAX", "MIDI_MESSAGE_NONE",
                "MIDI_MESSAGE_NOTE_OFF", "MIDI_MESSAGE_NOTE_ON", "MIDI_MESSAGE_AFTERTOUCH",
                "MIDI_MESSAGE_CONTROL_CHANGE", "MIDI_MESSAGE_PROGRAM_CHANGE", "MIDI_MESSAGE_CHANNEL_PRESSURE",
                "MIDI_MESSAGE_PITCH_BEND", "MIDI_MESSAGE_SYSTEM_EXCLUSIVE", "MIDI_MESSAGE_QUARTER_FRAME",
                "MIDI_MESSAGE_SONG_POSITION_POINTER", "MIDI_MESSAGE_SONG_SELECT", "MIDI_MESSAGE_TUNE_REQUEST",
                "MIDI_MESSAGE_TIMING_CLOCK", "MIDI_MESSAGE_START", "MIDI_MESSAGE_CONTINUE",
                "MIDI_MESSAGE_STOP", "MIDI_MESSAGE_ACTIVE_SENSING", "MIDI_MESSAGE_SYSTEM_RESET", "OK",
                "FAILED", "ERR_UNAVAILABLE", "ERR_UNCONFIGURED", "ERR_UNAUTHORIZED",
                "ERR_PARAMETER_RANGE_ERROR", "ERR_OUT_OF_MEMORY", "ERR_FILE_NOT_FOUND", "ERR_FILE_BAD_DRIVE",
                "ERR_FILE_BAD_PATH", "ERR_FILE_NO_PERMISSION", "ERR_FILE_ALREADY_IN_USE",
                "ERR_FILE_CANT_OPEN", "ERR_FILE_CANT_WRITE", "ERR_FILE_CANT_READ", "ERR_FILE_UNRECOGNIZED",
                "ERR_FILE_CORRUPT", "ERR_FILE_MISSING_DEPENDENCIES", "ERR_FILE_EOF", "ERR_CANT_OPEN",
                "ERR_CANT_CREATE", "ERR_QUERY_FAILED", "ERR_ALREADY_IN_USE", "ERR_LOCKED", "ERR_TIMEOUT",
                "ERR_CANT_CONNECT", "ERR_CANT_RESOLVE", "ERR_CONNECTION_ERROR", "ERR_CANT_ACQUIRE_RESOURCE",
                "ERR_CANT_FORK", "ERR_INVALID_DATA", "ERR_INVALID_PARAMETER", "ERR_ALREADY_EXISTS",
                "ERR_DOES_NOT_EXIST", "ERR_DATABASE_CANT_READ", "ERR_DATABASE_CANT_WRITE",
                "ERR_COMPILATION_FAILED", "ERR_METHOD_NOT_FOUND", "ERR_LINK_FAILED", "ERR_SCRIPT_FAILED",
                "ERR_CYCLIC_LINK", "ERR_INVALID_DECLARATION", "ERR_DUPLICATE_SYMBOL", "ERR_PARSE_ERROR",
                "ERR_BUSY", "ERR_SKIP", "ERR_HELP", "ERR_BUG", "ERR_PRINTER_ON_FIRE", "PROPERTY_HINT_NONE",
                "PROPERTY_HINT_RANGE", "PROPERTY_HINT_ENUM", "PROPERTY_HINT_ENUM_SUGGESTION",
                "PROPERTY_HINT_EXP_EASING", "PROPERTY_HINT_LINK", "PROPERTY_HINT_FLAGS",
                "PROPERTY_HINT_LAYERS_2D_RENDER", "PROPERTY_HINT_LAYERS_2D_PHYSICS",
                "PROPERTY_HINT_LAYERS_2D_NAVIGATION", "PROPERTY_HINT_LAYERS_3D_RENDER",
                "PROPERTY_HINT_LAYERS_3D_PHYSICS", "PROPERTY_HINT_LAYERS_3D_NAVIGATION", "PROPERTY_HINT_FILE",
                "PROPERTY_HINT_DIR", "PROPERTY_HINT_GLOBAL_FILE", "PROPERTY_HINT_GLOBAL_DIR",
                "PROPERTY_HINT_RESOURCE_TYPE", "PROPERTY_HINT_MULTILINE_TEXT", "PROPERTY_HINT_EXPRESSION",
                "PROPERTY_HINT_PLACEHOLDER_TEXT", "PROPERTY_HINT_COLOR_NO_ALPHA",
                "PROPERTY_HINT_IMAGE_COMPRESS_LOSSY", "PROPERTY_HINT_IMAGE_COMPRESS_LOSSLESS",
                "PROPERTY_HINT_OBJECT_ID", "PROPERTY_HINT_TYPE_STRING",
                "PROPERTY_HINT_NODE_PATH_TO_EDITED_NODE", "PROPERTY_HINT_METHOD_OF_VARIANT_TYPE",
                "PROPERTY_HINT_METHOD_OF_BASE_TYPE", "PROPERTY_HINT_METHOD_OF_INSTANCE",
                "PROPERTY_HINT_METHOD_OF_SCRIPT", "PROPERTY_HINT_PROPERTY_OF_VARIANT_TYPE",
                "PROPERTY_HINT_PROPERTY_OF_BASE_TYPE", "PROPERTY_HINT_PROPERTY_OF_INSTANCE",
                "PROPERTY_HINT_PROPERTY_OF_SCRIPT", "PROPERTY_HINT_OBJECT_TOO_BIG",
                "PROPERTY_HINT_NODE_PATH_VALID_TYPES", "PROPERTY_HINT_SAVE_FILE",
                "PROPERTY_HINT_GLOBAL_SAVE_FILE", "PROPERTY_HINT_INT_IS_OBJECTID",
                "PROPERTY_HINT_INT_IS_POINTER", "PROPERTY_HINT_ARRAY_TYPE", "PROPERTY_HINT_LOCALE_ID",
                "PROPERTY_HINT_LOCALIZABLE_STRING", "PROPERTY_HINT_NODE_TYPE",
                "PROPERTY_HINT_HIDE_QUATERNION_EDIT", "PROPERTY_HINT_PASSWORD", "PROPERTY_HINT_MAX",
                "PROPERTY_USAGE_NONE", "PROPERTY_USAGE_STORAGE", "PROPERTY_USAGE_EDITOR",
                "PROPERTY_USAGE_CHECKABLE", "PROPERTY_USAGE_CHECKED", "PROPERTY_USAGE_INTERNATIONALIZED",
                "PROPERTY_USAGE_GROUP", "PROPERTY_USAGE_CATEGORY", "PROPERTY_USAGE_SUBGROUP",
                "PROPERTY_USAGE_CLASS_IS_BITFIELD", "PROPERTY_USAGE_NO_INSTANCE_STATE",
                "PROPERTY_USAGE_RESTART_IF_CHANGED", "PROPERTY_USAGE_SCRIPT_VARIABLE",
                "PROPERTY_USAGE_STORE_IF_NULL", "PROPERTY_USAGE_ANIMATE_AS_TRIGGER",
                "PROPERTY_USAGE_UPDATE_ALL_IF_MODIFIED", "PROPERTY_USAGE_SCRIPT_DEFAULT_VALUE",
                "PROPERTY_USAGE_CLASS_IS_ENUM", "PROPERTY_USAGE_NIL_IS_VARIANT", "PROPERTY_USAGE_INTERNAL",
                "PROPERTY_USAGE_DO_NOT_SHARE_ON_DUPLICATE", "PROPERTY_USAGE_HIGH_END_GFX",
                "PROPERTY_USAGE_NODE_PATH_FROM_SCENE_ROOT", "PROPERTY_USAGE_RESOURCE_NOT_PERSISTENT",
                "PROPERTY_USAGE_KEYING_INCREMENTS", "PROPERTY_USAGE_DEFERRED_SET_RESOURCE",
                "PROPERTY_USAGE_EDITOR_INSTANTIATE_OBJECT", "PROPERTY_USAGE_EDITOR_BASIC_SETTING",
                "PROPERTY_USAGE_READ_ONLY", "PROPERTY_USAGE_ARRAY", "PROPERTY_USAGE_DEFAULT",
                "PROPERTY_USAGE_DEFAULT_INTL", "PROPERTY_USAGE_NO_EDITOR", "METHOD_FLAG_NORMAL",
                "METHOD_FLAG_EDITOR", "METHOD_FLAG_CONST", "METHOD_FLAG_VIRTUAL", "METHOD_FLAG_VARARG",
                "METHOD_FLAG_STATIC", "METHOD_FLAG_OBJECT_CORE", "METHOD_FLAGS_DEFAULT", "TYPE_NIL",
                "TYPE_BOOL", "TYPE_INT", "TYPE_FLOAT", "TYPE_STRING", "TYPE_VECTOR2", "TYPE_VECTOR2I",
                "TYPE_RECT2", "TYPE_RECT2I", "TYPE_VECTOR3", "TYPE_VECTOR3I", "TYPE_TRANSFORM2D",
                "TYPE_VECTOR4", "TYPE_VECTOR4I", "TYPE_PLANE", "TYPE_QUATERNION", "TYPE_AABB", "TYPE_BASIS",
                "TYPE_TRANSFORM3D", "TYPE_PROJECTION", "TYPE_COLOR", "TYPE_STRING_NAME", "TYPE_NODE_PATH",
                "TYPE_RID", "TYPE_OBJECT", "TYPE_CALLABLE", "TYPE_SIGNAL", "TYPE_DICTIONARY", "TYPE_ARRAY",
                "TYPE_PACKED_BYTE_ARRAY", "TYPE_PACKED_INT32_ARRAY", "TYPE_PACKED_INT64_ARRAY",
                "TYPE_PACKED_FLOAT32_ARRAY", "TYPE_PACKED_FLOAT64_ARRAY", "TYPE_PACKED_STRING_ARRAY",
                "TYPE_PACKED_VECTOR2_ARRAY", "TYPE_PACKED_VECTOR3_ARRAY", "TYPE_PACKED_COLOR_ARRAY",
                "TYPE_MAX", "OP_EQUAL", "OP_NOT_EQUAL", "OP_LESS", "OP_LESS_EQUAL", "OP_GREATER",
                "OP_GREATER_EQUAL", "OP_ADD", "OP_SUBTRACT", "OP_MULTIPLY", "OP_DIVIDE", "OP_NEGATE",
                "OP_POSITIVE", "OP_MODULE", "OP_POWER", "OP_SHIFT_LEFT", "OP_SHIFT_RIGHT", "OP_BIT_AND",
                "OP_BIT_OR", "OP_BIT_XOR", "OP_BIT_NEGATE", "OP_AND", "OP_OR", "OP_XOR", "OP_NOT", "OP_IN",
                "OP_MAX",
                // Godot's singletons
                "AudioServer", "CameraServer", "ClassDB", "DisplayServer", "Engine",
                "EngineDebugger", "Geometry2D", "Geometry3D", "GodotSharp", "IP", "Input", "InputMap",
                "JavaClassWrapper", "JavaScriptBridge", "Marshalls", "NativeExtensionManager",
                "NavigationMeshGenerator", "NavigationServer2D", "NavigationServer3D", "OS", "Performance",
                "PhysicsServer2D", "PhysicsServer2DManager", "PhysicsServer3D", "PhysicsServer3DManager",
                "ProjectSettings", "RenderingServer", "ResourceLoader", "ResourceSaver", "ResourceUID",
                "TextServerManager", "ThemeDB", "Time", "TranslationServer", "WorkerThreadPool", "XRServer",

                // Tokens from GDScript
                // https://github.com/godotengine/godot/blob/master/modules/gdscript/gdscript_tokenizer.cpp
                // Logical
                "and", "or", "not",
                // Control flow
                "if", "elif", "else", "for", "while", "break", "continue", "pass", "return", "match",
                // Keywords – hey, namespace is here, and so is trait !
                "as", "assert", "await", "breakpoint", "class", "class_name", "const", "enum", "extends",
                "func", "in", "is", "namespace", "preload", "self", "signal", "static", "super", "trait",
                "var", "void", "yield",
                // Constants
                "PI", "TAU", "INF", "NaN",
                // Primitive (lowercase) types
                "float", "int", "bool",
                // Types
                "AABB",
                "AcceptDialog",
                "AESContext",
                "AnimatableBody2D",
                "AnimatableBody3D",
                "AnimatedSprite2D",
                "AnimatedSprite3D",
                "AnimatedTexture",
                "AnimationLibrary",
                "AnimationNodeAdd2",
                "AnimationNodeAdd3",
                "AnimationNodeAnimation",
                "AnimationNodeBlend2",
                "AnimationNodeBlend3",
                "AnimationNodeBlendSpace1D",
                "AnimationNodeBlendSpace2D",
                "AnimationNodeBlendTree",
                "AnimationNodeOneShot",
                "AnimationNodeOutput",
                "AnimationNodeStateMachinePlayback",
                "AnimationNodeStateMachineTransition",
                "AnimationNodeStateMachine",
                "AnimationNodeSync",
                "AnimationNodeTimeScale",
                "AnimationNodeTimeSeek",
                "AnimationNodeTransition",
                "AnimationNode",
                "AnimationPlayer",
                "AnimationRootNode",
                "AnimationTrackEditPlugin",
                "AnimationTree",
                "Animation",
                "Area2D",
                "Area3D",
                "ArrayMesh",
                "ArrayOccluder3D",
                "AspectRatioContainer",
                "AStar2D",
                "AStar3D",
                "AStarGrid2D",
                "AtlasTexture",
                "AudioBusLayout",
                "AudioEffectAmplify",
                "AudioEffectBandLimitFilter",
                "AudioEffectBandPassFilter",
                "AudioEffectCapture",
                "AudioEffectChorus",
                "AudioEffectCompressor",
                "AudioEffectDelay",
                "AudioEffectDistortion",
                "AudioEffectEQ10",
                "AudioEffectEQ21",
                "AudioEffectEQ6",
                "AudioEffectEQ",
                "AudioEffectFilter",
                "AudioEffectHighPassFilter",
                "AudioEffectHighShelfFilter",
                "AudioEffectInstance",
                "AudioEffectLimiter",
                "AudioEffectLowPassFilter",
                "AudioEffectLowShelfFilter",
                "AudioEffectNotchFilter",
                "AudioEffectPanner",
                "AudioEffectPhaser",
                "AudioEffectPitchShift",
                "AudioEffectRecord",
                "AudioEffectReverb",
                "AudioEffectSpectrumAnalyzerInstance",
                "AudioEffectSpectrumAnalyzer",
                "AudioEffectStereoEnhance",
                "AudioEffect",
                "AudioListener2D",
                "AudioListener3D",
                "AudioServer",
                "AudioStreamGeneratorPlayback",
                "AudioStreamGenerator",
                "AudioStreamMicrophone",
                "AudioStreamPlaybackResampled",
                "AudioStreamPlayback",
                "AudioStreamPlayer2D",
                "AudioStreamPlayer3D",
                "AudioStreamPlayer",
                "AudioStreamRandomizer",
                "AudioStreamWAV",
                "AudioStream",
                "BackBufferCopy",
                "BaseButton",
                "BaseMaterial3D",
                "Basis",
                "BitMap",
                "Bone2D",
                "BoneAttachment3D",
                "BoneMap",
                "BoxContainer",
                "BoxMesh",
                "BoxOccluder3D",
                "BoxShape3D",
                "ButtonGroup",
                "Button",
                "Callable",
                "CallbackTweener",
                "Camera2D",
                "Camera3D",
                "CameraAttributesPhysical",
                "CameraAttributesPractical",
                "CameraAttributes",
                "CameraFeed",
                "CameraServer",
                "CameraTexture",
                "CanvasGroup",
                "CanvasItemMaterial",
                "CanvasItem",
                "CanvasLayer",
                "CanvasModulate",
                "CanvasTexture",
                "CapsuleMesh",
                "CapsuleShape2D",
                "CapsuleShape3D",
                "CenterContainer",
                "CharacterBody2D",
                "CharacterBody3D",
                "CharFXTransform",
                "CheckBox",
                "CheckButton",
                "CircleShape2D",
                "ClassDB",
                "CodeEdit",
                "CodeHighlighter",
                "CollisionObject2D",
                "CollisionObject3D",
                "CollisionPolygon2D",
                "CollisionPolygon3D",
                "CollisionShape2D",
                "CollisionShape3D",
                "ColorPickerButton",
                "ColorPicker",
                "ColorRect",
                "Color",
                "CompressedCubemapArray",
                "CompressedCubemap",
                "CompressedTexture2DArray",
                "CompressedTexture2D",
                "CompressedTexture3D",
                "CompressedTextureLayered",
                "ConcavePolygonShape2D",
                "ConcavePolygonShape3D",
                "ConeTwistJoint3D",
                "ConfigFile",
                "ConfirmationDialog",
                "Container",
                "Control",
                "ConvexPolygonShape2D",
                "ConvexPolygonShape3D",
                "CPUParticles2D",
                "CPUParticles3D",
                "CryptoKey",
                "Crypto",
                "CubemapArray",
                "Cubemap",
                "Curve2D",
                "Curve3D",
                "CurveTexture",
                "Curve",
                "CurveXYZTexture",
                "CylinderMesh",
                "CylinderShape3D",
                "DampedSpringJoint2D",
                "Decal",
                "Dictionary",
                "DirAccess",
                "DirectionalLight2D",
                "DirectionalLight3D",
                "DisplayServer",
                "DTLSServer",
                "EditorCommandPalette",
                "EditorDebuggerPlugin",
                "EditorExportPlatform",
                "EditorExportPlugin",
                "EditorFeatureProfile",
                "EditorFileDialog",
                "EditorFileSystemDirectory",
                "EditorFileSystemImportFormatSupportQuery",
                "EditorFileSystem",
                "EditorImportPlugin",
                "EditorInspectorPlugin",
                "EditorInspector",
                "EditorInterface",
                "EditorNode3DGizmoPlugin",
                "EditorNode3DGizmo",
                "EditorPaths",
                "EditorPlugin",
                "EditorProperty",
                "EditorResourceConversionPlugin",
                "EditorResourcePicker",
                "EditorResourcePreviewGenerator",
                "EditorResourcePreview",
                "EditorSceneFormatImporter",
                "EditorScenePostImportPlugin",
                "EditorScenePostImport",
                "EditorScriptPicker",
                "EditorScript",
                "EditorSelection",
                "EditorSettings",
                "EditorSpinSlider",
                "EditorSyntaxHighlighter",
                "EditorTranslationParserPlugin",
                "EditorUndoRedoManager",
                "EditorVCSInterface",
                "EncodedObjectAsID",
                "EngineDebugger",
                "EngineProfiler",
                "Engine",
                "Environment",
                "Expression",
                "FileAccess",
                "FileDialog",
                "FileSystemDock",
                "FlowContainer",
                "FogMaterial",
                "FogVolume",
                "FontFile",
                "FontVariation",
                "Font",
                "Generic6DOFJoint3D",
                "Geometry2D",
                "Geometry3D",
                "GeometryInstance3D",
                "GPUParticles2D",
                "GPUParticles3D",
                "GPUParticlesAttractor3D",
                "GPUParticlesAttractorBox3D",
                "GPUParticlesAttractorSphere3D",
                "GPUParticlesAttractorVectorField3D",
                "GPUParticlesCollision3D",
                "GPUParticlesCollisionBox3D",
                "GPUParticlesCollisionHeightField3D",
                "GPUParticlesCollisionSDF3D",
                "GPUParticlesCollisionSphere3D",
                "GradientTexture1D",
                "GradientTexture2D",
                "Gradient",
                "GraphEdit",
                "GraphNode",
                "GridContainer",
                "GrooveJoint2D",
                "HashingContext",
                "HBoxContainer",
                "HeightMapShape3D",
                "HFlowContainer",
                "HingeJoint3D",
                "HMACContext",
                "HScrollBar",
                "HSeparator",
                "HSlider",
                "HSplitContainer",
                "HTTPClient",
                "HTTPRequest",
                "ImageFormatLoaderExtension",
                "ImageFormatLoader",
                "ImageTexture3D",
                "ImageTextureLayered",
                "ImageTexture",
                "Image",
                "ImmediateMesh",
                "ImporterMeshInstance3D",
                "ImporterMesh",
                "InputEventAction",
                "InputEventFromWindow",
                "InputEventGesture",
                "InputEventJoypadButton",
                "InputEventJoypadMotion",
                "InputEventKey",
                "InputEventMagnifyGesture",
                "InputEventMIDI",
                "InputEventMouseButton",
                "InputEventMouseMotion",
                "InputEventMouse",
                "InputEventPanGesture",
                "InputEventScreenDrag",
                "InputEventScreenTouch",
                "InputEventShortcut",
                "InputEventWithModifiers",
                "InputEvent",
                "InputMap",
                "Input",
                "InstancePlaceholder",
                "IntervalTweener",
                "IP",
                "ItemList",
                "JavaClassWrapper",
                "JavaClass",
                "JavaScriptBridge",
                "JavaScriptObject",
                "JNISingleton",
                "Joint2D",
                "Joint3D",
                "JSONRPC",
                "JSON",
                "KinematicCollision2D",
                "KinematicCollision3D",
                "Label3D",
                "LabelSettings",
                "Label",
                "Light2D",
                "Light3D",
                "LightmapGIData",
                "LightmapGI",
                "LightmapperRD",
                "Lightmapper",
                "LightmapProbe",
                "LightOccluder2D",
                "Line2D",
                "LineEdit",
                "LinkButton",
                "MainLoop",
                "MarginContainer",
                "Marker2D",
                "Marker3D",
                "Marshalls",
                "Material",
                "MenuBar",
                "MenuButton",
                "MeshDataTool",
                "MeshInstance2D",
                "MeshInstance3D",
                "MeshLibrary",
                "MeshTexture",
                "Mesh",
                "MethodTweener",
                "MissingNode",
                "MissingResource",
                "MovieWriter",
                "MultiMeshInstance2D",
                "MultiMeshInstance3D",
                "MultiMesh",
                "MultiplayerAPIExtension",
                "MultiplayerAPI",
                "MultiplayerPeerExtension",
                "MultiplayerPeer",
                "Mutex",
                "NativeExtensionManager",
                "NativeExtension",
                "NavigationAgent2D",
                "NavigationAgent3D",
                "NavigationLink2D",
                "NavigationLink3D",
                "NavigationMeshGenerator",
                "NavigationMesh",
                "NavigationObstacle2D",
                "NavigationObstacle3D",
                "NavigationPathQueryParameters2D",
                "NavigationPathQueryParameters3D",
                "NavigationPathQueryResult2D",
                "NavigationPathQueryResult3D",
                "NavigationPolygon",
                "NavigationRegion2D",
                "NavigationRegion3D",
                "NavigationServer2D",
                "NavigationServer3D",
                "NinePatchRect",
                "Node2D",
                "Node3DGizmo",
                "Node3D",
                "NodePath",
                "Node",
                "Object",
                "Occluder3D",
                "OccluderInstance3D",
                "OccluderPolygon2D",
                "OmniLight3D",
                "OptimizedTranslation",
                "OptionButton",
                "ORMMaterial3D",
                "OS",
                "PackedByteArray",
                "PackedColorArray",
                "PackedDataContainerRef",
                "PackedDataContainer",
                "PackedFloat32Array",
                "PackedFloat64Array",
                "PackedInt32Array",
                "PackedInt64Array",
                "PackedScene",
                "PackedStringArray",
                "PackedVector2Array",
                "PackedVector3Array",
                "PacketPeerDTLS",
                "PacketPeerExtension",
                "PacketPeerStream",
                "PacketPeerUDP",
                "PacketPeer",
                "PanelContainer",
                "Panel",
                "PanoramaSkyMaterial",
                "ParallaxBackground",
                "ParallaxLayer",
                "ParticleProcessMaterial",
                "Path2D",
                "Path3D",
                "PathFollow2D",
                "PathFollow3D",
                "PCKPacker",
                "Performance",
                "PhysicalBone2D",
                "PhysicalBone3D",
                "PhysicalSkyMaterial",
                "PhysicsBody2D",
                "PhysicsBody3D",
                "PhysicsDirectBodyState2DExtension",
                "PhysicsDirectBodyState2D",
                "PhysicsDirectBodyState3DExtension",
                "PhysicsDirectBodyState3D",
                "PhysicsDirectSpaceState2DExtension",
                "PhysicsDirectSpaceState2D",
                "PhysicsDirectSpaceState3DExtension",
                "PhysicsDirectSpaceState3D",
                "PhysicsMaterial",
                "PhysicsPointQueryParameters2D",
                "PhysicsPointQueryParameters3D",
                "PhysicsRayQueryParameters2D",
                "PhysicsRayQueryParameters3D",
                "PhysicsServer2DExtension",
                "PhysicsServer2DManager",
                "PhysicsServer2D",
                "PhysicsServer3DExtension",
                "PhysicsServer3DManager",
                "PhysicsServer3DRenderingServerHandler",
                "PhysicsServer3D",
                "PhysicsShapeQueryParameters2D",
                "PhysicsShapeQueryParameters3D",
                "PhysicsTestMotionParameters2D",
                "PhysicsTestMotionParameters3D",
                "PhysicsTestMotionResult2D",
                "PhysicsTestMotionResult3D",
                "PinJoint2D",
                "PinJoint3D",
                "PlaceholderCubemapArray",
                "PlaceholderCubemap",
                "PlaceholderMaterial",
                "PlaceholderMesh",
                "PlaceholderTexture2DArray",
                "PlaceholderTexture2D",
                "PlaceholderTexture3D",
                "PlaceholderTextureLayered",
                "PlaneMesh",
                "Plane",
                "PointLight2D",
                "PointMesh",
                "Polygon2D",
                "PolygonOccluder3D",
                "PolygonPathFinder",
                "PopupMenu",
                "PopupPanel",
                "Popup",
                "PortableCompressedTexture2D",
                "PrimitiveMesh",
                "PrismMesh",
                "ProceduralSkyMaterial",
                "ProgressBar",
                "Projection",
                "ProjectSettings",
                "PropertyTweener",
                "QuadMesh",
                "QuadOccluder3D",
                "Quaternion",
                "RandomNumberGenerator",
                "Range",
                "RayCast2D",
                "RayCast3D",
                "RDAttachmentFormat",
                "RDFramebufferPass",
                "RDPipelineColorBlendStateAttachment",
                "RDPipelineColorBlendState",
                "RDPipelineDepthStencilState",
                "RDPipelineMultisampleState",
                "RDPipelineRasterizationState",
                "RDPipelineSpecializationConstant",
                "RDSamplerState",
                "RDShaderFile",
                "RDShaderSource",
                "RDShaderSPIRV",
                "RDTextureFormat",
                "RDTextureView",
                "RDUniform",
                "RDVertexAttribute",
                "Rect2i",
                "Rect2",
                "RectangleShape2D",
                "RefCounted",
                "ReferenceRect",
                "ReflectionProbe",
                "RemoteTransform2D",
                "RemoteTransform3D",
                "RenderingDevice",
                "RenderingServer",
                "ResourceFormatLoader",
                "ResourceFormatSaver",
                "ResourceImporter",
                "ResourceLoader",
                "ResourcePreloader",
                "ResourceSaver",
                "ResourceUID",
                "Resource",
                "RibbonTrailMesh",
                "RichTextEffect",
                "RichTextLabel",
                "RID",
                "RigidBody2D",
                "RigidBody3D",
                "RootMotionView",
                "SceneState",
                "SceneTreeTimer",
                "SceneTree",
                "ScriptCreateDialog",
                "ScriptEditorBase",
                "ScriptEditor",
                "ScriptExtension",
                "ScriptLanguageExtension",
                "ScriptLanguage",
                "Script",
                "ScrollBar",
                "ScrollContainer",
                "SegmentShape2D",
                "Semaphore",
                "SeparationRayShape2D",
                "SeparationRayShape3D",
                "Separator",
                "ShaderGlobalsOverride",
                "ShaderInclude",
                "ShaderMaterial",
                "Shader",
                "Shape2D",
                "Shape3D",
                "ShapeCast2D",
                "ShapeCast3D",
                "Shortcut",
                "Signal",
                "Skeleton2D",
                "Skeleton3D",
                "SkeletonIK3D",
                "SkeletonModification2DCCDIK",
                "SkeletonModification2DFABRIK",
                "SkeletonModification2DJiggle",
                "SkeletonModification2DLookAt",
                "SkeletonModification2DPhysicalBones",
                "SkeletonModification2DStackHolder",
                "SkeletonModification2DTwoBoneIK",
                "SkeletonModification2D",
                "SkeletonModification3DCCDIK",
                "SkeletonModification3DFABRIK",
                "SkeletonModification3DJiggle",
                "SkeletonModification3DLookAt",
                "SkeletonModification3DStackHolder",
                "SkeletonModification3DTwoBoneIK",
                "SkeletonModification3D",
                "SkeletonModificationStack2D",
                "SkeletonModificationStack3D",
                "SkeletonProfileHumanoid",
                "SkeletonProfile",
                "SkinReference",
                "Skin",
                "Sky",
                "SliderJoint3D",
                "Slider",
                "SoftBody3D",
                "SphereMesh",
                "SphereOccluder3D",
                "SphereShape3D",
                "SpinBox",
                "SplitContainer",
                "SpotLight3D",
                "SpringArm3D",
                "Sprite2D",
                "Sprite3D",
                "SpriteBase3D",
                "SpriteFrames",
                "StandardMaterial3D",
                "StaticBody2D",
                "StaticBody3D",
                "StreamPeerBuffer",
                "StreamPeerExtension",
                "StreamPeerGZIP",
                "StreamPeerTCP",
                "StreamPeerTLS",
                "StreamPeer",
                "StringName",
                "String",
                "StyleBoxEmpty",
                "StyleBoxFlat",
                "StyleBoxLine",
                "StyleBoxTexture",
                "StyleBox",
                "SubViewportContainer",
                "SubViewport",
                "SurfaceTool",
                "SyntaxHighlighter",
                "SystemFont",
                "TabBar",
                "TabContainer",
                "TCPServer",
                "TextEdit",
                "TextLine",
                "TextMesh",
                "TextParagraph",
                "TextServerDummy",
                "TextServerExtension",
                "TextServerManager",
                "TextServer",
                "Texture2DArray",
                "Texture2D",
                "Texture3D",
                "TextureButton",
                "TextureLayered",
                "TextureProgressBar",
                "TextureRect",
                "Texture",
                "ThemeDB",
                "Theme",
                "Thread",
                "TileData",
                "TileMapPattern",
                "TileMap",
                "TileSetAtlasSource",
                "TileSetScenesCollectionSource",
                "TileSetSource",
                "TileSet",
                "Timer",
                "Time",
                "TorusMesh",
                "TouchScreenButton",
                "Transform2D",
                "Transform3D",
                "TranslationServer",
                "Translation",
                "TreeItem",
                "Tree",
                "TriangleMesh",
                "TubeTrailMesh",
                "Tweener",
                "Tween",
                "UDPServer",
                "UndoRedo",
                "Variant",
                "VBoxContainer",
                "Vector2i",
                "Vector2",
                "Vector3i",
                "Vector3",
                "Vector4i",
                "Vector4",
                "VehicleBody3D",
                "VehicleWheel3D",
                "VFlowContainer",
                "VideoStreamPlayer",
                "VideoStream",
                "ViewportTexture",
                "Viewport",
                "VisibleOnScreenEnabler2D",
                "VisibleOnScreenEnabler3D",
                "VisibleOnScreenNotifier2D",
                "VisibleOnScreenNotifier3D",
                "VisualInstance3D",
                "VisualShaderNodeBillboard",
                "VisualShaderNodeBooleanConstant",
                "VisualShaderNodeBooleanParameter",
                "VisualShaderNodeClamp",
                "VisualShaderNodeColorConstant",
                "VisualShaderNodeColorFunc",
                "VisualShaderNodeColorOp",
                "VisualShaderNodeColorParameter",
                "VisualShaderNodeComment",
                "VisualShaderNodeCompare",
                "VisualShaderNodeConstant",
                "VisualShaderNodeCubemapParameter",
                "VisualShaderNodeCubemap",
                "VisualShaderNodeCurveTexture",
                "VisualShaderNodeCurveXYZTexture",
                "VisualShaderNodeCustom",
                "VisualShaderNodeDerivativeFunc",
                "VisualShaderNodeDeterminant",
                "VisualShaderNodeDistanceFade",
                "VisualShaderNodeDotProduct",
                "VisualShaderNodeExpression",
                "VisualShaderNodeFaceForward",
                "VisualShaderNodeFloatConstant",
                "VisualShaderNodeFloatFunc",
                "VisualShaderNodeFloatOp",
                "VisualShaderNodeFloatParameter",
                "VisualShaderNodeFresnel",
                "VisualShaderNodeGlobalExpression",
                "VisualShaderNodeGroupBase",
                "VisualShaderNodeIf",
                "VisualShaderNodeInput",
                "VisualShaderNodeIntConstant",
                "VisualShaderNodeIntFunc",
                "VisualShaderNodeIntOp",
                "VisualShaderNodeIntParameter",
                "VisualShaderNodeIs",
                "VisualShaderNodeLinearSceneDepth",
                "VisualShaderNodeMix",
                "VisualShaderNodeMultiplyAdd",
                "VisualShaderNodeOuterProduct",
                "VisualShaderNodeOutput",
                "VisualShaderNodeParameterRef",
                "VisualShaderNodeParameter",
                "VisualShaderNodeParticleAccelerator",
                "VisualShaderNodeParticleBoxEmitter",
                "VisualShaderNodeParticleConeVelocity",
                "VisualShaderNodeParticleEmitter",
                "VisualShaderNodeParticleEmit",
                "VisualShaderNodeParticleMeshEmitter",
                "VisualShaderNodeParticleMultiplyByAxisAngle",
                "VisualShaderNodeParticleOutput",
                "VisualShaderNodeParticleRandomness",
                "VisualShaderNodeParticleRingEmitter",
                "VisualShaderNodeParticleSphereEmitter",
                "VisualShaderNodeProximityFade",
                "VisualShaderNodeRandomRange",
                "VisualShaderNodeRemap",
                "VisualShaderNodeResizableBase",
                "VisualShaderNodeSample3D",
                "VisualShaderNodeScreenUVToSDF",
                "VisualShaderNodeSDFRaymarch",
                "VisualShaderNodeSDFToScreenUV",
                "VisualShaderNodeSmoothStep",
                "VisualShaderNodeStep",
                "VisualShaderNodeSwitch",
                "VisualShaderNodeTexture2DArrayParameter",
                "VisualShaderNodeTexture2DArray",
                "VisualShaderNodeTexture2DParameter",
                "VisualShaderNodeTexture3DParameter",
                "VisualShaderNodeTexture3D",
                "VisualShaderNodeTextureParameterTriplanar",
                "VisualShaderNodeTextureParameter",
                "VisualShaderNodeTextureSDFNormal",
                "VisualShaderNodeTextureSDF",
                "VisualShaderNodeTexture",
                "VisualShaderNodeTransformCompose",
                "VisualShaderNodeTransformConstant",
                "VisualShaderNodeTransformDecompose",
                "VisualShaderNodeTransformFunc",
                "VisualShaderNodeTransformOp",
                "VisualShaderNodeTransformParameter",
                "VisualShaderNodeTransformVecMult",
                "VisualShaderNodeUVFunc",
                "VisualShaderNodeUVPolarCoord",
                "VisualShaderNodeVaryingGetter",
                "VisualShaderNodeVaryingSetter",
                "VisualShaderNodeVarying",
                "VisualShaderNodeVec2Constant",
                "VisualShaderNodeVec2Parameter",
                "VisualShaderNodeVec3Constant",
                "VisualShaderNodeVec3Parameter",
                "VisualShaderNodeVec4Constant",
                "VisualShaderNodeVec4Parameter",
                "VisualShaderNodeVectorBase",
                "VisualShaderNodeVectorCompose",
                "VisualShaderNodeVectorDecompose",
                "VisualShaderNodeVectorDistance",
                "VisualShaderNodeVectorFunc",
                "VisualShaderNodeVectorLen",
                "VisualShaderNodeVectorOp",
                "VisualShaderNodeVectorRefract",
                "VisualShaderNode",
                "VisualShader",
                "VoxelGIData",
                "VoxelGI",
                "VScrollBar",
                "VSeparator",
                "VSlider",
                "VSplitContainer",
                "WeakRef",
                "Window",
                "WorkerThreadPool",
                "World2D",
                "World3D",
                "WorldBoundaryShape2D",
                "WorldBoundaryShape3D",
                "WorldEnvironment",
                "X509Certificate",
                "XMLParser",
                "XRAnchor3D",
                "XRCamera3D",
                "XRController3D",
                "XRInterfaceExtension",
                "XRInterface",
                "XRNode3D",
                "XROrigin3D",
                "XRPose",
                "XRPositionalTracker",
                "XRServer"
        );
    }
}

