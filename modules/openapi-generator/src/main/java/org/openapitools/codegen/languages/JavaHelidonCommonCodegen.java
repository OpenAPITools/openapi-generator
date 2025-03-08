/*
 * Copyright 2022, 2024 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 * Copyright (c) 2022, 2024 Oracle and/or its affiliates
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

import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.parameters.Parameter;
import io.swagger.v3.oas.models.responses.ApiResponse;
import io.swagger.v3.oas.models.servers.Server;
import lombok.Getter;
import org.apache.commons.lang3.StringUtils;
import org.eclipse.aether.util.version.GenericVersionScheme;
import org.eclipse.aether.version.InvalidVersionSpecificationException;
import org.eclipse.aether.version.Version;
import org.eclipse.aether.version.VersionConstraint;
import org.eclipse.aether.version.VersionScheme;
import org.openapitools.codegen.*;
import org.openapitools.codegen.languages.features.BeanValidationFeatures;
import org.openapitools.codegen.languages.features.PerformBeanValidationFeatures;
import org.openapitools.codegen.model.ModelMap;
import org.openapitools.codegen.model.OperationMap;
import org.openapitools.codegen.model.OperationsMap;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.net.URL;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.time.Duration;
import java.util.*;
import java.util.concurrent.locks.ReentrantLock;
import java.util.prefs.BackingStoreException;
import java.util.prefs.Preferences;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import static org.openapitools.codegen.CodegenConstants.*;

public abstract class JavaHelidonCommonCodegen extends AbstractJavaCodegen
        implements BeanValidationFeatures, PerformBeanValidationFeatures {

    static final String HELIDON_MP = "mp";
    static final String HELIDON_SE = "se";

    static final String MICROPROFILE_ROOT_PACKAGE = "rootJavaEEPackage";
    static final String MICROPROFILE_ROOT_DEP_PREFIX = "x-helidon-rootJavaEEDepPrefix";
    static final String X_USE_MP_TESTING = "x-helidon-useMpTesting";
    static final String X_USE_SMALLRYE_JANDEX_PLUGIN = "x-helidon-useSmallRyeJandexPlugin";
    static final String X_HAS_RESPONSE_PROPS = "x-helidon-hasResponseProps";
    static final String X_ALL_RESPONSE_PROPS = "x-helidon-allResponseProps";
    static final String X_OPTIONAL_RESPONSE_PROPS = "x-helidon-optionalResponseProps";
    static final String X_REQUIRED_RESPONSE_PROPS = "x-helidon-requiredResponseProps";
    static final String X_HAS_REQUIRED_RESPONSE_PROPS = "x-helidon-hasRequiredResponseProps";
    static final String X_RESULT_BUILDER_NEEDS_CTOR = "x-helidon-resultBuilderNeedsCtor";
    static final String X_IS_MULTIPART_FORM_PARAM = "x-helidon-isMultipartFormParam";
    static final String X_HAS_RETURN_TYPE = "x-helidon-hasReturnType";
    static final String X_RETURN_TYPE_EXAMPLE_VALUE = "x-helidon-exampleReturnTypeValue";
    static final String X_MEDIA_SUPPORT_PACKAGE_PREFIX = "x-helidon-media-support-package-prefix";
    static final String X_MEDIA_COMMON_MEDIA_TYPE_PACKAGE_PREFIX = "x-helidon-common-media-type-package-prefix";
    static final String X_USE_REACTIVE = "x-helidon-use-reactive";
    static final String X_USE_OPTIONAL = "x-helidon-use-optional";
    static final String MICROPROFILE_ROOT_PACKAGE_DESC = "Root package name for Java EE";
    static final String MICROPROFILE_ROOT_PACKAGE_JAVAX = "javax";
    static final String MICROPROFILE_ROOT_PACKAGE_JAKARTA = "jakarta";
    static final String HELIDON_ENUM_CLASS = "x-helidon-hasEnumClass";
    private static final String VALIDATION_ARTIFACT_PREFIX_KEY = "x-helidon-validationArtifactPrefix";
    private static final String X_PATH_SUFFIX = "x-helidon-pathSuffix";
    private static final String X_FIXUP_PATH = "x-helidon-fixupPath";
    private static final String VALIDATION_ARTIFACT_PREFIX_JAVAX = "";
    private static final String VALIDATION_ARTIFACT_PREFIX_JAKARTA = MICROPROFILE_ROOT_PACKAGE_JAKARTA + ".";
    private static final Map<String, String> EXAMPLE_RETURN_VALUES = new HashMap<String, String>();
    private final Logger LOGGER = LoggerFactory.getLogger(JavaHelidonCommonCodegen.class);

    // for generated doc
    static final String MICROPROFILE_ROOT_PACKAGE_DEFAULT =
            "Helidon 2.x and earlier: " + MICROPROFILE_ROOT_PACKAGE_JAVAX
                    + "; Helidon 3.x and later: " + MICROPROFILE_ROOT_PACKAGE_JAKARTA;

    static final String SERIALIZATION_LIBRARY_JACKSON = "jackson";
    static final String SERIALIZATION_LIBRARY_JSONB = "jsonb";

    public static final String HELIDON_VERSION = "helidonVersion";

    // Helidon 3 featured reactive style; more recent releases feature synchronous.
    static final String V3_STYLE = "x-helidon-v3";

    static final String HELIDON_VERSION_DESC = "Helidon complete version identifier or major version number. "
            + "The specified exact Helidon release or, if specified as a major version the latest release of that major version, "
            + " is used in the generated code.";

    static final String X_HELIDON_USE_OPTIONAL = "x-helidon-useOptional";
    static final String X_HELIDON_USE_OPTIONAL_DESC = "Wrap optional parameters in an Optional (Helidon 4 and later)";

    static final String FULL_PROJECT = "fullProject";
    static final String FULL_PROJECT_DESC = "If set to true, it will generate all files; if set to false, " +
            "it will only generate API files. If unspecified, the behavior depends on whether a project " +
            "exists or not: if it does not, same as true; if it does, same as false. Note that test files " +
            "are never overwritten.";

    static final String HELIDON_GROUP_BY = "x-helidon-groupBy";
    static final String HELIDON_GROUP_BY_DESC = "Selects how to group operations into APIs";

    protected String helidonVersion;
    protected int helidonMajorVersion;
    protected boolean useReactive;
    protected boolean useOptional = true; // effective with Helidon 4
    protected final GenericTypeDeclarations genericTypeDeclarations = new GenericTypeDeclarations();
    protected Map<String, List<CodegenOperation>> tagToOperations;

    private String rootJavaEEPackage;
    private String rootJavaEEDepPrefix;
    private String mpTestsGroup;
    private String mpTestsArtifact;
    private String jandexGroup;
    private String jandexArtifact;
    private final Map<String, String> knownHttpStatusMap;
    protected GroupBy groupBy = GroupBy.DEFAULT;

    protected enum GroupBy {
        TAGS("tags", "Use the 'tags' settings on each operation"),
        FIRST_PATH_SEGMENT("first-path-segment", "Use the first segment of the path");

        static GroupBy DEFAULT = TAGS;

        static final Map<String, String> OPTION_VALUES = Arrays.stream(values())
                .collect(HashMap::new, (map, value) -> map.put(value.groupingName, value.desc), Map::putAll);

        static GroupBy from(String value) {
            return Arrays.stream(values())
                    .filter(candidate -> candidate.groupingName.equals(value))
                    .findFirst()
                    .orElseThrow();
        }

        private final String groupingName;
        private final String desc;

        GroupBy(String groupingName, String desc) {
            this.groupingName = groupingName;
            this.desc = desc;
        }
    }

    public static String defaultHelidonVersion() {
        return VersionUtil.instance().defaultVersion();
    }

    public static String chooseVersion(String requestedVersionPrefix) {
        return VersionUtil.instance().chooseVersion(requestedVersionPrefix);
    }

    public JavaHelidonCommonCodegen() {
        super();

        EXAMPLE_RETURN_VALUES.put("set", "Set");
        EXAMPLE_RETURN_VALUES.put("array", "List");
        EXAMPLE_RETURN_VALUES.put("map", "Map");

        cliOptions.add(new CliOption(HELIDON_VERSION, HELIDON_VERSION_DESC)
                .defaultValue("Highest released version."));
        cliOptions.add(new CliOption(MICROPROFILE_ROOT_PACKAGE, MICROPROFILE_ROOT_PACKAGE_DESC)
                .defaultValue(MICROPROFILE_ROOT_PACKAGE_DEFAULT));
        cliOptions.add(new CliOption(FULL_PROJECT, FULL_PROJECT_DESC)
                .defaultValue(""));     // depends on project state
        cliOptions.add(new CliOption(X_HELIDON_USE_OPTIONAL, X_HELIDON_USE_OPTIONAL_DESC)
                .defaultValue("true"));
        addOption(HELIDON_GROUP_BY, HELIDON_GROUP_BY_DESC, GroupBy.DEFAULT.groupingName, GroupBy.OPTION_VALUES);

        knownHttpStatusMap = loadKnownHttpStatusMap();
    }

    @Override
    public void processOpts() {
        super.processOpts();

        importMapping.put("Headers", helidonMajorVersion == 3 ? "io.helidon.http.common.Headers" : "io.helidon.http.Headers");
        importMapping.put("Optional", "java.util.Optional");
        importMapping.put("Collectors", "java.util.stream.Collectors");

        String userHelidonVersion = "";
        String userParentVersion = "";

        if (additionalProperties.containsKey(CodegenConstants.PARENT_VERSION)) {
            userParentVersion = additionalProperties.get(CodegenConstants.PARENT_VERSION).toString();
        }

        if (additionalProperties.containsKey(HELIDON_VERSION)) {
            userHelidonVersion = additionalProperties.get(HELIDON_VERSION).toString();
        }

        if (!userHelidonVersion.isEmpty()) {
            if (!userParentVersion.isEmpty() && !userHelidonVersion.equals(userParentVersion)) {
                throw new IllegalArgumentException(
                        String.format(Locale.ROOT,
                                "Both %s and %s properties were set with different value.",
                                CodegenConstants.PARENT_VERSION,
                                HELIDON_VERSION));
            }
            setHelidonVersion(userHelidonVersion);

        } else if (!userParentVersion.isEmpty()) {
            setHelidonVersion(userParentVersion);
        } else {
            setHelidonVersion(VersionUtil.instance().defaultVersion());
        }

        if (helidonMajorVersion > 3) {
            importMapping.put("Status", "io.helidon.http.Status");
            importMapping.put("HexFormat", "java.util.HexFormat");
        }

        additionalProperties.put(HELIDON_VERSION, helidonVersion);
        additionalProperties.put(V3_STYLE, (helidonMajorVersion == 3));

        setEEPackageAndDependencies(helidonVersion);
        setMpTestDependency(helidonVersion);
        setJandexPluginDependency(helidonVersion);
        setMediaPackageInfo();
        setUseReactive();
        setUseOptional();
        convertPropertyToTypeAndWriteBack(HELIDON_GROUP_BY, GroupBy::from, (value) -> this.groupBy = value);
    }

    @Override
    public void addOperationToGroup(String tag,
                                    String resourcePath,
                                    Operation operation,
                                    CodegenOperation co,
                                    Map<String, List<CodegenOperation>> operations) {

        if (helidonMajorVersion <= 3) {
            super.addOperationToGroup(tag, resourcePath, operation, co, operations);
            return;
        }
        /*
         By convention, the operation's baseName is assigned the name of the group (API) in which it is placed.
         */
        String pathPrefixWithoutLeadingSlash = StringUtils.substringBefore(StringUtils.removeStart(resourcePath, "/"),
                "/");
        String groupName = (groupBy == GroupBy.TAGS)
                ? tag
                : sanitizeName(pathPrefixWithoutLeadingSlash);
        if (groupName.isEmpty()) {
            groupName = "Default";
        }
        co.baseName = groupName;
        // Invoke the superclass with our chosen group name as the "tag".
        super.addOperationToGroup(groupName, resourcePath, operation, co, operations);
    }

    @Override
    public CodegenOperation fromOperation(String path, String httpMethod, Operation operation, List<Server> servers) {
        CodegenOperation op = super.fromOperation(path, httpMethod, operation, servers);

        op.allParams.forEach(p -> p.vendorExtensions.put(X_IS_MULTIPART_FORM_PARAM, op.isMultipart && p.isFormParam));
        op.vendorExtensions.put(X_HAS_RETURN_TYPE, op.returnType != null && !op.returnType.equals("void"));
        op.vendorExtensions.put(X_RETURN_TYPE_EXAMPLE_VALUE, chooseExampleReturnTypeValue(op));
        return op;
    }

    @Override
    public CodegenParameter fromParameter(Parameter parameter, Set<String> imports) {
        CodegenParameter result = super.fromParameter(parameter, imports);
        if (!result.required && helidonMajorVersion > 3) {
            imports.add("Optional");
        }
        if (helidonMajorVersion > 3 && result.containerTypeMapped != null) {
            imports.add(result.containerTypeMapped);
        }
        if (result.items != null && result.items.getRef() != null) {
            result.vendorExtensions.put(HELIDON_ENUM_CLASS, true);
        }
        return result;
    }

    @Override
    public CodegenResponse fromResponse(String responseCode, ApiResponse response) {
        CodegenResponse result = super.fromResponse(responseCode, response);
        result.vendorExtensions.put(X_HAS_RESPONSE_PROPS, result.hasHeaders || result.dataType != null);
        List<CodegenProperty> allResponseProps = new ArrayList<>(result.headers);
        List<CodegenProperty> requiredResponseProps = new ArrayList<>();
        List<CodegenProperty> optionalResponseProps = new ArrayList<>();
        if (result.returnProperty != null) {
            allResponseProps.add(result.returnProperty);
        }
        result.vendorExtensions.put(X_ALL_RESPONSE_PROPS, allResponseProps);
        for (CodegenProperty responseProp : allResponseProps) {
            if (responseProp.required) {
                requiredResponseProps.add(responseProp);
            } else {
                optionalResponseProps.add(responseProp);
            }
        }

        result.vendorExtensions.put(X_REQUIRED_RESPONSE_PROPS, requiredResponseProps);
        result.vendorExtensions.put(X_OPTIONAL_RESPONSE_PROPS, optionalResponseProps);
        result.vendorExtensions.put(X_HAS_REQUIRED_RESPONSE_PROPS, !allResponseProps.equals(optionalResponseProps));
        result.vendorExtensions.put(X_RESULT_BUILDER_NEEDS_CTOR, result.isDefault || !requiredResponseProps.isEmpty());

        return result;
    }

    @Override
    public OperationsMap postProcessOperationsWithModels(OperationsMap objs, List<ModelMap> allModels) {
        /*
        Scan the paths of all the operations, computing the longest common prefix. Then compute and set the path suffix
        for each operation.
         */
        String commonPathPrefixForApi = commonPathPrefix(objs.getOperations().getOperation()
                .stream()
                .map(op -> op.path)
                .map(path -> path.charAt(0) != '/' ? "/" + path : path)
                .toArray(String[]::new));
        String commonPathPrefix;
        if (commonPathPrefixForApi.equals("/")) {
            commonPathPrefix = "/";
            objs.getOperations().put(X_FIXUP_PATH, "TODO - fix path or operation grouping for better performance");
        } else {
            int end = commonPathPrefixForApi.charAt(commonPathPrefixForApi.length() - 1) == '/'
                    ? commonPathPrefixForApi.length() - 1
                    : commonPathPrefixForApi.length();
            commonPathPrefix = commonPathPrefixForApi.substring(0, end);
        }

        objs.getOperations().setPathPrefix(commonPathPrefix);
        List<String> mismatchedOperations = new ArrayList<>();
        objs.getOperations().getOperation().forEach(op -> {
            if (commonPathPrefix.length() > 1) {
                op.vendorExtensions.put(X_PATH_SUFFIX,
                        op.path.equals(commonPathPrefix) ? "/" : op.path.substring(commonPathPrefix.length()));
            } else {
                op.vendorExtensions.put(X_PATH_SUFFIX, op.path);
                mismatchedOperations.add("operation = " + op.operationId + ", path = " + op.path);
            }
        });

        if (!mismatchedOperations.isEmpty()) {
            LOGGER.warn(
                    "Grouping operations by tag has placed operations into API '{}' for which the generated "
                            + "routing works inefficiently because the operation paths do not share a common path prefix. "
                            + "Consider specifying the generator option '"
                            + HELIDON_GROUP_BY
                            + " {}' or changing the tag settings or path in your OpenAPI document for the following operations: {}{}",
                    objs.getOperations().getClassname(),
                    GroupBy.FIRST_PATH_SEGMENT.groupingName,
                    System.lineSeparator(),
                    mismatchedOperations);
        }
        return objs;
    }

    protected String statusDeclaration(String code) {
        return "Status." + knownHttpStatusMap.getOrDefault(code, ".create(" + code + ")");
    }

    /**
     * Remove set of options not currently used by any Helidon generator. Should be
     * called during construction but only on leaf classes.
     */
    protected void removeUnusedOptions() {
        removeCliOptions(SCM_CONNECTION,
                SCM_DEVELOPER_CONNECTION,
                SCM_URL,
                DEVELOPER_NAME,
                DEVELOPER_ORGANIZATION,
                DEVELOPER_ORGANIZATION_URL,
                DEVELOPER_EMAIL,
                PARENT_ARTIFACT_ID,
                PARENT_VERSION,
                PARENT_GROUP_ID,
                DISABLE_HTML_ESCAPING);
    }

    /**
     * Determine whether to generate or overwrite files depending on fullProject property.
     * If property is unspecified, then check if sources are already there and avoid overwriting
     * modifiable files.
     *
     * @param modifiable   list of modifiable files to be processed
     * @param unmodifiable list of unmodifiable files to be processed
     */
    protected void processSupportingFiles(List<SupportingFile> modifiable, List<SupportingFile> unmodifiable) {
        Boolean fullProject = !additionalProperties.containsKey(FULL_PROJECT) ? null :
                Boolean.parseBoolean(additionalProperties.get(FULL_PROJECT).toString());

        if (fullProject == null && !projectFilesExist()) {   // not explicitly set
            supportingFiles.addAll(modifiable);
        } else if (Boolean.TRUE.equals(fullProject)) {       // explicitly set to true
            supportingFiles.addAll(modifiable);
        }
        supportingFiles.addAll(unmodifiable);
    }

    /**
     * Check if project is already generated to determine default for the fullProject
     * flag. Can be overridden in subclasses to strengthen test condition.
     *
     * @return outcome of test
     */
    protected boolean projectFilesExist() {
        return Paths.get(getOutputTestFolder()).toFile().exists();
    }

    protected String rootJavaEEPackage() {
        return rootJavaEEPackage;
    }

    static String commonPathPrefix(String[] paths) {

        if (paths.length == 0) {
            return "/";
        }

        // Start out with the first path as the longest common prefix. The eventual longest common
        // prefix can be no longer than the first path, so as we check other paths we simply
        // revise the number of matching segments we have.
        String[] commonSegments = stripAnyLeadingSlash(paths[0]).split("/");
        int commonSegmentsCount = commonSegments.length;

        // Examine the remaining paths.
        for (int i = 1; i < paths.length; i++) {
            String[] segments = stripAnyLeadingSlash(paths[i]).split("/");

            // Check each segment of this next path against the common segments we have so far.
            int segmentIndex = 0;
            while (segmentIndex < Math.min(commonSegmentsCount, segments.length)
                    && commonSegments[segmentIndex].equals(segments[segmentIndex])) {
                segmentIndex++;
            }
            commonSegmentsCount = segmentIndex;
            if (commonSegmentsCount == 0) {
                break;
            }
        }
        StringJoiner commonPath = new StringJoiner("/", "/", "");
        commonPath.setEmptyValue("/");

        for (int i = 0; i < commonSegmentsCount; i++) {
            commonPath.add(commonSegments[i]);
        }
        return commonPath.toString();
    }

    private static String stripAnyLeadingSlash(String path) {
        return path.startsWith("/") ? path.substring(1) : path;
    }

    /**
     * Prepares a map of predefined HTTP status code constants.
     * <p>
     * Helidon uses its own HTTP status type, and the Helidon code predefines many HTTP status code constants but also allows
     * ad hoc creation of other values based on the numeric status value. It's more efficient at runtime to use a constant
     * if it exists.
     * <p>
     * This method scans a copy of the Helidon Java file which contains the predefined constants and prepares a map
     * from the string representation of the numeric code to the Helidon constant name. This table allows us, when we are
     * generating the Response records for an operation, to use the Helidon predefined constant--if it exists--for the
     * response code declared for an operation in the OpenAPI document.
     * </p>
     *
     * @return prepared map
     */
    private HashMap<String, String> loadKnownHttpStatusMap() {
        try (InputStream is = getClass().getResourceAsStream("/java-helidon/common/Status.java")) {
            if (is == null) {
                throw new RuntimeException("Unable to locate /java-helidon/common/Status.java to discover known HTTP statuses");
            }
            Pattern statusPattern = Pattern.compile("public static final Status (\\w+)\\s*=\\s*new\\s*Status\\((\\d+)",
                    Pattern.MULTILINE);
            return new Scanner(is, StandardCharsets.UTF_8)
                    .findAll(statusPattern)
                    .collect(HashMap::new,
                            (map, match) -> map.put(match.group(2), match.group(1)),
                            Map::putAll);

        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    private void setHelidonVersion(String version) {
        helidonVersion = VersionUtil.instance().chooseVersionBestMatchOrSelf(version);
        setParentVersion(helidonVersion);
        helidonMajorVersion = VersionUtil.majorVersion(helidonVersion);
    }

    private void setEEPackageAndDependencies(String version) {

        rootJavaEEPackage = checkAndSelectRootEEPackage(version);
        additionalProperties.put(MICROPROFILE_ROOT_PACKAGE, rootJavaEEPackage);

        rootJavaEEDepPrefix = checkAndSelectRootEEDepPrefix(version);
        additionalProperties.put(MICROPROFILE_ROOT_DEP_PREFIX, rootJavaEEDepPrefix);

        additionalProperties.put(VALIDATION_ARTIFACT_PREFIX_KEY,
                rootJavaEEDepPrefix.equals(MICROPROFILE_ROOT_PACKAGE_JAVAX)
                        ? VALIDATION_ARTIFACT_PREFIX_JAVAX
                        : VALIDATION_ARTIFACT_PREFIX_JAKARTA);
    }


    private void setMpTestDependency(String version) {
        // The Helidon MP test dependency changed from io.helidon.microprofile.tests:helidon-microprofile-tests-junit5 in 3.x
        // to io.helidon.microprofile.testing:helidon-microprofile-testing-junit5  in 4.x.
        additionalProperties.put(X_USE_MP_TESTING, helidonMajorVersion >= 4);
    }

    private void setJandexPluginDependency(String version) {
        // The Jandex plug-in GAV changed from org.jboss.jandex:jandex-maven-plugin in 3.x to
        // io.smallrye:jandex-maven-plugin in 4.x.
        additionalProperties.put(X_USE_SMALLRYE_JANDEX_PLUGIN, helidonMajorVersion >= 4);
    }

    private void setMediaPackageInfo() {
        additionalProperties.put(X_MEDIA_SUPPORT_PACKAGE_PREFIX,
                (helidonMajorVersion >= 4) ? "io.helidon.http.media" : "io.helidon.media");
        additionalProperties.put(X_MEDIA_COMMON_MEDIA_TYPE_PACKAGE_PREFIX,
                (helidonMajorVersion >= 4) ? "io.helidon.common.media.type" : "io.helidon.common.http");
    }

    private void setUseReactive() {
        useReactive = (helidonMajorVersion < 4);
        additionalProperties.put(X_USE_REACTIVE, useReactive);
    }

    private void setUseOptional() {
        Object useOptionalSetting = additionalProperties.get(X_USE_OPTIONAL);
        if (useOptionalSetting == null) {
            useOptional = true;
        } else if (useOptionalSetting instanceof Boolean) {
            useOptional = (Boolean) useOptionalSetting;
        } else if (useOptionalSetting instanceof String) {
            useOptional = Boolean.parseBoolean((String) useOptionalSetting);
        }
        additionalProperties.put(X_USE_OPTIONAL, useOptional);
    }

    private String checkAndSelectRootEEPackage(String version) {
        String packagePrefixImpliedByVersion = usesJakartaPackages(version)
                ? MICROPROFILE_ROOT_PACKAGE_JAKARTA
                : MICROPROFILE_ROOT_PACKAGE_JAVAX;

        // Make sure any user-specified root EE package is correct for the chosen Helidon version.
        if (additionalProperties.containsKey(MICROPROFILE_ROOT_PACKAGE)) {
            String userRootEEPackage = additionalProperties.get(MICROPROFILE_ROOT_PACKAGE).toString();
            if (!packagePrefixImpliedByVersion.equals(userRootEEPackage)) {
                throw new IllegalArgumentException(
                        String.format(Locale.ROOT,
                                "Helidon version %s uses the %s namespace but options specified '%s'",
                                version,
                                packagePrefixImpliedByVersion,
                                userRootEEPackage));
            }
            return userRootEEPackage;
        }

        // No explicit setting for the root EE package.
        return packagePrefixImpliedByVersion;
    }

    private String checkAndSelectRootEEDepPrefix(String version) {
        String mavenDepPrefixImpliedByVersion = usesJakartaPrefix(version)
                ? MICROPROFILE_ROOT_PACKAGE_JAKARTA
                : MICROPROFILE_ROOT_PACKAGE_JAVAX;

        // Make sure any user-specified prefix is correct for the chosen Helidon version.
        if (additionalProperties.containsKey(MICROPROFILE_ROOT_DEP_PREFIX)) {
            String userMavenDepPrefix = additionalProperties.get(MICROPROFILE_ROOT_DEP_PREFIX).toString();
            if (!mavenDepPrefixImpliedByVersion.equals(userMavenDepPrefix)) {
                throw new IllegalArgumentException(
                        String.format(Locale.ROOT,
                                "Helidon version %s uses the %s prefix for EE dependencies but options specified '%s'",
                                version,
                                mavenDepPrefixImpliedByVersion,
                                userMavenDepPrefix));
            }
            return userMavenDepPrefix;
        }

        // No explicit setting for the dependency prefix.
        return mavenDepPrefixImpliedByVersion;
    }

    private boolean usesJakartaPackages(String version) {
        return !version.startsWith("2.") && !version.startsWith("1.");
    }

    private boolean usesJakartaPrefix(String version) {
        return !version.startsWith("1.");
    }

    protected void removeCliOptions(String... opt) {
        List<String> opts = Arrays.asList(opt);
        Set<CliOption> forRemoval = cliOptions.stream()
                .filter(cliOption -> opts.contains(cliOption.getOpt()))
                .collect(Collectors.toSet());
        forRemoval.forEach(cliOptions::remove);
    }

    protected String apiFolder() {
        return folder(apiPackage);
    }

    protected String modelFolder() {
        return folder(modelPackage);
    }

    private String folder(String packageName) {
        return (sourceFolder + File.separator + packageName).replace(".", java.io.File.separator);
    }

    private String chooseExampleReturnTypeValue(CodegenOperation op) {


        // See DefaultCodegen#handleMethodResponse to see how the various op fields related to the return type are set.
        if (op.returnType == null) {
            return ""; // won't be used anyway in the templates
        }
        if (op.returnContainer != null) {
            return "java.util.Collections.empty" + EXAMPLE_RETURN_VALUES.get(op.returnContainer) + "()";
        }
        switch (op.returnType) {
            case "Integer":
                return "new Integer(0)";

            case "byte[]":
                return "new byte[0]";

            case "Float":
                return "new Float(0.0f)";

            case "boolean":
                return "false";

            case "Long":
                return "new Long(0L)";

            case "Object":
                return "new Object()";

            case "String":
                return "\"\"";

            case "Boolean":
                return "new Boolean(false)";

            case "Double":
                return "new Double(0.0d)";

            default:
                return "null";
        }
    }

    public static class GenericTypeDeclaration {
        @Getter private final String collectionType;
        @Getter private final String baseType;

        public GenericTypeDeclaration(String collectionType, String baseType) {
            this.collectionType = collectionType;
            this.baseType = baseType;
        }

        public boolean isMap() {
            return collectionType.equals("Map");
        }
    }

    /**
     * Captures information about the model types for which we need GenericType declarations.
     */
    public static class GenericTypeDeclarations {

        protected static final String ATTR_NAME = "x-helidon-genericTypeDeclarations";
        protected static final String HAS_ATTR_NAME = "x-helidon-hasGenericTypeDeclarations";

        // Maps collection type (array or map) to an inner map of base type to declaration. This structure makes it easy to
        // avoid duplicate declarations for the same collection and base type.
        private final Map<String, Map<String, GenericTypeDeclaration>> declarations = new HashMap<>();

        protected void register(OperationsMap opns) {
            OperationMap ops = opns.getOperations();
            ops.getOperation().stream()
                    .flatMap(op -> op.allParams.stream())
                    .filter(p -> p.isArray || p.isMap)
                    .forEach(p -> {
                        String collectionType = p.isArray ? "List" : "Map";
                        declarations.computeIfAbsent(collectionType, ct -> new TreeMap<>())
                                .computeIfAbsent(p.baseType, bt -> new GenericTypeDeclaration(collectionType, bt));
                    });
        }

        public List<GenericTypeDeclaration> genericTypeDeclarations() {
            return declarations.values().stream()
                    .flatMap(m -> m.values().stream())
                    .collect(Collectors.toList());
        }
    }

    /**
     * Logic for determining the Helidon versions available for user selection, either from helidon.io
     * or from local preferences or from hard-coded values as a last-chance fallback.
     */
    static class VersionUtil {

        private static final String PREFERENCES_KEY = "/io/helidon/openapigenerators";
        private static final String VERSIONS_KEY = "versions";
        private static final String HELIDON_VERSIONS_URL = "https://helidon.io/cli-data/versions.xml";
        private static final Duration CONNECTION_TIMEOUT = Duration.ofSeconds(10);
        private static final Duration REQUEST_TIMEOUT = Duration.ofSeconds(5);

        private final System.Logger LOGGER = System.getLogger(VersionUtil.class.getName());

        private static final String DEFAULT_VERSIONS = "<data>\n" +
                "  <archetypes>\n" +
                "    <version>2.6.10</version>\n" +
                "    <version>3.2.11</version>\n" +
                "    <version>4.1.4</version>\n" +
                "  </archetypes>\n" +
                "</data>";

        private static VersionUtil INSTANCE = null;
        private static ReentrantLock lock = new ReentrantLock();

        static VersionUtil instance() {
            lock.lock();
            try {
                if (INSTANCE == null) {
                    INSTANCE = new VersionUtil();
                }
            } catch (IOException | BackingStoreException ex) {
                throw new RuntimeException(ex);
            } finally {
                lock.unlock();
            }
            return INSTANCE;
        }

        private final List<String> versions;

        private VersionUtil() throws BackingStoreException, IOException {
            versions = versions();
        }

        String defaultVersion() {
            return versions.get(versions.size() - 1);
        }

        static int majorVersion(String fullVersion) {
            return Integer.parseUnsignedInt(fullVersion.substring(0, fullVersion.indexOf('.')));
        }

        /**
         * Returns the version that is the "closest" match to the requested version expression from among the known releases,
         * where the expression is one of the following:
         * <ul>
         *     <li>a single major version number (e.g., {@code 4})</li>
         *     <li>the full exact version to use</li>
         *     <li>a Maven version range</li>
         * </ul>
         *
         * @param requestedVersion version to search for
         * @return matching version
         */
        String chooseVersion(String requestedVersion) {
            return chooseVersion(requestedVersion, versions);
        }

        /**
         * Returns either the best match version or, if there is none, the requested version itself to allow references to
         * unpublished releases such as snapshots.
         *
         * @param requestedVersion version to search for
         * @return either the best match or, if none, the requested version itself
         */
        String chooseVersionBestMatchOrSelf(String requestedVersion) {
            return chooseVersionBestMatchOrSelf(requestedVersion, versions);
        }

        /**
         * Returns either the best match version or, if there is none, the requested version itself to allow references to
         * unpublished releases such as snapshots.
         *
         * @param requestedVersion  version to search for
         * @param candidateVersions releases to consider
         * @return either the best match or, if none, the requested version itself
         */
        String chooseVersionBestMatchOrSelf(String requestedVersion, List<String> candidateVersions) {
            String bestMatch = chooseVersion(requestedVersion, candidateVersions);
            return bestMatch != null ? bestMatch : requestedVersion;
        }

        /**
         * Returns the version that is the "closest" match to the requested version expression from among the provided
         * releases, where the expression expression is one of the following:
         * <ul>
         *     <li>a single major version number (e.g., {@code 4})</li>
         *     <li>the full exact version to use</li>
         *     <li>a Maven version range</li>
         * </ul>
         *
         * @param requestedVersion  version to search for
         * @param candidateVersions releases to consider
         * @return matching version
         */
        String chooseVersion(String requestedVersion, List<String> candidateVersions) {

            VersionScheme versionScheme = new GenericVersionScheme();

            // If the requested version is a single number then treat it as "highest dot release of this major version".
            // Otherwise, just create a constraint from the value. That also handles the case where the requested version is
            // the complete version.

            VersionConstraint requestedConstraint = constraint(versionScheme, requestedVersion);
            Version bestMatch = null;
            for (String candidate : candidateVersions) {
                Version candidateVersion;

                try {
                    candidateVersion = versionScheme.parseVersion(candidate);
                } catch (InvalidVersionSpecificationException ex) {
                    LOGGER.log(System.Logger.Level.WARNING, "Error parsing candidate version '" + candidate + "'", ex);
                    continue;
                }
                if (requestedConstraint.containsVersion(candidateVersion)) {
                    if (bestMatch == null || bestMatch.compareTo(candidateVersion) <= 0) {
                        bestMatch = candidateVersion;
                    }
                }
            }
            // The user might have requested a legal version we cannot fully validate because of a network outage
            // that prevents us from retrieving the current full list of versions, for example. In such cases return the
            // requested version itself as the best match.
            return bestMatch != null ? bestMatch.toString() : requestedVersion;
        }

        private VersionConstraint constraint(VersionScheme versionScheme, String requestedVersion) {
            try {
                int asSingleNumber = Integer.parseUnsignedInt(requestedVersion);
                try {
                    return versionScheme.parseVersionConstraint(String.format(Locale.getDefault(),
                            "[%s,%d-alpha)",
                            requestedVersion,
                            asSingleNumber + 1));
                } catch (InvalidVersionSpecificationException ex) {
                    throw new RuntimeException("Error preparing constraint for version expression '"
                            + requestedVersion
                            + "' treated as major version " + asSingleNumber,
                            ex);
                }
            } catch (NumberFormatException nfe) {
                try {
                    return versionScheme.parseVersionConstraint(requestedVersion);
                } catch (InvalidVersionSpecificationException ex) {
                    throw new RuntimeException("Error parsing version expression '"
                            + requestedVersion
                            + "' as a version constraint",
                            ex);
                }
            }
        }

        /**
         * Retrieves the list of supported versions from the web site or, failing that, local preferences or, failing that,
         * hard-coded versions.
         *
         * @return list of supported versions
         * @throws IOException in case of error accessing the web site and reading the local file
         */
        private List<String> versions() throws IOException, BackingStoreException {

            HttpClient httpClient = HttpClient.newBuilder()
                    .connectTimeout(CONNECTION_TIMEOUT)
                    .build();
            try {

                HttpRequest req = HttpRequest.newBuilder(URI.create(HELIDON_VERSIONS_URL))
                        .GET()
                        .timeout(REQUEST_TIMEOUT)
                        .build();

                HttpResponse<String> response = httpClient.send(req, HttpResponse.BodyHandlers.ofString());

                Preferences versionPrefs = Preferences.userRoot().node(PREFERENCES_KEY);
                String versions;
                if (response.statusCode() == 200) {
                    versions = response.body();
                    // Save just-retrieved versions for later off-line use.
                    versionPrefs.put(VERSIONS_KEY, versions);
                    versionPrefs.flush();
                    LOGGER.log(System.Logger.Level.DEBUG, "Saved retrieved versions in preferences");
                } else {
                    LOGGER.log(System.Logger.Level.INFO,
                            "Unable to retrieve versions remotely; using local preferences or hard-wired defaults");
                    // Try to get versions from preferences.
                    versions = versionPrefs.get(VERSIONS_KEY, DEFAULT_VERSIONS);
                }

                return extractVersions(versions);

            } catch (IOException | InterruptedException e) {
                // Fallback to use the local versions.xml contents.
                return localDefaultVersions();
            }
        }

        private static List<String> localDefaultVersions() throws IOException {
            URL versionsURL = VersionUtil.class.getResource("versions.xml");
            if (versionsURL == null) {
                return extractVersions(DEFAULT_VERSIONS);
            }

            File versionsFile = new File(versionsURL.getFile());
            return Files.readAllLines(versionsFile.toPath());
        }

        private static List<String> extractVersions(String xmlContent) {
            Pattern versionPattern = Pattern.compile("<version[^>]*>([^>]+)</version>");

            Matcher matcher = versionPattern.matcher(xmlContent);
            List<String> result = new ArrayList<>();
            while (matcher.find()) {
                result.add(matcher.group(1));
            }
            return result;
        }
    }
}
