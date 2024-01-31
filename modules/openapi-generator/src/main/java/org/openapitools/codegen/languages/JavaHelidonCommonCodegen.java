/*
 * Copyright 2022 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 * Copyright (c) 2022 Oracle and/or its affiliates
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

import java.nio.file.Paths;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.servers.Server;
import org.openapitools.codegen.CliOption;
import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.CodegenOperation;
import org.openapitools.codegen.SupportingFile;
import org.openapitools.codegen.languages.features.BeanValidationFeatures;
import org.openapitools.codegen.languages.features.PerformBeanValidationFeatures;

import static org.openapitools.codegen.CodegenConstants.DEVELOPER_EMAIL;
import static org.openapitools.codegen.CodegenConstants.DEVELOPER_NAME;
import static org.openapitools.codegen.CodegenConstants.DEVELOPER_ORGANIZATION;
import static org.openapitools.codegen.CodegenConstants.DEVELOPER_ORGANIZATION_URL;
import static org.openapitools.codegen.CodegenConstants.PARENT_ARTIFACT_ID;
import static org.openapitools.codegen.CodegenConstants.PARENT_GROUP_ID;
import static org.openapitools.codegen.CodegenConstants.PARENT_VERSION;
import static org.openapitools.codegen.CodegenConstants.SCM_CONNECTION;
import static org.openapitools.codegen.CodegenConstants.SCM_DEVELOPER_CONNECTION;
import static org.openapitools.codegen.CodegenConstants.SCM_URL;

public abstract class JavaHelidonCommonCodegen extends AbstractJavaCodegen
        implements BeanValidationFeatures, PerformBeanValidationFeatures {

    static final String HELIDON_MP = "mp";
    static final String HELIDON_SE = "se";

    static final String HELIDON_NIMA = "nima";
    static final String HELIDON_NIMA_ANNOTATIONS = "nima-annotations";

    static final String MICROPROFILE_ROOT_PACKAGE = "rootJavaEEPackage";
    static final String MICROPROFILE_ROOT_DEP_PREFIX = "x-helidon-rootJavaEEDepPrefix";
    static final String X_HAS_RETURN_TYPE = "x-helidon-hasReturnType";
    static final String X_RETURN_TYPE_EXAMPLE_VALUE = "x-helidon-exampleReturnTypeValue";
    static final String MICROPROFILE_ROOT_PACKAGE_DESC = "Root package name for Java EE";
    static final String MICROPROFILE_ROOT_PACKAGE_JAVAX = "javax";
    static final String MICROPROFILE_ROOT_PACKAGE_JAKARTA = "jakarta";
    private static final String VALIDATION_ARTIFACT_PREFIX_KEY = "x-helidon-validationArtifactPrefix";
    private static final String VALIDATION_ARTIFACT_PREFIX_JAVAX = "";
    private static final String VALIDATION_ARTIFACT_PREFIX_JAKARTA = MICROPROFILE_ROOT_PACKAGE_JAKARTA + ".";
    private static final Map<String, String> EXAMPLE_RETURN_VALUES = new HashMap<String, String>();

    // for generated doc
    static final String MICROPROFILE_ROOT_PACKAGE_DEFAULT =
        "Helidon 2.x and earlier: " + MICROPROFILE_ROOT_PACKAGE_JAVAX
        + "; Helidon 3.x and later: " + MICROPROFILE_ROOT_PACKAGE_JAKARTA;

    static final String SERIALIZATION_LIBRARY_JACKSON = "jackson";
    static final String SERIALIZATION_LIBRARY_JSONB = "jsonb";

    public static final String HELIDON_VERSION = "helidonVersion";
    public static final String DEFAULT_HELIDON_VERSION = "3.0.1";
    static final String HELIDON_VERSION_DESC = "Helidon version for generated code";

    static final String FULL_PROJECT = "fullProject";
    static final String FULL_PROJECT_DESC = "If set to true, it will generate all files; if set to false, " +
            "it will only generate API files. If unspecified, the behavior depends on whether a project " +
            "exists or not: if it does not, same as true; if it does, same as false. Note that test files " +
            "are never overwritten.";

    private String helidonVersion;
    private String rootJavaEEPackage;
    private String rootJavaEEDepPrefix;

    public JavaHelidonCommonCodegen() {
        super();

        EXAMPLE_RETURN_VALUES.put("set", "Set");
        EXAMPLE_RETURN_VALUES.put("array", "List");
        EXAMPLE_RETURN_VALUES.put("map", "Map");

        cliOptions.add(new CliOption(HELIDON_VERSION, HELIDON_VERSION_DESC)
                .defaultValue(DEFAULT_HELIDON_VERSION));
        cliOptions.add(new CliOption(MICROPROFILE_ROOT_PACKAGE, MICROPROFILE_ROOT_PACKAGE_DESC)
                .defaultValue(MICROPROFILE_ROOT_PACKAGE_DEFAULT));
        cliOptions.add(new CliOption(FULL_PROJECT, FULL_PROJECT_DESC)
                .defaultValue(""));     // depends on project state
    }

    @Override
    public void processOpts() {
        super.processOpts();

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
            setHelidonVersion(DEFAULT_HELIDON_VERSION);
        }

        additionalProperties.put(HELIDON_VERSION, helidonVersion);
        setEEPackageAndDependencies(helidonVersion);
    }

    @Override
    public CodegenOperation fromOperation(String path, String httpMethod, Operation operation, List<Server> servers) {
        CodegenOperation op = super.fromOperation(path, httpMethod, operation, servers);
        op.vendorExtensions.put(X_HAS_RETURN_TYPE, op.returnType != null && !op.returnType.equals("void"));
        op.vendorExtensions.put(X_RETURN_TYPE_EXAMPLE_VALUE, chooseExampleReturnTypeValue(op));
        return op;
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
     * @param modifiable list of modifiable files to be processed
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

    private void setHelidonVersion(String version) {
        helidonVersion = version;
        setParentVersion(version);
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
}
