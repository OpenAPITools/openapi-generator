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

import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.net.URL;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.time.Duration;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.locks.ReentrantLock;
import java.util.prefs.BackingStoreException;
import java.util.prefs.Preferences;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.servers.Server;
import org.eclipse.aether.util.version.GenericVersionScheme;
import org.eclipse.aether.version.InvalidVersionSpecificationException;
import org.eclipse.aether.version.Version;
import org.eclipse.aether.version.VersionConstraint;
import org.eclipse.aether.version.VersionScheme;
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

    static final String MICROPROFILE_ROOT_PACKAGE = "rootJavaEEPackage";
    static final String MICROPROFILE_ROOT_DEP_PREFIX = "x-helidon-rootJavaEEDepPrefix";
    static final String X_USE_MP_TESTING = "x-helidon-useMpTesting";
    static final String X_USE_SMALLRYE_JANDEX_PLUGIN = "x-helidon-useSmallRyeJandexPlugin";
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

    public static final String DEFAULT_HELIDON_VERSION = "3.0.1";
    public static final String HELIDON_VERSION = "helidonVersion";
    static final String HELIDON_VERSION_DESC = "Helidon complete version identifier or major version number. "
        + "The specified exact Helidon release or, if specified as a major version the latest release of that major version, "
        + " is used in the generated code.";

    static final String FULL_PROJECT = "fullProject";
    static final String FULL_PROJECT_DESC = "If set to true, it will generate all files; if set to false, " +
            "it will only generate API files. If unspecified, the behavior depends on whether a project " +
            "exists or not: if it does not, same as true; if it does, same as false. Note that test files " +
            "are never overwritten.";

    private String helidonVersion;
    private int helidonMajorVersion;

    private String rootJavaEEPackage;
    private String rootJavaEEDepPrefix;
    private String mpTestsGroup;
    private String mpTestsArtifact;
    private String jandexGroup;
    private String jandexArtifact;

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
            setHelidonVersion(VersionUtil.instance().defaultVersion());
        }

        additionalProperties.put(HELIDON_VERSION, helidonVersion);

        setEEPackageAndDependencies(helidonVersion);
        setMpTestDependency(helidonVersion);
        setJandexPluginDependency(helidonVersion);
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
        helidonVersion = VersionUtil.instance().chooseVersion(version);
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

        private static final System.Logger LOGGER = System.getLogger(VersionUtil.class.getName());

        private static final String DEFAULT_VERSIONS = "<data>\n" +
                                                       "  <archetypes>\n" +
                                                       "    <version>2.6.5</version>\n" +
                                                       "    <version>3.2.6</version>\n" +
                                                       "    <version>4.0.5</version>\n" +
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
            return DEFAULT_HELIDON_VERSION;
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
         * Returns the version that is the "closest" match to the requested version expression from among the provided
         * releases, where the expression expression is one of the following:
         * <ul>
         *     <li>a single major version number (e.g., {@code 4})</li>
         *     <li>the full exact version to use</li>
         *     <li>a Maven version range</li>
         * </ul>
         *
         * @param requestedVersion version to search for
         * @param candidateVersions releases to consider
         * @return matching version
         */
        String chooseVersion(String requestedVersion, List<String> candidateVersions)  {

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
            return bestMatch != null ? bestMatch.toString() : null;
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
        private static List<String> versions() throws IOException, BackingStoreException {

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
