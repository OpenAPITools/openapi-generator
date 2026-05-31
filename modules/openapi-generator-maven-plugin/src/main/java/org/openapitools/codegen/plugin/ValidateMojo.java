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

package org.openapitools.codegen.plugin;

import static org.apache.commons.lang3.StringUtils.isBlank;
import static org.apache.commons.lang3.StringUtils.isEmpty;
import static org.apache.commons.lang3.StringUtils.isNotEmpty;

import io.swagger.parser.OpenAPIParser;
import io.swagger.v3.core.util.Json;
import io.swagger.v3.core.util.Yaml;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.parser.OpenAPIResolver;
import io.swagger.v3.parser.OpenAPIV3Parser;
import io.swagger.v3.parser.core.models.AuthorizationValue;
import io.swagger.v3.parser.core.models.ParseOptions;
import io.swagger.v3.parser.core.models.SwaggerParseResult;
import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.text.MessageFormat;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Optional;
import java.util.Set;
import lombok.Setter;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoExecution;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.logging.Log;
import org.apache.maven.plugins.annotations.Component;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.project.MavenProject;
import org.apache.maven.project.MavenProjectHelper;
import org.apache.maven.shared.utils.logging.MessageUtils;
import org.jspecify.annotations.NonNull;
import org.openapitools.codegen.auth.AuthParser;
import org.openapitools.codegen.config.GlobalSettings;
import org.openapitools.codegen.config.MergedSpecBuilder;
import org.openapitools.codegen.validation.Invalid;
import org.openapitools.codegen.validation.Severity;
import org.openapitools.codegen.validation.ValidationResult;
import org.openapitools.codegen.validation.ValidationRule;
import org.openapitools.codegen.validations.oas.OpenApiEvaluator;
import org.openapitools.codegen.validations.oas.RuleConfiguration;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.sonatype.plexus.build.incremental.BuildContext;
import org.sonatype.plexus.build.incremental.DefaultBuildContext;

/**
 * Goal that validates the OpenAPI json/yaml definition.
 */
@SuppressWarnings({"unused", "MismatchedQueryAndUpdateOfCollection"})
@Mojo(name = "validate", defaultPhase = LifecyclePhase.VALIDATE, threadSafe = true)
public class ValidateMojo extends AbstractMojo {

  private final Logger LOGGER = LoggerFactory.getLogger(ValidateMojo.class);
  /**
   * Location of the OpenAPI spec(s), as URL(s) and/or file(s). Standard maven style comma separated string list or list
   * of tags inside.
   */
  @Parameter(property = "openapi.generator.maven.plugin.inputSpec")
  protected String[] inputSpec;//the name is intentionally singular to remain backward compatible in the pom xml file.
  /**
   * Local root folder with spec files
   */
  @Parameter(name = "inputSpecRootDirectory", property = "openapi.generator.maven.plugin.inputSpecRootDirectory")
  protected String inputSpecRootDirectory;
  /**
   * The Maven project context.
   */
  @Parameter(defaultValue = "${project}", required = true, readonly = true)
  MavenProject mavenProject;
  /**
   * Maven ProjectHelper used to manage build artifacts.
   */
  @Component
  MavenProjectHelper mavenProjectHelper;

  /**
   * The build context is only avail when running from within eclipse. It is used to update the eclipse-m2e-layer when
   * the plugin is executed inside the IDE.
   */
  @Setter
  @Component
  private BuildContext buildContext = new DefaultBuildContext();
  /**
   * Name of the file that will contain all merged specs
   */
  @Parameter(name = "mergedFileName", property = "openapi.generator.maven.plugin.mergedFileName", defaultValue = "_merged_spec")
  private String mergedFileName;
  /**
   * Name that will appear in the info section of the merged spec
   */
  @Parameter(name = "mergedFileInfoName", property = "openapi.generator.maven.plugin.mergedFileInfoName", defaultValue = "merged spec")
  private String mergedFileInfoName;
  /**
   * Description that will appear in the info section of the merged spec
   */
  @Parameter(name = "mergedFileInfoDescription", property = "openapi.generator.maven.plugin.mergedFileInfoDescription", defaultValue = "merged spec")
  private String mergedFileInfoDescription;
  /**
   * Version that will appear in the info section of the merged spec
   */
  @Parameter(name = "mergedFileInfoVersion", property = "openapi.generator.maven.plugin.mergedFileInfoVersion", defaultValue = "1.0.0")
  private String mergedFileInfoVersion;
  /**
   * The path to the collapsed single-file representation of the OpenAPI spec.
   */
  @Parameter(name = "collapsedSpec", property = "openapi.generator.maven.plugin.collapsedSpec")
  private String collapsedSpec;
  /**
   * Includes the collapsed spec in the Maven artifacts.
   */
  @Parameter(name = "includeCollapsedSpecInArtifacts", property = "openapi.generator.maven.plugin.publishCollapsedSpec", defaultValue = "false")
  private boolean includeCollapsedSpecInArtifacts;
  /**
   * Adds authorization headers when fetching the swagger definitions remotely. " Pass in a URL-encoded string of
   * name:header with a comma separating multiple values
   */
  @Parameter(name = "auth", property = "openapi.generator.maven.plugin.auth")
  private String auth;
  /**
   * To skip spec validation
   */
  @Parameter(name = "skipValidateSpec", property = "openapi.generator.maven.plugin.skipValidateSpec")
  private Boolean skipValidateSpec;
  /**
   * To treat a document strictly against the spec.
   */
  @Parameter(name = "strictSpec", property = "openapi.generator.maven.plugin.strictSpec", defaultValue = "false")
  private Boolean strictSpec;
  /**
   * Skip the execution.
   */
  @Parameter(name = "skip", property = "codegen.skip", defaultValue = "false")
  private Boolean skip;
  @Parameter(defaultValue = "false", property = "openapi.generator.maven.plugin.dryRun")
  private Boolean dryRun;
  @Parameter(defaultValue = "${mojoExecution}", readonly = true)
  private MojoExecution mojo;
  /**
   * The project being built.
   */
  @Parameter(readonly = true, required = true, defaultValue = "${project}")
  MavenProject project;

  boolean hasError;
  boolean hasWarning;

  @Override
  public void execute() throws MojoExecutionException {
    validateInputSpecInput();

    if (shouldWeSkip()) {
      return;
    }

    if(inputSpec != null && inputSpec.length == 1 && inputSpec[0] != null) {
      inputSpec = inputSpec[0].split("\\s*,\\s*");
    }

    mergeInDirectory().ifPresent(mergedSpec -> {
      inputSpec = new String[1];
      inputSpec[0] = mergedSpec;
    });

    try {
      for (String oneInputSpec : inputSpec) {
        if(isEmpty(oneInputSpec)) {
          continue;
        }
        File inputSpecFile = new File(oneInputSpec);
        try {
          if (shouldWeSkipDeltaBuild(inputSpecFile)) {
            continue;
          }
          execute(oneInputSpec);
        } catch (Exception e) {
          if (isNotEmpty(e.getMessage())) {
            getLog().error(e.getMessage());
            if (buildContext != null) {
              buildContext.addMessage(inputSpecFile, 0, 0, e.getMessage(), BuildContext.SEVERITY_WARNING, null);
            }
          }
        }
      }
      if (dryRun) {
        if (hasError || (strictSpec && hasWarning)) {
          getLog().warn("Validation issues detected in dry-run mode. Please review the results.");
          getLog().info("The build will not fail because dryRun is active.");
        }
        return;
      }

      if (hasError || (strictSpec && hasWarning)) {
        throw new Exception();
      }
    } catch (Exception e) {
      throw new MojoExecutionException(
          "Validation has error(s). See above for the details.");
    } finally {
      GlobalSettings.log();
    }
  }

  private void validateInputSpecInput() throws MojoExecutionException {
    boolean isInputSpecEmpty = (inputSpec == null || inputSpec.length == 0 || isBlank(inputSpec[0]));

    if (isInputSpecEmpty && isBlank(inputSpecRootDirectory)) {
      LOGGER.error("inputSpec or inputSpecRootDirectory must be specified");
      throw new MojoExecutionException("inputSpec or inputSpecRootDirectory must be specified");
    }
  }

  private boolean shouldWeSkip() {
    if (Boolean.TRUE.equals(skip) || Boolean.TRUE.equals(skipValidateSpec)) {
      getLog().info("Validation is skipped.");
      return true;
    }
    return false;
  }

  private Optional<String> mergeInDirectory() {
    Optional<String> mergedSpec = Optional.empty();
    if (StringUtils.isNotBlank(inputSpecRootDirectory)) {
      inputSpecRootDirectory = replaceBackslashesToSlashes(inputSpecRootDirectory);

      mergedSpec = Optional.of(new MergedSpecBuilder(inputSpecRootDirectory, mergedFileName,
          mergedFileInfoName, mergedFileInfoDescription, mergedFileInfoVersion, auth)
          .buildMergedSpec());
      LOGGER.info("Merge input spec would be used - {}", mergedSpec.get());
    }
    return mergedSpec;
  }

  private boolean shouldWeSkipDeltaBuild(File inputSpecFile) {
    if (buildContext != null && inputSpec[0] != null) {
      if (buildContext.isIncremental() &&
          inputSpecFile.exists() &&
          !buildContext.hasDelta(inputSpecFile)) {
        getLog().info(
            "Validation is skipped in delta-build because source-json/yaml was not modified.");
        return true;
      }
    }
    return false;
  }

  private void execute(final String oneInputSpec) throws MojoExecutionException {
    String theInputSpec = replaceBackslashesToSlashes(oneInputSpec);

    theInputSpec = collapseSpecIfNeeded(oneInputSpec).orElse(theInputSpec);
    getLog().info(MessageUtils.buffer().a("Validating spec (").strong(theInputSpec).a(")").toString());
    ParseOptions options = new ParseOptions();
    options.setResolve(true);
    final List<AuthorizationValue> authorizationValues = AuthParser.parse(auth);
    SwaggerParseResult result = new OpenAPIParser().readLocation(theInputSpec, authorizationValues, options);

    Set<String> parseErrors = new HashSet<>(result.getMessages());
    Set<Invalid> errors = new HashSet<>();

    result.getMessages().forEach(message -> addParseErrorToErrors(message, errors));

    OpenAPI specification = result.getOpenAPI();

    RuleConfiguration ruleConfiguration = new RuleConfiguration();
    ruleConfiguration.setEnableRecommendations(false);

    OpenApiEvaluator evaluator = new OpenApiEvaluator(ruleConfiguration);
    ValidationResult validationResult = evaluator.validate(specification);

    Set<Invalid> warnings = new HashSet<>(validationResult.getWarnings());
    errors.addAll(validationResult.getErrors());

    logWarnings(warnings);
    logErrors(errors);

    hasError = hasError || !errors.isEmpty();
    hasWarning = hasWarning || !warnings.isEmpty();

    if (errors.isEmpty() && warnings.isEmpty()) {
      getLog().info(MessageUtils.buffer().success("No validation issues detected.").toString());
    }
  }

  /**
   * Replaces all backslashes ('\') in the provided string with forward slashes ('/'). This makes sure the path can be
   * processed correct under Windows OS
   *
   * @param input the input string in which backslashes should be replaced; must not be null
   * @return a new string with all backslashes replaced by forward slashes
   */
  private @NonNull String replaceBackslashesToSlashes(String input) {
    return input.replaceAll("\\\\", "/");
  }

  private Optional<String> collapseSpecIfNeeded(String inputFile) throws MojoExecutionException {
    Optional<String> collapsedSpecString = Optional.empty();
    if (collapsedSpec != null) {
      final var collapsedSpecPath = createCollapsedSpec(inputFile);
      collapsedSpecString = Optional.of(collapsedSpecPath.toString());
      if (includeCollapsedSpecInArtifacts) {
        mavenProjectHelper.attachArtifact(
            mavenProject,
            collapsedSpecPath.toString().toLowerCase(Locale.ROOT).endsWith(".json") ? "json" : "yaml",
            collapsedSpec,
            collapsedSpecPath.toFile());
      }
    }
    return collapsedSpecString;
  }

  private void addParseErrorToErrors(String message, Set<Invalid> errors) {
    ValidationRule failedRule =
        ValidationRule.error("Failed parsing the descriptor.", ignore -> ValidationRule.Fail.empty());
    Invalid invalidParseResult = new Invalid(failedRule, "Descriptor parsing failed.", message);
    errors.add(invalidParseResult);
  }

  private void logWarnings(Set<Invalid> warnings) {
    warnings.forEach(this::logInvalid);
    if (warnings.isEmpty()) {
      getLog().info("Spec has no recommendation(s).");
    } else {
      getLog().warn("Spec has " + warnings.size() + " recommendation(s).");
    }
  }

  private void logErrors(Set<Invalid> errors) {
    errors.forEach(this::logInvalid);
    if (errors.isEmpty()) {
      getLog().info("Spec has no errors.");
    } else {
      getLog().error("Spec has " + errors.size() + " error(s).");
    }
  }

  private Path createCollapsedSpec(String inputFile) throws MojoExecutionException {
    // Merge the OpenAPI spec file.
    final var parseOptions = new ParseOptions();
    parseOptions.setResolve(true);
    final List<AuthorizationValue> authorizationValues = AuthParser.parse(this.auth);

    final var openApiMerged = new OpenAPIResolver(
        new OpenAPIV3Parser().readLocation(inputFile, authorizationValues, parseOptions).getOpenAPI()).resolve();

    // Switch based on JSON or YAML.
    final var extension = inputFile.toLowerCase(Locale.ROOT).endsWith(".json") ? ".json" : ".yaml";
    final var mapper = inputFile.toLowerCase(Locale.ROOT).endsWith(".json") ? Json.mapper() : Yaml.mapper();

    // Write the merged spec to the output file.
    final var collapsedSpecPath =
        Paths.get(project.getBuild().getOutputDirectory(), collapsedSpec + extension).toAbsolutePath();
    try {
      final var openApiString = mapper.writeValueAsString(openApiMerged);
      FileUtils.writeStringToFile(collapsedSpecPath.toFile(), openApiString, StandardCharsets.UTF_8);
    } catch (final IOException e) {
      throw new MojoExecutionException(
          new MessageFormat("Failed to write collapsed spec {0}", Locale.ROOT).format(collapsedSpecPath), e);
    }

    // Return the path to the collapsed spec file.
    return collapsedSpecPath;
  }

  private void logInvalid(Invalid invalid) {
    LOGLEVEL loglevel = invalid.getSeverity() == Severity.ERROR ? LOGLEVEL.ERROR : LOGLEVEL.WARNING;
    loglevel.logColored(getLog(), invalid.getMessage());
    loglevel.log(getLog(), MessageUtils.buffer().format("Based on rule: %s", invalid.getRule()).toString());
    loglevel.log(getLog(), MessageUtils.buffer()
        .format(isNotEmpty(invalid.getDetails()) ? "Details: %s" : "No details available.", invalid.getDetails())
        .toString());
    loglevel.logColored(getLog(), "----------");
  }

  private enum LOGLEVEL {
    WARNING {
      @Override
      void log(Log logger, String message) {
        logger.warn(message);
      }

      @Override
      void logColored(Log logger, String message) {
        log(logger, MessageUtils.buffer().warning(message).toString());
      }
    }, ERROR {
      @Override
      void log(Log logger, String message) {
        logger.error(message);
      }

      @Override
      void logColored(Log logger, String message) {
        log(logger, MessageUtils.buffer().failure(message).toString());
      }
    };

    abstract void log(Log logger, String message);

    abstract void logColored(Log logger, String message);
  }
}
