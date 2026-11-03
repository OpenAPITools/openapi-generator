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

import com.github.javaparser.StaticJavaParser;
import com.github.javaparser.ast.CompilationUnit;
import com.github.javaparser.ast.body.MethodDeclaration;
import com.github.javaparser.ast.body.Parameter;
import com.github.javaparser.ast.expr.AnnotationExpr;
import com.github.javaparser.ast.type.ClassOrInterfaceType;
import com.github.javaparser.ast.type.Type;
import com.samskivert.mustache.Mustache;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.PathItem;
import io.swagger.v3.oas.models.media.MediaType;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.servers.Server;
import io.swagger.v3.oas.models.security.SecurityRequirement;
import io.swagger.v3.oas.models.security.SecurityScheme;
import io.swagger.v3.oas.models.tags.Tag;
import io.swagger.v3.parser.util.SchemaTypeUtil;
import lombok.Getter;
import lombok.Setter;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.commons.text.StringEscapeUtils;
import org.openapitools.codegen.*;
import org.openapitools.codegen.languages.features.*;
import org.openapitools.codegen.meta.features.*;
import org.openapitools.codegen.model.ModelMap;
import org.openapitools.codegen.model.ModelsMap;
import org.openapitools.codegen.model.OperationMap;
import org.openapitools.codegen.model.OperationsMap;
import org.openapitools.codegen.templating.mustache.SplitStringLambda;
import org.openapitools.codegen.templating.mustache.SpringHttpStatusLambda;
import org.openapitools.codegen.templating.mustache.TrimWhitespaceLambda;
import org.openapitools.codegen.utils.JsonAnnotationPolicyUtils;
import org.openapitools.codegen.utils.JsonIncludePolicy;
import org.openapitools.codegen.utils.ModelUtils;
import org.openapitools.codegen.utils.ProcessUtils;
import org.openapitools.codegen.utils.TriStateBoolean;
import org.openapitools.codegen.utils.URLPathUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.net.URL;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import static org.apache.commons.lang3.StringUtils.isNotEmpty;
import static org.openapitools.codegen.CodegenConstants.*;
import static org.openapitools.codegen.utils.CamelizeOption.LOWERCASE_FIRST_LETTER;
import static org.openapitools.codegen.utils.StringUtils.camelize;

/**
 * <p>Mustache templates are located in
 * {@code src/main/resources/JavaSpring/} (root templates shared across all libraries) and
 * {@code src/main/resources/JavaSpring/libraries/} (library-specific overrides).
 * A library-specific template shadows a root-level template of the same name.
 */
public class SpringCodegen extends AbstractJavaCodegen
        implements BeanValidationFeatures, PerformBeanValidationFeatures, OptionalFeatures, SwaggerUIFeatures {
    private final Logger LOGGER = LoggerFactory.getLogger(SpringCodegen.class);
    public static final String TITLE = "title";
    public static final String SERVER_PORT = "serverPort";
    public static final String CONFIG_PACKAGE = "configPackage";
    public static final String BASE_PACKAGE = "basePackage";
    public static final String USE_FEIGN_CLIENT_URL = "useFeignClientUrl";
    public static final String USE_FEIGN_CLIENT = "useFeignClient";
    public static final String USE_FEIGN_CLIENT_CONTEXT_ID = "useFeignClientContextId";
    public static final String DELEGATE_PATTERN = "delegatePattern";
    public static final String SINGLE_CONTENT_TYPES = "singleContentTypes";
    public static final String VIRTUAL_SERVICE = "virtualService";
    public static final String SKIP_DEFAULT_INTERFACE = "skipDefaultInterface";
    public static final String GENERATE_CONSTRUCTOR_WITH_REQUIRED_ARGS = "generatedConstructorWithRequiredArgs";

    public static final String RESOURCE_FOLDER = "resourceFolder";
    public static final String RESOURCE_FOLDER_DESC = "resource folder for generated resources";

    public static final String ASYNC = "async";
    public static final String REACTIVE = "reactive";
    public static final String SSE = "serverSentEvents";
    public static final String RESPONSE_WRAPPER = "responseWrapper";
    public static final String USE_TAGS = "useTags";
    public static final String SPRING_BOOT = "spring-boot";
    public static final String SPRING_CLOUD_LIBRARY = "spring-cloud";
    public static final String SPRING_HTTP_INTERFACE = "spring-http-interface";
    public static final String USE_HTTP_SERVICE_PROXY_FACTORY_INTERFACES_CONFIGURATOR = "useHttpServiceProxyFactoryInterfacesConfigurator";
    public static final String HTTP_INTERFACES_CONFIGURATOR_DEPENDENCY = "httpInterfacesConfiguratorDependency";
    public static final String API_FIRST = "apiFirst";
    public static final String SPRING_CONTROLLER = "useSpringController";
    public static final String HATEOAS = "hateoas";
    public static final String RETURN_SUCCESS_CODE = "returnSuccessCode";
    public static final String UNHANDLED_EXCEPTION_HANDLING = "unhandledException";
    public static final String USE_RESPONSE_ENTITY = "useResponseEntity";
    public static final String GENERATE_GENERIC_RESPONSE_ENTITY = "generateGenericResponseEntity";
    public static final String USE_ENUM_CASE_INSENSITIVE = "useEnumCaseInsensitive";
    public static final String USE_SPRING_BOOT3 = "useSpringBoot3";
    public static final String USE_SPRING_BOOT4 = "useSpringBoot4";
    public static final String INCLUDE_HTTP_REQUEST_CONTEXT = "includeHttpRequestContext";
    public static final String REQUEST_MAPPING_OPTION = "requestMappingMode";
    public static final String USE_REQUEST_MAPPING_ON_CONTROLLER = "useRequestMappingOnController";
    public static final String USE_REQUEST_MAPPING_ON_INTERFACE = "useRequestMappingOnInterface";
    public static final String USE_SEALED = "useSealed";
    public static final String USE_RECORDS = "useRecords";
    public static final String OPTIONAL_ACCEPT_NULLABLE = "optionalAcceptNullable";
    public static final String USE_SPRING_BUILT_IN_VALIDATION = "useSpringBuiltInValidation";
    public static final String SPRING_API_VERSION = "springApiVersion";
    public static final String USE_JACKSON_3 = "useJackson3";
    public static final String JACKSON2_PACKAGE = "com.fasterxml.jackson";
    public static final String JACKSON3_PACKAGE = "tools.jackson";
    public static final String JACKSON_PACKAGE = "jacksonPackage";
    public static final String ADDITIONAL_NOT_NULL_ANNOTATIONS = "additionalNotNullAnnotations";
    public static final String AUTO_X_SPRING_PAGINATED = "autoXSpringPaginated";
    public static final String GENERATE_SORT_VALIDATION = "generateSortValidation";
    public static final String GENERATE_PAGEABLE_CONSTRAINT_VALIDATION = "generatePageableConstraintValidation";
    public static final String SUBSTITUTE_GENERIC_PAGED_MODEL = "substituteGenericPagedModel";
    public static final String CLIENT_REGISTRATION_ID = "clientRegistrationId";
    public static final String USE_SPRING_SECURITY_PRE_AUTHORIZE = "useSpringSecurityPreAuthorize";
    public static final String SPRING_SECURITY_AUTHORITY_PREFIX = "springSecurityAuthorityPrefix";

    @Getter
    public enum RequestMappingMode {
        api_interface("Generate the @RequestMapping annotation on the generated Api Interface."),
        controller("Generate the @RequestMapping annotation on the generated Api Controller Implementation."),
        none("Do not add a class level @RequestMapping annotation.");

        private final String description;

        RequestMappingMode(String description) {
            this.description = description;
        }
    }

    @Setter protected String title = "OpenAPI Spring";
    @Getter @Setter
    protected String configPackage = "org.openapitools.configuration";
    @Getter @Setter
    protected String basePackage = "org.openapitools";
    @Getter @Setter
    protected String resourceFolder = projectFolder + "/resources";

    @Setter protected boolean interfaceOnly = false;
    @Setter protected boolean useFeignClientUrl = true;
    @Setter protected boolean useFeignClientContextId = true;
    @Setter protected boolean delegatePattern = false;
    protected boolean delegateMethod = false;
    @Setter protected boolean singleContentTypes = false;
    @Setter protected boolean async = false;
    @Setter protected boolean reactive = false;
    @Setter protected boolean sse = false;
    @Setter protected String responseWrapper = null;
    @Setter protected boolean skipDefaultInterface = false;
    @Setter protected boolean useTags = false;
    protected boolean performBeanValidation = false;
    @Setter protected boolean apiFirst = false;
    protected boolean useOptional = false;
    @Setter protected boolean useSealed = false;
    @Setter protected boolean useRecords = false;
    @Getter @Setter protected JsonIncludePolicy optionalNonNullPropertyJsonInclude = JsonIncludePolicy.NON_NULL;
    @Getter @Setter protected JsonAnnotationPolicyUtils.JsonSetterNullsMode optionalNonNullPropertyJsonSetterNulls = null;
    @Getter @Setter protected TriStateBoolean generateJsonIncludeAnnotations = TriStateBoolean.UNSET;
    @Getter @Setter protected TriStateBoolean generateJsonSetterNullsAnnotations = TriStateBoolean.UNSET;
    @Setter protected boolean virtualService = false;
    @Setter protected boolean hateoas = false;
    @Setter protected boolean returnSuccessCode = false;
    @Getter @Setter
    protected boolean unhandledException = false;
    @Setter protected boolean useSpringController = false;
    protected boolean useSwaggerUI = true;
    @Setter protected boolean useResponseEntity = true;
    @Setter protected boolean generateGenericResponseEntity = false;
    @Setter protected boolean useEnumCaseInsensitive = false;
    @Getter @Setter
    protected boolean useSpringBoot3 = true;
    @Getter @Setter
    protected boolean useSpringBoot4 = false;
    @Getter @Setter
    private Boolean includeHttpRequestContext = null;
    @Getter
    private final boolean defaultIncludeHttpRequestContextForReactive = true;
    @Getter
    private final boolean defaultIncludeHttpRequestContextForBlocking = false;
    protected boolean generatedConstructorWithRequiredArgs = true;
    @Getter @Setter
    protected RequestMappingMode requestMappingMode = RequestMappingMode.controller;
    @Getter @Setter
    protected boolean optionalAcceptNullable = true;
    @Getter @Setter
    protected boolean useSpringBuiltInValidation = false;
    @Getter @Setter
    protected boolean useJackson3 = false;
    @Getter @Setter
    protected boolean additionalNotNullAnnotations = false;
    @Setter boolean useHttpServiceProxyFactoryInterfacesConfigurator = false;
    @Getter protected String autoXSpringPaginated = SpringPageableScanUtils.AUTO_PAGINATION_MODE_NONE;
    @Getter private SpringPageableScanUtils.AutoPaginationMode autoXSpringPaginatedMode = SpringPageableScanUtils.AutoPaginationMode.NONE;
    /**
     * Configures automatic Spring Pageable detection using a canonical mode or legacy boolean alias.
     *
     * @param autoXSpringPaginated the configured mode
     */
    public void setAutoXSpringPaginated(String autoXSpringPaginated) {
        autoXSpringPaginatedMode = SpringPageableScanUtils.resolveAutoPaginationMode(autoXSpringPaginated);
        this.autoXSpringPaginated = autoXSpringPaginatedMode.getCanonicalValue();
    }

    /**
     * @deprecated Use {@link #setAutoXSpringPaginated(String)} with {@code page-size-sort} or
     * {@code none}.
     */
    @Deprecated
    public void setAutoXSpringPaginated(boolean autoXSpringPaginated) {
        setAutoXSpringPaginated(Boolean.toString(autoXSpringPaginated));
    }

    @Setter protected boolean generateSortValidation = false;
    @Setter protected boolean generatePageableConstraintValidation = false;
    @Setter protected boolean substituteGenericPagedModel = false;
    @Getter @Setter
    protected String clientRegistrationId = null;
    @Setter protected boolean useSpringSecurityPreAuthorize = false;
    @Getter @Setter
    protected String springSecurityAuthorityPrefix = "SCOPE_";
    @Setter protected boolean useEnumValueInterface = false;
    private String valuedEnumClassName = "ValuedEnum";

    // Map from schema name to detected paged-model info (populated when substituteGenericPagedModel=true)
    private Map<String, PagedModelScanUtils.DetectedPagedModel> pagedModelRegistry = new HashMap<>();
    // Simple class name of the PagedModel substitute (derived from importMapping; defaults to "PagedModel")
    private String pagedModelClassName = "PagedModel";

    // Holds scan results for Spring Pageable features (populated during preprocessOpenAPI)
    private final SpringPageableScanUtils pageableUtils = new SpringPageableScanUtils();

    public SpringCodegen() {
        super();

        modifyFeatureSet(features -> features.includeDocumentationFeatures(DocumentationFeature.Readme)
                .wireFormatFeatures(EnumSet.of(WireFormatFeature.JSON, WireFormatFeature.XML, WireFormatFeature.Custom))
                .securityFeatures(EnumSet.of(SecurityFeature.OAuth2_Implicit, SecurityFeature.OAuth2_AuthorizationCode,
                        SecurityFeature.OAuth2_ClientCredentials, SecurityFeature.OAuth2_Password,
                        SecurityFeature.ApiKey, SecurityFeature.BasicAuth))
                .excludeGlobalFeatures(GlobalFeature.Callbacks, GlobalFeature.LinkObjects,
                        GlobalFeature.ParameterStyling)
                .includeGlobalFeatures(GlobalFeature.XMLStructureDefinitions)
                .includeSchemaSupportFeatures(SchemaSupportFeature.Polymorphism)
                .excludeParameterFeatures(ParameterFeature.Cookie));

        useBeanValidation = true;
        outputFolder = "generated-code/javaSpring";
        embeddedTemplateDir = templateDir = "JavaSpring";
        apiPackage = "org.openapitools.api";
        modelPackage = "org.openapitools.model";
        invokerPackage = "org.openapitools.api";
        artifactId = "openapi-spring";

        // clioOptions default redefinition need to be updated
        updateOption(CodegenConstants.INVOKER_PACKAGE, this.getInvokerPackage());
        updateOption(CodegenConstants.ARTIFACT_ID, this.getArtifactId());
        updateOption(CodegenConstants.API_PACKAGE, apiPackage);
        updateOption(CodegenConstants.MODEL_PACKAGE, modelPackage);

        // Enable discriminator-based oneOf interface generation by default
        useOneOfInterfaces = true;
        legacyDiscriminatorBehavior = false;
        updateOption(USE_ONE_OF_INTERFACES, String.valueOf(useOneOfInterfaces));
        updateOption(CodegenConstants.LEGACY_DISCRIMINATOR_BEHAVIOR, String.valueOf(legacyDiscriminatorBehavior));

        apiTestTemplateFiles.clear(); // TODO: add test template

        // spring uses the jackson lib
        jackson = true;

        cliOptions.add(new CliOption(TITLE, "server title name or client service name").defaultValue(title));
        cliOptions.add(new CliOption(CONFIG_PACKAGE, "configuration package for generated code")
                .defaultValue(this.getConfigPackage()));
        cliOptions.add(new CliOption(BASE_PACKAGE, "base package (invokerPackage) for generated code")
                .defaultValue(this.getBasePackage()));
        cliOptions.add(CliOption.newBoolean(INTERFACE_ONLY, INTERFACE_ONLY_DESC, interfaceOnly));
        cliOptions.add(CliOption.newBoolean(USE_FEIGN_CLIENT_URL,
                "Whether to generate Feign client with url parameter.", useFeignClientUrl));
        cliOptions.add(CliOption.newBoolean(USE_FEIGN_CLIENT_CONTEXT_ID,
                "Whether to generate Feign client with contextId parameter.", useFeignClientContextId));
        cliOptions.add(CliOption.newBoolean(DELEGATE_PATTERN,
                "Whether to generate the server files using the delegate pattern", delegatePattern));
        cliOptions.add(CliOption.newBoolean(SINGLE_CONTENT_TYPES,
                "Whether to select only one produces/consumes content-type by operation.", singleContentTypes));
        cliOptions.add(CliOption.newBoolean(SKIP_DEFAULT_INTERFACE,
                "Whether to skip generation of default implementations for java8 interfaces", skipDefaultInterface));
        cliOptions.add(CliOption.newBoolean(ASYNC, "use async Callable controllers", async));
        cliOptions.add(CliOption.newBoolean(REACTIVE, "wrap responses in Mono/Flux Reactor types (spring-boot only)",
                reactive));
        cliOptions.add(new CliOption(RESPONSE_WRAPPER,
                "wrap the responses in given type (Future, Callable, CompletableFuture,ListenableFuture, DeferredResult, RxObservable, RxSingle or fully qualified type)"));
        cliOptions.add(CliOption.newBoolean(VIRTUAL_SERVICE,
                "Generates the virtual service. For more details refer - https://github.com/virtualansoftware/virtualan/wiki"));
        cliOptions.add(
                CliOption.newBoolean(USE_TAGS, "use tags for creating interface and controller classnames", useTags));
        cliOptions
                .add(CliOption.newBoolean(USE_BEANVALIDATION, "Use BeanValidation API annotations", useBeanValidation));
        cliOptions.add(CliOption.newBoolean(USE_SPRING_BUILT_IN_VALIDATION,
                "Disable `@Validated` at the class level when using built-in validation.",
                useSpringBuiltInValidation));
        cliOptions.add(CliOption.newBoolean(PERFORM_BEANVALIDATION,
                "Use Bean Validation Impl. to perform BeanValidation", performBeanValidation));
        cliOptions.add(CliOption.newBoolean(USE_SEALED,
                "Whether to generate sealed model interfaces and classes"));
        cliOptions.add(CliOption.newBoolean(USE_RECORDS,
                "Generate models as Java records where possible (requires Spring Boot 3 or 4). A model that cannot "
                        + "be a record yet stays a class, and the reason is logged: inheritance, additionalProperties, "
                        + "an anyOf or oneOf composition, a byte[] property, more than 250 properties, the FAIL null policy, a "
                        + "property name a record cannot have, or a request-parameter model that Spring's data binder cannot fill as a record. "
                        + "Not supported with hateoas, Lombok annotations, withXml and useOptional.", useRecords));

        CliOption optionalNonNullPropertyJsonIncludeOpt = CliOption.newString(CodegenConstants.OPTIONAL_NON_NULL_PROPERTY_JSON_INCLUDE,
                JsonAnnotationPolicyUtils.OPTIONAL_NON_NULL_PROPERTY_JSON_INCLUDE_DESC);
        for (JsonIncludePolicy policy : JsonIncludePolicy.OPTIONAL_NON_NULL_POLICIES) {
            optionalNonNullPropertyJsonIncludeOpt.addEnum(policy.name(), policy.getDescription());
        }
        optionalNonNullPropertyJsonIncludeOpt.setDefault(optionalNonNullPropertyJsonInclude.name());
        cliOptions.add(optionalNonNullPropertyJsonIncludeOpt);

        CliOption optionalNonNullPropertyJsonSetterNullsOpt = CliOption.newString(CodegenConstants.OPTIONAL_NON_NULL_PROPERTY_JSON_SETTER_NULLS,
                JsonAnnotationPolicyUtils.OPTIONAL_NON_NULL_PROPERTY_JSON_SETTER_NULLS_DESC);
        optionalNonNullPropertyJsonSetterNullsOpt.addEnum(JsonAnnotationPolicyUtils.JsonSetterNullsMode.SKIP.name(),
                "Emit @JsonSetter(nulls = Nulls.SKIP): silently ignore an explicit JSON null, keeping the field's default.");
        optionalNonNullPropertyJsonSetterNullsOpt.addEnum(JsonAnnotationPolicyUtils.JsonSetterNullsMode.FAIL.name(),
                "Emit @JsonSetter(nulls = Nulls.FAIL): reject an explicit JSON null.");
        cliOptions.add(optionalNonNullPropertyJsonSetterNullsOpt);

        cliOptions.add(CliOption.newBoolean(CodegenConstants.GENERATE_JSON_INCLUDE_ANNOTATIONS,
                JsonAnnotationPolicyUtils.GENERATE_JSON_INCLUDE_ANNOTATIONS_DESC, false));
        cliOptions.add(CliOption.newBoolean(CodegenConstants.GENERATE_JSON_SETTER_NULLS_ANNOTATIONS,
                "Whether to generate @JsonSetter(nulls = ...) annotations on optional non-nullable model properties. "
                        + "When true, emits @JsonSetter so an explicit null in the payload does not overwrite the field. "
                        + "When false, none are generated and deserialization null-handling defers to the global ObjectMapper. "
                        + "When left unset it defaults to false (7.23.0-equivalent output) and logs a warning; set it "
                        + "explicitly to silence the warning.", false));
        cliOptions.add(CliOption.newBoolean(API_FIRST,
                "Generate the API from the OAI spec at server compile time (API first approach)", apiFirst));
        cliOptions
                .add(CliOption.newBoolean(USE_OPTIONAL, "Use Optional container for optional parameters", useOptional));
        cliOptions.add(
                CliOption.newBoolean(HATEOAS, "Use Spring HATEOAS library to allow adding HATEOAS links", hateoas));
        cliOptions
                .add(CliOption.newBoolean(RETURN_SUCCESS_CODE, "Generated server returns 2xx code", returnSuccessCode));
        cliOptions.add(CliOption.newBoolean(SPRING_CONTROLLER, "Annotate the generated API as a Spring Controller", useSpringController));
        cliOptions.add(CliOption.newString(X_IMPLEMENTS_SKIP, "Ability to choose interfaces that should NOT be implemented in the models despite their presence in vendor extension `x-implements`. Takes a list of fully qualified interface names. Example: yaml `xImplementsSkip: [com.some.pack.WithPhotoUrls]` skips implementing the interface `com.some.pack.WithPhotoUrls` in any schema", "empty list"));
        cliOptions.add(CliOption.newString(SCHEMA_IMPLEMENTS, "Ability to supply interfaces per schema that should be implemented (serves similar purpose as vendor extension `x-implements`, but is fully decoupled from the api spec). Example: yaml `schemaImplements: {Pet: com.some.pack.WithId, Category: [com.some.pack.CategoryInterface], Dog: [com.some.pack.Canine, com.some.pack.OtherInterface]}` implements interfaces in schemas `Pet` (interface `com.some.pack.WithId`), `Category` (interface `com.some.pack.CategoryInterface`), `Dog`(interfaces `com.some.pack.Canine`, `com.some.pack.OtherInterface`)", "empty map"));

        CliOption requestMappingOpt = new CliOption(REQUEST_MAPPING_OPTION,
                "Where to generate the class level @RequestMapping annotation.")
                .defaultValue(requestMappingMode.name());
        for (RequestMappingMode mode : RequestMappingMode.values()) {
            requestMappingOpt.addEnum(mode.name(), mode.getDescription());
        }
        cliOptions.add(requestMappingOpt);

        cliOptions.add(CliOption.newBoolean(UNHANDLED_EXCEPTION_HANDLING,
                "Declare operation methods to throw a generic exception and allow unhandled exceptions (useful for Spring `@ControllerAdvice` directives).",
                unhandledException));
        cliOptions.add(CliOption.newBoolean(USE_SWAGGER_UI,
                "Open the OpenApi specification in swagger-ui. Will also import and configure needed dependencies",
                useSwaggerUI));
        cliOptions.add(CliOption.newBoolean(USE_RESPONSE_ENTITY,
                "Use the `ResponseEntity` type to wrap return values of generated API methods. "
                        + "If disabled, method are annotated using a `@ResponseStatus` annotation, which has the status of the first response declared in the Api definition",
                useResponseEntity));
        cliOptions.add(CliOption.newBoolean(GENERATE_GENERIC_RESPONSE_ENTITY,
                "Use a generic type for the `ResponseEntity` wrapping return values of generated API methods. "
                        + "If enabled, method are generated with return type ResponseEntity<?>",
                generateGenericResponseEntity));
        cliOptions.add(CliOption.newBoolean(USE_ENUM_CASE_INSENSITIVE,
                "Use `equalsIgnoreCase` when String for enum comparison",
                useEnumCaseInsensitive));
        cliOptions.add(CliOption.newBoolean(USE_SPRING_BOOT3,
                "Generate code and provide dependencies for use with Spring Boot ≥ 3 (use jakarta instead of javax in imports). Enabling this option will also enable `useJakartaEe`.",
                useSpringBoot3));
        cliOptions.add(CliOption.newBoolean(USE_SPRING_BOOT4,
                "Generate code and provide dependencies for use with Spring Boot 4.x. (Use jakarta instead of javax in imports). Enabling this option will also enable `useJakartaEe`.",
                useSpringBoot4));
        cliOptions.add(CliOption.newBoolean(USE_JACKSON_3, "Set it in order to use jackson 3 dependencies (only allowed when `" + USE_SPRING_BOOT4 + "` is set).", useJackson3));
        cliOptions.add(new CliOption(INCLUDE_HTTP_REQUEST_CONTEXT,
                "Whether to include HttpServletRequest (blocking) or ServerWebExchange (reactive) as additional parameter in generated methods. Defaults to 'true' for reactive and 'false' for blocking.",
                SchemaTypeUtil.BOOLEAN_TYPE).defaultValue("true (reactive) / false (blocking)"));
        cliOptions.add(CliOption.newBoolean(GENERATE_CONSTRUCTOR_WITH_REQUIRED_ARGS,
                "Whether to generate constructors with required args for models",
                generatedConstructorWithRequiredArgs));
        cliOptions.add(new CliOption(RESOURCE_FOLDER, RESOURCE_FOLDER_DESC).defaultValue(this.getResourceFolder()));
        cliOptions.add(CliOption.newBoolean(OPTIONAL_ACCEPT_NULLABLE,
                "Use `ofNullable` instead of just `of` to accept null values when using Optional.",
                optionalAcceptNullable));

        cliOptions.add(CliOption.newBoolean(USE_DEDUCTION_FOR_ONE_OF_INTERFACES, USE_DEDUCTION_FOR_ONE_OF_INTERFACES_DESC, useDeductionForOneOfInterfaces));
        cliOptions.add(new CliOption(TYPE_INFO_DEFAULT_IMPLS, TYPE_INFO_DEFAULT_IMPLS_DESC).defaultValue("empty map"));
        cliOptions.add(CliOption.newString(SPRING_API_VERSION, "Value for 'version' attribute in @RequestMapping (for Spring 7 and above)."));
        cliOptions.add(CliOption.newString(USE_HTTP_SERVICE_PROXY_FACTORY_INTERFACES_CONFIGURATOR,
            "Generate HttpInterfacesAbstractConfigurator based on an HttpServiceProxyFactory instance (as opposed to a WebClient instance, when disabled) for generating Spring HTTP interfaces.")
            .defaultValue("false")
        );
        cliOptions.add(CliOption.newBoolean(USE_JSPECIFY, "Use Jspecify for null checks", useJspecify));
        cliOptions.add(CliOption.newString(CLIENT_REGISTRATION_ID, "Client registration ID for OAuth2 in Spring HTTP Interface (@ClientRegistrationId annotation). Requires library=spring-http-interface and useSpringBoot4=true (Spring Security 7)."));
        cliOptions.add(CliOption.newBoolean(USE_SPRING_SECURITY_PRE_AUTHORIZE,
                "Generate Spring Security @PreAuthorize annotations from OAuth2/OpenID Connect security scopes.",
                useSpringSecurityPreAuthorize));
        cliOptions.add(CliOption.newString(SPRING_SECURITY_AUTHORITY_PREFIX,
                "Prefix added to OAuth2/OpenID Connect scopes when generating Spring Security authorities.",
                springSecurityAuthorityPrefix));
        supportedLibraries.put(SPRING_BOOT, "Spring-boot Server application.");
        supportedLibraries.put(SPRING_CLOUD_LIBRARY,
                "Spring-Cloud-Feign client with Spring-Boot auto-configured settings.");
        supportedLibraries.put(SPRING_HTTP_INTERFACE, "Spring 6 HTTP interfaces (testing). Requires Spring Boot 3 or 4.");
        setLibrary(SPRING_BOOT);
        final CliOption library = new CliOption(CodegenConstants.LIBRARY, CodegenConstants.LIBRARY_DESC)
                .defaultValue(SPRING_BOOT);
        library.setEnum(supportedLibraries);
        cliOptions.add(library);

        cliOptions.add(CliOption.newBoolean(ADDITIONAL_NOT_NULL_ANNOTATIONS,
                "Add @NotNull to path variables (required by default) and requestBody.",
                additionalNotNullAnnotations));
        final CliOption autoXSpringPaginatedOption = new CliOption(AUTO_X_SPRING_PAGINATED,
                "Automatically add x-spring-paginated to operations that have the given set of pagination query "
                + "parameters. 'page-size-sort' requires 'page', 'size', and 'sort'; 'page-size' requires only "
                + "'page' and 'size' (sort may be absent). Operations with x-spring-paginated explicitly set to "
                + "false will not be auto-detected. Only applies when library is spring-boot or spring-cloud. "
                + "The legacy values 'true' (alias for 'page-size-sort') and 'false' (alias for 'none') are "
                + "deprecated and will be removed in a future release.")
                .defaultValue(autoXSpringPaginated);
        autoXSpringPaginatedOption.setEnum(SpringPageableScanUtils.getAutoPaginationModeEnumValues());
        cliOptions.add(autoXSpringPaginatedOption);
        cliOptions.add(CliOption.newBoolean(GENERATE_SORT_VALIDATION,
                "Generate a @ValidSort annotation and SortValidator class, and apply @ValidSort to "
                + "the injected Pageable parameter of operations whose 'sort' parameter has enum values. "
                + "The annotation validates that sort values in the Pageable object match the allowed enum values from the spec. "
                + "Requires useBeanValidation=true and library is spring-boot or spring-cloud.",
                generateSortValidation));
        cliOptions.add(CliOption.newBoolean(GENERATE_PAGEABLE_CONSTRAINT_VALIDATION,
                "Generate a @ValidPageable annotation and PageableConstraintValidator class, and apply @ValidPageable to "
                + "the injected Pageable parameter of operations whose 'page' or 'size' parameter specifies a maximum constraint. "
                + "The annotation enforces those constraints on the Pageable object that replaces the individual page/size query parameters. "
                + "Requires useBeanValidation=true and library is spring-boot or spring-cloud.",
                generatePageableConstraintValidation));
        cliOptions.add(CliOption.newBoolean(SUBSTITUTE_GENERIC_PAGED_MODEL,
                "Detect schemas that represent paginated responses (an object with a 'content' array property and a 'page' "
                + "pagination-metadata property) and replace their generated references with "
                + "PagedModel<T>. By default this uses a generated type in the config package (default 'org.openapitools.configuration'), but `importMappings.PagedModel` can override it to a custom/FQCN-mapped type. The detected page schemas and the pagination metadata "
                + "schema are suppressed from code generation.",
                substituteGenericPagedModel));
        cliOptions.add(CliOption.newBoolean(CodegenConstants.USE_ENUM_VALUE_INTERFACE,
                CodegenConstants.USE_ENUM_VALUE_INTERFACE_DESC,
                useEnumValueInterface));

    }

    @Override
    public CodegenType getTag() {
        return CodegenType.SERVER;
    }

    @Override
    public String getName() {
        return "spring";
    }

    @Override
    public String getHelp() {
        return "Generates a Java SpringBoot Server application using the SpringDoc integration.";
    }

    @Override
    public DocumentationProvider defaultDocumentationProvider() {
        return isLibrary(SPRING_HTTP_INTERFACE) ? null : DocumentationProvider.SPRINGDOC;
    }

    @Override
    public List<DocumentationProvider> supportedDocumentationProvider() {
        List<DocumentationProvider> supportedProviders = new ArrayList<>();
        supportedProviders.add(DocumentationProvider.NONE);
        supportedProviders.add(DocumentationProvider.SOURCE);
        supportedProviders.add(DocumentationProvider.SPRINGDOC);
        return supportedProviders;
    }

    @Override
    public List<AnnotationLibrary> supportedAnnotationLibraries() {
        List<AnnotationLibrary> supportedLibraries = new ArrayList<>();
        supportedLibraries.add(AnnotationLibrary.NONE);
        supportedLibraries.add(AnnotationLibrary.SWAGGER1);
        supportedLibraries.add(AnnotationLibrary.SWAGGER2);
        return supportedLibraries;
    }

    /**
     * Whether the selected {@link DocumentationProviderFeatures.DocumentationProvider} requires us to bootstrap and
     * configure swagger-ui by ourselves. Springdoc, for example ships its own swagger-ui integration.
     *
     * @return true if the selected DocumentationProvider requires us to bootstrap swagger-ui.
     */
    private boolean selectedDocumentationProviderRequiresSwaggerUiBootstrap() {
        return getDocumentationProvider().equals(DocumentationProvider.SOURCE);
    }

    @Override
    public void processOpts() {
        final List<Pair<String, String>> configOptions = additionalProperties.entrySet().stream()
                .filter(e -> !Arrays.asList(API_FIRST, "hideGenerationTimestamp").contains(e.getKey()))
                .filter(e -> cliOptions.stream().map(CliOption::getOpt).anyMatch(opt -> opt.equals(e.getKey())))
                .map(e -> Pair.of(e.getKey(), e.getValue().toString())).collect(Collectors.toList());
        additionalProperties.put("configOptions", configOptions);

        // TODO remove "file" from reserved word list as feign client doesn't support using `baseName`
        // as the parameter name yet
        reservedWords.remove("file");

        // Process java8 option before common java ones to change the default
        // dateLibrary to java8.
        LOGGER.info("----------------------------------");
        if (!additionalProperties.containsKey(DATE_LIBRARY)) {
            setDateLibrary("java8");
        }

        if (!additionalProperties.containsKey(BASE_PACKAGE)
                && additionalProperties.containsKey(CodegenConstants.INVOKER_PACKAGE)) {
            // set invokerPackage as basePackage:
            this.setBasePackage((String) additionalProperties.get(CodegenConstants.INVOKER_PACKAGE));
            LOGGER.info("Set base package to invoker package ({})", basePackage);
        }

        convertPropertyToTypeAndWriteBack(REQUEST_MAPPING_OPTION, RequestMappingMode::valueOf, this::setRequestMappingMode);

        // Please refrain from updating values of Config Options after super.ProcessOpts() is called
        super.processOpts();

        if (additionalProperties.containsKey(TYPE_INFO_DEFAULT_IMPLS)) {
            typeInfoDefaultImpls.putAll(getPropertyAsStringMap(TYPE_INFO_DEFAULT_IMPLS));
        }

        if (SPRING_HTTP_INTERFACE.equals(library)) {
            if (documentationProvider != null) {
                additionalProperties.remove(documentationProvider.getPropertyName());
            }
            if (annotationLibrary != null) {
                additionalProperties.remove(annotationLibrary.getPropertyName());
            }
            documentationProvider = DocumentationProvider.NONE;
            annotationLibrary = AnnotationLibrary.NONE;
            useJakartaEe = true;
            if(additionalProperties.containsKey(USE_BEANVALIDATION)) {
                useBeanValidation = convertPropertyToBoolean(USE_BEANVALIDATION);
            } else {
                //default to false if not specified
                useBeanValidation = false;
            }
            if(additionalProperties.containsKey(PERFORM_BEANVALIDATION)) {
                performBeanValidation = convertPropertyToBoolean(PERFORM_BEANVALIDATION);
            } else {
                //default to false if not specified
                performBeanValidation = false;
            }

            additionalProperties.put(USE_JAKARTA_EE, useJakartaEe);
            additionalProperties.put(USE_BEANVALIDATION, useBeanValidation);
            additionalProperties.put(PERFORM_BEANVALIDATION, performBeanValidation);
            additionalProperties.put(DOCUMENTATION_PROVIDER, documentationProvider.toCliOptValue());
            additionalProperties.put(documentationProvider.getPropertyName(), true);
            additionalProperties.put(ANNOTATION_LIBRARY, annotationLibrary.toCliOptValue());
            additionalProperties.put(annotationLibrary.getPropertyName(), true);

            applyJakartaPackage();

            LOGGER.warn("For Spring HTTP Interface following options are disabled: documentProvider, annotationLibrary. useJakartaEe defaulted to 'true'. useBeanValidation and performBeanValidation are supported.");
        }

        // clear model and api doc template as this codegen
        // does not support auto-generated markdown doc at the moment
        // TODO: add doc templates
        modelDocTemplateFiles.remove("model_doc.mustache");
        apiDocTemplateFiles.remove("api_doc.mustache");

        convertPropertyToStringAndWriteBack(TITLE, this::setTitle);
        convertPropertyToStringAndWriteBack(CONFIG_PACKAGE, this::setConfigPackage);
        convertPropertyToStringAndWriteBack(BASE_PACKAGE, this::setBasePackage);
        convertPropertyToBooleanAndWriteBack(VIRTUAL_SERVICE, this::setVirtualService);
        convertPropertyToBooleanAndWriteBack(INTERFACE_ONLY, this::setInterfaceOnly);
        convertPropertyToBooleanAndWriteBack(USE_FEIGN_CLIENT_URL, this::setUseFeignClientUrl);
        convertPropertyToBooleanAndWriteBack(USE_FEIGN_CLIENT_CONTEXT_ID, this::setUseFeignClientContextId);
        convertPropertyToBooleanAndWriteBack(DELEGATE_PATTERN, this::setDelegatePattern);
        convertPropertyToBooleanAndWriteBack(SINGLE_CONTENT_TYPES, this::setSingleContentTypes);
        convertPropertyToBooleanAndWriteBack(SKIP_DEFAULT_INTERFACE, this::setSkipDefaultInterface);
        convertPropertyToBooleanAndWriteBack(ASYNC, this::setAsync);
        if (additionalProperties.containsKey(REACTIVE)) {
            if (SPRING_CLOUD_LIBRARY.equals(library)) {
                throw new IllegalArgumentException("Currently, reactive option doesn't supported by Spring Cloud");
            }
            convertPropertyToBooleanAndWriteBack(REACTIVE, this::setReactive);
            convertPropertyToBooleanAndWriteBack(SSE, this::setSse);
        }
        if (additionalProperties.containsKey(INCLUDE_HTTP_REQUEST_CONTEXT)) {
            convertPropertyToBooleanAndWriteBack(INCLUDE_HTTP_REQUEST_CONTEXT, this::setIncludeHttpRequestContext);
        }
        //set default value for includeHttpRequestContext based on reactive/blocking
        if (includeHttpRequestContext == null) {
            if (this.reactive) {
                //default to true for reactive
                this.setIncludeHttpRequestContext(this.isDefaultIncludeHttpRequestContextForReactive());
                LOGGER.info("Defaulting {} to '{}' for reactive", INCLUDE_HTTP_REQUEST_CONTEXT, this.isDefaultIncludeHttpRequestContextForReactive());
            } else {
                //default to false for blocking
                this.setIncludeHttpRequestContext(this.isDefaultIncludeHttpRequestContextForBlocking());
                LOGGER.info("Defaulting {} to '{}' for blocking", INCLUDE_HTTP_REQUEST_CONTEXT, this.isDefaultIncludeHttpRequestContextForBlocking());
            }
        additionalProperties.put(INCLUDE_HTTP_REQUEST_CONTEXT, this.getIncludeHttpRequestContext());
        }

        convertPropertyToStringAndWriteBack(RESPONSE_WRAPPER, this::setResponseWrapper);
        convertPropertyToBooleanAndWriteBack(USE_TAGS, this::setUseTags);
        convertPropertyToBooleanAndWriteBack(USE_BEANVALIDATION, this::setUseBeanValidation);
        convertPropertyToBooleanAndWriteBack(PERFORM_BEANVALIDATION, this::setPerformBeanValidation);
        convertPropertyToBooleanAndWriteBack(USE_OPTIONAL, this::setUseOptional);
        convertPropertyToBooleanAndWriteBack(API_FIRST, this::setApiFirst);
        convertPropertyToBooleanAndWriteBack(HATEOAS, this::setHateoas);
        convertPropertyToBooleanAndWriteBack(SPRING_CONTROLLER, this::setUseSpringController);
        convertPropertyToBooleanAndWriteBack(GENERATE_CONSTRUCTOR_WITH_REQUIRED_ARGS, value -> this.generatedConstructorWithRequiredArgs = value);
        convertPropertyToBooleanAndWriteBack(RETURN_SUCCESS_CODE, this::setReturnSuccessCode);
        convertPropertyToBooleanAndWriteBack(USE_SWAGGER_UI, this::setUseSwaggerUI);
        convertPropertyToBooleanAndWriteBack(USE_SEALED, this::setUseSealed);
        convertPropertyToBooleanAndWriteBack(USE_RECORDS, this::setUseRecords);
        convertPropertyToBooleanAndWriteBack(CodegenConstants.GENERATE_JSON_INCLUDE_ANNOTATIONS,
                value -> this.generateJsonIncludeAnnotations = TriStateBoolean.fromNullableBoolean(value));
        convertPropertyToBooleanAndWriteBack(CodegenConstants.GENERATE_JSON_SETTER_NULLS_ANNOTATIONS,
                value -> this.generateJsonSetterNullsAnnotations = TriStateBoolean.fromNullableBoolean(value));
        this.optionalNonNullPropertyJsonInclude = JsonAnnotationPolicyUtils.resolveOptionalNonNullPropertyJsonInclude(
                additionalProperties, optionalNonNullPropertyJsonInclude);
        additionalProperties.put(CodegenConstants.OPTIONAL_NON_NULL_PROPERTY_JSON_INCLUDE, optionalNonNullPropertyJsonInclude.name());
        this.optionalNonNullPropertyJsonSetterNulls = JsonAnnotationPolicyUtils.resolveOptionalNonNullPropertyJsonSetterNulls(
                additionalProperties, optionalNonNullPropertyJsonSetterNulls);
        if (optionalNonNullPropertyJsonSetterNulls != null) {
            additionalProperties.put(CodegenConstants.OPTIONAL_NON_NULL_PROPERTY_JSON_SETTER_NULLS, optionalNonNullPropertyJsonSetterNulls.name());
        }
        if (jackson) {
            JsonAnnotationPolicyUtils.warnIfUnset(LOGGER, generateJsonIncludeAnnotations, generateJsonSetterNullsAnnotations);
            JsonAnnotationPolicyUtils.warnIfJsonSetterNullsDefaultRisky(LOGGER, generateJsonSetterNullsAnnotations,
                    optionalNonNullPropertyJsonSetterNulls, openApiNullable, false);
        }
        if (DocumentationProvider.NONE.equals(getDocumentationProvider())) {
            this.setUseSwaggerUI(false);
        }

        convertPropertyToBooleanAndWriteBack(UNHANDLED_EXCEPTION_HANDLING, this::setUnhandledException);
        convertPropertyToBooleanAndWriteBack(USE_RESPONSE_ENTITY, this::setUseResponseEntity);
        convertPropertyToBooleanAndWriteBack(GENERATE_GENERIC_RESPONSE_ENTITY, this::setGenerateGenericResponseEntity);
        if (!useResponseEntity) {
            this.setGenerateGenericResponseEntity(false);
            this.additionalProperties.put(GENERATE_GENERIC_RESPONSE_ENTITY, false);
        }
        convertPropertyToBooleanAndWriteBack(OPTIONAL_ACCEPT_NULLABLE, this::setOptionalAcceptNullable);
        convertPropertyToBooleanAndWriteBack(USE_SPRING_BUILT_IN_VALIDATION, this::setUseSpringBuiltInValidation);
        convertPropertyToBooleanAndWriteBack(CodegenConstants.USE_DEDUCTION_FOR_ONE_OF_INTERFACES, this::setUseDeductionForOneOfInterfaces);

        convertPropertyToStringAndWriteBack(CLIENT_REGISTRATION_ID, this::setClientRegistrationId);
        convertPropertyToBooleanAndWriteBack(USE_SPRING_SECURITY_PRE_AUTHORIZE, this::setUseSpringSecurityPreAuthorize);
        convertPropertyToStringAndWriteBack(SPRING_SECURITY_AUTHORITY_PREFIX, this::setSpringSecurityAuthorityPrefix);

        additionalProperties.put("springHttpStatus", new SpringHttpStatusLambda());

        convertPropertyToBooleanAndWriteBack(USE_ENUM_CASE_INSENSITIVE, this::setUseEnumCaseInsensitive);
        convertPropertyToBooleanAndWriteBack(USE_JACKSON_3, this::setUseJackson3);
        convertPropertyToBooleanAndWriteBack(USE_SPRING_BOOT3, this::setUseSpringBoot3);
        convertPropertyToBooleanAndWriteBack(USE_SPRING_BOOT4, this::setUseSpringBoot4);

        if (isUseSpringBoot4()) {
            setUseSpringBoot3(false);
        }
        if (isNotEmpty(clientRegistrationId)) {
            if (!SPRING_HTTP_INTERFACE.equals(library)) {
                throw new IllegalArgumentException(CLIENT_REGISTRATION_ID + " is only supported with the " + SPRING_HTTP_INTERFACE + " library");
            }
            if (!isUseSpringBoot4()) {
                throw new IllegalArgumentException(CLIENT_REGISTRATION_ID + " requires " + USE_SPRING_BOOT4 + "=true because @ClientRegistrationId is provided by Spring Security 7");
            }
        }
        if (useSpringSecurityPreAuthorize && !SPRING_BOOT.equals(library)) {
            throw new IllegalArgumentException(USE_SPRING_SECURITY_PRE_AUTHORIZE
                    + " is only supported with the " + SPRING_BOOT + " library");
        }

        if (isUseSpringBoot3() || isUseSpringBoot4()) {
            if (AnnotationLibrary.SWAGGER1.equals(getAnnotationLibrary())) {
                throw new IllegalArgumentException(AnnotationLibrary.SWAGGER1.getPropertyName() + " is not supported with Spring Boot > 3.x");
            }
            useJakartaEe = true;
            applyJakartaPackage();
        }
        if(isUseJackson3() && !isUseSpringBoot4()){
            throw new IllegalArgumentException("useJackson3 is only available with Spring Boot >= 4");
        }
        validateUseRecords();
        if(this.useJackson3){
            this.applyJackson3Package();
        } else {
            this.applyJackson2Package();
        }

        convertPropertyToStringAndWriteBack(RESOURCE_FOLDER, this::setResourceFolder);
        convertPropertyToBooleanAndWriteBack(USE_HTTP_SERVICE_PROXY_FACTORY_INTERFACES_CONFIGURATOR, this::setUseHttpServiceProxyFactoryInterfacesConfigurator);

        convertPropertyToBooleanAndWriteBack(ADDITIONAL_NOT_NULL_ANNOTATIONS, this::setAdditionalNotNullAnnotations);

        convertPropertyToBooleanAndWriteBack(SUBSTITUTE_GENERIC_PAGED_MODEL, this::setSubstituteGenericPagedModel);
        convertPropertyToBooleanAndWriteBack(CodegenConstants.USE_ENUM_VALUE_INTERFACE, this::setUseEnumValueInterface);

        // Validate an explicitly supplied value for every library so typos are not silently ignored;
        // the resolved mode is only used (and written back) when Pageable is supported.
        if (additionalProperties.containsKey(AUTO_X_SPRING_PAGINATED)) {
            String rawAutoXSpringPaginated = String.valueOf(additionalProperties.get(AUTO_X_SPRING_PAGINATED));
            pageableUtils.warnIfDeprecatedAutoPaginationValue(rawAutoXSpringPaginated);
            setAutoXSpringPaginated(rawAutoXSpringPaginated);
        }
        if (isPageableSupported()) {
            if (additionalProperties.containsKey(AUTO_X_SPRING_PAGINATED)) {
                writePropertyBack(AUTO_X_SPRING_PAGINATED, this.autoXSpringPaginated);
            }
            convertPropertyToBooleanAndWriteBack(GENERATE_SORT_VALIDATION, this::setGenerateSortValidation);
            convertPropertyToBooleanAndWriteBack(GENERATE_PAGEABLE_CONSTRAINT_VALIDATION, this::setGeneratePageableConstraintValidation);
        }

        // override parent one
        importMapping.put("JsonDeserialize", (useJackson3 ? JACKSON3_PACKAGE : JACKSON2_PACKAGE) + ".databind.annotation.JsonDeserialize");
        // JsonSetter and Nulls always come from com.fasterxml.jackson.annotation regardless of Jackson 2 or 3
        // (Jackson 3.x intentionally keeps jackson-annotations at 2.x, same package)
        importMapping.put("JsonSetter", "com.fasterxml.jackson.annotation.JsonSetter");
        importMapping.put("Nulls", "com.fasterxml.jackson.annotation.Nulls");

        typeMapping.put("file", "org.springframework.core.io.Resource");
        importMapping.put("Nullable", useJspecify? "org.jspecify.annotations.Nullable": "org.springframework.lang.Nullable");
        importMapping.put("org.springframework.core.io.Resource", "org.springframework.core.io.Resource");
        importMapping.put("DateTimeFormat", "org.springframework.format.annotation.DateTimeFormat");
        importMapping.put("ParameterObject", "org.springdoc.api.annotations.ParameterObject");
        if (isUseSpringBoot3() || isUseSpringBoot4()) {
            importMapping.put("ParameterObject", "org.springdoc.core.annotations.ParameterObject");
        }

        if (interfaceOnly && delegatePattern) {
            delegateMethod = true;
            additionalProperties.put("delegate-method", true);
        }

        if (isUseSpringBoot4()) {
            supportingFiles.add(new SupportingFile("pom-sb4.mustache", "", "pom.xml"));
        } else if (isUseSpringBoot3()) {
            supportingFiles.add(new SupportingFile("pom-sb3.mustache", "", "pom.xml"));
        } else {
            supportingFiles.add(new SupportingFile("pom.mustache", "", "pom.xml"));
        }

        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));

        if (!interfaceOnly) {
            if (SPRING_BOOT.equals(library)) {
                if (useSwaggerUI && selectedDocumentationProviderRequiresSwaggerUiBootstrap()) {
                    supportingFiles.add(new SupportingFile("swagger-ui.mustache", "src/main/resources/static", "swagger-ui.html"));
                }
                // rename template to SpringBootApplication.mustache
                supportingFiles.add(new SupportingFile("openapi2SpringBoot.mustache",
                        (sourceFolder + File.separator + basePackage).replace(".", java.io.File.separator),
                        "OpenApiGeneratorApplication.java"));
                supportingFiles.add(new SupportingFile("SpringBootTest.mustache",
                        (testFolder + File.separator + basePackage).replace(".", java.io.File.separator),
                        "OpenApiGeneratorApplicationTests.java"));
                supportingFiles.add(new SupportingFile("RFC3339DateFormat.mustache",
                        (sourceFolder + File.separator + basePackage).replace(".", java.io.File.separator),
                        "RFC3339DateFormat.java"));
            }
            if (SPRING_CLOUD_LIBRARY.equals(library)) {

                supportingFiles.add(new SupportingFile("apiKeyRequestInterceptor.mustache",
                        (sourceFolder + File.separator + configPackage).replace(".", java.io.File.separator),
                        "ApiKeyRequestInterceptor.java"));

                if (ProcessUtils.hasOAuthMethods(openAPI)) {
                    supportingFiles.add(new SupportingFile("clientPropertiesConfiguration.mustache",
                            (sourceFolder + File.separator + configPackage).replace(".", java.io.File.separator),
                            "ClientPropertiesConfiguration.java"));
                }

                supportingFiles.add(new SupportingFile("clientConfiguration.mustache",
                        (sourceFolder + File.separator + configPackage).replace(".", java.io.File.separator),
                        "ClientConfiguration.java"));

                apiTemplateFiles.put("apiClient.mustache", "Client.java");
                if (!additionalProperties.containsKey(SINGLE_CONTENT_TYPES)) {
                    additionalProperties.put(SINGLE_CONTENT_TYPES, "true");
                    this.setSingleContentTypes(true);
                }
                // @RequestMapping not supported with spring cloud openfeign.
                setRequestMappingMode(RequestMappingMode.none);
                additionalProperties.put(USE_FEIGN_CLIENT, "true");
            } else if (SPRING_BOOT.equals(library)) {
                apiTemplateFiles.put("apiController.mustache", "Controller.java");
                supportingFiles.add(new SupportingFile("application.mustache",
                        ("src.main.resources").replace(".", java.io.File.separator), "application.properties"));
                supportingFiles.add(new SupportingFile("homeController.mustache",
                        (sourceFolder + File.separator + configPackage).replace(".", java.io.File.separator),
                        "HomeController.java"));
                supportingFiles.add(new SupportingFile("openapi.mustache",
                        ("src/main/resources").replace("/", java.io.File.separator), "openapi.yaml"));
                if (!reactive && !apiFirst) {
                    if (DocumentationProvider.SPRINGDOC.equals(getDocumentationProvider())) {
                        supportingFiles.add(new SupportingFile("springdocDocumentationConfig.mustache",
                                (sourceFolder + File.separator + configPackage).replace(".", java.io.File.separator),
                                "SpringDocConfiguration.java"));
                    }
                }
            } else if (SPRING_HTTP_INTERFACE.equals(library)) {
                if (!(isUseSpringBoot3() || isUseSpringBoot4())) {
                    throw new IllegalArgumentException("Library '" + SPRING_HTTP_INTERFACE + "' is only supported with Spring Boot 3 or 4");
                }

                String httpInterfacesAbstractConfiguratorFile = useHttpServiceProxyFactoryInterfacesConfigurator ?
                    "httpServiceProxyFactoryInterfacesConfigurator.mustache" :
                    "httpInterfacesConfiguration.mustache";

                supportingFiles.add(new SupportingFile(httpInterfacesAbstractConfiguratorFile,
                        (sourceFolder + File.separator + configPackage).replace(".", java.io.File.separator), "HttpInterfacesAbstractConfigurator.java"));

                writePropertyBack(HTTP_INTERFACES_CONFIGURATOR_DEPENDENCY,
                    useHttpServiceProxyFactoryInterfacesConfigurator ?
                    "HttpServiceProxyFactory" :
                    reactive ? "WebClient" : "RestClient"
                );
            }
        }

        if (SPRING_BOOT.equals(library)) {
            supportingFiles.add(new SupportingFile("apiUtil.mustache",
                    (sourceFolder + File.separator + apiPackage).replace(".", java.io.File.separator), "ApiUtil.java"));
        }

        if (delegatePattern && !delegateMethod) {
            additionalProperties.put("isDelegate", true);
            apiTemplateFiles.put("apiDelegate.mustache", "Delegate.java");
        }

        additionalProperties.put("javaVersion", "1.8");
        if (SPRING_CLOUD_LIBRARY.equals(library)) {
            additionalProperties.put("jdk8-default-interface", false);
        } else {
            additionalProperties.put("jdk8-default-interface", !skipDefaultInterface);
        }

        if (async) {
            additionalProperties.put(RESPONSE_WRAPPER, "CompletableFuture");
        }
        if (reactive) {
            // The response wrapper when Reactive is enabled must depend on the return type:
            // Flux<X> when X is an array
            // Mono<X> otherwise
            // But there are corner cases when also using response entity.
            // When reactive is enabled, all this is managed in the mustache templates.
            additionalProperties.put(RESPONSE_WRAPPER, "");
        }

        // Some well-known Spring or Spring-Cloud response wrappers
        if (isNotEmpty(responseWrapper)) {
            additionalProperties.put("jdk8-default-interface", false);
            switch (responseWrapper) {
                case "Future":
                case "Callable":
                case "CompletableFuture":
                    additionalProperties.put(RESPONSE_WRAPPER, "java.util.concurrent." + responseWrapper);
                    break;
                case "ListenableFuture":
                    additionalProperties.put(RESPONSE_WRAPPER, "org.springframework.util.concurrent.ListenableFuture");
                    break;
                case "DeferredResult":
                    additionalProperties.put(RESPONSE_WRAPPER,
                            "org.springframework.web.context.request.async.DeferredResult");
                    break;
                case "RxObservable":
                    additionalProperties.put(RESPONSE_WRAPPER, "rx.Observable");
                    break;
                case "RxSingle":
                    additionalProperties.put(RESPONSE_WRAPPER, "rx.Single");
                    break;
                default:
                    break;
            }
        }

        switch (getRequestMappingMode()) {
            case api_interface:
                additionalProperties.put(USE_REQUEST_MAPPING_ON_INTERFACE, true);
                break;
            case controller:
                additionalProperties.put(USE_REQUEST_MAPPING_ON_CONTROLLER, true);
                break;
            case none:
                additionalProperties.put(USE_REQUEST_MAPPING_ON_INTERFACE, false);
                additionalProperties.put(USE_REQUEST_MAPPING_ON_CONTROLLER, false);
                break;
        }

        // add lambda for mustache templates
        additionalProperties.put("lambdaRemoveDoubleQuote", (Mustache.Lambda) (fragment, writer) -> writer
                .write(fragment.execute().replaceAll("\"", Matcher.quoteReplacement(""))));
        additionalProperties.put("lambdaEscapeDoubleQuote", (Mustache.Lambda) (fragment, writer) -> writer
                .write(fragment.execute()
                        .replaceAll("\\\\", Matcher.quoteReplacement("\\\\"))
                        .replaceAll("\"", Matcher.quoteReplacement("\\\""))));
        additionalProperties.put("lambdaRemoveLineBreak",
                (Mustache.Lambda) (fragment, writer) -> writer.write(fragment.execute().replaceAll("\\r|\\n", "")));

        additionalProperties.put("lambdaTrimWhitespace", new TrimWhitespaceLambda());

        additionalProperties.put("lambdaSplitString", new SplitStringLambda());

        // apiController: hide implementation behind undocumented flag to temporarily preserve code
        additionalProperties.put("_api_controller_impl_", false);
        // HEADS-UP: Do not add more template file after this block
        if (apiFirst) {
            apiTemplateFiles.clear();
            modelTemplateFiles.clear();
        }
        supportsAdditionalPropertiesWithComposedSchema = true;
        if (useJspecify) {
            applyJspecify();
        }
    }

    protected void applyJackson2Package() {
        writePropertyBack(JACKSON_PACKAGE, JACKSON2_PACKAGE);
    }

    protected void applyJackson3Package() {
        writePropertyBack(JACKSON_PACKAGE, JACKSON3_PACKAGE);
    }

    private boolean supportLibraryUseTags() {
        return SPRING_BOOT.equals(library)
               || SPRING_CLOUD_LIBRARY.equals(library)
               || SPRING_HTTP_INTERFACE.equals(library);
    }

    /**
     * Whether the current library supports the {@code x-spring-paginated} extension (i.e. a
     * Spring Data {@code Pageable} parameter). Supported for the {@code spring-boot} server and
     * the {@code spring-cloud} (Feign) client, which handles {@code Pageable} through
     * {@code org.springframework.cloud.openfeign.support.PageableSpringEncoder}.
     */
    private boolean isPageableSupported() {
        return SPRING_BOOT.equals(library) || SPRING_CLOUD_LIBRARY.equals(library);
    }

    @Override
    public void addOperationToGroup(String tag, String resourcePath, Operation operation, CodegenOperation co, Map<String, List<CodegenOperation>> operations) {
        if (supportLibraryUseTags() && !useTags) {
            String basePath = resourcePath;
            if (basePath.startsWith("/")) {
                basePath = basePath.substring(1);
            }
            final int pos = basePath.indexOf("/");
            if (pos > 0) {
                basePath = basePath.substring(0, pos);
            }

            if (basePath.isEmpty()) {
                basePath = "default";
            } else {
                co.subresourceOperation = !co.path.isEmpty();
            }
            if (SPRING_HTTP_INTERFACE.equals(library)) {
                super.addOperationToGroup(getUniquePathGroupName(basePath, operations), resourcePath, operation, co, operations);
                return;
            }
            final List<CodegenOperation> opList = operations.computeIfAbsent(basePath, k -> new ArrayList<>());
            opList.add(co);
            co.baseName = basePath;
            return;
        }
        super.addOperationToGroup(tag, resourcePath, operation, co, operations);

    }

    private String getUniquePathGroupName(String basePath, Map<String, List<CodegenOperation>> operations) {
        String sanitizedBasePath = sanitizeName(basePath);
        if (sanitizedBasePath.isEmpty()) {
            sanitizedBasePath = "Path";
        } else if (sanitizedBasePath.matches("^\\d.*")) {
            sanitizedBasePath = "Class" + sanitizedBasePath;
        }
        String groupName = camelize(sanitizedBasePath, LOWERCASE_FIRST_LETTER);
        String uniqueGroupName = groupName;
        int suffix = 2;
        while (operations.containsKey(uniqueGroupName)
                && !getFirstPathSegment(operations.get(uniqueGroupName).get(0).path).equals(basePath)) {
            uniqueGroupName = groupName + suffix++;
        }
        return uniqueGroupName;
    }

    private String getFirstPathSegment(String path) {
        String basePath = path.startsWith("/") ? path.substring(1) : path;
        int pos = basePath.indexOf("/");
        basePath = pos > 0 ? basePath.substring(0, pos) : basePath;
        return basePath.isEmpty() ? "default" : basePath;
    }

    @Override
    public void preprocessOpenAPI(OpenAPI openAPI) {
        super.preprocessOpenAPI(openAPI);

        if (SPRING_BOOT.equals(library) && ModelUtils.containsEnums(this.openAPI)) {
            supportingFiles.add(new SupportingFile("converter.mustache",
                    (sourceFolder + File.separator + configPackage).replace(".", java.io.File.separator), "EnumConverterConfiguration.java"));
        }

        if (isPageableSupported()) {
            pageableUtils.scanAll(openAPI, autoXSpringPaginatedMode);

            if (generateSortValidation && useBeanValidation && !pageableUtils.sortValidationEnums.isEmpty()) {
                importMapping.putIfAbsent("ValidSort", configPackage + ".ValidSort");
                supportingFiles.add(new SupportingFile("validSort.mustache",
                        (sourceFolder + File.separator + configPackage).replace(".", java.io.File.separator), "ValidSort.java"));
            }

            if (!pageableUtils.pageableDefaultsRegistry.isEmpty()) {
                importMapping.putIfAbsent("PageableDefault", "org.springframework.data.web.PageableDefault");
                importMapping.putIfAbsent("SortDefault", "org.springframework.data.web.SortDefault");
                importMapping.putIfAbsent("Sort", "org.springframework.data.domain.Sort");
            }

            if (generatePageableConstraintValidation && useBeanValidation && !pageableUtils.pageableConstraintsRegistry.isEmpty()) {
                importMapping.putIfAbsent("ValidPageable", configPackage + ".ValidPageable");
                supportingFiles.add(new SupportingFile("validPageable.mustache",
                        (sourceFolder + File.separator + configPackage).replace(".", java.io.File.separator), "ValidPageable.java"));
            }
        }

        if (substituteGenericPagedModel) {
            pagedModelRegistry = PagedModelScanUtils.scanPagedModels(openAPI, this::toModelName);
            if (!pagedModelRegistry.isEmpty()) {
                boolean customMapping = importMapping.containsKey("PagedModel");
                importMapping.putIfAbsent("PagedModel", configPackage + ".PagedModel");
                if (!customMapping) {
                    // No custom class provided — generate the simple PagedModel into the config package.
                    supportingFiles.add(new SupportingFile("pagedModel.mustache",
                            (sourceFolder + File.separator + configPackage).replace(".", java.io.File.separator), "PagedModel.java"));
                }
                // Derive the actual simple class name from the FQN in importMapping so that a
                // custom mapping (e.g. "PagedModel" → "com.example.MyPagedModel") is respected.
                // The simple name of the FQN becomes the token used in generated code, and is
                // registered in importMapping so that template import resolution works.
                String fqn = importMapping.get("PagedModel");
                pagedModelClassName = fqn.substring(fqn.lastIndexOf('.') + 1);
                if (!pagedModelClassName.equals("PagedModel")) {
                    importMapping.put(pagedModelClassName, fqn);
                }
                LOGGER.info("substituteGenericPagedModel: detected {} paged-model schema(s): {}",
                        pagedModelRegistry.size(), pagedModelRegistry.keySet());
            }
        }

        if (useEnumValueInterface) {
            valuedEnumClassName = EnumValueInterfaceUtils.setupInPreprocessOpenAPI(
                    importMapping, additionalProperties, supportingFiles,
                    sourceFolder, configPackage,
                    "enumValueInterface.mustache", "ValuedEnum.java");
        }

        /*
         * TODO the following logic should not need anymore in OAS 3.0 if
         * ("/".equals(swagger.getBasePath())) { swagger.setBasePath(""); }
         */

        if (!additionalProperties.containsKey(TITLE)) {
            // From the title, compute a reasonable name for the package and the API
            String title = openAPI.getInfo().getTitle();

            // Drop any API suffix
            if (title != null) {
                title = title.trim().replace(" ", "-");
                if (title.toUpperCase(Locale.ROOT).endsWith("API")) {
                    title = title.substring(0, title.length() - 3);
                }

                this.title = camelize(sanitizeName(title), LOWERCASE_FIRST_LETTER);
            }
            additionalProperties.put(TITLE, this.title);
        }

        if (!additionalProperties.containsKey(SERVER_PORT)) {
            final URL url = URLPathUtils.getServerURL(openAPI, serverVariableOverrides());
            additionalProperties.put(SERVER_PORT, URLPathUtils.getPort(url, 8080));
        }

        if (openAPI.getPaths() != null) {
            for (final Map.Entry<String, PathItem> openAPIGetPathsEntry : openAPI.getPaths().entrySet()) {
                final PathItem path = openAPIGetPathsEntry.getValue();
                if (path.readOperations() != null) {
                    for (final Operation operation : path.readOperations()) {
                        if (operation.getTags() != null) {
                            final List<Map<String, String>> tags = new ArrayList<>();
                            for (final String tag : operation.getTags()) {
                                final Map<String, String> value = new HashMap<>();
                                value.put("tag", escapeText(tag));
                                tags.add(value);
                            }
                            if (!operation.getTags().isEmpty()) {
                                final String tag = operation.getTags().get(0);
                                operation.setTags(Collections.singletonList(tag));
                            }
                            operation.addExtension("x-tags", tags);
                        }
                    }
                }
            }
        }
    }

    @Override
    public OperationsMap postProcessOperationsWithModels(OperationsMap objs, List<ModelMap> allModels) {
        final OperationMap operations = objs.getOperations();
        if (operations != null) {
            final List<CodegenOperation> ops = operations.getOperation();
            for (final CodegenOperation operation : ops) {
                final List<CodegenResponse> responses = operation.responses;
                if (responses != null) {
                    for (final CodegenResponse resp : responses) {
                        if ("0".equals(resp.code)) {
                            resp.code = "200";
                        }
                        doDataTypeAssignment(resp.dataType, new DataTypeAssigner() {
                            @Override
                            public void setReturnType(final String returnType) {
                                resp.dataType = returnType;
                            }

                            @Override
                            public void setReturnContainer(final String returnContainer) {
                                resp.containerType = returnContainer;
                            }

                            @Override
                            public void setIsVoid(boolean isVoid) {
                                resp.isVoid = isVoid;
                            }
                        });
                    }
                }

                doDataTypeAssignment(operation.returnType, new DataTypeAssigner() {

                    @Override
                    public void setReturnType(final String returnType) {
                        operation.returnType = returnType;
                    }

                    @Override
                    public void setReturnContainer(final String returnContainer) {
                        operation.returnContainer = returnContainer;
                    }

                    @Override
                    public void setIsVoid(boolean isVoid) {
                        operation.isVoid = isVoid;
                    }
                });

                prepareVersioningParameters(ops);
                handleImplicitHeaders(operation);
                normalizeVendorExtensionWithStringList(operation.vendorExtensions, VendorExtension.X_OPERATION_EXTRA_ANNOTATION.getName());
                normalizeOperationParameterVendorExtensions(operation, VendorExtension.X_FIELD_EXTRA_ANNOTATION.getName());

                if (useSpringSecurityPreAuthorize) {
                    addSpringSecurityPreAuthorize(operation);
                }

                if (isLibrary(SPRING_HTTP_INTERFACE) || isLibrary(SPRING_BOOT)) {
                    if (operation.isArray && "string".equalsIgnoreCase(operation.returnBaseType)) {
                        operation.vendorExtensions.put(VendorExtension.X_REACTIVE_RETURN_EXCEPT_LIST_OF_STRING.getName(), true);
                    }
                }
            }
            // The tag for the controller is the first tag of the first operation
            final CodegenOperation firstOperation = ops.get(0);
            final Tag firstTag = firstOperation.tags.get(0);
            final String firstTagName = firstTag.getName();
            // But use a sensible tag name if there is none
            objs.put("tagName", escapeText("default".equals(firstTagName) ? firstOperation.baseName : firstTagName));
            objs.put("tagDescription", escapeText(firstTag.getDescription()));

            // Add clientRegistrationId for spring-http-interface with OAuth
            if (SPRING_HTTP_INTERFACE.equals(library) && clientRegistrationId != null && !clientRegistrationId.isEmpty()) {
                operations.put("clientRegistrationId", clientRegistrationId);
            }
        }

        removeImport(objs, "java.util.List");

        return objs;
    }

    /**
     * Adds a Spring Security expression that preserves the OpenAPI security requirement semantics:
     * entries in a {@code security} array are OR alternatives, while schemes and scopes in one
     * entry are combined with AND.
     */
    private void addSpringSecurityPreAuthorize(CodegenOperation codegenOperation) {
        String originalOperationId = codegenOperation.operationIdOriginal != null
                ? codegenOperation.operationIdOriginal
                : codegenOperation.operationId;
        Operation rawOperation = findOperation(originalOperationId);
        if (rawOperation == null) {
            LOGGER.warn("Could not find OpenAPI operation '{}' while generating @PreAuthorize.", originalOperationId);
            return;
        }

        List<SecurityRequirement> requirements = rawOperation.getSecurity();
        if (requirements == null) {
            requirements = openAPI.getSecurity();
        }
        if (requirements == null || requirements.isEmpty()) {
            return;
        }

        Map<String, SecurityScheme> schemes = openAPI.getComponents() != null
                ? openAPI.getComponents().getSecuritySchemes() : null;
        if (schemes == null || schemes.isEmpty()) {
            return;
        }

        List<String> alternatives = new ArrayList<>();
        for (SecurityRequirement requirement : requirements) {
            if (requirement.isEmpty()) {
                return;
            }
            List<String> groupAuthorities = new ArrayList<>();
            for (Map.Entry<String, List<String>> entry : requirement.entrySet()) {
                SecurityScheme scheme = schemes.get(entry.getKey());
                if (scheme == null || (scheme.getType() != SecurityScheme.Type.OAUTH2
                        && scheme.getType() != SecurityScheme.Type.OPENIDCONNECT)) {
                    return;
                }
                if (entry.getValue() == null || entry.getValue().isEmpty()) {
                    groupAuthorities.add("isAuthenticated()");
                } else {
                    for (String scope : entry.getValue()) {
                        groupAuthorities.add("hasAuthority('" + escapeSpelStringLiteral(springSecurityAuthorityPrefix + scope) + "')");
                    }
                }
            }
            String group = String.join(" and ", groupAuthorities);
            alternatives.add(groupAuthorities.size() > 1 && requirements.size() > 1 ? "(" + group + ")" : group);
        }

        if (!alternatives.isEmpty()) {
            codegenOperation.vendorExtensions.put("x-spring-security-pre-authorize",
                    escapeJavaStringLiteral(String.join(" or ", alternatives)));
        }
    }

    private String escapeSpelStringLiteral(String value) {
        return value.replace("'", "''");
    }

    private String escapeJavaStringLiteral(String value) {
        return value.replace("\\", "\\\\")
                .replace("\"", "\\\"")
                .replace("\r", "\\r")
                .replace("\n", "\\n");
    }

    private Operation findOperation(String operationId) {
        if (openAPI.getPaths() == null || operationId == null) {
            return null;
        }
        for (PathItem pathItem : openAPI.getPaths().values()) {
            if (pathItem.readOperations() == null) {
                continue;
            }
            for (Operation operation : pathItem.readOperations()) {
                if (operationId.equals(operation.getOperationId())) {
                    return operation;
                }
            }
        }
        return null;
    }

    private interface DataTypeAssigner {
        void setReturnType(String returnType);

        void setReturnContainer(String returnContainer);

        void setIsVoid(boolean isVoid);
    }

    /**
     * @param returnType       The return type that needs to be converted
     * @param dataTypeAssigner An object that will assign the data to the respective
     *                         fields in the model.
     */
    private void doDataTypeAssignment(String returnType, DataTypeAssigner dataTypeAssigner) {
        if (returnType == null) {
            dataTypeAssigner.setReturnType("Void");
            dataTypeAssigner.setIsVoid(true);
        } else if (returnType.startsWith("List") || returnType.startsWith("java.util.List")) {
            final int start = returnType.indexOf("<");
            final int end = returnType.lastIndexOf(">");
            if (start > 0 && end > 0) {
                dataTypeAssigner.setReturnType(returnType.substring(start + 1, end).trim());
                dataTypeAssigner.setReturnContainer("List");
            }
        } else if (returnType.startsWith("Map") || returnType.startsWith("java.util.Map")) {
            final int start = returnType.indexOf("<");
            final int end = returnType.lastIndexOf(">");
            if (start > 0 && end > 0) {
                dataTypeAssigner.setReturnType(returnType.substring(start + 1, end).split(",", 2)[1].trim());
                dataTypeAssigner.setReturnContainer("Map");
            }
        } else if (returnType.startsWith("Set") || returnType.startsWith("java.util.Set")) {
            final int start = returnType.indexOf("<");
            final int end = returnType.lastIndexOf(">");
            if (start > 0 && end > 0) {
                dataTypeAssigner.setReturnType(returnType.substring(start + 1, end).trim());
                dataTypeAssigner.setReturnContainer("Set");
            }
        }
    }

    private void prepareVersioningParameters(List<CodegenOperation> operations) {
        Object apiVersion = additionalProperties.get(SPRING_API_VERSION);
        boolean hasApiVersion = apiVersion != null;
        for (CodegenOperation operation : operations) {
            if (operation.getHasHeaderParams()) {
                List<CodegenParameter> versionParams = operation.headerParams.stream()
                        .filter(param -> {
                            String xVersionParam = Objects.toString(param.vendorExtensions.get(VendorExtension.X_VERSION_PARAM.getName()), "false");
                            return Boolean.parseBoolean(xVersionParam);
                        })
                        .collect(Collectors.toList());
                operation.hasVersionHeaders = !versionParams.isEmpty();
                operation.vendorExtensions.put("versionHeaderParamsList", versionParams);
            }

            if (operation.getHasQueryParams()) {
                List<CodegenParameter> versionParams = operation.queryParams.stream()
                        .filter(param -> {
                            String xVersionParam = Objects.toString(param.vendorExtensions.get(VendorExtension.X_VERSION_PARAM.getName()), "false");
                            return Boolean.parseBoolean(xVersionParam);
                        })
                        .collect(Collectors.toList());
                operation.hasVersionQueryParams = !versionParams.isEmpty();
                operation.vendorExtensions.put("versionQueryParamsList", versionParams);
            }
            if (hasApiVersion) {
                operation.vendorExtensions.putIfAbsent(VendorExtension.X_SPRING_API_VERSION.getName(), apiVersion);
            }
        }
    }

    @Override
    public Map<String, Object> postProcessSupportingFileData(Map<String, Object> objs) {
        generateYAMLSpecFile(objs);
        if (SPRING_CLOUD_LIBRARY.equals(library)) {
            final List<CodegenSecurity> authMethods = (List<CodegenSecurity>) objs.get("authMethods");
            if (authMethods != null) {
                for (final CodegenSecurity authMethod : authMethods) {
                    authMethod.name = camelize(sanitizeName(authMethod.name), LOWERCASE_FIRST_LETTER);
                }
            }
        }
        return objs;
    }

    @Override
    public String toApiName(String name) {
        if (name.isEmpty()) {
            return "DefaultApi";
        }
        name = sanitizeName(name);
        return camelize(name) + apiNameSuffix;
    }

    @Override
    public void setParameterExampleValue(CodegenParameter p) {
        String type = p.baseType;
        if (type == null) {
            type = p.dataType;
        }

        if ("File".equals(type)) {
            String example;

            if (p.defaultValue == null) {
                example = p.example;
            } else {
                example = p.defaultValue;
            }

            if (example == null) {
                example = "/path/to/file";
            }
            example = "new org.springframework.core.io.FileSystemResource(new java.io.File(\"" + escapeText(example)
                    + "\"))";
            p.example = example;
        } else {
            super.setParameterExampleValue(p);
        }
    }

    @Override
    public void postProcessModelProperty(CodegenModel model, CodegenProperty property) {
        super.postProcessModelProperty(model, property);

        // add org.springframework.format.annotation.DateTimeFormat when needed
        if (property.isDate || property.isDateTime) {
            model.imports.add("DateTimeFormat");
        }

        if ("null".equals(property.example)) {
            property.example = null;
        }

        // Add imports for Jackson
        if (!model.isEnum) {
            model.imports.add("JsonProperty");

            if (model.hasEnums) {
                model.imports.add("JsonValue");
            }
        } else { // enum class
            // Needed imports for Jackson's JsonCreator
            if (additionalProperties.containsKey(JACKSON)) {
                model.imports.add("JsonCreator");
            }
        }

        // Add imports for java.util.Arrays
        if (property.isByteArray) {
            model.imports.add("Arrays");
        }

        if (model.getVendorExtensions().containsKey("x-jackson-optional-nullable-helpers")) {
            model.imports.add("Arrays");
        }

        // Optional + non-nullable: emit @JsonSetter(nulls = ...) so an explicit null in the payload does not
        // silently overwrite the field's default. Also honors a per-property x-jackson-json-setter-nulls override
        // and the optionalNonNullPropertyJsonSetterNulls option; see JsonAnnotationPolicyUtils#resolveJsonSetterNulls.
        // spring's default path never emits Nulls.FAIL (failModeSupported=false), but the option/extension can.
        JsonAnnotationPolicyUtils.resolveJsonSetterNulls(model, property, generateJsonSetterNullsAnnotations,
                optionalNonNullPropertyJsonSetterNulls, openApiNullable, false);

        // Resolve the @JsonInclude policy into the x-jackson-json-include-policy vendor extension; see
        // JsonAnnotationPolicyUtils#resolveJsonIncludePolicy for the full precedence/matrix rules. For spring,
        // required properties resolve to NON_NULL when non-nullable (contract protection) and ALWAYS when
        // nullable (explicit null is valid and must be serialized).
        JsonAnnotationPolicyUtils.resolveJsonIncludePolicy(model, property, generateJsonIncludeAnnotations,
                optionalNonNullPropertyJsonInclude, JsonIncludePolicy.NON_NULL, JsonIncludePolicy.ALWAYS);
    }


    @Override
    public CodegenModel fromModel(String name, Schema model) {
        CodegenModel codegenModel = super.fromModel(name, model);
        if (getAnnotationLibrary() != AnnotationLibrary.SWAGGER1) {
            // remove swagger imports
            codegenModel.imports.remove("ApiModelProperty");
            codegenModel.imports.remove("ApiModel");
        }

        if (getAnnotationLibrary() != AnnotationLibrary.SWAGGER2) {
            // remove swagger imports
            codegenModel.imports.remove("Schema");
        }

        // Only add Nullable import for non-enum models that may have nullable fields
        if (!codegenModel.isEnum) {
            addSpringNullableImport(codegenModel.imports);
        }

        return codegenModel;
    }

    @Override
    protected boolean isConstructorWithAllArgsAllowed(CodegenModel codegenModel) {
        if (lombokAnnotations != null && lombokAnnotations.containsKey("AllArgsConstructor")) {
            // constructor generated by lombok
            return false;
        }
        if ((!generatedConstructorWithRequiredArgs && !codegenModel.vars.isEmpty())
                || !codegenModel.optionalVars.isEmpty()) {
            return super.isConstructorWithAllArgsAllowed(codegenModel);
        }
        return false;
    }

    /*
     * Add dynamic imports based on the parameters and vendor extensions of an operation.
     * The imports are expanded by the mustache {{import}} tag available to model and api
     * templates.
     *
     * #8315 Also handles removing 'size', 'page' and 'sort' query parameters if using 'x-spring-paginated'.
     */
    @Override
    public CodegenOperation fromOperation(String path, String httpMethod, Operation operation, List<Server> servers) {

        // Auto-detect pagination parameters and set x-spring-paginated if autoXSpringPaginated auto-detection is enabled.
        // Must be done BEFORE super.fromOperation() so that the base codegen populates
        // codegenOperation.vendorExtensions from the extension we just set on 'operation'.
        // Only for libraries that support Pageable (spring-boot, spring-cloud);
        // respect manual x-spring-paginated: false override.
        if (isPageableSupported()) {
            SpringPageableScanUtils.applyAutoXSpringPaginatedIfNeeded(
                    openAPI, operation, autoXSpringPaginatedMode);
        }

        // add Pageable import only if x-spring-paginated explicitly used AND it's a pageable-supporting library.
        // this allows to use a custom Pageable schema without importing Spring Pageable,
        // and avoids polluting the import mapping for libraries that don't support Pageable.
        if (isPageableSupported() && operation.getExtensions() != null
                && Boolean.TRUE.equals(operation.getExtensions().get("x-spring-paginated"))) {
            importMapping.put("Pageable", "org.springframework.data.domain.Pageable");
        }

        ProvideArgsParams provideArgsParams = reformatProvideArgsParams(operation);

        CodegenOperation codegenOperation = super.fromOperation(path, httpMethod, operation, servers);
        if (!provideArgsParams.names.isEmpty()) {
            codegenOperation.vendorExtensions.put("springProvideArgsNames", provideArgsParams.names);
        }
        if (!provideArgsParams.delegateArgs.isEmpty()) {
            codegenOperation.vendorExtensions.put("springProvideArgsDelegate", provideArgsParams.delegateArgs);
        }

        // add org.springframework.format.annotation.DateTimeFormat when needed
        codegenOperation.allParams.stream().filter(p -> p.isDate || p.isDateTime).findFirst()
                .ifPresent(p -> codegenOperation.imports.add("DateTimeFormat"));
        // For libraries that do not support Pageable (e.g. spring-http-interface) x-spring-paginated
        // cannot be honored: they need explicit query parameters for HTTP calls, not a Pageable object.
        // Strip the extension so the template does not render @ParameterObject Pageable, and warn.
        if (!isPageableSupported() && codegenOperation.vendorExtensions.remove("x-spring-paginated") != null) {
            LOGGER.warn("x-spring-paginated on operation '{}' is ignored for library '{}'; "
                    + "Pageable is only supported for spring-boot and spring-cloud. "
                    + "Individual page/size/sort query parameters will be used instead.",
                    codegenOperation.operationId, library);
        }
        // add org.springframework.data.domain.Pageable import when needed
        if (isPageableSupported()
                && Boolean.TRUE.equals(codegenOperation.vendorExtensions.get("x-spring-paginated"))) {
            codegenOperation.imports.add("Pageable");
            SpringPageableScanUtils.applySpringDocPageableAnnotation(codegenOperation,
                    SpringPageableScanUtils.AnnotationSyntax.JAVA,
                    DocumentationProvider.SPRINGDOC.equals(getDocumentationProvider()));

            // #8315 Remove matching Spring Data Web default query params if 'x-spring-paginated' with Pageable is used
            SpringPageableScanUtils.removePageableQueryParams(codegenOperation);

            // Build and attach pageable parameter annotations
            pageableUtils.applyPageableAnnotations(codegenOperation,
                    generatePageableConstraintValidation, useBeanValidation,
                    generateSortValidation, SpringPageableScanUtils.AnnotationSyntax.JAVA);
        }
        if (codegenOperation.vendorExtensions.containsKey("x-spring-provide-args") && !provideArgsParams.imports.isEmpty()) {
            codegenOperation.imports.addAll(provideArgsParams.imports);
        }

        if (isSpringCodegen()) {
            addNullableImportForOperation(codegenOperation);
        }

        if (reactive && sse) {
            var MEDIA_EVENT_STREAM = "text/event-stream";
            // inspecting used streaming media types
                /*
                 expected definition:
                 content:
                    text/event-stream:
                        schema:
                        type: array
                        format: event-stream
                        items:
                            type: <type> or
                            $ref: <typeRef>
                 */
            Map<String, List<Schema>> schemaTypes = operation.getResponses().entrySet().stream()
                    .map(e -> Pair.of(e.getValue(), fromResponse(e.getKey(), e.getValue())))
                    .filter(p -> p.getRight().is2xx) // consider only success
                    .map(p -> p.getLeft().getContent().get(MEDIA_EVENT_STREAM))
                    .map(MediaType::getSchema)
                    .collect(Collectors.toList()).stream()
                    .collect(Collectors.groupingBy(Schema::getType));
            if (schemaTypes.containsKey("array")) {
                // we have a match with SSE pattern
                // double check potential conflicting, multiple specs
                if (schemaTypes.size() > 1) {
                    throw new RuntimeException("only 1 response media type supported, when SSE is detected");
                }
                // double check schema format
                List<Schema> eventTypes = schemaTypes.get("array");
                if (eventTypes.stream().anyMatch(schema -> !"event-stream".equalsIgnoreCase(schema.getFormat()))) {
                    throw new RuntimeException("schema format 'event-stream' is required, when SSE is detected");
                }
                // double check item types
                Set<String> itemTypes = eventTypes.stream()
                        .map(schema -> schema.getItems().getType() != null
                                ? schema.getItems().getType()
                                : schema.getItems().get$ref())
                        .collect(Collectors.toSet());
                if (itemTypes.size() > 1) {
                    throw new RuntimeException("only single item type is supported, when SSE is detected");
                }
                codegenOperation.vendorExtensions.put("x-sse", true);
            } // Not an SSE compliant definition
        }

        // If substituteGenericPagedModel is enabled, replace paged-model return types
        // with org.springframework.data.web.PagedModel<T>.
        if (substituteGenericPagedModel && !pagedModelRegistry.isEmpty()
                && codegenOperation.returnBaseType != null) {
            PagedModelScanUtils.DetectedPagedModel detected =
                    pagedModelRegistry.get(codegenOperation.returnBaseType);
            if (detected != null) {
                String oldType = codegenOperation.returnType;
                // Run through toModelName so that schemaMappings (e.g. User → com.example.MyUser)
                // are honored: the mapped name is used both in the type arg and for import resolution.
                String itemType = toModelName(detected.itemSchemaName);
                codegenOperation.returnType = pagedModelClassName + "<" + itemType + ">";
                codegenOperation.returnBaseType = pagedModelClassName;
                // Clear any container flag — PagedModel is not itself a List/array
                codegenOperation.returnContainer = null;
                // Add item type import (needed for PagedModel<T> in method signature)
                codegenOperation.imports.add(itemType);
                codegenOperation.imports.add(pagedModelClassName);
                // Remove paged schema import when no annotations are generated —
                // the class is suppressed and not referenced anywhere
                if (getAnnotationLibrary() == AnnotationLibrary.NONE) {
                    codegenOperation.imports.remove(detected.schemaName);
                }
                LOGGER.info("substituteGenericPagedModel: operation '{}': replacing return type '{}' with {}<{}>",
                        codegenOperation.operationId, oldType, pagedModelClassName, itemType);
            }
        }

        return codegenOperation;
    }

    private ProvideArgsParams reformatProvideArgsParams(Operation operation) {
        ProvideArgsParams provideArgsParams = new ProvideArgsParams();
        Object argObj = operation.getExtensions().get("x-spring-provide-args");
        if (argObj instanceof List) {
            List<String> provideArgs = (List<String>) argObj;
            if (!provideArgs.isEmpty()) {
                List<String> formattedArgs = new ArrayList<>();
                List<String> formattedArgNames = new ArrayList<>();
                List<String> formattedDelegateArgs = new ArrayList<>();
                for (String oneArg : provideArgs) {
                    if (StringUtils.isNotEmpty(oneArg)) {
                        Parameter parameter = parseProvideArgParameter(oneArg);
                        collectImportsAndSimplify(parameter, provideArgsParams);

                        String newArg = parameter.toString();
                        LOGGER.trace("new arg {} {}", newArg);
                        formattedArgs.add(newArg);

                        Parameter delegateParameter = parameter.clone();
                        delegateParameter.getAnnotations().clear();
                        formattedDelegateArgs.add(delegateParameter.toString());
                        formattedArgNames.add(parameter.getNameAsString());
                    }
                }
                operation.getExtensions().put("x-spring-provide-args", formattedArgs);
                provideArgsParams.names.addAll(formattedArgNames);
                provideArgsParams.delegateArgs.addAll(formattedDelegateArgs);
            }
        }
        return provideArgsParams;
    }

    private Parameter parseProvideArgParameter(String oneArg) {
        CompilationUnit compilationUnit = StaticJavaParser.parse(String.format(Locale.ROOT, "class Dummy { void method(%s) {} }", oneArg));
        return compilationUnit.findFirst(MethodDeclaration.class)
                .orElseThrow(() -> new IllegalArgumentException("Unable to parse x-spring-provide-args parameter: " + oneArg))
                .getParameter(0);
    }

    private void collectImportsAndSimplify(Parameter parameter, ProvideArgsParams provideArgsParams) {
        parameter.findAll(AnnotationExpr.class).forEach(annotation -> {
            String annotationName = annotation.getNameAsString();
            if (annotationName.contains(".")) {
                String simpleName = annotation.getName().getIdentifier();
                importMapping.put(simpleName, annotationName);
                provideArgsParams.imports.add(simpleName);
                annotation.setName(simpleName);
                LOGGER.trace("put import mapping {} {}", simpleName, annotationName);
            }
        });

        parameter.getType().toClassOrInterfaceType().ifPresent(type -> simplifyClassOrInterfaceType(type, provideArgsParams));
    }

    private void simplifyClassOrInterfaceType(ClassOrInterfaceType type, ProvideArgsParams provideArgsParams) {
        type.getTypeArguments().stream()
                .flatMap(Collection::stream)
                .map(Type::toClassOrInterfaceType)
                .flatMap(Optional::stream)
                .forEach(classOrInterfaceType -> simplifyClassOrInterfaceType(classOrInterfaceType, provideArgsParams));
        if (type.getScope().isPresent()) {
            String typeName = type.getNameWithScope();
            if (typeName.contains(".")) {
                String simpleName = type.getNameAsString();
                importMapping.put(simpleName, typeName);
                provideArgsParams.imports.add(simpleName);
                type.setScope(null);
                LOGGER.trace("put import mapping {} {}", simpleName, typeName);
            }
        }
    }

    private static final class ProvideArgsParams {
        private final Set<String> imports = new HashSet<>();
        private final List<String> names = new ArrayList<>();
        private final List<String> delegateArgs = new ArrayList<>();
    }

    @Override
    public Map<String, ModelsMap> postProcessAllModels(Map<String, ModelsMap> objs) {
        objs = super.postProcessAllModels(objs);

        Map<String, CodegenModel> allModels = getAllModels(objs);

        // conditionally force the generation of no args constructor
        for (CodegenModel cm : allModels.values()) {
            boolean hasLombokNoArgsConstructor = lombokAnnotations != null && lombokAnnotations.containsKey("NoArgsConstructor");
            if (!hasLombokNoArgsConstructor
                    && (cm.hasRequired || cm.vendorExtensions.containsKey("x-java-all-args-constructor"))) {
                cm.vendorExtensions.put("x-java-no-args-constructor", true);
            }
            // the required args constructor only has lombok's signature when every own field is a plain required
            // argument: requiredVars also holds inherited properties, a JsonNullable field takes a plain value
            // and the additionalProperties map is a field too
            if (lombokAnnotations != null && lombokAnnotations.containsKey("AllArgsConstructor")
                    && cm.additionalPropertiesType == null
                    && cm.requiredVars.size() == cm.vars.size()
                    && cm.vars.stream().allMatch(p -> p.required && !(openApiNullable && p.isNullable))) {
                cm.vendorExtensions.put("x-java-lombok-duplicates-required-args-constructor", true);
            }
        }

        if (substituteGenericPagedModel && !pagedModelRegistry.isEmpty()) {
            if (getAnnotationLibrary() == AnnotationLibrary.NONE) {
                // No @ApiResponse annotations are generated when annotationLibrary=none,
                // so paged schemas are not referenced anywhere → safe to suppress.
                // metaSchemasToCheck maps transformed name (for imports check) → raw name (for objs.remove)
                Map<String, String> metaSchemasToCheck = new LinkedHashMap<>();
                for (PagedModelScanUtils.DetectedPagedModel detected : pagedModelRegistry.values()) {
                    if (detected.metaSchemaName != null) {
                        metaSchemasToCheck.put(detected.metaSchemaName, detected.rawMetaSchemaName);
                    }
                }
                // Remove paged schemas first so reference checks below reflect the post-suppression state.
                for (Map.Entry<String, PagedModelScanUtils.DetectedPagedModel> entry : pagedModelRegistry.entrySet()) {
                    PagedModelScanUtils.DetectedPagedModel detected = entry.getValue();
                    // objs is keyed by raw schema name (DefaultGenerator uses the raw OpenAPI name as key)
                    if (objs.remove(detected.rawSchemaName) != null) {
                        LOGGER.info("substituteGenericPagedModel: suppressing model '{}' — replaced by PagedModel<{}>",
                                detected.rawSchemaName, detected.itemSchemaName);
                    }
                }
                // Suppress meta schemas only when no remaining (non-suppressed) schema references them.
                // Example: if SearchResult has a 'page: PageMeta' property, PageMeta must be kept.
                for (Map.Entry<String, String> metaEntry : metaSchemasToCheck.entrySet()) {
                    String metaName = metaEntry.getKey();       // transformed — matches cm.imports values
                    String rawMetaName = metaEntry.getValue();  // raw — matches objs key
                    boolean referencedElsewhere = objs.values().stream()
                            .flatMap(mm -> mm.getModels().stream())
                            .map(ModelMap::getModel)
                            .anyMatch(cm -> cm.imports.contains(metaName));
                    if (referencedElsewhere) {
                        LOGGER.info("substituteGenericPagedModel: keeping pagination metadata model '{}'"
                                + " — referenced by a non-paged schema", metaName);
                    } else if (objs.remove(rawMetaName) != null) {
                        LOGGER.info("substituteGenericPagedModel: suppressing pagination metadata model '{}'"
                                + " — replaced by PagedModel.PageMetadata", metaName);
                    }
                }
            } else {
                LOGGER.info("substituteGenericPagedModel: keeping paged-model schemas (annotationLibrary={}) — @ApiResponse annotations reference them",
                        getAnnotationLibrary().toCliOptValue());
            }
        }

        if (useRecords) {
            markRecordModels(allModels);
        }

        return objs;
    }

    /**
     * Whether this generator can emit models as records. {@code java-camel} extends this class but ships its own
     * {@code pojo.mustache}, so it turns the option off.
     */
    protected boolean isRecordsSupported() {
        return true;
    }

    private void validateUseRecords() {
        if (!useRecords) {
            return;
        }
        if (!isRecordsSupported()) {
            throw new IllegalArgumentException(USE_RECORDS + " is not supported by the " + getName() + " generator");
        }
        if (!isUseSpringBoot3() && !isUseSpringBoot4()) {
            throw new IllegalArgumentException(USE_RECORDS + " requires Spring Boot 3 or 4 (Java 17); set "
                    + USE_SPRING_BOOT3 + "=true or " + USE_SPRING_BOOT4 + "=true");
        }
        if (hateoas) {
            throw new IllegalArgumentException(USE_RECORDS + " cannot be combined with " + HATEOAS
                    + ": a record cannot extend RepresentationModel");
        }
        if (additionalModelTypeAnnotations.stream().anyMatch(a -> a.trim().startsWith("@lombok."))) {
            throw new IllegalArgumentException(USE_RECORDS + " cannot be combined with Lombok annotations in "
                    + ADDITIONAL_MODEL_TYPE_ANNOTATIONS);
        }
        if (withXml) {
            throw new IllegalArgumentException(USE_RECORDS + " cannot be combined with withXml: "
                    + "XML annotations on records are not supported yet");
        }
        if (useOptional) {
            throw new IllegalArgumentException(USE_RECORDS + " cannot be combined with " + USE_OPTIONAL
                    + ": Optional components are not supported yet");
        }
    }

    /**
     * Largest number of components of a generated record. Measured: javac accepts 254 constructor parameters, but
     * Jackson's method-handle path on Spring Boot 4 fails for 254 ("bad parameter count 256") and works for 253;
     * the rest is margin.
     */
    private static final int RECORD_MAX_COMPONENTS = 250;

    /** Component names whose accessor would clash with the static {@code builder()} and {@code toBuilder()} of a record. */
    private static final Set<String> RECORD_BUILDER_CLASHING_COMPONENT_NAMES = new HashSet<>(Arrays.asList(
            "builder", "toBuilder"));

    /** Names that a record component cannot have (JLS 8.10.1). */
    private static final Set<String> RECORD_FORBIDDEN_COMPONENT_NAMES = new HashSet<>(Arrays.asList(
            "clone", "finalize", "getClass", "hashCode", "notify", "notifyAll", "toString", "wait"));

    /** A constructor call in a default value, as {@code toDefaultValue} writes it: {@code new Sub()}, {@code new Sub().x("a")}. */
    private static final Pattern RECORD_DEFAULT_CONSTRUCTOR_CALL = Pattern.compile("(?<![\\w.])new (\\w+)\\(");

    /**
     * Decides for every model whether it is generated as a record and stores the decision in the vendor
     * extension {@code x-is-record}. A model that stays a class is logged with the reason.
     */
    private void markRecordModels(Map<String, CodegenModel> allModels) {
        Map<String, List<CodegenModel>> oneOfInterfacesOfMember = new HashMap<>();
        for (CodegenModel cm : allModels.values()) {
            if (Boolean.TRUE.equals(cm.vendorExtensions.get(X_IS_ONE_OF_INTERFACE)) && cm.oneOf != null) {
                for (String member : cm.oneOf) {
                    oneOfInterfacesOfMember.computeIfAbsent(member, k -> new ArrayList<>()).add(cm);
                }
            }
        }

        ParameterObjects parameterObjects = parameterObjects();
        Map<String, String> constructedByDefault = modelsConstructedByDefault(allModels);

        List<CodegenModel> candidates = new ArrayList<>();
        Map<String, String> reasons = new HashMap<>();
        Map<String, List<Map<String, String>>> accessorsOfModel = new HashMap<>();
        for (CodegenModel cm : allModels.values()) {
            // enums and oneOf interfaces are untouched; aliases (a schema that is only a $ref or a primitive) are
            // never written as a model file (DefaultGenerator skips CodegenModel.isAlias), so they get no decision
            if (cm.isEnum || cm.isAlias || Boolean.TRUE.equals(cm.vendorExtensions.get(X_IS_ONE_OF_INTERFACE))) {
                continue;
            }
            candidates.add(cm);
            List<Map<String, String>> discriminatorAccessors = new ArrayList<>();
            String reason = reasonForClass(cm, parameterObjects, constructedByDefault);
            if (reason == null) {
                reason = oneOfDiscriminatorAccessors(cm, oneOfInterfacesWithEnclosing(cm.classname, oneOfInterfacesOfMember),
                        discriminatorAccessors);
            }
            if (reason != null) {
                reasons.put(cm.classname, reason);
            }
            accessorsOfModel.put(cm.classname, discriminatorAccessors);
        }
        keepParameterObjectGraphAsClasses(candidates, reasons, parameterObjects);

        for (CodegenModel cm : candidates) {
            String reason = reasons.get(cm.classname);
            if (reason != null) {
                LOGGER.info("{}: model '{}' is generated as a class: {}", USE_RECORDS, cm.classname, reason);
                continue;
            }
            cm.vendorExtensions.put("x-is-record", true);
            boolean boundByName = parameterObjects.boundByName.contains(cm.schemaName);
            boolean hasInitialValue = false;
            for (CodegenProperty cp : cm.vars) {
                if (boundByName && !cp.baseName.equals(cp.name)) {
                    // the data binder takes the component name; this makes it take the documented name, as the
                    // setter-based class does not
                    cp.vendorExtensions.put("x-record-bind-param", StringEscapeUtils.escapeJava(cp.baseName));
                }
                String initialValue = recordInitialValue(cp);
                if (initialValue != null) {
                    cp.vendorExtensions.put("x-record-initial-value", initialValue);
                    hasInitialValue = true;
                }
            }
            cm.vendorExtensions.put("x-record-has-initial-values", hasInitialValue);
            cm.vendorExtensions.put("x-record-discriminator-accessors", accessorsOfModel.get(cm.classname));
            cm.vendorExtensions.put("x-record-masks-password", cm.vars.stream().anyMatch(cp -> cp.isPassword));
        }
    }

    /**
     * Classnames of the models that the default value of a model-typed property constructs, each with the first
     * property that does it. A record has only its canonical constructor, so {@code new Sub()} and the fluent
     * {@code new Sub().x("a")} that {@code toDefaultValue} writes do not compile for a record; such a model stays a class.
     * Only the default of a property whose type, or whose innermost items type, is a model is read, never the text
     * of a string default. A model without properties is left out: its record has a no-argument constructor, and so
     * is a model that gets no file.
     */
    private Map<String, String> modelsConstructedByDefault(Map<String, CodegenModel> allModels) {
        Set<String> modelNames = new HashSet<>();
        Set<String> modelsWithProperties = new HashSet<>();
        for (CodegenModel cm : allModels.values()) {
            modelNames.add(cm.classname);
            if (!cm.vars.isEmpty() && !hasNoModelFile(cm)) {
                modelsWithProperties.add(cm.classname);
            }
        }
        Map<String, String> constructed = new HashMap<>();
        for (CodegenModel cm : new TreeMap<>(allModels).values()) {
            for (CodegenProperty cp : cm.vars) {
                CodegenProperty inner = cp.mostInnerItems != null ? cp.mostInnerItems : cp;
                // complexType is null when the model is named like a Java type, as a model named Integer is
                String type = inner.complexType != null ? inner.complexType : inner.isModel ? inner.dataType : null;
                // the initial value that the templates print, which is not the default for a nullable property
                String initialValue = recordInitialValue(cp);
                if (type == null || !modelNames.contains(type) || initialValue == null) {
                    continue;
                }
                Matcher matcher = RECORD_DEFAULT_CONSTRUCTOR_CALL.matcher(initialValue);
                while (matcher.find()) {
                    if (modelsWithProperties.contains(matcher.group(1))) {
                        constructed.putIfAbsent(matcher.group(1), "property '" + cp.name + "' in model '" + cm.classname + "'");
                    }
                }
            }
        }
        return constructed;
    }

    /**
     * Whether {@code DefaultGenerator} writes no file for this model: its name is type-mapped to a type that has an
     * import mapping, as {@code URI} and {@code UUID} are (the property then has the JDK class as its type).
     */
    private boolean hasNoModelFile(CodegenModel cm) {
        String mappedType = typeMapping.get(cm.schemaName);
        return mappedType != null && importMapping.containsKey(mappedType);
    }

    /**
     * A request binds a query parameter object by name through Spring's data binder, which fills a class through
     * its setters and auto-grows a nested model with a no-argument constructor. Measured on Spring Framework 6.1,
     * 6.2 and 7.0: a record nested under a class fails (HTTP 500), and on 6.1 a record also fails as the element of
     * a list or map. So everything below a parameter object that stays a class stays a class, and so does every
     * list or map element of a parameter object (the generator does not know which Spring Framework the parent
     * brings).
     */
    private void keepParameterObjectGraphAsClasses(List<CodegenModel> models, Map<String, String> reasons,
                                                   ParameterObjects parameterObjects) {
        Map<String, CodegenModel> modelOfSchema = new HashMap<>();
        for (CodegenModel cm : models) {
            modelOfSchema.put(cm.schemaName, cm);
            if (parameterObjects.elements.contains(cm.schemaName)) {
                reasons.putIfAbsent(cm.classname, "it is a list or map element inside a parameter object");
            }
        }
        // breadth first from every parameter object that is a class, so the reason names the nearest one
        Map<String, String> rootOf = new HashMap<>();
        Deque<String> queue = new ArrayDeque<>();
        for (CodegenModel cm : models) {
            if (parameterObjects.boundByName.contains(cm.schemaName) && reasons.containsKey(cm.classname)) {
                rootOf.put(cm.schemaName, cm.classname);
                queue.add(cm.schemaName);
            }
        }
        while (!queue.isEmpty()) {
            String name = queue.poll();
            for (String nested : parameterObjects.referenced.getOrDefault(name, Collections.emptySet())) {
                if (rootOf.putIfAbsent(nested, rootOf.get(name)) != null) {
                    continue;
                }
                queue.add(nested);
                CodegenModel cm = modelOfSchema.get(nested);
                if (cm != null) {
                    reasons.putIfAbsent(cm.classname, "it is nested in '" + rootOf.get(name)
                            + "', a parameter object that is a class");
                }
            }
        }
    }

    /**
     * The models that a request binds by name, found once from the document: what the data binder fills from query
     * parameters. A model that is only converted from one string (a header, cookie or path value, the item of an
     * array or the value of a map parameter) is not in it.
     */
    private static final class ParameterObjects {
        /** The schema of every query parameter that is not an array or a map, and every model reachable from it. */
        final Set<String> boundByName = new HashSet<>();
        /** The item of an array property or the value of a map property of a model in {@code boundByName}. */
        final Set<String> elements = new HashSet<>();
        /** For each schema name in {@code boundByName}, the schema names it refers to directly. */
        final Map<String, Set<String>> referenced = new HashMap<>();
        /**
         * The schema of every header, cookie and path parameter, the items of an array and the values of a map
         * parameter, and every model reachable from them: converted from one string, which a record does not
         * always survive (measured: a class with a required-arguments constructor is converted, a record is not).
         */
        final Set<String> converted = new HashSet<>();
    }

    private ParameterObjects parameterObjects() {
        ParameterObjects parameterObjects = new ParameterObjects();
        if (openAPI == null || openAPI.getPaths() == null) {
            return parameterObjects;
        }
        ParameterObjects converted = new ParameterObjects();
        for (PathItem pathItem : openAPI.getPaths().values()) {
            collectParameterObjects(pathItem.getParameters(), parameterObjects, converted);
            for (Operation operation : pathItem.readOperations()) {
                collectParameterObjects(operation.getParameters(), parameterObjects, converted);
            }
        }
        parameterObjects.converted.addAll(converted.boundByName);
        return parameterObjects;
    }

    private void collectParameterObjects(List<io.swagger.v3.oas.models.parameters.Parameter> parameters,
                                         ParameterObjects parameterObjects, ParameterObjects converted) {
        if (parameters == null) {
            return;
        }
        for (io.swagger.v3.oas.models.parameters.Parameter parameter : parameters) {
            io.swagger.v3.oas.models.parameters.Parameter resolved = ModelUtils.getReferencedParameter(openAPI, parameter);
            if (resolved == null) {
                continue;
            }
            boolean query = "query".equals(resolved.getIn());
            collectParameterObjects(resolved.getSchema(), query, parameterObjects, converted);
            if (resolved.getContent() != null) {
                for (MediaType mediaType : resolved.getContent().values()) {
                    collectParameterObjects(mediaType.getSchema(), query, parameterObjects, converted);
                }
            }
        }
    }

    /**
     * A query parameter that is not an array or a map is bound by name; the items of an array and the values of a map
     * are converted from strings, like the schema of a header, cookie or path parameter.
     */
    private void collectParameterObjects(Schema<?> schema, boolean query, ParameterObjects parameterObjects,
                                         ParameterObjects converted) {
        Schema<?> target = schema;
        Set<String> followed = new HashSet<>();
        while (target != null && target.get$ref() != null && followed.add(target.get$ref())) {
            target = ModelUtils.getReferencedSchema(openAPI, target);
        }
        // a oneOf or anyOf is an interface, which the data binder cannot fill
        boolean byName = query && target != null && !ModelUtils.isArraySchema(target)
                && !(ModelUtils.isMapSchema(target) && (target.getProperties() == null || target.getProperties().isEmpty()))
                && (target.getOneOf() == null || target.getOneOf().isEmpty())
                && (target.getAnyOf() == null || target.getAnyOf().isEmpty());
        collectParameterObjects(schema, null, false, byName ? parameterObjects : converted);
    }

    /**
     * Adds the schema and everything reachable from it through a property, array items, additional properties,
     * {@code allOf}, {@code oneOf} and {@code anyOf}, following alias chains, to the models bound by name.
     *
     * @param owner   the name of the model whose body is being read, or null at a parameter
     * @param element whether the schema is the item of an array or the value of a map of that model
     */
    private void collectParameterObjects(Schema<?> schema, String owner, boolean element, ParameterObjects parameterObjects) {
        if (schema == null) {
            return;
        }
        Schema<?> target = schema;
        String name = owner;
        if (schema.get$ref() != null) {
            name = ModelUtils.getSimpleRef(schema.get$ref());
            if (owner != null) {
                parameterObjects.referenced.computeIfAbsent(owner, k -> new HashSet<>()).add(name);
            }
            boolean first = parameterObjects.boundByName.add(name);
            boolean newElement = element && parameterObjects.elements.add(name);
            target = ModelUtils.getReferencedSchema(openAPI, schema);
            if (target == null) {
                return;
            }
            if (target.get$ref() != null) {
                // an alias: a component schema that is only a $ref; follow it to the real schema, recording each name
                if (first || newElement) {
                    collectParameterObjects(target, name, element, parameterObjects);
                }
                return;
            }
            if (!first) {
                return;
            }
        }
        collectParameterObjects(target.getItems(), name, true, parameterObjects);
        if (target.getAdditionalProperties() instanceof Schema) {
            collectParameterObjects((Schema<?>) target.getAdditionalProperties(), name, true, parameterObjects);
        }
        if (target.getProperties() != null) {
            for (Schema<?> property : ((Map<String, Schema<?>>) (Map) target.getProperties()).values()) {
                collectParameterObjects(property, name, false, parameterObjects);
            }
        }
        for (List<Schema> composed : Arrays.asList(target.getAllOf(), target.getOneOf(), target.getAnyOf())) {
            if (composed != null) {
                for (Schema<?> member : composed) {
                    collectParameterObjects(member, name, false, parameterObjects);
                }
            }
        }
    }

    /**
     * The interface of a oneOf with a discriminator declares {@code getX()} for it, and a record has only {@code x()}.
     * The interfaces include those the member's interfaces extend, because an interface that is itself a member of
     * another oneOf extends that one.
     * Collects one accessor for each distinct getter name of the interfaces this member implements: the getter name
     * is the interface's own, it returns the component whose JSON name is the discriminator's property, and carries
     * {@code @JsonProperty} with that JSON name like the class's getter (Jackson would otherwise derive another
     * name from the method for names such as {@code TYPE}). Returns the reason to keep the model a class when a
     * discriminator has no such property or a nullable one under openApiNullable (the class does not compile
     * either), or when one getter name would have to return two components, otherwise null. A component whose
     * Java name starts with {@code get} never gets here: {@code reasonForClass} keeps the model a class.
     */
    private String oneOfDiscriminatorAccessors(CodegenModel cm, List<CodegenModel> interfaces,
                                               List<Map<String, String>> accessors) {
        if (interfaces == null) {
            return null;
        }
        Map<String, CodegenProperty> componentOfGetter = new HashMap<>();
        Map<String, String> interfaceOfGetter = new HashMap<>();
        for (CodegenModel oneOfInterface : interfaces) {
            CodegenDiscriminator discriminator = oneOfInterface.discriminator;
            if (discriminator == null) {
                continue;
            }
            CodegenProperty component = null;
            for (CodegenProperty cp : cm.vars) {
                if (cp.baseName.equals(discriminator.getPropertyBaseName())) {
                    component = cp;
                    break;
                }
            }
            if (component == null) {
                return "it has no property '" + discriminator.getPropertyBaseName()
                        + "', the discriminator of the oneOf interface " + oneOfInterface.classname;
            }
            if (openApiNullable && component.isNullable) {
                // the component is a JsonNullable<T> while the interface declares T getX(): the class's getter
                // returns a JsonNullable too and does not implement the interface either
                return "its discriminator property '" + component.baseName + "' for the oneOf interface "
                        + oneOfInterface.classname + " is nullable";
            }
            CodegenProperty previous = componentOfGetter.putIfAbsent(discriminator.getPropertyGetter(), component);
            if (previous != null) {
                if (previous != component) {
                    return "the oneOf interfaces " + interfaceOfGetter.get(discriminator.getPropertyGetter()) + " and "
                            + oneOfInterface.classname + " both declare " + discriminator.getPropertyGetter()
                            + "() for different properties";
                }
                continue;
            }
            interfaceOfGetter.put(discriminator.getPropertyGetter(), oneOfInterface.classname);
            Map<String, String> accessor = new HashMap<>();
            accessor.put("getter", discriminator.getPropertyGetter());
            accessor.put("component", component.name);
            accessor.put("baseName", component.baseName);
            accessor.put("type", component.datatypeWithEnum);
            accessors.add(accessor);
        }
        return null;
    }

    /**
     * The oneOf interfaces a member has to satisfy: those listing it in {@code oneOf}, then the interfaces each of
     * them is itself a member of, and so on (an interface that is a oneOf member extends the interface above it).
     * The direct interfaces come first in their order, then the enclosing ones depth-first, each interface once.
     */
    private static List<CodegenModel> oneOfInterfacesWithEnclosing(String member,
                                                                   Map<String, List<CodegenModel>> interfacesOfMember) {
        List<CodegenModel> result = new ArrayList<>();
        collectOneOfInterfaces(member, interfacesOfMember, result, new HashSet<>());
        return result;
    }

    private static void collectOneOfInterfaces(String member, Map<String, List<CodegenModel>> interfacesOfMember,
                                               List<CodegenModel> result, Set<String> visited) {
        List<CodegenModel> added = new ArrayList<>();
        for (CodegenModel oneOfInterface : interfacesOfMember.getOrDefault(member, Collections.emptyList())) {
            if (visited.add(oneOfInterface.classname)) {
                result.add(oneOfInterface);
                added.add(oneOfInterface);
            }
        }
        for (CodegenModel oneOfInterface : added) {
            collectOneOfInterfaces(oneOfInterface.classname, interfacesOfMember, result, visited);
        }
    }

    private String reasonForClass(CodegenModel cm, ParameterObjects parameterObjects,
                                  Map<String, String> constructedByDefault) {
        if (cm.parent != null) {
            return "it has a parent (" + cm.parent + ")";
        }
        if (cm.hasChildren || (cm.children != null && !cm.children.isEmpty())) {
            return "it has child models";
        }
        if (cm.discriminator != null) {
            return "it is a discriminator base";
        }
        if (isNotEmpty(cm.additionalPropertiesType)) {
            return "it has additionalProperties";
        }
        if ((cm.oneOf != null && !cm.oneOf.isEmpty()) || (cm.anyOf != null && !cm.anyOf.isEmpty())) {
            return "it is a oneOf or anyOf composition";
        }
        if (cm.vars.size() > RECORD_MAX_COMPONENTS) {
            return "it has more than " + RECORD_MAX_COMPONENTS + " properties";
        }
        if (parameterObjects.converted.contains(cm.schemaName)) {
            return "it is bound from request parameters";
        }
        for (CodegenProperty cp : cm.vars) {
            if (cp.isByteArray) {
                return "property '" + cp.name + "' is a byte[]";
            }
            if (RECORD_FORBIDDEN_COMPONENT_NAMES.contains(cp.name)) {
                return "property '" + cp.name + "' cannot be a record component name";
            }
            if (cp.name.startsWith("get")) {
                // the record's accessor getName() looks like a bean getter, so the bean validator checks the
                // constraints a second time under a phantom path (measured: every name from "get" on)
                return "property '" + cp.name + "' has a Java name that starts with 'get'";
            }
            if (generateBuilders && RECORD_BUILDER_CLASHING_COMPONENT_NAMES.contains(cp.name)) {
                return "property '" + cp.name + "' clashes with a method of the record's builder";
            }
            if (Boolean.TRUE.equals(cp.vendorExtensions.get("x-has-json-setter-nulls-fail"))) {
                return "property '" + cp.name + "' uses the FAIL null policy";
            }
            if (parameterObjects.boundByName.contains(cm.schemaName)) {
                String reason = reasonForParameterObjectComponent(cp);
                if (reason != null) {
                    return reason;
                }
            }
        }
        if (constructedByDefault.containsKey(cm.classname)) {
            return "the default of " + constructedByDefault.get(cm.classname) + " constructs it with new " + cm.classname + "()";
        }
        return null;
    }

    /**
     * The data binder reaches a nested model, an indexed list element and a mapped value by the component's Java
     * name, and fills them in place. Measured: a renamed one loses its values, a component wrapped in
     * {@code JsonNullable} or typed {@code Object} is something Spring tries to construct, and a container with no
     * initial value has nothing to write into. Only a scalar can carry another name, through {@code BindParam}.
     */
    private String reasonForParameterObjectComponent(CodegenProperty cp) {
        boolean modelOrContainer = cp.isModel || cp.isContainer;
        if (modelOrContainer && !cp.baseName.equals(cp.name)) {
            return "property '" + cp.name + "' is a model or a container whose documented name differs from its Java name";
        }
        // the condition record.mustache writes the JsonNullable wrapper under (nullableDataType.mustache)
        if (openApiNullable && cp.isNullable) {
            return "property '" + cp.name + (modelOrContainer ? "' is a nullable model or container"
                    : "' is wrapped in JsonNullable");
        }
        if (cp.isAnyType || cp.isFreeFormObject || "Object".equals(cp.dataType)) {
            return "property '" + cp.name + "' has no fixed type";
        }
        if (cp.isContainer && recordInitialValue(cp) == null) {
            return "property '" + cp.name + "' is a container without an initial value";
        }
        if (cp.isModel && !cp.isContainer && recordInitialValue(cp) != null) {
            // the class's field starts as the default's object and the binder writes the request's values into it;
            // a record has no such object: the binder constructs a fresh one and the default is lost
            return "property '" + cp.name + "' is a model with a default, which the data binder fills in place only in a class";
        }
        return null;
    }

    /**
     * The expression the class gives the field of this property in {@code pojo.mustache}, or null when the
     * field has no initialiser (the record then keeps whatever it is given). Mirrors the field declaration there.
     */
    private String recordInitialValue(CodegenProperty cp) {
        if (openApiNullable && cp.isNullable) {
            return "JsonNullable.<" + cp.datatypeWithEnum + ">undefined()";
        }
        if (!openApiNullable && !cp.isContainer && cp.isNullable) {
            return null;
        }
        return isNotEmpty(cp.defaultValue) ? cp.defaultValue : null;
    }

    @Override
    public ModelsMap postProcessModelsEnum(ModelsMap objs) {
        objs = super.postProcessModelsEnum(objs);

        // Add imports for Jackson
        final List<Map<String, String>> imports = objs.getImports();
        for (ModelMap mo : objs.getModels()) {
            CodegenModel cm = mo.getModel();
            boolean addNullableImports = false;
            for (CodegenProperty var : cm.vars) {
                addNullableImports = isAddNullableImports(cm, addNullableImports, var);
            }
            if (cm.isEnum && cm.allowableValues != null) {
                cm.imports.add(importMapping.get("JsonValue"));
                final Map<String, String> item = new HashMap<>();
                item.put("import", importMapping.get("JsonValue"));
                imports.add(item);
            }
            if (addNullableImports) {
                Map<String, String> imports2Classnames = new HashMap<>();
                imports2Classnames.put("NoSuchElementException", "java.util.NoSuchElementException");
                addImports(imports, cm, imports2Classnames);
            }
        }

        if (useEnumValueInterface) {
            EnumValueInterfaceUtils.injectInPostProcessModelsEnum(
                    objs, valuedEnumClassName, importMapping.get("ValuedEnum"),
                    CodegenConstants.X_IMPLEMENTS);
        }

        return objs;
    }

    @Override
    public void setPerformBeanValidation(boolean performBeanValidation) {
        this.performBeanValidation = performBeanValidation;
    }

    @Override
    public void setUseOptional(boolean useOptional) {
        this.useOptional = useOptional;
    }

    @Override
    public void setUseSwaggerUI(boolean useSwaggerUI) {
        this.useSwaggerUI = useSwaggerUI;
    }

    @Override
    public List<VendorExtension> getSupportedVendorExtensions() {
        List<VendorExtension> extensions = super.getSupportedVendorExtensions();
        extensions.add(VendorExtension.X_OPERATION_EXTRA_ANNOTATION);
        extensions.add(VendorExtension.X_SPRING_PAGINATED);
        extensions.add(VendorExtension.X_VERSION_PARAM);
        extensions.add(VendorExtension.X_PATTERN_MESSAGE);
        extensions.add(VendorExtension.X_SIZE_MESSAGE);
        extensions.add(VendorExtension.X_MINIMUM_MESSAGE);
        extensions.add(VendorExtension.X_MAXIMUM_MESSAGE);
        extensions.add(VendorExtension.X_SPRING_API_VERSION);
        extensions.add(VendorExtension.X_JACKSON_DEFAULT_IMPL);
        extensions.add(VendorExtension.X_JACKSON_JSON_INCLUDE_POLICY);
        extensions.add(VendorExtension.X_JACKSON_JSON_SETTER_NULLS);
        return extensions;
    }

    @Override
    protected boolean useBeanValidationOnMapValueType() {
        // The Spring templates place container element validation on the type argument
        // (List<@Valid T>, Map<String, @Valid V>) rather than the deprecated
        // container-level @Valid (HV000271).
        return true;
    }

    protected boolean isSpringCodegen() {
        return getName().contains("spring");
    }

    private void addSpringNullableImport(Set<String> imports) {
        if (isSpringCodegen()) {
            imports.add("Nullable");
        }
    }
}
