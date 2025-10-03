package org.openapitools.codegen;

import lombok.Getter;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

@Getter
public enum VendorExtension {

    X_IMPLEMENTS("x-implements", ExtensionLevel.MODEL, "Ability to specify interfaces that model must implements", "empty array"),
    X_KOTLIN_IMPLEMENTS("x-kotlin-implements", ExtensionLevel.MODEL, "Ability to specify interfaces that model must implement", "empty array"),
    X_KOTLIN_IMPLEMENTS_FIELDS("x-kotlin-implements-fields", ExtensionLevel.MODEL, "Specify attributes that are implemented by the interface(s) added via `x-kotlin-implements`", "empty array"),
    X_SPRING_PAGINATED("x-spring-paginated", ExtensionLevel.OPERATION, "Add `org.springframework.data.domain.Pageable` to controller method. Can be used to handle `page`, `size` and `sort` query parameters. If these query parameters are also specified in the operation spec, they will be removed from the controller method as their values can be obtained from the `Pageable` object.", "false"),
    X_SPRING_API_VERSION("x-spring-api-version", ExtensionLevel.OPERATION, "Value for 'version' attribute in @RequestMapping (for Spring 7 and above).", null),
    X_SPRING_PROVIDE_ARGS("x-spring-provide-args", ExtensionLevel.OPERATION, "Allows adding additional hidden parameters in the API specification to allow access to content such as header values or properties", "empty array"),
    X_DISCRIMINATOR_VALUE("x-discriminator-value", ExtensionLevel.MODEL, "Used with model inheritance to specify value for discriminator that identifies current model", ""),
    X_SETTER_EXTRA_ANNOTATION("x-setter-extra-annotation", ExtensionLevel.FIELD, "Custom annotation that can be specified over java setter for specific field", "When field is array & uniqueItems, then this extension is used to add `@JsonDeserialize(as = LinkedHashSet.class)` over setter, otherwise no value"),
    X_WEBCLIENT_BLOCKING("x-webclient-blocking", ExtensionLevel.OPERATION, "Specifies if method for specific operation should be blocking or non-blocking(ex: return `Mono<T>/Flux<T>` or `return T/List<T>/Set<T>` & execute `.block()` inside generated method)", "false"),
    X_WEBCLIENT_RETURN_EXCEPT_LIST_OF_STRING("x-webclient-return-except-list-of-string", ExtensionLevel.OPERATION, "Specifies if method for specific operation should return the type except List<String> and Set<String>(ex: return type expect the `Mono<List<String>>/Flux<List<String>>` and `Mono<Set<String>>/Flux<Set<String>>`)", "false"),
    X_TAGS("x-tags", ExtensionLevel.OPERATION, "Specify multiple swagger tags for operation", null),
    X_ACCEPTS("x-accepts", ExtensionLevel.OPERATION, "Specify custom value for 'Accept' header for operation", null),
    X_CONTENT_TYPE("x-content-type", ExtensionLevel.OPERATION, "Specify custom value for 'Content-Type' header for operation", null),
    X_CLASS_EXTRA_ANNOTATION("x-class-extra-annotation", ExtensionLevel.MODEL, "List of custom annotations to be added to model", null),
    X_FIELD_EXTRA_ANNOTATION("x-field-extra-annotation", Arrays.asList(ExtensionLevel.FIELD, ExtensionLevel.OPERATION_PARAMETER), "List of custom annotations to be added to property", null),
    X_OPERATION_EXTRA_ANNOTATION("x-operation-extra-annotation", ExtensionLevel.OPERATION, "List of custom annotations to be added to operation", null),
    X_VERSION_PARAM("x-version-param", ExtensionLevel.OPERATION_PARAMETER, "Marker property that tells that this parameter would be used for endpoint versioning. Applicable for headers & query params. true/false", null),
    X_PATTERN_MESSAGE("x-pattern-message", Arrays.asList(ExtensionLevel.FIELD, ExtensionLevel.OPERATION_PARAMETER), "Add this property whenever you need to customize the invalidation error message for the regex pattern of a variable", null),
    X_ZERO_BASED_ENUM("x-zero-based-enum", ExtensionLevel.MODEL, "When used on an enum, the index will not be generated and the default numbering will be used, zero-based", "false");

    private final String name;
    private final List<ExtensionLevel> levels;
    private final String description;
    private final String defaultValue;

    VendorExtension(final String name, final List<ExtensionLevel> levels, final String description, final String defaultValue) {
        this.name = name;
        this.levels = levels;
        this.description = description;
        this.defaultValue = defaultValue;
    }

    VendorExtension(final String name, final ExtensionLevel level, final String description, final String defaultValue) {
        this(name, Collections.singletonList(level), description, defaultValue);
    }

    public enum ExtensionLevel {
        FIELD,
        MODEL,
        OPERATION,
        OPERATION_PARAMETER
    }

}
