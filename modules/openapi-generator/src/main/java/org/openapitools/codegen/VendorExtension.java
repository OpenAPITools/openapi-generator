package org.openapitools.codegen;

import java.util.Collections;
import java.util.List;

public enum VendorExtension {

    X_IMPLEMENTS("x-implements", ExtensionLevel.MODEL, "Ability to specify interfaces that model must implements", "empty array"),
    X_SPRING_PAGINATED("x-spring-paginated", ExtensionLevel.OPERATION, "Add org.springframework.data.domain.Pageable to controller method. Can be used to handle page & size query parameters", "false"),
    X_DISCRIMINATOR_VALUE("x-discriminator-value", ExtensionLevel.MODEL, "Used with model inheritance to specify value for discriminator that identifies current model", ""),
    X_SETTER_EXTRA_ANNOTATION("x-setter-extra-annotation", ExtensionLevel.FIELD, "Custom annotation that can be specified over java setter for specific field", "When field is array & uniqueItems, then this extension is used to add `@JsonDeserialize(as = LinkedHashSet.class)` over setter, otherwise no value"),
    X_WEBCLIENT_BLOCKING("x-webclient-blocking", ExtensionLevel.OPERATION, "Specifies if method for specific operation should be blocking or non-blocking(ex: return `Mono<T>/Flux<T>` or `return T/List<T>/Set<T>` & execute `.block()` inside generated method)", "false")
    ;

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

    public String getName() {
        return name;
    }

    public List<ExtensionLevel> getLevels() {
        return levels;
    }

    public String getDescription() {
        return description;
    }

    public String getDefaultValue() {
        return defaultValue;
    }

    public enum ExtensionLevel {
        FIELD,
        MODEL,
        OPERATION
    }

}
