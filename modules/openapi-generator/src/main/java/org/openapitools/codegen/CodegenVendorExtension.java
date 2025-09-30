package org.openapitools.codegen;

import lombok.Getter;

@Getter
public enum CodegenVendorExtension {

    X_PARENT("x-parent"),
    X_INTERNAL("x-internal");

    private final String name;

    CodegenVendorExtension(final String name) {
        this.name = name;
    }

}
