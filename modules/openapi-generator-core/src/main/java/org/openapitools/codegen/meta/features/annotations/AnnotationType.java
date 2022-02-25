package org.openapitools.codegen.meta.features.annotations;

public enum AnnotationType {
    OAS2, OAS3, ToolingExtension;

    public static AnnotationType fromAnnotation(Class<?> input) {
        if(input ==  OAS2.class) return AnnotationType.OAS2;
        if(input == OAS3.class) return AnnotationType.OAS3;
        if(input == ToolingExtension.class) return AnnotationType.ToolingExtension;
        return null;
    }
}
