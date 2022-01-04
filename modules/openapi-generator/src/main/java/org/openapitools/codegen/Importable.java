package org.openapitools.codegen;

import java.util.List;

public interface Importable {
    String getBaseType();
    String getComplexType();

    boolean isContainer();

    CodegenComposedSchemas getComposedSchemas();

    Importable getInner();
}
