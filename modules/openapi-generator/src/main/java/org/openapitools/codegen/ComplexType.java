package org.openapitools.codegen;

import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Stream;

public interface ComplexType {
    String getBaseType();
    String getComplexType();

    boolean isContainer();

    CodegenComposedSchemas getComposedSchemas();

    ComplexType getInner();

    /**
     * Recursively collect all necessary imports to include in order to resolve this type.
     *
     * @param includeContainerTypes
     * @return
     */
    default Set<String> getImports(boolean includeContainerTypes) {
        Set<String> imports = new HashSet<>();
            if (includeContainerTypes || !this.isContainer()) {
                if (this.getComposedSchemas() != null) {
                    CodegenComposedSchemas composed = (CodegenComposedSchemas) this.getComposedSchemas();
                    List<CodegenProperty> allOfs =  composed.getAllOf() == null ? Collections.emptyList() : composed.getAllOf();
                    List<CodegenProperty> oneOfs =  composed.getOneOf() == null ? Collections.emptyList() : composed.getOneOf();
                    Stream<CodegenProperty> innerTypes = Stream.concat(allOfs.stream(), oneOfs.stream());
                    innerTypes.flatMap(cp -> cp.getImports(includeContainerTypes).stream()).forEach(s -> imports.add(s));
                } else {
                    imports.add(this.getComplexType());
                    imports.add(this.getBaseType());
                }
            }
        if (this.getInner() !=null) {
            imports.addAll(this.getInner().getImports(includeContainerTypes));
        }
        return imports;
    }
}
