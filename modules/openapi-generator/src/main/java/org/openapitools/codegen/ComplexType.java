package org.openapitools.codegen;

import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Stream;

public interface ComplexType {

    /**
      * @return basic type - no generics supported.
     */
    String getBaseType();

    /**
     * @return complex type that can contain type parameters - like {@code List<Items>} for Java
     */
    String getComplexType();

    /**
     * @return true if set, map or list.
     */
    boolean isContainer();

    /**
     * @return the composed schemas that might make up this type, null otherwise.
     */
    CodegenComposedSchemas getComposedSchemas();

    /**
     * @return the inner type in the case of a type with type parameters
     */
    ComplexType getInner();

    /**
     * Recursively collect all necessary imports to include so that the type may be resolved.
     *
     * @param includeContainerTypes
     * @return all of the imports
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
