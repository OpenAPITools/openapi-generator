package org.openapitools.codegen.v2;

import org.yaml.snakeyaml.Yaml;

import javax.annotation.Nonnull;
import java.util.*;
import java.util.stream.Collectors;

public abstract class CodegenObject implements CodegenTaggable, Comparable<CodegenObject> {

    private final String id;
    private final CodegenObject parent;
    private final Set<CodegenObject> children;
    private final Map<CodegenTag, Object> tags;

    protected CodegenObject(String id) {
        this(id, null);
    }

    protected CodegenObject(String id, CodegenObject parent) {
        this.id = Objects.requireNonNull(id);
        this.parent = parent;
        this.children = new TreeSet<>();
        this.tags = new TreeMap<>();
    }

    @Override
    public String toString() {
        return new Yaml().dump(this);
    }

    @Override
    public Map<CodegenTag, Object> getTags() {
        return tags;
    }

    @Override
    public int compareTo(@Nonnull CodegenObject object) {
        return id.compareTo(object.getId());
    }

    @Override
    public boolean equals(@Nonnull Object obj) {
        if (obj instanceof CodegenObject) {
            CodegenObject object = (CodegenObject) obj;
            return Objects.equals(id, object.getId());
        }
        return false;
    }

    public String getId() {
        return id;
    }

    public CodegenObject getParent() {
        return parent;
    }

    public Collection<CodegenObject> getChildren() {
        return children;
    }

    public <TCodegenObject extends CodegenObject> Collection<TCodegenObject> getChildrenOfType(Class<TCodegenObject> objectClass) {
        Objects.requireNonNull(objectClass);
        return children.stream()
                .filter(o -> getClass().isAssignableFrom(objectClass))
                .map(objectClass::cast)
                .collect(Collectors.toList());
    }

    protected void addChild(CodegenObject object) {
        Objects.requireNonNull(object);
        children.add(object);
    }
}
