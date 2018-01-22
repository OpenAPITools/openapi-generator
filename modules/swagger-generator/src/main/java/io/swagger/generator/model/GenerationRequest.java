    package io.swagger.generator.model;

import java.io.Serializable;
import java.util.Map;
import java.util.Objects;

public class GenerationRequest implements Serializable {
    private static final long serialVersionUID = 1L;

    private Object spec = null;
    private Options options = null;

    public GenerationRequest spec(Object spec) {
        this.spec = spec;
        return this;
    }

    public Object getSpec() {
        return spec;
    }

    public void setSpec(Object spec) {
        this.spec = spec;
    }

    public GenerationRequest options(Options options) {
        this.options = options;
        return this;
    }

    public Options getOptions() {
        return options;
    }

    public void setOptions(Options options) {
        this.options = options;
    }
}
