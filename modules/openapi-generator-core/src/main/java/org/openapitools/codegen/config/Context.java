package org.openapitools.codegen.config;

// TODO
public class Context<T extends ApiDocument> {
    private T specDocument;
    private GeneratorSettings settings;

    public Context(T specDocument, GeneratorSettings settings) {
        this.specDocument = specDocument;
        this.settings = settings;
    }

    public GeneratorSettings getSettings() {
        return settings;
    }

    public T getSpecDocument() {
        return specDocument;
    }
}
