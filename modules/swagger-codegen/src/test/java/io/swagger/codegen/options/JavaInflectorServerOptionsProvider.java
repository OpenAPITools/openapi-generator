package io.swagger.codegen.options;

public class JavaInflectorServerOptionsProvider extends JavaOptionsProvider {
    @Override
    public String getLanguage() {
        return "inflector";
    }

    @Override
    public boolean isServer() {
        return true;
    }
}
