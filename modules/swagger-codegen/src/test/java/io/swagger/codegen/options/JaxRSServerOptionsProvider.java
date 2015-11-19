package io.swagger.codegen.options;

public class JaxRSServerOptionsProvider extends JavaOptionsProvider {
    @Override
    public boolean isServer() {
        return true;
    }

    @Override
    public String getLanguage() {
        return "jaxrs";
    }
}
