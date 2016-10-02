package io.swagger.codegen.options;

/**
 * Created by steve on 18/09/16.
 */
public class JavaUndertowServerOptionsProvider extends JavaOptionsProvider {
    @Override
    public String getLanguage() {
        return "undertow";
    }

    @Override
    public boolean isServer() {
        return true;
    }
}
