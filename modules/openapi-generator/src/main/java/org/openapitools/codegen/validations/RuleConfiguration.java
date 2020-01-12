package org.openapitools.codegen.validations;

public class RuleConfiguration {
    private static String propertyPrefix = "openapi.generator.rule";
    private static boolean defaultedBoolean(String key, boolean defaultValue) {
        String property = System.getProperty(key);
        if (property == null) return defaultValue;
        return Boolean.parseBoolean(property);
    }

    private boolean enableRecommendations = defaultedBoolean(propertyPrefix + ".recommendations", true);
    private boolean enableApacheNginxUnderscoreSuggestion = defaultedBoolean(propertyPrefix + ".apache-nginx-underscore", true);
    private boolean enableOneOfWithPropertiesSuggestion = defaultedBoolean(propertyPrefix + ".oneof-properties-ambiguity", true);
    private boolean enableUnusedSchemasSuggestion = defaultedBoolean(propertyPrefix + ".unused-schemas", true);

    public boolean isEnableApacheNginxUnderscoreSuggestion() {
        return enableApacheNginxUnderscoreSuggestion;
    }

    public boolean isEnableOneOfWithPropertiesSuggestion() {
        return enableOneOfWithPropertiesSuggestion;
    }

    public boolean isEnableRecommendations() {
        return enableRecommendations;
    }

    public boolean isEnableUnusedSchemasSuggestion() {
        return enableUnusedSchemasSuggestion;
    }

    public void setEnableApacheNginxUnderscoreSuggestion(boolean enableApacheNginxUnderscoreSuggestion) {
        this.enableApacheNginxUnderscoreSuggestion = enableApacheNginxUnderscoreSuggestion;
    }

    public void setEnableOneOfWithPropertiesSuggestion(boolean enableOneOfWithPropertiesSuggestion) {
        this.enableOneOfWithPropertiesSuggestion = enableOneOfWithPropertiesSuggestion;
    }

    public void setEnableRecommendations(boolean enableRecommendations) {
        this.enableRecommendations = enableRecommendations;
    }

    public void setEnableUnusedSchemasSuggestion(boolean enableUnusedSchemasSuggestion) {
        this.enableUnusedSchemasSuggestion = enableUnusedSchemasSuggestion;
    }
}
