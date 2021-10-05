package org.openapitools.codegen.validations.oas;

/**
 * Allows for configuration of validation rules which will be applied to a specification.
 */
@SuppressWarnings({"WeakerAccess", "unused"})
public class RuleConfiguration {
    private static String propertyPrefix = "openapi.generator.rule";
    private boolean enableRecommendations = defaultedBoolean(propertyPrefix + ".recommendations", true);
    private boolean enableApacheNginxUnderscoreRecommendation = defaultedBoolean(propertyPrefix + ".apache-nginx-underscore", true);
    private boolean enableOneOfWithPropertiesRecommendation = defaultedBoolean(propertyPrefix + ".oneof-properties-ambiguity", true);
    private boolean enableUnusedSchemasRecommendation = defaultedBoolean(propertyPrefix + ".unused-schemas", true);
    private boolean enableSchemaTypeRecommendation = defaultedBoolean(propertyPrefix + ".schema-type", true);
    private boolean enableNullableAttributeRecommendation = defaultedBoolean(propertyPrefix + ".nullable-deprecated", true);
    private boolean enableInvalidTypeRecommendation = defaultedBoolean(propertyPrefix + ".invalid-type", true);

    private boolean enableApiRequestUriWithBodyRecommendation = defaultedBoolean(propertyPrefix + ".anti-patterns.uri-unexpected-body", true);

    @SuppressWarnings("SameParameterValue")
    private static boolean defaultedBoolean(String key, boolean defaultValue) {
        String property = System.getProperty(key);
        if (property == null) return defaultValue;
        return Boolean.parseBoolean(property);
    }

    /**
     * Gets whether we will raise awareness that header parameters with underscore may be ignored in Apache or Nginx by default.
     * For more details, see https://stackoverflow.com/a/22856867/151445.
     *
     * @return <code>true</code> if enabled, <code>false</code> if disabled
     */
    public boolean isEnableApacheNginxUnderscoreRecommendation() {
        return enableApacheNginxUnderscoreRecommendation;
    }

    /**
     * Enable or Disable the recommendation check for Apache/Nginx potentially ignoring header with underscore by default.
     * <p>
     * For more details, see {@link RuleConfiguration#isEnableApacheNginxUnderscoreRecommendation()}
     *
     * @param enableApacheNginxUnderscoreRecommendation <code>true</code> to enable, <code>false</code> to disable
     */
    public void setEnableApacheNginxUnderscoreRecommendation(boolean enableApacheNginxUnderscoreRecommendation) {
        this.enableApacheNginxUnderscoreRecommendation = enableApacheNginxUnderscoreRecommendation;
    }

    /**
     * Gets whether we will raise awareness a GET or HEAD operation is defined with body.
     *
     * @return <code>true</code> if enabled, <code>false</code> if disabled
     */
    public boolean isEnableApiRequestUriWithBodyRecommendation() {
        return enableApiRequestUriWithBodyRecommendation;
    }

    /**
     * Enable or Disable the recommendation check for GET or HEAD operations with bodies.
     * <p>
     * For more details, see {@link RuleConfiguration#isEnableApiRequestUriWithBodyRecommendation()}
     *
     * @param enableApiRequestUriWithBodyRecommendation <code>true</code> to enable, <code>false</code> to disable
     */
    public void setEnableApiRequestUriWithBodyRecommendation(boolean enableApiRequestUriWithBodyRecommendation) {
        this.enableApiRequestUriWithBodyRecommendation = enableApiRequestUriWithBodyRecommendation;
    }

    /**
     * Gets whether the recommendation check for oneOf with sibling properties exists.
     * <p>
     * JSON Schema defines oneOf as a validation property which can be applied to any schema.
     * <p>
     * OpenAPI Specification is a variant of JSON Schema for which oneOf is defined as:
     * "Inline or referenced schema MUST be of a Schema Object and not a standard JSON Schema."
     * <p>
     * Where the only examples of oneOf in OpenAPI Specification are used to define either/or type structures rather than validations.
     * Because of this ambiguity in the spec about what is non-standard about oneOf support, we'll warn as a recommendation that
     * properties on the schema defining oneOf relationships may not be intentional in the OpenAPI Specification.
     *
     * @return <code>true</code> if enabled, <code>false</code> if disabled
     */
    public boolean isEnableOneOfWithPropertiesRecommendation() {
        return enableOneOfWithPropertiesRecommendation;
    }

    /**
     * Enable or Disable the recommendation check for schemas containing properties and oneOf definitions.
     * <p>
     * For more details, see {@link RuleConfiguration#isEnableOneOfWithPropertiesRecommendation()}
     *
     * @param enableOneOfWithPropertiesRecommendation <code>true</code> to enable, <code>false</code> to disable
     */
    public void setEnableOneOfWithPropertiesRecommendation(boolean enableOneOfWithPropertiesRecommendation) {
        this.enableOneOfWithPropertiesRecommendation = enableOneOfWithPropertiesRecommendation;
    }

    /**
     * Enable or Disable the recommendation check for schemas containing type definitions, specifically
     * for changes between OpenAPI 3.0.x and 3.1.
     *
     * <p>
     * For more details, see {@link RuleConfiguration#isEnableSchemaTypeRecommendation()}
     *
     * @param enableSchemaTypeRecommendation <code>true</code> to enable, <code>false</code> to disable
     */
    public void setEnableSchemaTypeRecommendation(boolean enableSchemaTypeRecommendation) {
        this.enableSchemaTypeRecommendation = enableSchemaTypeRecommendation;
    }

    /**
     * Gets whether the recommendation check for for schemas containing type definitions.
     * <p>
     * In OpenAPI 3.0.x, the "type" attribute must be a string value.
     * In OpenAPI 3.1, the type attribute may be:
     *   A string value
     *   The 'null' value
     *   An array containing primitive types and the 'null' value.
     *
     * @return <code>true</code> if enabled, <code>false</code> if disabled
     */
    public boolean isEnableSchemaTypeRecommendation() {
        return enableSchemaTypeRecommendation;
    }

    /**
     * Enable or Disable the recommendation check for the 'nullable' attribute.
     *
     * <p>
     * For more details, see {@link RuleConfiguration#isEnableNullableAttributeRecommendation()}
     *
     * @param enableNullableAttributeRecommendation <code>true</code> to enable, <code>false</code> to disable
     */
    public void setEnableNullableAttributeRecommendation(boolean enableNullableAttributeRecommendation) {
        this.enableNullableAttributeRecommendation = enableNullableAttributeRecommendation;
    }

    /**
     * Gets whether the recommendation check for for schemas containing the 'nullable' attribute.
     * <p>
     * In OpenAPI 3.0.x, the "nullable" attribute is supported. However, because it is deprecated in 3.1
     * and above, a warning is logged to prepare for OpenAPI 3.1 recommendations.
     * In OpenAPI 3.1, the 'nullable' attribute is deprecated. Instead the OpenAPI specification should
     * use the 'null' type.
     *
     * @return <code>true</code> if enabled, <code>false</code> if disabled
     */
    public boolean isEnableNullableAttributeRecommendation() {
        return enableNullableAttributeRecommendation;
    }

    /**
     * Enable or Disable the recommendation check for the 'type' attribute.
     *
     * <p>
     * For more details, see {@link RuleConfiguration#isEnableInvalidTypeRecommendation()}
     *
     * @param enableInvalidTypeRecommendation <code>true</code> to enable, <code>false</code> to disable
     */
    public void setEnableInvalidTypeRecommendation(boolean enableInvalidTypeRecommendation) {
        this.enableInvalidTypeRecommendation = enableInvalidTypeRecommendation;
    }

    /**
     * Gets whether the recommendation check for for schemas containing invalid values for the 'type' attribute.
     * <p>
     *
     * @return <code>true</code> if enabled, <code>false</code> if disabled
     */
    public boolean isEnableInvalidTypeRecommendation() {
        return enableInvalidTypeRecommendation;
    }

    /**
     * Get whether recommendations are enabled.
     * <p>
     * The 'type' attribute must be one of 'null', 'boolean', 'object', 'array', 'number', 'string', or 'integer'
     *
     * @return <code>true</code> if enabled, <code>false</code> if disabled
     */
    public boolean isEnableRecommendations() {
        return enableRecommendations;
    }

    /**
     * Enable or Disable recommendations. Recommendations are either informational or warning level type validations
     * which are raised to communicate issues to the user which they may not be aware of, or for which support in the
     * tooling/spec may not be clearly defined.
     *
     * @param enableRecommendations <code>true</code> to enable, <code>false</code> to disable
     */
    public void setEnableRecommendations(boolean enableRecommendations) {
        this.enableRecommendations = enableRecommendations;
    }

    /**
     * Gets whether the recommendation to check for unused schemas is enabled.
     * <p>
     * While the tooling may or may not support generation of models representing unused schemas, we take the stance that
     * a schema which is defined but not referenced in an operation or by some schema bound to an operation may be a good
     * indicator of a programming error. We surface this information to the user in case the orphaned schema(s) are not
     * intentional.
     *
     * @return <code>true</code> if enabled, <code>false</code> if disabled
     */
    public boolean isEnableUnusedSchemasRecommendation() {
        return enableUnusedSchemasRecommendation;
    }

    /**
     * Enable or Disable the recommendation check for unused schemas.
     * <p>
     * For more details, see {@link RuleConfiguration#isEnableUnusedSchemasRecommendation()}
     *
     * @param enableUnusedSchemasRecommendation <code>true</code> to enable, <code>false</code> to disable
     */
    public void setEnableUnusedSchemasRecommendation(boolean enableUnusedSchemasRecommendation) {
        this.enableUnusedSchemasRecommendation = enableUnusedSchemasRecommendation;
    }
}
