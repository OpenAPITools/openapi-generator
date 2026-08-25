package org.openapitools.codegen.utils;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.media.Discriminator;
import io.swagger.v3.oas.models.media.Schema;
import org.jspecify.annotations.Nullable;
import org.openapitools.codegen.CodegenProperty;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.helpers.MessageFormatter;

import java.util.*;

import static org.openapitools.codegen.CodegenConstants.X_DISCRIMINATOR_VALUE;
import static org.openapitools.codegen.utils.OnceLogger.once;

public class DiscriminatorUtils {

    private static final Logger LOGGER = LoggerFactory.getLogger(DiscriminatorUtils.class);

    private static final String CONFLICTING_DISCRIMINATOR_NAMES =
            "The alternative schemas have conflicting discriminator property names. The schemas must have the same property name, but found {}";
    private static final String CONFLICTING_DISCRIMINATOR_TYPES =
            "The alternative schemas have conflicting discriminator property types ({})";
    private static final String DEFINES_DISCRIMINATOR_BUT_REFERENCE_ALTERNATIVE_IS_MISSING =
            "'{}' defines discriminator '{}', but the referenced schema '{}' is missing {}";
    private static final String DEFINES_DISCRIMINATOR_BUT_ALTERNATIVE_HAS_OTHER_DEFINITION =
            "'{}' defines discriminator '{}', but the schema '{}' has a different {} definition than the prior schema's. Make sure the {} type and required values are the same";
    private static final String DEFINES_DISCRIMINATOR_BUT_REFERENCE_IS_INCORRECT =
            "'{}' defines discriminator '{}', but the referenced schema '{}' is incorrect. {}";
    /**
     * Recursively look in Schema sc for the discriminator discPropName
     * and return a CodegenProperty with the dataType and required params set
     * the returned CodegenProperty may not be required, and it may not be of type string
     *
     * @param openAPI            The openAPI specification
     * @param composedSchemaName The name of the sc Schema
     * @param sc                 The Schema that may contain the discriminator
     * @param discPropName       The String that is the discriminator propertyName in the schema
     * @param visitedSchemas     A set of visited schema names
     */
    public static CodegenProperty discriminatorFound(OpenAPI openAPI,
                                                     String composedSchemaName,
                                                     Schema sc,
                                                     String discPropName,
                                                     Set<String> visitedSchemas) {
        Schema refSchema = ModelUtils.getReferencedSchema(openAPI, sc);
        String schemaName = Optional.ofNullable(composedSchemaName)
                .or(() -> Optional.ofNullable(refSchema.getName()))
                .or(() -> Optional.ofNullable(sc.get$ref()).map(ModelUtils::getSimpleRef))
                .orElseGet(sc::toString);
        if (visitedSchemas.contains(schemaName)) { // recursive schema definition found
            return null;
        } else {
            visitedSchemas.add(schemaName);
        }

        if (refSchema.getProperties() != null && refSchema.getProperties().get(discPropName) != null) {
            Schema discSchema = ModelUtils.getReferencedSchema(openAPI, getDiscriminatorSchema(refSchema, discPropName));
            CodegenProperty cp = new CodegenProperty();
            if (ModelUtils.isStringSchema(discSchema)) {
                cp.isString = true;
            }
            cp.setRequired(false);
            if (refSchema.getRequired() != null && refSchema.getRequired().contains(discPropName)) {
                cp.setRequired(true);
            }
            cp.setIsEnum(discSchema.getEnum() != null && !discSchema.getEnum().isEmpty());
            return cp;
        }
        if (ModelUtils.isComposedSchema(refSchema)) {
            Schema composedSchema = refSchema;
            if (composedSchema.getAllOf() != null) {
                // If our discriminator is in one of the allOf schemas break when we find it
                for (Object allOf : composedSchema.getAllOf()) {
                    Schema allOfSchema = (Schema) allOf;
                    CodegenProperty cp = discriminatorFound(openAPI, allOfSchema.getName(), allOfSchema, discPropName, visitedSchemas);
                    if (cp != null) {
                        return cp;
                    }
                }
            }
            if (ModelUtils.hasOneOf(composedSchema)) {
                // All oneOf definitions must contain the discriminator
                CodegenProperty cp = new CodegenProperty();
                for (Object oneOf : composedSchema.getOneOf()) {
                    Schema oneOfSchema = (Schema) oneOf;
                    CodegenProperty discCP = getDiscriminatorCodegenProperty(openAPI, cp, composedSchemaName, oneOfSchema, discPropName, visitedSchemas);
                    if (discCP != null) {
                        cp = discCP;
                    }
                }
                return cp;
            }
            if (ModelUtils.hasAnyOf(composedSchema)) {
                // All anyOf definitions must contain the discriminator because a min of one must be selected
                CodegenProperty cp = new CodegenProperty();
                for (Object anyOf : composedSchema.getAnyOf()) {
                    Schema anyOfSchema = (Schema) anyOf;
                    CodegenProperty discCP = getDiscriminatorCodegenProperty(openAPI, cp, composedSchemaName, anyOfSchema, discPropName, visitedSchemas);
                    if (discCP != null) {
                        cp = discCP;
                    }
                }
                return cp;

            }
        }
        return null;
    }

    /**
     * Gets the simple ref name of the discriminator property type from the schema.
     *
     * @param schema                    The input OAS schema.
     * @param discriminatorPropertyName The name of the discriminator property.
     * @return referenced type name, or an empty optional if unavailable
     */
    public static Optional<String> getDiscriminatorPropertyType(Schema schema, String discriminatorPropertyName) {
        return Optional.ofNullable(getDiscriminatorSchema(schema, discriminatorPropertyName))
                .map(Schema::get$ref)
                .map(ModelUtils::getSimpleRef);
    }

    /**
     * Recursively look in Schema sc for the discriminator and return it
     *
     * @param openAPI                     the openAPI specification
     * @param legacyDiscriminatorBehavior whether legacy discriminator behavior is enabled
     * @param sc                          The Schema that may contain the discriminator
     * @param visitedSchemas              an array list of visited schemas
     */
    public static DiscriminatorData recursiveGetDiscriminator(
            OpenAPI openAPI,
            boolean legacyDiscriminatorBehavior,
            Schema sc,
            ArrayList<Schema> visitedSchemas) {
        Schema refSchema = ModelUtils.getReferencedSchema(openAPI, sc);
        DiscriminatorData foundDisc = new DiscriminatorData(refSchema.getDiscriminator(), null);
        if (foundDisc.getDiscriminator() != null) {
            String discriminatorPropertyName = foundDisc.getDiscriminator().getPropertyName();
            return new DiscriminatorData(
                    foundDisc.getDiscriminator(),
                    DiscriminatorUtils.getDiscriminatorSchema(refSchema, discriminatorPropertyName)
            );
        }

        if (legacyDiscriminatorBehavior) {
            return null;
        }

        for (Schema s : visitedSchemas) {
            if (s == refSchema) {
                return null;
            }
        }
        visitedSchemas.add(refSchema);

        Discriminator disc = new Discriminator();
        if (ModelUtils.isComposedSchema(refSchema)) {
            Schema composedSchema = refSchema;
            if (composedSchema.getAllOf() != null) {
                // If our discriminator is in one of the allOf schemas break when we find it
                for (Object allOf : composedSchema.getAllOf()) {
                    foundDisc = recursiveGetDiscriminator(openAPI, legacyDiscriminatorBehavior, (Schema) allOf, visitedSchemas);
                    if (foundDisc != null) {
                        disc.setPropertyName(foundDisc.getPropertyName());
                        disc.setMapping(foundDisc.getMapping());
                        return new DiscriminatorData(disc, foundDisc.getDiscriminatorSchema());
                    }
                }
            }
            if (ModelUtils.hasOneOf(composedSchema)) {
                foundDisc = getDiscriminatorFromAlternatives(openAPI, legacyDiscriminatorBehavior, composedSchema.getOneOf(), visitedSchemas);
                if (foundDisc != null) {
                    return foundDisc;
                }
            }
            if (ModelUtils.hasAnyOf(composedSchema)) {
                return getDiscriminatorFromAlternatives(openAPI, legacyDiscriminatorBehavior, composedSchema.getAnyOf(), visitedSchemas);
            }
        }
        return null;
    }

    /**
     * Get the value of the vendor extension {@code x-discriminator-value} from the schema, if present.
     * @param schema the schema to check for the vendor extension
     * @return the value of the vendor extension, or an empty optional if not present
     */
    public static Optional<String> discriminatorVendorExtensionValue(Schema schema) {
        return Optional.ofNullable(schema)
                .map(Schema::getExtensions)
                .map(vendorExtensions -> vendorExtensions.get(X_DISCRIMINATOR_VALUE))
                .map(discriminatorValue -> (String) discriminatorValue);
    }

    public static String getDiscriminatorSchemaError(CodegenProperty codegenProperty, String discPropName,
                                                     String modelName, String composedSchemaName) {
        String msgSuffix = "";
        if (codegenProperty == null) {
            msgSuffix += discPropName + " is missing from the schema, define it as required and type string";
        } else {
            if (!codegenProperty.isString) {
                msgSuffix += "invalid type for " + discPropName + ", set it to string";
            }
            if (!codegenProperty.required) {
                String spacer = "";
                if (!msgSuffix.isEmpty()) {
                    spacer = ". ";
                }
                msgSuffix += spacer + "invalid optional definition of " + discPropName + ", include it in required";
            }
        }
        return MessageFormatter.arrayFormat(DEFINES_DISCRIMINATOR_BUT_REFERENCE_IS_INCORRECT,
                new Object[]{composedSchemaName, discPropName, modelName, msgSuffix}).getMessage();
    }

    /**
     * Get the Schema for the discriminator type. Requires special handling due to siblings from OAS 3.1.
     * An example of a sibling is an enum-ref that has its own description. This will lead to the enum being
     * referenced as an allOf that in turn has a ref, rather than a regular ref directly to the enum.
     *
     * @param schema            The input OAS schema that has the discriminator as a property.
     * @param discriminatorName The name of the discriminator property.
     */
    private static Schema getDiscriminatorSchema(Schema schema, String discriminatorName) {
        if (schema.getProperties() == null) {
            return null;
        }
        Schema discSchema = (Schema) schema.getProperties().get(discriminatorName);
        if (ModelUtils.isAllOf(discSchema)) {
            discSchema = (Schema) discSchema.getAllOf().get(0);
        }
        return discSchema;
    }

    /**
     * Check whether the alternative schemas share a discriminator, and if they do, return it
     *
     * @param openAPI                     the openAPI specification
     * @param legacyDiscriminatorBehavior whether legacy discriminator behavior is enabled
     * @param alternativeSchemas          list of schemas that should be checked for a shared discriminator
     * @param visitedSchemas              an array list of visited schemas
     * @return the discriminator if the alternatives correctly shares one, otherwise null
     */
    private static DiscriminatorData getDiscriminatorFromAlternatives(
            OpenAPI openAPI,
            boolean legacyDiscriminatorBehavior,
            List<Schema> alternativeSchemas,
            ArrayList<Schema> visitedSchemas) {
        Discriminator discriminator = new Discriminator();
        DiscriminatorData foundDisc = null;
        Integer hasDiscriminatorCnt = 0;
        Integer hasNullTypeCnt = 0;
        Set<String> discriminatorsPropNames = new HashSet<>();
        Set<Schema> discriminatorTypes = new HashSet<>();
        for (Object alternative : alternativeSchemas) {
            if (ModelUtils.isNullType((Schema) alternative)) {
                // The null type does not have a discriminator. Skip.
                hasNullTypeCnt++;
                continue;
            }
            foundDisc = recursiveGetDiscriminator(openAPI, legacyDiscriminatorBehavior, (Schema) alternative, visitedSchemas);
            if (foundDisc != null) {
                discriminatorsPropNames.add(foundDisc.getPropertyName());
                hasDiscriminatorCnt++;
                if (foundDisc.getDiscriminatorSchema() != null) {
                    discriminatorTypes.add(foundDisc.getDiscriminatorSchema());
                }
            }
        }
        if (discriminatorsPropNames.size() > 1) {
            once(LOGGER).warn(CONFLICTING_DISCRIMINATOR_NAMES, String.join(", ", discriminatorsPropNames));
        }
        if (discriminatorTypes.size() > 1) {
            once(LOGGER).warn(CONFLICTING_DISCRIMINATOR_TYPES, String.join(", ", discriminatorTypes.toString()));
        }
        boolean allAlternativesHaveADiscriminator = hasDiscriminatorCnt + hasNullTypeCnt == alternativeSchemas.size();
        if (foundDisc != null && allAlternativesHaveADiscriminator && discriminatorsPropNames.size() == 1) {
            discriminator.setPropertyName(foundDisc.getPropertyName());
            discriminator.setMapping(foundDisc.getMapping());
            Schema uniqueDiscriminatorType = discriminatorTypes.size() == 1 ? discriminatorTypes.iterator().next() : null;
            return new DiscriminatorData(discriminator, uniqueDiscriminatorType);
        }
        // If the scenario when composite schema has two children and one of them is the 'null' type,
        // there is no need for a discriminator.
        return null;
    }

    /**
     * Recursively look in schema for the discriminator discPropName
     *
     * @param openAPI            The openAPI specification
     * @param cp                 Any previously calculated discriminator codegen property
     * @param composedSchemaName The name of the sc Schema
     * @param schema             The Schema that may contain the discriminator
     * @param discPropName       The String that is the discriminator propertyName in the schema
     * @param visitedSchemas     A set of visited schema names
     */
    private static CodegenProperty getDiscriminatorCodegenProperty(OpenAPI openAPI,
                                                                   CodegenProperty cp,
                                                                   String composedSchemaName,
                                                                   Schema schema,
                                                                   String discPropName,
                                                                   Set<String> visitedSchemas) {
        String modelName = ModelUtils.getSimpleRef(schema.get$ref());
        // Must use a copied set as the alternative schemas can point to the same discriminator.
        Set<String> visitedSchemasCopy = new TreeSet<>(visitedSchemas);
        CodegenProperty thisCp = discriminatorFound(openAPI, schema.getName(), schema, discPropName, visitedSchemasCopy);
        if (thisCp == null) {
            once(LOGGER).warn(DEFINES_DISCRIMINATOR_BUT_REFERENCE_ALTERNATIVE_IS_MISSING,
                    composedSchemaName, discPropName, modelName, discPropName);
        }
        if (cp != null && cp.dataType == null) {
            return thisCp;
        }
        if (cp != thisCp) {
            once(LOGGER).warn(DEFINES_DISCRIMINATOR_BUT_ALTERNATIVE_HAS_OTHER_DEFINITION,
                    composedSchemaName, discPropName, modelName, discPropName, discPropName);
        }
        return null;
    }

    public static class DiscriminatorData {
        private final Discriminator discriminator;

        @Nullable
        private final Schema discriminatorSchema;

        public DiscriminatorData(Discriminator discriminator, @Nullable Schema discriminatorSchema) {
            this.discriminator = discriminator;
            this.discriminatorSchema = discriminatorSchema;
        }

        public Discriminator getDiscriminator() {
            return discriminator;
        }

        public @Nullable Schema getDiscriminatorSchema() {
            return discriminatorSchema;
        }

        public String getPropertyName() {
            return discriminator.getPropertyName();
        }

        public Map<String, String> getMapping() {
            return discriminator.getMapping();
        }

        public Map<String, Object> getExtensions() {
            return discriminator.getExtensions();
        }
    }

}
