package org.openapitools.codegen.utils;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.media.Discriminator;
import io.swagger.v3.oas.models.media.Schema;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;

import static org.openapitools.codegen.utils.OnceLogger.once;

public class DiscriminatorUtils {

    private static final Logger LOGGER = LoggerFactory.getLogger(DiscriminatorUtils.class);

    private static final String CONFLICTING_DISCRIMINATOR_NAMES =
            "The alternative schemas have conflicting discriminator property names. The schemas must have the same property name, but found {}";

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
     * Get the Schema for the discriminator type. Requires special handling due to siblings from OAS 3.1.
     * An example of a sibling is an enum-ref that has its own description. This will lead to the enum being
     * referenced as an allOf that in turn has a ref, rather than a regular ref directly to the enum.
     *
     * @param schema            The input OAS schema.
     * @param discriminatorName The name of the discriminator property.
     */
    public static Schema getDiscriminatorSchema(Schema schema, String discriminatorName) {
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
     * Recursively resolve the schema of the discriminator property, descending into composed
     * (allOf/oneOf/anyOf) schemas and following $refs. Mirrors the traversal of
     * {@link #recursiveGetDiscriminator} but returns the property {@link Schema} rather than the
     * {@link Discriminator} object, so the two recursive lookups share a single pass through the
     * schema hierarchy.
     *
     * @param openAPI                     The OpenAPI specification (used to dereference $refs).
     * @param legacyDiscriminatorBehavior Whether legacy discriminator behaviour is enabled.
     * @param schema                      The schema that may (transitively) declare the discriminator property.
     * @param discPropName                The name of the discriminator property.
     * @param visitedSchemas              Schemas already visited, to guard against cycles.
     * @return the discriminator property schema, or {@code null} if it cannot be resolved.
     */
    public static Optional<Schema> recursiveGetDiscriminatorPropertySchema(OpenAPI openAPI, boolean legacyDiscriminatorBehavior, Schema schema, String discPropName) {
        return Optional.ofNullable(recursiveGetDiscriminatorPropertySchema(openAPI, legacyDiscriminatorBehavior, schema, discPropName, new ArrayList<>()));
    }

    private static Schema recursiveGetDiscriminatorPropertySchema(
            OpenAPI openAPI,
            boolean legacyDiscriminatorBehavior,
            Schema schema,
            String discPropName,
            ArrayList<Schema> visitedSchemas) {
        Schema refSchema = ModelUtils.getReferencedSchema(openAPI, schema);
        Schema propSchema = getDiscriminatorSchema(refSchema, discPropName);
        if (propSchema != null) {
            return propSchema;
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
        if (ModelUtils.isComposedSchema(refSchema)) {
            if (refSchema.getAllOf() != null) {
                for (Object allOf : refSchema.getAllOf()) {
                    Schema found = recursiveGetDiscriminatorPropertySchema(openAPI, legacyDiscriminatorBehavior, (Schema) allOf, discPropName, visitedSchemas);
                    if (found != null) return found;
                }
            }
            if (ModelUtils.hasOneOf(refSchema)) {
                for (Object oneOf : refSchema.getOneOf()) {
                    Schema found = recursiveGetDiscriminatorPropertySchema(openAPI, legacyDiscriminatorBehavior, (Schema) oneOf, discPropName, visitedSchemas);
                    if (found != null) return found;
                }
            }
            if (ModelUtils.hasAnyOf(refSchema)) {
                for (Object anyOf : refSchema.getAnyOf()) {
                    Schema found = recursiveGetDiscriminatorPropertySchema(openAPI, legacyDiscriminatorBehavior, (Schema) anyOf, discPropName, visitedSchemas);
                    if (found != null) return found;
                }
            }
        }
        return null;
    }

    /**
     * Recursively look in Schema sc for the discriminator and return it
     *
     * @param openAPI                     the openAPI specification
     * @param legacyDiscriminatorBehavior whether legacy discriminator behavior is enabled
     * @param sc                          The Schema that may contain the discriminator
     * @param visitedSchemas              an array list of visited schemas
     */
    public static Discriminator recursiveGetDiscriminator(
            OpenAPI openAPI,
            boolean legacyDiscriminatorBehavior,
            Schema sc,
            ArrayList<Schema> visitedSchemas) {
        Schema refSchema = ModelUtils.getReferencedSchema(openAPI, sc);
        Discriminator foundDisc = refSchema.getDiscriminator();
        if (foundDisc != null) {
            return foundDisc;
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
                        return disc;
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
     * Check whether the alternative schemas share a discriminator, and if they do, return it
     *
     * @param openAPI                     the openAPI specification
     * @param legacyDiscriminatorBehavior whether legacy discriminator behavior is enabled
     * @param alternativeSchemas          list of schemas that should be checked for a shared discriminator
     * @param visitedSchemas              an array list of visited schemas
     * @return the discriminator if the alternatives correctly shares one, otherwise null
     */
    private static Discriminator getDiscriminatorFromAlternatives(
            OpenAPI openAPI,
            boolean legacyDiscriminatorBehavior,
            List<Schema> alternativeSchemas,
            ArrayList<Schema> visitedSchemas) {
        Discriminator discriminator = new Discriminator();
        Discriminator foundDisc = null;
        Integer hasDiscriminatorCnt = 0;
        Integer hasNullTypeCnt = 0;
        Set<String> discriminatorsPropNames = new HashSet<>();
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
            }
        }
        if (discriminatorsPropNames.size() > 1) {
            once(LOGGER).warn(CONFLICTING_DISCRIMINATOR_NAMES, String.join(", ", discriminatorsPropNames));
        }
        boolean allAlternativesHaveADiscriminator = hasDiscriminatorCnt + hasNullTypeCnt == alternativeSchemas.size();
        if (foundDisc != null && allAlternativesHaveADiscriminator && discriminatorsPropNames.size() == 1) {
            discriminator.setPropertyName(foundDisc.getPropertyName());
            discriminator.setMapping(foundDisc.getMapping());
            return discriminator;
        }
        // If the scenario when composite schema has two children and one of them is the 'null' type,
        // there is no need for a discriminator.
        return null;
    }

}
