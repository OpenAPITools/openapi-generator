package org.openapitools.codegen.utils;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.media.Discriminator;
import io.swagger.v3.oas.models.media.Schema;
import org.jspecify.annotations.Nullable;
import org.openapitools.codegen.CodegenDiscriminator.MappedModel;
import org.openapitools.codegen.CodegenProperty;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.helpers.MessageFormatter;

import java.util.*;
import java.util.stream.Collectors;

import static org.openapitools.codegen.CodegenConstants.X_DISCRIMINATOR_VALUE;
import static org.openapitools.codegen.utils.ModelUtils.isEnumSchema;
import static org.openapitools.codegen.utils.OnceLogger.once;

public class DiscriminatorUtils {

    private static final Logger LOGGER = LoggerFactory.getLogger(DiscriminatorUtils.class);

    private static final String FAILED_TO_FIND_SCHEMA =
            "Failed to lookup the schema '{}' when processing the discriminator mapping oneOf/anyOf. Please check to ensure it's defined properly.";
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
    private static final String STACK_OVERFLOW_ERROR =
            "Stack overflow hit when looking for %s, an infinite loop starting and ending at %s was seen";
    private static final String FAILED_TO_FIND_SCHEMA_WHEN_WALKING_ONE_OF_ANY_OF =
            "Failed to lookup the schema '{}' when processing oneOf/anyOf. Please check to ensure it's defined properly.";
    private static final String INVALID_SCHEMA_DEFINED =
            "Invalid inline schema defined in oneOf/anyOf in '{}'. Per the OpenApi spec, for this case when a composed schema defines a discriminator, the oneOf/anyOf schemas must use $ref. Change this inline definition to a $ref definition";

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
            cp.setIsEnum(isEnumSchema(discSchema));
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
     * Returns the discriminator descendants defined explicitly by the source discriminator mapping.
     * Each mapping entry is converted into a {@link MappedModel}, using the mapped name when a
     * discriminator value is provided, or resolving the referenced model name when the mapping points
     * to a schema reference.
     *
     * @param openAPI the OpenAPI specification
     * @param sourceDiscriminator the discriminator containing explicit mappings
     * @return the list of unique descendant models defined by the discriminator mapping
     */
    public static List<MappedModel> getUniqueDescendants(OpenAPI openAPI, Discriminator sourceDiscriminator) {
        List<MappedModel> uniqueDescendants = new ArrayList<>();
        if (sourceDiscriminator.getMapping() != null && !sourceDiscriminator.getMapping().isEmpty()) {
            for (Map.Entry<String, String> entry : sourceDiscriminator.getMapping().entrySet()) {
                String name;
                if (entry.getValue().indexOf('/') >= 0) {
                    name = ModelUtils.getSimpleRef(entry.getValue());
                    if (ModelUtils.getSchema(openAPI, name) == null) {
                        once(LOGGER).error(FAILED_TO_FIND_SCHEMA, name);
                    }
                } else {
                    name = entry.getValue();
                }
                uniqueDescendants.add(new MappedModel(entry.getKey(), name, name, true));
            }
        }
        return uniqueDescendants;
    }

    /**
     * Finds all descendant schemas that inherit from the given schema via {@code allOf}.
     * The method walks the schema graph, collects descendant models, and resolves any discriminator
     * vendor extension value when present to determine the mapping name.
     *
     * @param openAPI the OpenAPI specification
     * @param thisSchemaName the name of the schema whose descendants should be found
     * @return the list of descendant models reachable through {@code allOf}
     */
    public static List<MappedModel> getAllOfDescendants(OpenAPI openAPI,
                                                        String thisSchemaName) {
        ArrayList<String> queue = new ArrayList<>();
        List<MappedModel> descendentSchemas = new ArrayList<>();
        Map<String, Schema> schemas = ModelUtils.getSchemas(openAPI);
        String currentSchemaName = thisSchemaName;
        Set<String> keys = schemas.keySet();

        int count = 0;
        // hack: avoid infinite loop on potential self-references in event our checks fail.
        while (100000 > count++) {
            for (String childName : keys) {
                if (childName.equals(thisSchemaName)) {
                    continue;
                }
                Schema child = schemas.get(childName);
                if (ModelUtils.isComposedSchema(child)) {
                    List<Schema> parents = child.getAllOf();
                    if (parents != null) {
                        for (Schema parent : parents) {
                            String ref = parent.get$ref();
                            if (ref == null) {
                                // for schemas with no ref, it is not possible to build the discriminator map
                                // because ref is how we get the model name
                                // we hit this use case when an allOf composed schema contains an inline schema
                                continue;
                            }
                            String parentName = ModelUtils.getSimpleRef(ref);
                            if (parentName != null && parentName.equals(currentSchemaName)) {
                                if (queue.contains(childName) || descendentSchemas.stream().anyMatch(i -> childName.equals(i.getMappingName()))) {
                                    throw new RuntimeException(String.format(Locale.ROOT, STACK_OVERFLOW_ERROR, thisSchemaName, childName));
                                }
                                queue.add(childName);
                                break;
                            }
                        }
                    }
                }
            }
            if (queue.isEmpty()) {
                break;
            }
            currentSchemaName = queue.remove(0);
            Schema sc = schemas.get(currentSchemaName);
            String mappingName = discriminatorVendorExtensionValue(sc)
                            .orElse(currentSchemaName);
            MappedModel mm = new MappedModel(mappingName, currentSchemaName, currentSchemaName, !mappingName.equals(currentSchemaName));
            descendentSchemas.add(mm);
        }
        return descendentSchemas;
    }

    /**
     * This function is only used for composed schemas which have a discriminator.
     * Process {@code oneOf} and {@code anyOf} models in a composed schema and adds them into
     * a list if the {@code oneOf} and {@code anyOf} models contain the required discriminator.
     * If they don't contain the required discriminator or the discriminator is the wrong type then an error is thrown.
     *
     * @param composedSchemaName The String model name of the composed schema where we are setting the discriminator map
     * @param discPropName       The String that is the discriminator propertyName in the schema
     * @param c                  The ComposedSchema that contains the discriminator and oneOf/anyOf schemas
     * @return the list of {@code oneOf} and {@code anyOf} {@link MappedModel} that need to be added to the discriminator map
     */
    public static List<MappedModel> getOneOfAnyOfDescendants(
            OpenAPI openAPI,
            String composedSchemaName,
            String discPropName,
            Schema c) {
        ArrayList<List<Schema>> listOfAlternatives = new ArrayList<>();
        listOfAlternatives.add(c.getOneOf());
        listOfAlternatives.add(c.getAnyOf());
        List<MappedModel> descendentSchemas = new ArrayList<>();
        for (List<Schema> schemaList : listOfAlternatives) {
            if (schemaList == null) {
                continue;
            }
            for (Schema schema : schemaList) {
                if (ModelUtils.isNullType(schema)) {
                    continue;
                }
                String ref = schema.get$ref();
                if (ref == null) {
                    // for schemas with no ref, it is not possible to build the discriminator map
                    // because ref is how we get the model name
                    // we only hit this use case for a schema with inline composed schemas, and one of those
                    // schemas also has inline composed schemas
                    // Note: if it is only inline one level, then the inline model resolver will move it into its own
                    // schema and make it a $ref schema in the oneOf/anyOf location
                    once(LOGGER).warn(INVALID_SCHEMA_DEFINED, composedSchemaName);
                }
                CodegenProperty df = discriminatorFound(openAPI, composedSchemaName, schema, discPropName, new TreeSet<>());
                String modelName = ModelUtils.getSimpleRef(ref);
                if (df == null || !df.isString || !df.required) {
                    once(LOGGER).warn(getDiscriminatorSchemaError(df, discPropName, modelName, composedSchemaName));
                }
                MappedModel mm = new MappedModel(modelName, modelName, modelName, false);
                descendentSchemas.add(mm);
                Schema modelSchema = ModelUtils.getSchema(openAPI, modelName);
                if (modelSchema == null) { // cannot look up the model based on the name
                    once(LOGGER).error(FAILED_TO_FIND_SCHEMA_WHEN_WALKING_ONE_OF_ANY_OF, modelName);
                } else {
                    discriminatorVendorExtensionValue(modelSchema)
                            .ifPresent(discriminatorValue -> descendentSchemas.add(new MappedModel(discriminatorValue, modelName, modelName, true)));
                }
            }
        }
        return descendentSchemas;
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

    /**
     * Get the best matching simple types for all the mapped schemas
     * @param openAPI            The openAPI specification
     * @param schema             The Schema that may contain the discriminator
     * @param discPropName       The String that is the discriminator propertyName in the schema
     * @return the distinct list of property Schema with the requested discPropName
     */
    public static List<Schema> getDistinctTypes(OpenAPI openAPI, Schema schema, String discPropName) {
        List<Schema> mappedSchemas = getMappedSchemas(openAPI, schema);
        List<Schema> properties = new ArrayList<>();
        for (Schema s: mappedSchemas) {
            Schema prop = findProperty(openAPI, s, discPropName, new HashSet<>());
            if (prop != null && properties.stream().noneMatch(p ->
                    Objects.equals(p.getType(), prop.getType()) &&
                    Objects.equals(p.get$ref(), prop.get$ref()) &&
                    Objects.equals(p.getEnum(), prop.getEnum()) &&
                    Objects.equals(p.getFormat(), prop.getFormat()))) {
                properties.add(prop);
            }
        }
        return properties;
    }

    /**
     * return the list of dereferenced mapping schemas.
     * @return the schemas found or empty list if not found.
     */
    public static List<Schema> getMappedSchemas(OpenAPI openAPI, Schema schema) {
        if (schema.getDiscriminator() != null) {
            if (schema.getDiscriminator().getMapping() != null) {
                return schema.getDiscriminator().getMapping().values().stream()
                        .map(ref -> getReferencedSchema(openAPI, ref))
                        .filter(Objects::nonNull)
                        .collect(Collectors.toList());
            }
            if (schema.getOneOf() != null && !schema.getOneOf().isEmpty()) {
                // try also oneOf without discriminator mapping
                List<Schema> oneOfs = (List<Schema>) schema.getOneOf();
                return oneOfs.stream()
                        .filter(Objects::nonNull)
                        .map(oneOf -> oneOf.get$ref()!=null
                                ? getReferencedSchema(openAPI, oneOf.get$ref())
                                : oneOf)
                        .filter(Objects::nonNull)
                        .collect(Collectors.toList());
            }
        }
        return Collections.emptyList();
    }

    private static Schema getReferencedSchema(OpenAPI openAPI, String ref) {
        if (ref == null) {
            return null;
        }
        if (ref.indexOf('/') >= 0) {
            ref = ModelUtils.getSimpleRef(ref);
        }
        return ModelUtils.getSchema(openAPI, ref);
    }

    /**
     * Recursively try to find the schema matching the propertyName.
     *
     * @return the schema found or null if not found
     */
    public static Schema findProperty(OpenAPI openAPI, Schema schema, String propertyName, Set<Schema> visitedSchemas) {
        schema = ModelUtils.getReferencedSchema(openAPI, schema);
        if (propertyName == null || schema == null || visitedSchemas.contains(schema)) {
            return null;
        }
        visitedSchemas.add(schema);
        Map<String, Schema>  properties = schema.getProperties();
        if (properties != null) {
            Schema property = properties.get(propertyName);
            if (property != null) {
                Schema found = simplifyProperty(property, visitedSchemas);
                return found;
            }
        }
        // loop into parent allOfs
        List<Schema> allOfs = schema.getAllOf();
        if (allOfs != null) {
            for (Schema child : allOfs) {
                Schema found = findProperty(openAPI, child, propertyName, visitedSchemas);
                if (found != null) {
                    return simplifyProperty(found, visitedSchemas);
                }
            }
        }
        return null;
    }

    /**
     * simplify allOf property represented as [$ref, {description: ...}].
     */
    private static Schema simplifyProperty(Schema schema, Set<Schema> visitedSchemas) {
        if (!ModelUtils.isAllOf(schema)) {
            return schema;
        }
        if (visitedSchemas.contains(schema)) {
            return null;
        }
        visitedSchemas.add(schema);

        /*
         * handle type+description. For Example:
         * petType:
         *  $ref: '#/components/schemas/PetType'
         *  description: DOG
         */
        Schema found = null;
        int count = 0;
        for (Schema sc: (List<Schema>)schema.getAllOf()) {
            if (ModelUtils.getType(sc) != null || ModelUtils.hasRef(sc)) {
                found = sc;
                count++;
            }
        }
        if (count == 1) {
            return found;
        }
        // multiple types, too complex.
        return null;

    }

    private static String getDiscriminatorSchemaError(CodegenProperty codegenProperty, String discPropName,
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
     * Get the value of the vendor extension {@code x-discriminator-value} from the schema, if present.
     * @param schema the schema to check for the vendor extension
     * @return the value of the vendor extension, or an empty optional if not present
     */
    private static Optional<String> discriminatorVendorExtensionValue(Schema schema) {
        return Optional.ofNullable(schema)
                .map(Schema::getExtensions)
                .map(vendorExtensions -> vendorExtensions.get(X_DISCRIMINATOR_VALUE))
                .map(discriminatorValue -> (String) discriminatorValue);
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
