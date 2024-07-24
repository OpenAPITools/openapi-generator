package org.openapitools.codegen;

import io.swagger.v3.oas.models.media.Schema;
import org.openapitools.codegen.utils.ModelUtils;

import java.util.EnumMap;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

public class ContainerDefaultEvaluator {
    public static final boolean[] DEFAULT_7_5_0 = new boolean[]{false, true, false, true, false, false};
    public static final boolean[] DEFAULT_7_4_0 = new boolean[]{true, true, false, true, true, false};

    public static final boolean[] ALL = new boolean[]{true, true, true, true, true, true};
    public static final boolean[] CONTAINER_TO_NULLABLE = new boolean[]{false, true, false, true, true, true};

    public static final boolean[] NONE = new boolean[]{true, true, true, true, true, true};
    static Map<String, boolean[]> SHORTCUTS = Map.of(
            "7.5.0", DEFAULT_7_5_0,
            "openapi3specification", DEFAULT_7_5_0,
            "containerToNullable", CONTAINER_TO_NULLABLE,
            "7.4.0", DEFAULT_7_4_0,
            "all", ALL,
            "none", NONE
    );
    private final boolean legacy;
    private boolean containerDefaultToNull;

    private final Map<ContainerType, boolean[]> decisionTable;


    public ContainerDefaultEvaluator(String containerDefaultToNull) {
        if (containerDefaultToNull == null) {
            // legacy
            containerDefaultToNull = "false";
        }

        if (containerDefaultToNull.equalsIgnoreCase("true") || containerDefaultToNull.equalsIgnoreCase("false")) {
            this.legacy = true;
            this.containerDefaultToNull = Boolean.parseBoolean(containerDefaultToNull);
            this.decisionTable = null;
        } else {
            this.legacy = false;
            this.containerDefaultToNull = false;
            this.decisionTable = evaluate(containerDefaultToNull);
        }
    }


    public boolean isNullDefault(CodegenProperty cp, Schema schema) {
        if (legacy) {
            // nullable or containerDefaultToNull set to true
            return (cp.isNullable || containerDefaultToNull);
        }

        int index = evaluateIndex(cp, schema);

        ContainerType containerType = ContainerType.valueOf(cp, schema);
        return decisionTable.get(containerType)[index];
    }


    enum ContainerType {
        array, map, set;

        public static ContainerType valueOf(CodegenProperty cp, Schema schema) {
            if (ModelUtils.isSet(schema)) {
                return set;
            }
            if (cp.isMap) {
                return map;
            }
            return array;
        }
    }


    public static Map<ContainerType, boolean[]> evaluate(String expression) {
        return new ContainerDefaultParser(expression).evaluate();
    }

    static class ContainerDefaultParser {
        private final java.util.StringTokenizer tokenizer;

        private String token;

        private Boolean required;
        private Boolean nullable;
        private boolean nullableParsed;

        private boolean positive;
        private boolean notDefined;

        private boolean booleans[];

        private Set<ContainerType> containerTypeSet;

        private Map<ContainerType, boolean[]> map = new HashMap<>();
        private boolean rightExpressionParsing;


        ContainerDefaultParser(String expression) {
            this.tokenizer = new java.util.StringTokenizer(expression, "&!| ?:;", true);
            reset();
        }

        private void reset() {
            this.booleans = initAllToFalse();
            this.required = false;
            this.nullable = null;
            this.nullableParsed = false;
            this.positive = false;
            this.notDefined = false;
            this.containerTypeSet = null;
            this.rightExpressionParsing = false;
        }

        private boolean[] initAllToFalse() {
            return new boolean[6];
        }

        private boolean nextToken() {
            if ("EOF".equals(token)) {
                return false;
            }
            for (; ; ) {
                if (tokenizer.hasMoreElements()) {
                    token = tokenizer.nextToken();
                    if (" ".equals(token)) {
                        continue;
                    }
                    return true;
                } else {
                    token = "EOF";
                    return false;
                }
            }
        }

        private void advance() {
            nextToken();
        }

        Map<ContainerType, boolean[]> evaluate() {
            nextToken();
            while (!"EOF".equals(token)) {
                switch (token) {
                    case ";":
                        reset();
                        nextToken();
                        break;
                    default:
                        evaluateToken();
                        break;
                }
            }

            // add default to not defined values
            for (ContainerType other : ContainerType.values()) {
                if (!map.containsKey(other)) {
                    map.put(other, DEFAULT_7_5_0);
                }
            }

            return map;
        }

        private void addToMap() {
            if (containerTypeSet != null) {
                containerTypeSet.forEach(c -> map.put(c, booleans));
            } else {
                map = allContainerTypes(booleans);
            }
            reset();
        }

        private void assignBooleanExpression() {
            rightExpressionParsing = true;
            if (!nullableParsed) {
                throw new IllegalArgumentException("nullable should be specified");
            }
            if (required == null) {
                throw new IllegalArgumentException("nullable should be specified");
            }

            if (required == Boolean.FALSE && nullable == Boolean.FALSE) {
                booleans[0] = true;
            } else if (required == Boolean.FALSE && nullable == Boolean.TRUE) {
                booleans[1] = true;
            } else if (required == Boolean.TRUE && nullable == Boolean.FALSE) {
                booleans[2] = true;
            } else if (required == Boolean.TRUE && nullable == Boolean.TRUE) {
                booleans[3] = true;
            } else if (required == Boolean.FALSE && nullable == null) {
                booleans[4] = true;
            } else if (required == Boolean.TRUE && nullable == null) {
                booleans[5] = true;
            } else {
                // should not happen
                throw new IllegalArgumentException("Unexpected combination");
            }
        }

        private void evaluateToken() {
            switch (token) {
                case "array":
                case "set":
                case "map":
                    parseContainerTypes();
                    break;
                default:
                    evaluateAndExpression();
                    break;
            }
        }

        void evaluateAndExpression() {

            evaluateExpression();
            for (; ; ) {
                switch (token) {
                    case "EOF":
                    case ";":
                        return;
                }
                evaluateExpression();
                ;
            }
        }

        void evaluateExpression() {
            boolean[] predefinedValues = SHORTCUTS.get(token);
            if (predefinedValues != null) {
                booleans = predefinedValues;
                nextToken();
                if ("|".equals(token)) {
                    nextToken();
                } else {
                    addToMap();
                    return;
                }
            }
            for (; ; ) {
                switch (token) {

                    case "required":
                        rightExpressionParsing = true;
                        required = positive;
                        if (notDefined) {
                            throw new IllegalArgumentException("?required not allowed");
                        }
                        positive = true;
                        break;
                    case "nullable":
                        nullableParsed = true;
                        nullable = notDefined ? null : positive;
                        positive = true;
                        break;
                    case "!":
                        positive = false;
                        break;
                    case "&":
                        if (!rightExpressionParsing) {
                            throw new IllegalArgumentException(token + " unexpected in right expression parsing");
                        }
                        positive = true;
                        notDefined = false;
                        break;
                    case "?":
                        notDefined = true;
                        break;
                    case ";":
                    case "EOF":
                        assignBooleanExpression();
                        addToMap();
                        nextToken();
                        reset();
                        return;
                    case "|":
                        assignBooleanExpression();
                        nextToken();
                        return;
                    default:
                        throw new IllegalArgumentException("Unknow token " + token);
                }
                nextToken();

            }
        }

        private void parseContainerTypes() {
            this.containerTypeSet = EnumSet.of(ContainerType.valueOf(token));
            for (; ; ) {
                nextToken();
                switch (token) {
                    case ":":
                        nextToken();
                        evaluateExpression();
                        return;
                    case "|":
                        continue;
                    default:
                        this.containerTypeSet.add(ContainerType.valueOf(token));
                }
            }
        }

        protected static Map<ContainerType, boolean[]> allContainerTypes(boolean[] booleans) {
            EnumMap<ContainerType, boolean[]> map = new EnumMap<>(ContainerType.class);
            map.put(ContainerType.array, booleans);
            map.put(ContainerType.set, booleans);
            map.put(ContainerType.map, booleans);
            return map;
        }
    }


    private static int evaluateIndex(CodegenProperty cp, Schema schema) {
        boolean nullableSpecified = schema.getNullable() != null;
        if (nullableSpecified) {
            if (cp.required) {
                return cp.isNullable ? 3 : 2;
            } else {
                return cp.isNullable ? 1 : 0;
            }
        } else {
            return cp.required ? 5 : 4;
        }
    }
}

