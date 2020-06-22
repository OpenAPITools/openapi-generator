/*
 * Copyright 2019 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 * Copyright 2019 SmartBear Software
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.openapitools.codegen.languages;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.math.BigDecimal;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.*;

import org.apache.commons.lang3.StringEscapeUtils;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.CliOption;
import org.openapitools.codegen.CodegenModel;
import org.openapitools.codegen.CodegenOperation;
import org.openapitools.codegen.CodegenParameter;
import org.openapitools.codegen.CodegenProperty;
import org.openapitools.codegen.SupportingFile;
import org.openapitools.codegen.languages.features.CXFExtServerFeatures;
import org.openapitools.codegen.utils.JsonCache;
import org.openapitools.codegen.utils.ModelUtils;
import org.openapitools.codegen.utils.JsonCache.CacheException;
import org.openapitools.codegen.utils.JsonCache.Root.MergePolicy;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.fasterxml.jackson.core.JsonPointer;
import com.fasterxml.jackson.databind.node.JsonNodeFactory;
import com.mifmif.common.regex.Generex;

import io.swagger.v3.oas.models.media.Schema;

/**
 * An Apache CXF-based JAX-RS server with extended capabilities.
 *
 * @author Adrian Price, TIBCO Software Inc.
 */
public class JavaCXFExtServerCodegen extends JavaCXFServerCodegen implements CXFExtServerFeatures {
    class CodegenVariable {
        CodegenVariable parent;
        String name;
        String dataFormat;
        String dataType;
        String enumName;
        Map<String, Object> allowableValues;
        boolean isArray;
        boolean isContainer;
        boolean isListContainer;
        boolean isMapContainer;
        boolean isPrimitiveType;
        CodegenVariable items;
        Integer minItems;
        int itemCount = 1;
        String minimum;
        String maximum;
        boolean exclusiveMinimum;
        boolean exclusiveMaximum;
        Integer minLength;
        Integer maxLength;
        String pattern;
        String setter;
        String testDataPath;
        int index;
        Map<String, Object> varVendorExtensions;

        private CodegenVariable() {
            varVendorExtensions = new HashMap<>();
        }

        private CodegenVariable(CodegenVariable parent, CodegenOperation op, String testDataPath,
                                Map<String, CodegenModel> models) {

            name = "response";
            dataFormat = null;// op.dataFormat;
            dataType = op.returnType;
            enumName = null;// op.enumName;
            allowableValues = null;// op.allowableValues;
            isContainer = op.isListContainer || op.isMapContainer;
            isListContainer = op.isListContainer;
            isMapContainer = op.isMapContainer;
            isPrimitiveType = op.returnTypeIsPrimitive;
            minItems = null;// op.minItems;
            minimum = null;// op.minimum;
            maximum = null;// op.maximum;
            exclusiveMinimum = false;// op.exclusiveMinimum;
            exclusiveMaximum = false;// op.exclusiveMaximum;
            minLength = null;// op.minLength;
            maxLength = null;// op.maxLength;
            pattern = null;// op.pattern;
            setter = null;// op.getSetter();
            varVendorExtensions = op.vendorExtensions;
            init(parent, testDataPath, models);

            if (op.isListContainer || op.isMapContainer) {
                items = new CodegenVariable();
                items.dataType = op.returnBaseType;
                items.isPrimitiveType = op.returnTypeIsPrimitive;
                items.name = "item";
                // TODO: populate other fields?

                items.init(this, testDataPath, models);
            }
        }

        private CodegenVariable(CodegenVariable parent, CodegenParameter param, String testDataPath,
                                Map<String, CodegenModel> models) {

            name = param.paramName;
            dataFormat = param.dataFormat;
            dataType = param.dataType;
            enumName = param.enumName;
            allowableValues = param.allowableValues;
            isContainer = param.isContainer;
            isListContainer = param.isListContainer;
            isMapContainer = param.isMapContainer;
            isPrimitiveType = param.isPrimitiveType;
            minItems = param.minItems;
            minimum = param.minimum;
            maximum = param.maximum;
            exclusiveMinimum = param.exclusiveMinimum;
            exclusiveMaximum = param.exclusiveMaximum;
            minLength = param.minLength;
            maxLength = param.maxLength;
            pattern = param.pattern;
            setter = null;
            varVendorExtensions = param.vendorExtensions;
            init(parent, testDataPath, models);

            items = param.items == null ? null : new CodegenVariable(this, param.items, null, models);
        }

        private CodegenVariable(CodegenVariable parent, CodegenProperty prop, String testDataPath,
                                Map<String, CodegenModel> models) {

            name = prop.name;
            dataFormat = prop.dataFormat;
            dataType = prop.dataType;
            enumName = prop.enumName;
            allowableValues = prop.allowableValues;
            isContainer = prop.isContainer;
            isListContainer = prop.isListContainer;
            isMapContainer = prop.isMapContainer;
            isPrimitiveType = prop.isPrimitiveType;
            minItems = prop.minItems;
            minimum = prop.minimum;
            maximum = prop.maximum;
            exclusiveMinimum = prop.exclusiveMinimum;
            exclusiveMaximum = prop.exclusiveMaximum;
            minLength = prop.minLength;
            maxLength = prop.maxLength;
            pattern = prop.pattern;
            setter = prop.getSetter();
            varVendorExtensions = prop.vendorExtensions;
            init(parent, testDataPath, models);

            items = prop.items == null ? null : new CodegenVariable(this, prop.items, null, models);
        }

        void addTestData(Object value) {
            JsonPointer ptr = getPointer(null, true, true);
            if (!testDataCache.exists(ptr)) {
                try {
                    testDataCache.set(ptr, value);
                } catch (CacheException e) {
                    LOGGER.error("Unable to update test data cache for " + ptr, e);
                }
            }
        }

        private void appendPath(StringBuilder path, boolean includeIndexes) {
            if (parent == null)
                path.append(testDataPath);
            else
                parent.appendPath(path, includeIndexes);
            if (!isListItem())
                path.append('/').append(name);
            if (includeIndexes && isIndexed())
                path.append('/').append(index);
        }

        String getComponentType() {
            return isArray ? dataType.substring(0, dataType.length() - 2) : (isContainer ? items : this).dataType;
        }

        private JsonPointer getPointer(String suffix, boolean includeIndexes, boolean includeLastIndex) {
            StringBuilder path = new StringBuilder();
            appendPath(path, includeIndexes);
            if (includeIndexes && !includeLastIndex && isIndexed())
                path.setLength(path.lastIndexOf("/"));
            if (suffix != null)
                path.append('/').append(suffix);
            return JsonPointer.compile(path.toString());
        }

        private void init(CodegenVariable parent, String testDataPath, Map<String, CodegenModel> models) {
            this.parent = parent;
            this.isArray = dataType.endsWith("[]");
            this.testDataPath = testDataPath;
            CodegenModel cm = models.get(dataType);
            if (cm != null && (cm.isArrayModel || cm.isMapModel)) {
                this.isContainer = true;
                this.isListContainer = cm.isArrayModel;
                this.isMapContainer = cm.isMapModel;
                this.items = new CodegenVariable();
                this.items.name = "item";
                this.items.dataType = cm.additionalPropertiesType;
                this.items.init(this, testDataPath, models);
            }
            try {
                if ((isArray || isContainer) && testDataControlCache != null)
                    this.itemCount = testDataControlCache.getInt(getPointer("testItemCount", false, false), 1);
            } catch (CacheException e) {
                LOGGER.error("Error accessing test data control cache", e);
            }
        }

        private boolean isIndexed() {
            return isListContainer || isArray && !dataType.equals("byte[]");
        }

        private boolean isListItem() {
            return parent != null && parent.isListContainer;
        }

        int size() {
            return loadTestDataFromFile ? testDataCache.size(getPointer(null, true, false)) : 0;
        }

        @Override
        public String toString() {
            return "CodegenVariable [name=" + name + ", dataType=" + dataType + ", dataFormat=" + dataFormat
                    + ", isArray=" + isArray + ", isContainer=" + isContainer + ", isListContainer=" + isListContainer
                    + ", isMapContainer=" + isMapContainer + ", isPrimitiveType=" + isPrimitiveType + ", testDataPath="
                    + testDataPath + ", enumName=" + enumName + ", allowableValues=" + allowableValues + ", minItems="
                    + minItems + ", itemCount=" + itemCount + ", minimum=" + minimum + ", maximum=" + maximum
                    + ", exclusiveMinimum=" + exclusiveMinimum + ", exclusiveMaximum=" + exclusiveMaximum
                    + ", minLength=" + minLength + ", maxLength=" + maxLength + ", pattern=" + pattern + ", setter="
                    + setter + ", vendorExtensions=" + varVendorExtensions + "]";
        }
    }

    private static final Logger LOGGER = LoggerFactory.getLogger(JavaCXFExtServerCodegen.class);

    private static final String INDENT = "        ";

    // SimpleDateFormat is not thread-safe, and may not be stored in a static field unless stored by ThreadLocal.
    // It's not enough to add a ThreadLocal at the usage site.
    @SuppressWarnings("squid:S5164")
    private static final ThreadLocal<SimpleDateFormat> ISO8601_DATE_FORMAT = ThreadLocal.withInitial(() ->
    {
        SimpleDateFormat f = new SimpleDateFormat("yyyy-MM-dd", Locale.getDefault());
        f.setTimeZone(TimeZone.getTimeZone("UTC"));
        return f;
    });

    // SimpleDateFormat is not thread-safe, and may not be stored in a static field unless stored by ThreadLocal.
    // It's not enough to add a ThreadLocal at the usage site.
    @SuppressWarnings("squid:S5164")
    private static final ThreadLocal<SimpleDateFormat> ISO8601_DATETIME_FORMAT = ThreadLocal.withInitial(() ->
    {
        SimpleDateFormat f = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSSX", Locale.getDefault());
        f.setTimeZone(TimeZone.getTimeZone("UTC"));
        return f;
    });

    private static final long MILLIS_PER_DAY = 24 * 60 * 60 * 1000;

    private static final long MIN_DATE;

    private static final long MAX_DATE;

    private static final String NL = System.lineSeparator();

    private static final Collection<String> DATE_TYPES = Arrays.asList("Date", "DateTime", "OffsetDateTime",
            "LocalDateTime", "LocalDate");

    static {
        long minDate = 0;
        long maxDate = 0;
        try {
            minDate = ISO8601_DATETIME_FORMAT.get().parse("1970-01-01T00:00:00Z").getTime();
            maxDate = ISO8601_DATETIME_FORMAT.get().parse("2099-12-31T23:59:59Z").getTime();
        } catch (ParseException e) {
            // Won't happen with the values provided.
        }
        MIN_DATE = minDate;
        MAX_DATE = maxDate;
    }

    private Map<String, Generex> REGEX_GENERATORS = new HashMap<>();

    protected boolean generateOperationBody = false;

    protected boolean loadTestDataFromFile = false;

    protected boolean supportMultipleSpringServices = false;

    protected JsonCache testDataCache = null;

    protected JsonCache testDataControlCache = null;

    protected File testDataFile = null;

    protected File testDataControlFile = null;

    public JavaCXFExtServerCodegen() {
        super();

        embeddedTemplateDir = templateDir = JAXRS_TEMPLATE_DIRECTORY_NAME + File.separator + "cxf-ext";
        cliOptions.add(CliOption.newBoolean(SUPPORT_MULTIPLE_SPRING_SERVICES,
                "Support generation of Spring services from multiple specifications"));
        cliOptions.add(CliOption.newBoolean(GENERATE_OPERATION_BODY, "Generate fully functional operation bodies"));
        cliOptions.add(CliOption.newBoolean(LOAD_TEST_DATA_FROM_FILE, "Load test data from a generated JSON file"));
        cliOptions.add(CliOption.newString(TEST_DATA_FILE, "JSON file to contain generated test data"));
        cliOptions.add(CliOption.newString(TEST_DATA_CONTROL_FILE, "JSON file to control test data generation"));
    }

    private void appendArrayValue(StringBuilder buffer, String indent, CodegenOperation op, CodegenVariable var,
                                  String localVar, Collection<String> localVars, Map<String, CodegenModel> models) {

        if (var.dataType.equals("byte[]")) {
            // Byte arrays are represented as Base64-encoded strings.
            appendByteArrayValue(buffer, indent, op, var, localVar, localVars, models);
        } else {
            boolean isPrimitiveType = var.isPrimitiveType && !"Object".equals(var.items.dataType);
            int itemCount = Math.max(var.itemCount, var.minItems == null ? 1 : Math.max(1, var.minItems));
            if (!loadTestDataFromFile) {
                buffer.append("new ").append(var.getComponentType());
                if (isPrimitiveType)
                    buffer.append("[] {");
                else
                    buffer.append('[').append(itemCount).append("];");
            }
            var.index = var.size();
            for (int i = var.index; i < itemCount; i++) {
                if (isPrimitiveType) {
                    // We don't need a local variable for a primitive value
                    appendValue(buffer, indent, op, var.items, localVar, localVars, models);
                    if (i < itemCount - 1 && !loadTestDataFromFile)
                        buffer.append(", ");
                } else {
                    String itemVar = appendLocalVariable(buffer, indent, op, var.items, localVars, models);
                    if (!loadTestDataFromFile) {
                        buffer.append(NL).append(indent).append(localVar).append('[').append(i).append("] = ")
                                .append(itemVar).append(';');
                    }
                }
                var.index++;
            }
            var.index = 0;
            if (isPrimitiveType && !loadTestDataFromFile)
                buffer.append("};");
        }
    }

    private void appendByteArrayValue(StringBuilder buffer, String indent, CodegenOperation op, CodegenVariable var,
                                      String localVar, Collection<String> localVars, Map<String, CodegenModel> models) {

        if (!loadTestDataFromFile)
            buffer.append('"');
        short min = var == null || var.minimum == null ? Byte.MIN_VALUE : Byte.parseByte(var.minimum);
        short max = var == null || var.maximum == null ? Byte.MAX_VALUE : Byte.parseByte(var.maximum);
        short exclusiveMin = (short) (var != null && var.exclusiveMinimum ? 1 : 0);
        short inclusiveMax = (short) (var == null || !var.exclusiveMaximum ? 1 : 0);
        int itemCount = 0;
        if (var != null) {
            itemCount = Math.max(var.itemCount, var.minItems == null ? 1 : Math.max(1, var.minItems));
        }
        byte[] randomBytes = new byte[itemCount];
        for (int i = 0; i < itemCount; i++)
            randomBytes[i] = (byte) (min + exclusiveMin + ((max + inclusiveMax - min - exclusiveMin) * Math.random()));
        String randomBytesBase64 = Base64.getEncoder().encodeToString(randomBytes);
        if (loadTestDataFromFile && var != null)
            var.addTestData(randomBytesBase64);
        else
            buffer.append('"');
    }

    private void appendListValue(StringBuilder buffer, String indent, CodegenOperation op, CodegenVariable var,
                                 String localVar, Collection<String> localVars, Map<String, CodegenModel> models) {

        op.imports.add("List");
        if (!loadTestDataFromFile) {
            op.imports.add("ArrayList");
            buffer.append("new ArrayList<");
            if (supportJava6)
                buffer.append(var.dataType);
            buffer.append(">();");
        }
        var.index = var.size();
        int itemCount = Math.max(var.itemCount, var.minItems == null ? 1 : Math.max(1, var.minItems));
        for (int i = var.index; i < itemCount; i++) {
            if (var.isPrimitiveType && !"Object".equals(var.items.dataType)) {
                // We don't need a local variable for a primitive value
                if (!loadTestDataFromFile)
                    buffer.append(NL).append(indent).append(localVar).append(".add(");
                appendValue(buffer, indent, op, var.items, localVar, localVars, models);
            } else {
                String itemVar = appendLocalVariable(buffer, indent, op, var.items, localVars, models);
                if (!loadTestDataFromFile)
                    buffer.append(NL).append(indent).append(localVar).append(".add(").append(itemVar);
            }
            if (!loadTestDataFromFile)
                buffer.append(");");
            var.index++;
        }
        var.index = 0;
    }

    /**
     * Declares and initialises a local variable of the specified type.
     *
     * @param buffer
     * @param indent
     * @param op
     * @param var
     * @param localVars
     * @param models
     *
     * @return <code>localVar</code> with a numeric suffix if necessary to ensure uniqueness.
     */
    private String appendLocalVariable(StringBuilder buffer, String indent, CodegenOperation op, CodegenVariable var,
                                       Collection<String> localVars, Map<String, CodegenModel> models) {

        // Ensure that we're using a unique local variable name (to avoid typing and overwriting conflicts).
        String localVar = var.name;
        for (int i = 2; localVars.contains(localVar); i++)
            localVar = var.name + i;
        localVars.add(localVar);

        if (!loadTestDataFromFile)
            buffer.append(NL).append(indent).append(var.dataType).append(' ').append(localVar).append(" = ");
        appendValue(buffer, indent, op, var, localVar, localVars, models);
        if (!loadTestDataFromFile)
            appendSemicolon(buffer);

        return localVar;
    }

    private void appendMapValue(StringBuilder buffer, String indent, CodegenOperation op, CodegenVariable var,
                                String localVar, Collection<String> localVars, Map<String, CodegenModel> models) {

        op.imports.add("Map");
        if (!loadTestDataFromFile) {
            op.imports.add("HashMap");
            buffer.append("new HashMap<");
            if (supportJava6)
                buffer.append("String, ").append(var.dataType);
            buffer.append(">();");
        }
        var.index = var.size();
        int itemCount = Math.max(var.itemCount, var.minItems == null ? 1 : Math.max(1, var.minItems));
        for (int i = var.index; i < itemCount; i++) {
            // Map entries need a random key (the default value for var.items.name is "items").
            var.items.name = generateRandomString(null);
            if (var.isPrimitiveType) {
                // We don't need a local variable for a primitive value
                if (!loadTestDataFromFile) {
                    buffer.append(NL).append(indent).append(localVar).append(".put(").append(var.items.name)
                            .append(", ");
                }
                appendValue(buffer, indent, op, var.items, localVar, localVars, models);
            } else {
                String itemVar = appendLocalVariable(buffer, indent, op, var.items, localVars, models);
                if (!loadTestDataFromFile)
                    buffer.append(localVar).append(".put(\"").append(var.items.name).append("\", ").append(itemVar);
            }
            if (!loadTestDataFromFile)
                buffer.append(");");
            var.index++;
        }
    }

    private void appendObjectValue(StringBuilder buffer, String indent, CodegenOperation op, CodegenVariable var,
                                   String localVar, Collection<String> localVars, Map<String, CodegenModel> models) {

        if ("Object".equals(var.dataType)) {
            // Jackson can't serialize java.lang.Object, so we'll provide an empty JSON ObjectNode instead.
            if (loadTestDataFromFile)
                var.addTestData(JsonNodeFactory.instance.objectNode());
            else
                buffer.append("JsonNodeFactory.instance.objectNode();");
        } else {
            if (needToImport(var.dataType))
                op.imports.add(var.dataType);
            if (!loadTestDataFromFile)
                buffer.append("new ").append(var.dataType).append("();");
            appendPropertyAssignments(buffer, indent, op, var, localVar, localVars, models);
        }
    }

    private void appendPropertyAssignments(StringBuilder buffer, String indent, CodegenOperation op,
                                           CodegenVariable parent, String localVar, Collection<String> localVars, Map<String, CodegenModel> models) {

        CodegenModel cm = models.get(parent.dataType);
        if (cm != null) { // TODO: handle isArrayModel and isMapModel
            for (CodegenProperty cp : cm.allVars) {
                CodegenVariable var = new CodegenVariable(parent, cp, null, models);
                if (var.isContainer || !var.isPrimitiveType) {
                    String containerVar = appendLocalVariable(buffer, indent, op, var, localVars, models);
                    if (!loadTestDataFromFile) {
                        buffer.append(NL).append(indent).append(localVar).append('.').append(var.setter).append('(')
                                .append(containerVar);
                    }
                } else {
                    // No need to use a local variable for types which can be initialised with a single expression.
                    if (!loadTestDataFromFile)
                        buffer.append(NL).append(indent).append(localVar).append('.').append(var.setter).append('(');
                    appendValue(buffer, indent, op, var, localVar, localVars, models);
                }
                if (!loadTestDataFromFile)
                    buffer.append(");");
            }
        }
    }

    private void appendRandomBoolean(StringBuilder buffer, CodegenOperation op, CodegenVariable var) {
        boolean randomBoolean = Math.random() > 0.5;

        if (loadTestDataFromFile)
            var.addTestData(randomBoolean);
        else
            buffer.append(randomBoolean);
    }

    private void appendRandomByte(StringBuilder buffer, CodegenOperation op, CodegenVariable var) {
        if (!appendRandomEnum(buffer, op, var)) {
            // NOTE: use short to hold byte values, to avoid numeric overflow.
            short min = var == null || var.minimum == null ? Byte.MIN_VALUE : Byte.parseByte(var.minimum);
            short max = var == null || var.maximum == null ? Byte.MAX_VALUE : Byte.parseByte(var.maximum);
            short exclusiveMin = (short) (var != null && var.exclusiveMinimum ? 1 : 0);
            short inclusiveMax = (short) (var == null || !var.exclusiveMaximum ? 1 : 0);
            byte randomByte = (byte) (min + exclusiveMin + ((max + inclusiveMax - min - exclusiveMin) * Math.random()));

            if (loadTestDataFromFile)
                var.addTestData(randomByte);
            else
                buffer.append(String.format(Locale.getDefault(), "(byte)%0#2x", randomByte));
        }
    }

    private void appendRandomChar(StringBuilder buffer, CodegenOperation op, CodegenVariable var) {
        if (!appendRandomEnum(buffer, op, var)) {
            // TODO: consider whether to broaden the default range.
            // NOTE: char is unsigned, so there's no overflow issue in computing (max - min).
            char min = var == null || var.minimum == null ? 'a' : var.minimum.charAt(0);
            char max = var == null || var.maximum == null ? 'z' : var.maximum.charAt(0);
            char exclusiveMin = (char) (var != null && var.exclusiveMinimum ? 1 : 0);
            char inclusiveMax = (char) (var == null || !var.exclusiveMaximum ? 1 : 0);
            char randomChar = (char) (min + exclusiveMin + ((max + inclusiveMax - min - exclusiveMin) * Math.random()));

            if (loadTestDataFromFile)
                var.addTestData(randomChar);
            else
                buffer.append(String.format(Locale.getDefault(), "'%c'", randomChar));
        }
    }

    private void appendRandomDate(StringBuilder buffer, CodegenOperation op, CodegenVariable var) {
        if (!appendRandomEnum(buffer, op, var)) {
            long minDate = MIN_DATE;
            long maxDate = MAX_DATE;
            if (var != null) {
                DateFormat df = var.dataFormat.equals("date-time") ? ISO8601_DATETIME_FORMAT.get() : ISO8601_DATE_FORMAT.get();
                String isoFormat = var.dataFormat.equals("date-time") ? "date-time" : "full-date";
                if (var.minimum != null) {
                    try {
                        minDate = df.parse(var.minimum).getTime();
                    } catch (ParseException e) {
                        // Ignore, use MIN_DATE.
                        LOGGER.warn("Could not parse minimum {} value for '{}/{}' as an ISO-8601 {}: {}",
                                var.dataFormat, op.operationId, var.name, isoFormat, var.minimum);
                    }
                }
                if (var.maximum != null) {
                    try {
                        maxDate = df.parse(var.maximum).getTime();
                    } catch (ParseException e) {
                        // Ignore, use MAX_DATE.
                        LOGGER.warn("Could not parse maximum {} value for '{}/{}' as an ISO-8601 {}: {}",
                                var.dataFormat, op.operationId, var.name, isoFormat, var.minimum);
                    }
                }
            }
            // NOTE: use BigDecimal to hold long values, to avoid numeric overflow.
            BigDecimal minLong = new BigDecimal(minDate);
            BigDecimal maxLong = new BigDecimal(maxDate);
            BigDecimal exclusiveMinLong = new BigDecimal(var != null && var.exclusiveMinimum ? 1 : 0);
            BigDecimal inclusiveMaxLong = new BigDecimal(var == null || !var.exclusiveMaximum ? 1 : 0);
            long randomDateLong = minLong.add(exclusiveMinLong).add(maxLong.add(inclusiveMaxLong).subtract(minLong)
                    .subtract(exclusiveMinLong).multiply(new BigDecimal(Math.random()))).longValue();

            // If it's just a date without a time, round downwards to the nearest day.
            if ("date".equals(var.dataFormat))
                randomDateLong = (randomDateLong % MILLIS_PER_DAY) * MILLIS_PER_DAY;

            // NOTE: By default Jackson serializes Date as long milliseconds since epoch date, but that conflicts with
            // the OpenAPI 2.0/3.0 specs, which mandates the ISO-8601 full-date or date-time formats. Accordingly, date
            // and date-time fields are annotated with @JsonFormat to specify the appropriate ISO format.
            if (loadTestDataFromFile) {
                Date randomDate = new Date(randomDateLong);
                switch (var.dataFormat) {
                    case "date":
                        var.addTestData(ISO8601_DATE_FORMAT.get().format(randomDate));
                        break;
                    case "date-time":
                        var.addTestData(ISO8601_DATETIME_FORMAT.get().format(randomDate));
                        break;
                }
            } else {
                buffer.append("new Date(").append(randomDateLong).append(')');
            }
        }
    }

    private void appendRandomDouble(StringBuilder buffer, CodegenOperation op, CodegenVariable var) {
        if (!appendRandomEnum(buffer, op, var)) {
            // NOTE: use BigDecimal to hold double values, to avoid numeric overflow.
            BigDecimal min = new BigDecimal(
                    var == null || var.minimum == null ? Long.MIN_VALUE : Double.parseDouble(var.minimum));
            BigDecimal max = new BigDecimal(
                    var == null || var.maximum == null ? Long.MAX_VALUE : Double.parseDouble(var.maximum));
            BigDecimal exclusiveMin = new BigDecimal(var != null && var.exclusiveMinimum ? 1 : 0);
            BigDecimal inclusiveMax = new BigDecimal(var == null || !var.exclusiveMaximum ? 1 : 0);
            BigDecimal randomBigDecimal = min.add(exclusiveMin).add(max.add(inclusiveMax).subtract(min)
                    .subtract(exclusiveMin).multiply(new BigDecimal(String.valueOf(Math.random()))));

            if (loadTestDataFromFile)
                var.addTestData(randomBigDecimal);
            else
                buffer.append(randomBigDecimal.toString()).append('D');
        }
    }

    private boolean appendRandomEnum(StringBuilder buffer, CodegenOperation op, CodegenVariable var) {
        if (var != null && var.allowableValues != null) {
            List<?> values = (List<?>) var.allowableValues.get("values");
            int i = (int) (values.size() * Math.random());
            Object randomEnum = values.get(i);
            boolean usingEnumLiteral = false;
            String definingClass = (String) var.varVendorExtensions.get("x-defining-class");
            if (definingClass != null) {
                @SuppressWarnings("unchecked")
                List<Map<String, Object>> enumVars = (List<Map<String, Object>>) var.allowableValues.get("enumVars");
                if (enumVars != null) {
                    if (!loadTestDataFromFile) {
                        Map<String, Object> randomEnumVar = enumVars.get(i);
                        // NOTE: to disambiguate identically named inner enums, qualify enum name with defining class.
                        buffer.append(definingClass).append('.').append(var.enumName).append('.')
                                .append(randomEnumVar.get("name"));
                        op.imports.add(definingClass);
                    }
                    usingEnumLiteral = true;
                }
            }
            if (loadTestDataFromFile) {
                var.addTestData(randomEnum);
            } else if (!usingEnumLiteral) {
                String quoteString = randomEnum instanceof String ? "\"" : "";
                buffer.append(quoteString).append(randomEnum).append(quoteString);
            }
            return true;
        }
        return false;
    }

    private void appendRandomFile(StringBuilder buffer, CodegenOperation op, CodegenVariable var) {
        // For code generation purposes we'll just supply a random string as the contents of a text 'file'.
        String randomString = generateRandomString(var);

        if (loadTestDataFromFile) {
            var.addTestData(randomString);
        } else {
            buffer.append('"').append(randomString).append('"');
        }
    }

    private void appendRandomFloat(StringBuilder buffer, CodegenOperation op, CodegenVariable var) {
        if (!appendRandomEnum(buffer, op, var)) {
            // NOTE: use double to hold float values, to avoid numeric overflow.
            double min = var == null || var.minimum == null ? -Float.MAX_VALUE : Float.parseFloat(var.minimum);
            double max = var == null || var.maximum == null ? Float.MAX_VALUE : Float.parseFloat(var.maximum);
            double exclusiveMin = (double) (var != null && var.exclusiveMinimum ? 1 : 0);
            double inclusiveMax = (double) (var == null || !var.exclusiveMaximum ? 1 : 0);
            float randomFloat = (float) (min + exclusiveMin
                    + ((max + inclusiveMax - min - exclusiveMin) * Math.random()));

            if (loadTestDataFromFile)
                var.addTestData(randomFloat);
            else
                buffer.append(String.format(Locale.getDefault(), "%g", randomFloat)).append('F');
        }
    }

    private void appendRandomInt(StringBuilder buffer, CodegenOperation op, CodegenVariable var) {
        if (!appendRandomEnum(buffer, op, var)) {
            // NOTE: use double to hold int values, to avoid numeric overflow.
            long min = var == null || var.minimum == null ? Integer.MIN_VALUE : Integer.parseInt(var.minimum);
            long max = var == null || var.maximum == null ? Integer.MAX_VALUE : Integer.parseInt(var.maximum);
            long exclusiveMin = var != null && var.exclusiveMinimum ? 1 : 0;
            long inclusiveMax = var == null || !var.exclusiveMaximum ? 1 : 0;
            int randomInt = (int) (min + exclusiveMin + ((max + inclusiveMax - min - exclusiveMin) * Math.random()));

            if (loadTestDataFromFile)
                var.addTestData(randomInt);
            else
                buffer.append(randomInt);
        }
    }

    private void appendRandomLong(StringBuilder buffer, CodegenOperation op, CodegenVariable var) {
        if (!appendRandomEnum(buffer, op, var)) {
            // NOTE: use BigDecimal to hold long values, to avoid numeric overflow.
            BigDecimal min = new BigDecimal(
                    var == null || var.minimum == null ? Long.MIN_VALUE : Long.parseLong(var.minimum));
            BigDecimal max = new BigDecimal(
                    var == null || var.maximum == null ? Long.MAX_VALUE : Long.parseLong(var.maximum));
            BigDecimal exclusiveMin = new BigDecimal(var != null && var.exclusiveMinimum ? 1 : 0);
            BigDecimal inclusiveMax = new BigDecimal(var == null || !var.exclusiveMaximum ? 1 : 0);
            long randomLong = min.add(exclusiveMin).add(
                    max.add(inclusiveMax).subtract(min).subtract(exclusiveMin).multiply(new BigDecimal(Math.random())))
                    .longValue();

            if (loadTestDataFromFile)
                var.addTestData(randomLong);
            else
                buffer.append(randomLong).append('L');
        }
    }

    private void appendRandomShort(StringBuilder buffer, CodegenOperation op, CodegenVariable var) {
        if (!appendRandomEnum(buffer, op, var)) {
            // NOTE: use int to hold short values, to avoid numeric overflow.
            int min = var == null || var.minimum == null ? Short.MIN_VALUE : Short.parseShort(var.minimum);
            int max = var == null || var.maximum == null ? Short.MAX_VALUE : Short.parseShort(var.maximum);
            int exclusiveMin = var != null && var.exclusiveMinimum ? 1 : 0;
            int inclusiveMax = var == null || !var.exclusiveMaximum ? 1 : 0;
            short randomShort = (short) (min + exclusiveMin
                    + ((max + inclusiveMax - min - exclusiveMin) * Math.random()));

            if (loadTestDataFromFile)
                var.addTestData(randomShort);
            else
                buffer.append(String.format(Locale.getDefault(), "(short)%d", randomShort));
        }
    }

    private void appendRandomString(StringBuilder buffer, CodegenOperation op, CodegenVariable var) {
        if (!appendRandomEnum(buffer, op, var)) {
            String randomString = generateRandomString(var);

            if (loadTestDataFromFile) {
                var.addTestData(randomString);
            } else {
                buffer.append('"').append(randomString).append('"');
            }
        }
    }

    /**
     * Appends a sample value for a scalar type - that is, one which is neither a list nor a map container.
     *
     * @param buffer    The operation body to which the value expression is to be appended.
     * @param indent    Indentation to apply.
     * @param op
     * @param localVar  The variable whose value is to be set.
     * @param localVars Tracks local variables which have been allocated.
     * @param models    A map of models, keyed on class name.
     */
    private void appendScalarValue(StringBuilder buffer, String indent, CodegenOperation op, CodegenVariable var,
                                   String localVar, Collection<String> localVars, Map<String, CodegenModel> models) {

        if (!var.isPrimitiveType && !DATE_TYPES.contains(var.dataType) || var.dataType.equals("Object")) {
            // All other non-container types: allocate a new object on the heap.
            appendObjectValue(buffer, indent, op, var, localVar, localVars, models);
        } else {
            openValueBuffer(buffer, var);
            // NOTE: this loop is only iterated once unless var.isArray is true and var.type is not byte[].
            int itemCount = Math.max(var.itemCount, var.minItems == null ? 1 : Math.max(1, var.minItems));
            for (int i = 0; i < itemCount; i++) {
                switch (var.getComponentType()) {
                    case "byte":
                    case "Byte":
                        appendRandomByte(buffer, op, var);
                        break;
                    case "boolean":
                    case "Boolean":
                        appendRandomBoolean(buffer, op, var);
                        break;
                    case "char":
                    case "Char":
                        appendRandomChar(buffer, op, var);
                        break;
                    case "double":
                    case "Double":
                        appendRandomDouble(buffer, op, var);
                        break;
                    case "float":
                    case "Float":
                        appendRandomFloat(buffer, op, var);
                        break;
                    case "short":
                    case "Short":
                        appendRandomShort(buffer, op, var);
                        break;
                    case "int":
                    case "Integer":
                        appendRandomInt(buffer, op, var);
                        break;
                    case "long":
                    case "Long":
                        appendRandomLong(buffer, op, var);
                        break;
                    case "String":
                        appendRandomString(buffer, op, var);
                        break;
                    case "Date":
                        appendRandomDate(buffer, op, var);
                        break;
                    case "File":
                        appendRandomFile(buffer, op, var);
                        break;
                    default:
                        LOGGER.warn("Unrecognized component type '{}" + "' in '{}' property for '{}'operation",
                                var.getComponentType(), var.name, op.operationId);
                }
                if (!loadTestDataFromFile && i < itemCount - 1)
                    buffer.append(", ");
            }
            closeValueBuffer(buffer, var);
        }
    }

    private void appendSemicolon(StringBuilder buffer) {
        if (buffer.charAt(buffer.length() - 1) != ';')
            buffer.append(';');
    }

    private StringBuilder appendValue(StringBuilder buffer, String indent, CodegenOperation op, CodegenVariable var,
                                      String localVar, Collection<String> localVars, Map<String, CodegenModel> models) {

        if (var.isListContainer)
            appendListValue(buffer, indent, op, var, localVar, localVars, models);
        else if (var.isMapContainer)
            appendMapValue(buffer, indent, op, var, localVar, localVars, models);
        else if (var.isArray)
            appendArrayValue(buffer, indent, op, var, localVar, localVars, models);
        else
            appendScalarValue(buffer, indent, op, var, localVar, localVars, models);

        return buffer;
    }

    /**
     * Applies appropriate default consumes and/or produces content type(s). Affects operations which do not specify a
     * content type for consumes and/or produces and respectively have a body parameter and/or a return value. This is
     * necessary since none of the configured CXF or Jackson JAX-RS providers support the inferred default content type
     * of <code>&ast;/*</code>.
     *
     * @param op
     */
    private void applyDefaultContentTypes(CodegenOperation op) {
        if (op.bodyParam != null && !op.hasConsumes) {
            CodegenParameter bodyParam = op.bodyParam;
            String mediaType;
            if (bodyParam.isContainer || bodyParam.isModel || bodyParam.isFreeFormObject)
                mediaType = "application/json";
            else if (bodyParam.isBinary || bodyParam.isFile)
                mediaType = "application/octet-stream";
            else
                mediaType = "text/plain";
            Map<String, String> contentType = new HashMap<>();
            contentType.put("mediaType", mediaType);
            contentType.put("hasMore", null);
            if (op.consumes == null)
                op.consumes = new ArrayList<>();
            op.consumes.add(contentType);
            op.hasConsumes = true;
        }
        if (!"void".equals(op.returnType) && !op.hasProduces) {
            String mediaType;
            if (op.returnContainer != null || !op.returnTypeIsPrimitive)
                mediaType = "application/json";
            else
                mediaType = "text/plain";
            Map<String, String> contentType = new HashMap<>();
            contentType.put("mediaType", mediaType);
            contentType.put("hasMore", null);
            if (op.produces == null)
                op.produces = new ArrayList<>();
            op.produces.add(contentType);
            op.hasProduces = true;
        }
    }

    private void closeValueBuffer(StringBuilder buffer, CodegenVariable var) {
        if (var.isArray && !loadTestDataFromFile)
            buffer.append('}');
    }

    private String generateRandomString(CodegenVariable var) {
        String pattern = patternFor(var);
        Generex generex = REGEX_GENERATORS.get(pattern);
        if (generex == null) {
            generex = new Generex(pattern);
            REGEX_GENERATORS.put(pattern, generex);
        }
        return generex.random();
    }

    private String getCacheMethod(CodegenVariable var) {
        String method;
        switch (var.dataType) {
            case "boolean":
            case "Boolean":
                method = "getBoolean";
                break;
            case "BigDecimal":
                method = "getBigDecimal";
                break;
            case "BigInteger":
                method = "getBigInteger";
                break;
            case "byte[]":
                method = "getBinary";
                break;
            case "double":
            case "Double":
                method = "getDouble";
                break;
            case "float":
            case "Float":
                method = "getFloat";
                break;
            case "long":
            case "Long":
                method = "getLong";
                break;
            case "int":
            case "Integer":
                method = "getInt";
                break;
            case "String":
                method = "getString";
                break;
            default:
                method = var.isListContainer ? "getObjects" : "getObject";
                break;
        }
        return method;
    }

    @Override
    public String getName() {
        return "jaxrs-cxf-extended";
    }

    @Override
    public String getHelp() {
        return "Extends jaxrs-cxf with options to generate a functional mock server.";
    }

    private boolean hasCacheMethod(CodegenVariable var) {
        boolean hasCacheMethod;
        switch (var.dataType) {
            case "boolean":
            case "Boolean":
            case "BigDecimal":
            case "BigInteger":
            case "byte[]":
            case "double":
            case "Double":
            case "float":
            case "Float":
            case "long":
            case "Long":
            case "int":
            case "Integer":
            case "String":
                hasCacheMethod = true;
                break;
            default:
                hasCacheMethod = false;
                break;
        }
        return hasCacheMethod;
    }

    private void openValueBuffer(StringBuilder buffer, CodegenVariable var) {
        if (var.isArray && !loadTestDataFromFile)
            buffer.append("new ").append(var.dataType).append(" {");
    }

    private String patternFor(CodegenVariable var) {
        String pattern = null;
        if (var != null) {
            if (var.pattern != null) {
                pattern = StringEscapeUtils.unescapeJava(var.pattern);
            } else if (var.dataFormat != null) {
                // According to JSON Schema (https://tools.ietf.org/html/draft-fge-json-schema-validation-00),
                // string-type values can be constrained by a format, one of: {date-time, email, hostname, ipv4, ipv6,
                // uri}. Custom formats are allowed. The OpenAPI Specification also mentions binary, byte, date,
                // password, uuid.
                switch (var.dataFormat) {
                    case "binary":
                        // Any sequence of octets.
                        pattern = "[0-9A-F]{2}{4,24}";
                        break;
                    case "byte":
                        // Base64 encoded bytes: 4 characters represent 3 bytes.
                        pattern = "[A-Za-z0-9+/]{4}{4,24}";
                        break;
                    case "date":
                        // ISO-8601: YYYY-MM-DD
                        pattern = "20\\d{2}-(?:" // YY- (20yy)
                                + "(?:01|03|05|07|08|10|12)-(?:0[1-9]|[1-2][0-9]|3[0-1])|" // MM-DD (31 days)
                                + "(?:04|06|09|11)-(?:0[1-9]|[1-2][0-9]|30)|" // MM-DD (30 days)
                                + "02-(?:0[1-9]|1[0-9]|2[0-8]))"; // MM-DD (28 days)
                        break;
                    case "date-time":
                        // ISO-8601: YYYY-MM-DDTHH:mm:ss(Z|+-HH:mm) NOTE: the time zone offset is mandatory.
                        pattern = "20\\d{2}-(?:" // YYYY-
                                + "(?:01|03|05|07|08|10|12)-(?:0[1-9]|[1-2][0-9]|3[0-1])|" // MM-DD (31 days)
                                + "(?:04|06|09|11)-(?:0[1-9]|[1-2][0-9]|30)|" // MM-DD (30 days)
                                + "02-(?:0[1-9]|1[0-9]|2[0-8]))" // MM-DD (28 days)
                                + "T(?:[0-1][0-9]|2[0-3])" // THH
                                + ":[0-5][0-9]" // :mm
                                + ":[0-5][0-9]" // :ss
                                + "(?:Z|(?:-0[1-9]|-1[0-2]|\\+0[0-9]|\\+1[0-4]):(?:00|30|45))"; // timezone
                        break;
                    case "email":
                        pattern = "[a-z][a-z0-9_.-]{1,8}@[a-z][a-z0-9-.]{2,12}\\.[a-z]{2,4}"; // (simplistic but
                        // sufficient)
                        break;
                    case "hostname":
                        pattern = "[a-z][a-z0-9-.]{2,12}\\.[a-z]{2,4}"; // (simplistic but sufficient)
                        break;
                    case "ipv4":
                        pattern = "(?:(?:25[0-5]|2[0-4][0-9]|[1-9][0-9]|[0-9])\\.){3}"
                                + "(?:25[0-5]|2[0-4][0-9]|[1-9][0-9]|[0-9])";
                        break;
                    case "ipv6":
                        // Simplified (!) from
                        // https://community.helpsystems.com/forums/intermapper/miscellaneous-topics/5acc4fcf-fa83-e511-80cf-0050568460e4
                        pattern = "(?:(?:[0-9A-F]{1,4}:){7}(?:[0-9A-F]{1,4}|:))|"
                                + "(?:(?:[0-9A-F]{1,4}:){6}(?::[0-9A-F]{1,4}|(?:(?:25[0-5]|2[0-4]\\d|1\\d\\d|[1-9]?\\d)(?:\\.(?:25[0-5]|2[0-4]\\d|1\\d\\d|[1-9]?\\d)){3})|:))|"
                                + "(?:(?:[0-9A-F]{1,4}:){5}(?:(?:(?::[0-9A-F]{1,4}){1,2})|:(?:(?:25[0-5]|2[0-4]\\d|1\\d\\d|[1-9]?\\d)(?:\\.(?:25[0-5]|2[0-4]\\d|1\\d\\d|[1-9]?\\d)){3})|:))|"
                                + "(?:(?:[0-9A-F]{1,4}:){4}(?:(?:(?::[0-9A-F]{1,4}){1,3})|(?:(?::[0-9A-F]{1,4})?:(?:(?:25[0-5]|2[0-4]\\d|1\\d\\d|[1-9]?\\d)(?:\\.(?:25[0-5]|2[0-4]\\d|1\\d\\d|[1-9]?\\d)){3}))|:))|"
                                + "(?:(?:[0-9A-F]{1,4}:){3}(?:(?:(?::[0-9A-F]{1,4}){1,4})|(?:(?::[0-9A-F]{1,4}){0,2}:(?:(?:25[0-5]|2[0-4]\\d|1\\d\\d|[1-9]?\\d)(?:\\.(?:25[0-5]|2[0-4]\\d|1\\d\\d|[1-9]?\\d)){3}))|:))|"
                                + "(?:(?:[0-9A-F]{1,4}:){2}(?:(?:(?::[0-9A-F]{1,4}){1,5})|(?:(?::[0-9A-F]{1,4}){0,3}:(?:(?:25[0-5]|2[0-4]\\d|1\\d\\d|[1-9]?\\d)(?:\\.(?:25[0-5]|2[0-4]\\d|1\\d\\d|[1-9]?\\d)){3}))|:))|"
                                + "(?:(?:[0-9A-F]{1,4}:){1}(?:(?:(?::[0-9A-F]{1,4}){1,6})|(?:(?::[0-9A-F]{1,4}){0,4}:(?:(?:25[0-5]|2[0-4]\\d|1\\d\\d|[1-9]?\\d)(?:\\.(?:25[0-5]|2[0-4]\\d|1\\d\\d|[1-9]?\\d)){3}))|:))|"
                                + "(?::(?:(?:(?::[0-9A-F]{1,4}){1,7})|(?:(?::[0-9A-F]{1,4}){0,5}:(?:(?:25[0-5]|2[0-4]\\d|1\\d\\d|[1-9]?\\d)(?:\\.(?:25[0-5]|2[0-4]\\d|1\\d\\d|[1-9]?\\d)){3}))|:))";
                        break;
                    case "uri":
                        // SCHEME://AUTHORITY/PATH/PATH?PARAM=VALUE,PARAM=VALUE#ANCHOR
                        pattern = "(?:(?:http[s]?|ftp):)?" // scheme
                                + "(?://[a-z][a-z.]{4,12})?" // authority
                                + "(?:/[a-z]{1,8}){0,4}" // path
                                + "(?:\\?[a-z]{1,8}=[a-z0-9]{1,8}(?:&[a-z]{1,8}=[a-z0-9]{1,8}){0,3})?" // query
                                + "(?:#[a-z0-9_]{1,16})?"; // fragment
                        break;
                    case "uuid":
                        pattern = "[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}"; // 8-4-4-4-12
                        break;
                    case "password":
                        // Fall through.
                    default:
                        break;
                }
            }
        }
        if (pattern == null) {
            int minLength = var == null || var.minLength == null ? 4 : var.minLength;
            int maxLength = var == null || var.maxLength == null ? 16 : var.maxLength;
            pattern = "[a-zA-Z][a-zA-Z0-9]{" + minLength + ',' + maxLength + '}';
        }
        return pattern;
    }

    @Override
    public Map<String, Object> postProcessAllModels(Map<String, Object> objs) {
        objs = super.postProcessAllModels(objs);

        // When populating operation bodies we need to import enum types, which requires the class that defines them.
        if (generateOperationBody) {
            for (Object value : objs.values()) {
                @SuppressWarnings("unchecked")
                Map<String, Object> inner = (Map<String, Object>) value;
                @SuppressWarnings("unchecked")
                List<Map<String, Object>> models = (List<Map<String, Object>>) inner.get("models");
                for (Map<String, Object> mo : models)
                    postProcessModel((CodegenModel) mo.get("model"));
            }
        }

        return objs;
    }

    private void postProcessModel(CodegenModel cm) {
        // NOTE: if supportsInheritance is false, cm.vars == cm.allVars so we only have to update one list.
        for (CodegenProperty var : cm.vars) {
            var.vendorExtensions.put("x-defining-class", cm.classname);
        }
        if (supportsInheritance) {
            if (cm.allVars != cm.vars) {
                for (CodegenProperty var : cm.allVars) {
                    String definingClass = cm.classname;
                    if (cm.vars.stream().noneMatch(v -> v.name.equals(var.name))) {
                        CodegenModel ancestor = cm;
                        while ((ancestor = ancestor.parentModel) != null) {
                            if (ancestor.vars.stream().anyMatch(v -> v.name.equals(var.name))) {
                                definingClass = ancestor.classname;
                                break;
                            }
                        }
                    }
                    var.vendorExtensions.put("x-defining-class", definingClass);
                }
            }
            if (cm.parentModel != null)
                postProcessModel(cm.parentModel);
        }
    }

    @Override
    public Map<String, Object> postProcessOperationsWithModels(Map<String, Object> objs, List<Object> allModels) {
        Map<String, Object> result = super.postProcessOperationsWithModels(objs, allModels);

        if (generateOperationBody) {
            // We generate the operation body in code because the logic to do so is far too complicated to be expressed
            // in the logic-less Mustache templating system.

            @SuppressWarnings("unchecked")
            Map<String, Object> operations = (Map<String, Object>) result.get("operations");
            if (operations != null) {
                String classname = (String) operations.get("classname");

                // Map the models so we can look them up by name.
                Map<String, CodegenModel> models = new HashMap<>();
                for (Object model : allModels) {
                    @SuppressWarnings("unchecked")
                    CodegenModel cgModel = ((Map<String, CodegenModel>) model).get("model");
                    models.put(cgModel.classname, cgModel);
                }

                StringBuilder buffer = new StringBuilder();
                @SuppressWarnings("unchecked")
                List<CodegenOperation> ops = (List<CodegenOperation>) operations.get("operation");
                for (CodegenOperation op : ops) {
                    applyDefaultContentTypes(op);
                    String testDataPath = '/' + classname + '/' + op.operationId;

                    // Test client: provide local variable declarations for all parameters.
                    boolean first = true;
                    for (CodegenParameter cp : op.allParams) {
                        buffer.setLength(0);
                        CodegenVariable var = new CodegenVariable(null, cp, testDataPath, models);
                        String localVar = appendLocalVariable(buffer, INDENT, op, var, new ArrayList<>(), models);
                        if (loadTestDataFromFile) {
                            buffer.append(NL).append(INDENT).append(var.dataType).append(' ').append(localVar)
                                    .append(" = cache.").append(getCacheMethod(var)).append("(\"/")
                                    .append(op.operationId).append('/').append(var.name).append('"');
                            if (var.isListContainer)
                                buffer.append(", ").append(var.getComponentType()).append(".class");
                            else if (var.isMapContainer)
                                buffer.append(", Map.class");
                            else if (!hasCacheMethod(var))
                                buffer.append(", ").append(var.dataType).append(".class");
                            buffer.append(");");
                        }
                        if (first && buffer.indexOf(NL) == 0) {
                            buffer.delete(0, NL.length());
                            first = false;
                        }
                        cp.vendorExtensions.put("x-java-param-decl", buffer.toString());
                    }

                    // Test server: generate operation body where it returns a non-void result.
                    if (!(Boolean) op.vendorExtensions.getOrDefault("x-java-is-response-void", false)) {
                        CodegenVariable var = new CodegenVariable(null, op, testDataPath, models);
                        buffer.setLength(0);
                        String localVar = appendLocalVariable(buffer, INDENT, op, var, new ArrayList<>(), models);
                        if (loadTestDataFromFile) {
                            buffer.append(NL).append(INDENT).append("try {") // split
                                    .append(NL).append(INDENT).append("    ").append(var.dataType).append(' ')
                                    .append(localVar).append(" = cache.").append(getCacheMethod(var)).append("(\"/")
                                    .append(op.operationId).append('/').append(var.name).append('"');
                            if (var.isListContainer)
                                buffer.append(", ").append(var.getComponentType()).append(".class");
                            else if (var.isMapContainer)
                                buffer.append(", Map.class");
                            else if (!hasCacheMethod(var))
                                buffer.append(", ").append(var.dataType).append(".class");
                            buffer.append(");") // split
                                    .append(NL).append(INDENT).append("    return ").append(localVar).append(';')
                                    .append(NL).append(INDENT).append("} catch (CacheException e) {") // split
                                    .append(NL).append(INDENT).append("    throw new RuntimeException(e);") // split
                                    .append(NL).append(INDENT).append("}");
                        } else {
                            buffer.append(NL).append(INDENT).append("return ").append(localVar).append(';');
                        }
                        if (buffer.indexOf(NL) == 0)
                            buffer.delete(0, NL.length());
                        op.vendorExtensions.put("x-java-operation-body", buffer.toString());
                    }
                }

                // DefaultGenerator already processed all the imports from the generated operations, but these imports
                // did not include the ones we've just added to support the code in the operation bodies. Therefore it
                // is necessary to recompute the imports and overwrite the existing ones. The code below was copied from
                // the private DefaultGenerator.processOperations() method to achieve this end.
                Set<String> allImports = new TreeSet<String>();
                for (CodegenOperation op : ops) {
                    allImports.addAll(op.imports);
                }
                allImports.add("List");
                allImports.add("Map");

                List<Map<String, String>> imports = new ArrayList<Map<String, String>>();
                for (String nextImport : allImports) {
                    Map<String, String> im = new LinkedHashMap<String, String>();
                    String mapping = importMapping().get(nextImport);
                    if (mapping == null) {
                        mapping = toModelImport(nextImport);
                    }
                    if (mapping != null) {
                        im.put("import", mapping);
                        if (!imports.contains(im)) { // avoid duplicates
                            imports.add(im);
                        }
                    }
                }

                objs.put("imports", imports);

                // add a flag to indicate whether there's any {{import}}
                if (imports.size() > 0) {
                    objs.put("hasImport", true);
                }
            }
        }

        return result;
    }

    @Override
    public Map<String, Object> postProcessSupportingFileData(Map<String, Object> objs) {
        objs = super.postProcessSupportingFileData(objs);

        if (loadTestDataFromFile) {
            // DefaultGenerator doesn't provide any mechanism for emitting supporting files other than by a Mustache
            // template, so we're obliged to serialize the caches to JSON strings and use templates to write them.
            try {
                if (testDataCache.root().isDirty()) {
                    ByteArrayOutputStream out = new ByteArrayOutputStream();
                    testDataCache.root().flush(out);
                    String testDataJson = new String(out.toByteArray(), "UTF-8");
                    objs.put("test-data.json", testDataJson);
                    supportingFiles.add(new SupportingFile("testData.mustache", testDataFile.getAbsolutePath()));
                }
            } catch (CacheException | UnsupportedEncodingException e) {
                LOGGER.error("Error writing JSON test data file " + testDataFile, e);
            }

            try {
                if (testDataControlCache.root().isDirty()) {
                    ByteArrayOutputStream out = new ByteArrayOutputStream();
                    testDataControlCache.root().flush(out);
                    String testDataControlJson = new String(out.toByteArray(), "UTF-8");
                    objs.put("test-data-control.json", testDataControlJson);
                    supportingFiles
                            .add(new SupportingFile("testDataControl.mustache", testDataControlFile.getAbsolutePath()));
                }
            } catch (CacheException | UnsupportedEncodingException e) {
                LOGGER.error("Error writing JSON test data control file " + testDataControlFile, e);
            }
        }

        return objs;
    }

    @Override
    public void processOpts() {
        super.processOpts();

        if (additionalProperties.containsKey(GENERATE_SPRING_APPLICATION)) {
            this.setSupportMultipleSpringServices(
                    convertPropertyToBooleanAndWriteBack(SUPPORT_MULTIPLE_SPRING_SERVICES));
        }
        if (additionalProperties.containsKey(GENERATE_OPERATION_BODY)) {
            boolean generateOperationBody = convertPropertyToBooleanAndWriteBack(GENERATE_OPERATION_BODY);
            this.setGenerateOperationBody(generateOperationBody);

            boolean loadTestDataFromFile = convertPropertyToBooleanAndWriteBack(LOAD_TEST_DATA_FROM_FILE);
            this.setLoadTestDataFromFile(loadTestDataFromFile);

            if (loadTestDataFromFile) {
                String testDataFileStr;
                if (additionalProperties.containsKey(TEST_DATA_FILE))
                    testDataFileStr = (String) additionalProperties.get(TEST_DATA_FILE);
                else
                    testDataFileStr = outputFolder() + "/src/main/resources/test-data.json";
                testDataFileStr = testDataFileStr.replace('/', File.separatorChar);
                try {
                    File testDataFile = new File(testDataFileStr).getCanonicalFile();
                    testDataFileStr = testDataFile.getPath();
                    additionalProperties.put(TEST_DATA_FILE, testDataFileStr.replaceAll("\\\\", "\\\\\\\\"));
                    setTestDataFile(testDataFile);
                } catch (IOException e) {
                    throw new RuntimeException("Failed to canonicalize file " + testDataFileStr, e);
                }

                testDataCache = JsonCache.Factory.instance.get("test-data").mergePolicy(MergePolicy.KEEP_EXISTING)
                        .child('/' + invokerPackage);
                if (this.testDataFile.exists()) {
                    try {
                        testDataCache.root().load(this.testDataFile);
                    } catch (CacheException e) {
                        LOGGER.error("Unable to load test data file " + testDataFileStr, e);
                    }
                }

                String testDataControlFileStr;
                if (additionalProperties.containsKey(TEST_DATA_CONTROL_FILE))
                    testDataControlFileStr = (String) additionalProperties.get(TEST_DATA_CONTROL_FILE);
                else
                    testDataControlFileStr = outputFolder() + "/test-data-control.json";
                testDataControlFileStr = testDataControlFileStr.replace('/', File.separatorChar);
                try {
                    File testDataControlFile = new File(testDataControlFileStr).getCanonicalFile();
                    testDataControlFileStr = testDataControlFile.getPath();
                    additionalProperties.put(TEST_DATA_CONTROL_FILE,
                            testDataControlFileStr.replaceAll("\\\\", "\\\\\\\\"));
                    setTestDataControlFile(testDataControlFile);
                } catch (IOException e) {
                    throw new RuntimeException("Failed to canonicalize file " + testDataControlFileStr, e);
                }

                testDataControlCache = JsonCache.Factory.instance.get("test-data-control")
                        .mergePolicy(MergePolicy.KEEP_EXISTING).child('/' + invokerPackage);
                if (this.testDataControlFile.exists()) {
                    try {
                        testDataControlCache.root().load(this.testDataControlFile);
                    } catch (CacheException e) {
                        LOGGER.error("Unable to load test data control file " + testDataControlFileStr, e);
                    }
                }
            }
        }
        if (this.generateSpringApplication) {
            if (supportMultipleSpringServices) {
                for (SupportingFile sf : supportingFiles) {
                    if ("server/ApplicationContext.xml.mustache".equals(sf.templateFile)) {
                        sf.destinationFilename = "ApplicationContext-" + invokerPackage + ".xml";
                        break;
                    }
                }
            }
        }
    }

    public void setGenerateOperationBody(boolean generateOperationBody) {
        this.generateOperationBody = generateOperationBody;
    }

    public void setLoadTestDataFromFile(boolean loadTestDataFromFile) {
        this.loadTestDataFromFile = loadTestDataFromFile;
    }

    public void setSupportMultipleSpringServices(boolean supportMultipleSpringServices) {
        this.supportMultipleSpringServices = supportMultipleSpringServices;
    }

    public void setTestDataControlFile(File testDataControlFile) {
        this.testDataControlFile = testDataControlFile;
    }

    public void setTestDataFile(File testDataFile) {
        this.testDataFile = testDataFile;
    }

    @Override
    public String toDefaultValue(Schema p) {
        if (ModelUtils.isGenerateAliasAsModel() && StringUtils.isNotEmpty(p.get$ref())) {
            Schema<?> ref = ModelUtils.getReferencedSchema(this.openAPI, p);
            if (ModelUtils.isArraySchema(ref) || ModelUtils.isMapSchema(ref)) {
                String typeDeclaration = getTypeDeclaration(p);
                return String.format(Locale.ROOT, "new %s()", typeDeclaration);
            }
        }
        return super.toDefaultValue(p);
    }
}
