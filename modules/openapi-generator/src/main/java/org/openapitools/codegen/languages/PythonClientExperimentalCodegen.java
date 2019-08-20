/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.openapitools.codegen.languages;

import java.text.DateFormat;
import java.text.SimpleDateFormat;

import org.openapitools.codegen.meta.GeneratorMetadata;
import org.openapitools.codegen.meta.Stability;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import io.swagger.v3.oas.models.media.Schema;
import org.openapitools.codegen.*;
import org.openapitools.codegen.utils.ModelUtils;

import java.util.*;
import java.util.regex.Pattern;

public class PythonClientExperimentalCodegen extends PythonClientCodegen {
    private static final Logger LOGGER = LoggerFactory.getLogger(PythonClientExperimentalCodegen.class);

    public PythonClientExperimentalCodegen() {
        super();

        supportingFiles.add(new SupportingFile("python-experimental/api_client.mustache", packagePath(), "api_client.py"));
        apiDocTemplateFiles.put("python-experimental/api_doc.mustache", ".md");
        apiTemplateFiles.put("python-experimental/api.mustache", ".py");
        modelDocTemplateFiles.put("python-experimental/model_doc.mustache", ".md");
        modelTemplateFiles.put("python-experimental/model.mustache", ".py");

        generatorMetadata = GeneratorMetadata.newBuilder(generatorMetadata)
                .stability(Stability.EXPERIMENTAL)
                .build();
    }

    /**
     * Configures a friendly name for the generator.  This will be used by the
     * generator to select the library with the -g flag.
     *
     * @return the friendly name for the generator
     */
    @Override
    public String getName() {
        return "python-experimental";
    }

    public String dateToString(Schema p, Date date, DateFormat dateFormatter, DateFormat dateTimeFormatter) {
        // converts a date into a date or date-time python string
        if (!(ModelUtils.isDateSchema(p) || ModelUtils.isDateTimeSchema(p))) {
            throw new RuntimeException("passed schema must be of type Date or DateTime");
        }
        if (ModelUtils.isDateSchema(p)) {
            return "dateutil_parser('" + dateFormatter.format(date) + "').date()";
        }
        return "dateutil_parser('" + dateTimeFormatter.format(date) + "')";
    }

    /**
     * Return the default value of the property
     * @param p OpenAPI property object
     * @return string presentation of the default value of the property
     */
    @Override
    public String toDefaultValue(Schema p) {
        // if a variable has no default set and only has one allowed value
        // using enum of length == 1 we use that value. Server/client usage:
        // python servers: should only use default values for optional params
        // python clients: should only use default values for required params
        Object defaultObject = null;
        Boolean enumLengthOne = (p.getEnum() != null && p.getEnum().size() == 1);
        if (p.getDefault() != null) {
            defaultObject = p.getDefault();
        } else if (enumLengthOne) {
            defaultObject = p.getEnum().get(0);
        }

        // convert datetime and date enums if they exist
        DateFormat iso8601Date = new SimpleDateFormat("yyyy-MM-dd", Locale.ROOT);
        DateFormat iso8601DateTime = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSSXXX", Locale.ROOT);
        TimeZone utc = TimeZone.getTimeZone("UTC");
        iso8601Date.setTimeZone(utc);
        iso8601DateTime.setTimeZone(utc);

        if (ModelUtils.isDateSchema(p) || ModelUtils.isDateTimeSchema(p)) {
            List<Object> currentEnum = p.getEnum();
            List<String> fixedEnum = new ArrayList<String>();
            String fixedValue = null;
            Date date = null;
            if (currentEnum != null && !currentEnum.isEmpty()) {
                for (Object enumItem : currentEnum) {
                    date = (Date) enumItem;
                    fixedValue = dateToString(p, date, iso8601Date, iso8601DateTime);
                    fixedEnum.add(fixedValue);
                }
                p.setEnum(fixedEnum);
            }

            // convert the example if it exists
            Object currentExample = p.getExample();
            if (currentExample != null) {
                date = (Date) currentExample;
                fixedValue = dateToString(p, date, iso8601Date, iso8601DateTime);
                fixedEnum.add(fixedValue);
                p.setExample(fixedValue);
            }

            // fix defaultObject
            if (defaultObject != null) {
                date = (Date) defaultObject;
                fixedValue = dateToString(p, date, iso8601Date, iso8601DateTime);
                p.setDefault(fixedValue);
                defaultObject = fixedValue;
            }
        }

        if (defaultObject == null) {
            return null;
        }

        String defaultValue = null;
        if (ModelUtils.isStringSchema(p)) {
            defaultValue = defaultObject.toString();
            if (ModelUtils.isDateSchema(p) || ModelUtils.isDateTimeSchema(p)) {
                return defaultValue;
            }

            if (!ModelUtils.isByteArraySchema(p) && !ModelUtils.isBinarySchema(p) && !ModelUtils.isFileSchema(p) && !ModelUtils.isUUIDSchema(p) && !ModelUtils.isEmailSchema(p) && !ModelUtils.isDateTimeSchema(p) && !ModelUtils.isDateSchema(p)) {
                if (Pattern.compile("\r\n|\r|\n").matcher((String) defaultValue).find()) {
                    defaultValue = "'''" + defaultValue + "'''";
                } else {
                    defaultValue = "'" + defaultValue + "'";
                }
            }
            return defaultValue;
        } else if (ModelUtils.isIntegerSchema(p) || ModelUtils.isNumberSchema(p) || ModelUtils.isBooleanSchema(p)) {
            defaultValue = String.valueOf(defaultObject);
            if (ModelUtils.isBooleanSchema(p)) {
                if (Boolean.valueOf(defaultValue) == false) {
                    return "False";
                } else {
                    return "True";
                }
            }
            return defaultValue;
        } else {
            return  defaultObject.toString();
        }
    }

}
