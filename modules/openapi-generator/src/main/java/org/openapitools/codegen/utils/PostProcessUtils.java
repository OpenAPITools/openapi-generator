/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 * Copyright 2018 SmartBear Software
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
package org.openapitools.codegen.utils;

import org.openapitools.codegen.CodegenParameter;

public class PostProcessUtils {
  public static void postProcessParameter(CodegenParameter p) {
    // we use a custom version of this function to remove the l, d, and f suffixes from Long/Double/Float
    // defaultValues
    // remove the l because our users will use Long.parseLong(String defaultValue)
    // remove the d because our users will use Double.parseDouble(String defaultValue)
    // remove the f because our users will use Float.parseFloat(String defaultValue)
    // NOTE: for CodegenParameters we DO need these suffixes because those defaultValues are used as java value
    // literals assigned to Long/Double/Float
    if (p.defaultValue == null) {
      return;
    }

    Boolean fixLong = (p.isLong && "l".equals(p.defaultValue.substring(p.defaultValue.length()-1)));
    Boolean fixDouble = (p.isDouble && "d".equals(p.defaultValue.substring(p.defaultValue.length()-1)));
    Boolean fixFloat = (p.isFloat && "f".equals(p.defaultValue.substring(p.defaultValue.length()-1)));
    if (fixLong || fixDouble || fixFloat) {
      p.defaultValue = p.defaultValue.substring(0, p.defaultValue.length()-1);
    }
  }
}
