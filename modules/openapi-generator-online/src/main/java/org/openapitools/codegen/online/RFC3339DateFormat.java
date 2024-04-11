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

package org.openapitools.codegen.online;

import com.fasterxml.jackson.databind.util.StdDateFormat;

import java.text.DateFormat;
import java.text.FieldPosition;
import java.text.ParsePosition;
import java.time.ZoneId;
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;
import java.util.Date;
import java.util.TimeZone;

public class RFC3339DateFormat extends DateFormat {

  private static final long serialVersionUID = 1L;
  private static final DateTimeFormatter dtf = DateTimeFormatter.ISO_OFFSET_DATE_TIME;
  private static final StdDateFormat sdf = new StdDateFormat()
      .withTimeZone(TimeZone.getTimeZone(ZoneId.systemDefault()))
      .withColonInTimeZone(true);

  @Override
  public StringBuffer format(Date date, StringBuffer toAppendTo, FieldPosition fieldPosition) {
    String value = date.toInstant().atOffset(ZoneOffset.UTC).format(dtf);
    toAppendTo.append(value);
    return toAppendTo;
  }

  @Override
  public Date parse(String source, ParsePosition pos) {
    return sdf.parse(source, pos);
  }

}
