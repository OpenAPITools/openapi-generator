package org.openapitools;

import com.fasterxml.jackson.databind.util.StdDateFormat;

import java.text.DateFormat;
import java.text.FieldPosition;
import java.text.ParsePosition;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.TimeZone;

public class RFC3339DateFormat extends DateFormat {
  private static final long serialVersionUID = 1L;
  private static final TimeZone TIMEZONE_Z = TimeZone.getTimeZone("UTC");

  public RFC3339DateFormat() {
    this.calendar = new GregorianCalendar();
    this.calendar.setTimeZone(TIMEZONE_Z);
  }

  @Override
  public Date parse(String source, ParsePosition pos) {
    return createFormatter().parse(source, pos);
  }

  @Override
  public StringBuffer format(Date date, StringBuffer toAppendTo, FieldPosition fieldPosition) {
    return createFormatter().format(date, toAppendTo, fieldPosition);
  }

  private StdDateFormat createFormatter() {
    return new StdDateFormat()
            .withTimeZone(TIMEZONE_Z)
            .withColonInTimeZone(true);
  }

  @Override
  public Object clone() {
    RFC3339DateFormat clone = (RFC3339DateFormat) super.clone();
    clone.calendar = (GregorianCalendar) this.calendar.clone();
    return clone;
  }
}