package org.openapitools.api;

import org.joda.time.DateTime;
import javax.ws.rs.ext.ParamConverter;
import javax.ws.rs.ext.ParamConverterProvider;
import javax.ws.rs.ext.Provider;
import java.lang.annotation.Annotation;
import java.lang.reflect.Type;
import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.Response;


@Provider
public class JodaDateTimeProvider implements ParamConverterProvider {

    public static class JodaDateTimeConverter implements ParamConverter<DateTime> {

        public DateTime fromString(String string) {
            try {
                DateTime dateTime = DateTime.parse(string);
                return dateTime;
            } catch (Exception e) {
                throw new WebApplicationException(Response.status(Response.Status.BAD_REQUEST).
                entity(string + " must be valid DateTime").build());
            }
        }

        public String toString(DateTime t) {
            return t.toString();
        }
    }

    public <T> ParamConverter<T> getConverter(Class<T> type, Type type1, Annotation[] antns) {
        if (DateTime.class.equals(type)) {
            return (ParamConverter<T>) new JodaDateTimeConverter();
        }
        return null;
    }
}