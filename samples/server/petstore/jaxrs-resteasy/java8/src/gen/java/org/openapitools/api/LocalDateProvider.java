package org.openapitools.api;

import java.time.LocalDate;
import javax.ws.rs.ext.ParamConverter;
import javax.ws.rs.ext.ParamConverterProvider;
import javax.ws.rs.ext.Provider;
import java.lang.annotation.Annotation;
import java.lang.reflect.Type;

@Provider
public class LocalDateProvider implements ParamConverterProvider {

    public static class LocalDateConverter implements ParamConverter<LocalDate> {

        public LocalDate fromString(String string) {
            LocalDate localDate = LocalDate.parse(string);
            return localDate;
        }

        public String toString(LocalDate t) {
            return t.toString();
        }
    }

    public <T> ParamConverter<T> getConverter(Class<T> type, Type type1, Annotation[] antns) {
        if (LocalDate.class.equals(type)) {
            return (ParamConverter<T>) new LocalDateConverter();
        }
        return null;
    }
}