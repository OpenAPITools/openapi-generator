package org.openapitools.server.api;

import java.lang.reflect.Array;
import java.math.BigDecimal;
import java.util.Collection;
import java.util.Map;
import java.util.Objects;

import javax.validation.ValidationException;

/**
* Validation utility methods.
*/
public final class ValidatorUtils {

    public static boolean validateMin(Integer value, Integer min) {
        checkNonNull(value);
        if (value < min) {
            throw new ValidationException(String.format("%s is less than %s", value, min));
        }
        return true;
    }

    public static boolean validateMax(Integer value, Integer max) {
        checkNonNull(value);
        if (value > max) {
            throw new ValidationException(String.format("%s is more than %s", value, max));
        }
        return true;
    }

    public static boolean validateSize(Object value, Integer min, Integer max) {
        checkNonNull(value);
        Integer size = -1;
        if (value instanceof Map) {
            size = ((Map<?, ?>) value).size();
        }
        if (value instanceof CharSequence) {
            size = ((CharSequence) value).length();
        }
        if (value instanceof Collection) {
            size = ((Collection<?>) value).size();
        }
        if (value.getClass().isArray()) {
            size = Array.getLength(value);
        }
        if (size == -1) {
            throw new ValidationException("Value has incorrect type");
        }
        if (min != null) {
            validateMin(size, min);
        }
        if (max != null) {
            validateMax(size, max);
        }
        return true;
    }

    public static boolean validatePattern(String value, String pattern) {
        checkNonNull(value, pattern);
        if (value.matches(pattern)) {
            return true;
        }
        throw new ValidationException(String.format("'%s' does not match the pattern '%s'", value, pattern));
    }

    public static boolean validateMin(BigDecimal value, String stringMinValue, boolean inclusive) {
        checkNonNull(value);
        BigDecimal minValue = new BigDecimal(stringMinValue);
        int result = value.compareTo(minValue);
        if (inclusive) {
            if (result >= 0) {
                return true;
            }
        } else {
            if (result > 0) {
                return true;
            }
        }
        throw new ValidationException(
            String.format("%s is not valid value. Min value '%s'. Inclusive - %s.", value, stringMinValue, inclusive)
        );
    }

    public static boolean validateMax(BigDecimal value, String stringMaxValue, boolean inclusive) {
        checkNonNull(value);
        BigDecimal maxValue = new BigDecimal(stringMaxValue);
        int result = value.compareTo(maxValue);
        if (inclusive) {
            if (result <= 0) {
                return true;
            }
        } else {
            if (result < 0) {
                return true;
            }
        }
        throw new ValidationException(
            String.format("%s is not valid value. Max value '%s'. Inclusive - %s.", value, stringMaxValue, inclusive)
        );
    }

    public static void checkNonNull(Object... args) {
        try {
            for (Object o : args) {
                Objects.requireNonNull(o);
            }
        } catch (Exception e) {
            throw new ValidationException(e);
        }
    }
}