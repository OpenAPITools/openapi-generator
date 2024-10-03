package org.openapitools.codegen.templating.handlebars;

import com.github.jknack.handlebars.context.FieldValueResolver;

import java.lang.reflect.AccessibleObject;
import java.util.Set;
import java.util.stream.Collectors;

// $ref: https://github.com/jknack/handlebars.java/issues/917
public class AccessAwareFieldValueResolver extends FieldValueResolver {

    public static final AccessAwareFieldValueResolver INSTANCE = new AccessAwareFieldValueResolver();

    @Override
    protected Set<FieldValueResolver.FieldWrapper> members(Class<?> clazz) {
        var members = super.members(clazz);
        return members.stream()
                .filter(this::isValidField)
                .collect(Collectors.toSet());
    }

    boolean isValidField(FieldWrapper fw) {
        if (fw instanceof AccessibleObject) {
            return isUseSetAccessible(fw);
        }
        return true;
    }
}
