package io.swagger.codegen.languages.helpers.java;

import com.github.jknack.handlebars.Options;
import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.CodegenModel;
import io.swagger.codegen.CodegenProperty;
import io.swagger.codegen.languages.JavaClientCodegen;
import org.apache.commons.lang3.StringUtils;

import java.io.IOException;

public class JavaHelper {

    public static final String NAME = JavaHelper.class.getName();

    public CharSequence getClassDefinition(CodegenModel codegenModel, Options options) throws IOException {
        final Boolean serializableModel = Boolean.valueOf(String.valueOf(options.get(CodegenConstants.SERIALIZABLE_MODEL)));
        final Boolean parceableModel = Boolean.valueOf(String.valueOf(options.get(JavaClientCodegen.PARCELABLE_MODEL)));
        final StringBuilder builder = new StringBuilder();
        builder.append(codegenModel.classname);
        if (StringUtils.isNotBlank(codegenModel.parent)) {
            builder.append(StringUtils.SPACE);
            builder.append("extends ");
            builder.append(codegenModel.parent);
        }
        if (parceableModel && serializableModel) {
            builder.append(" implements Parcelable, Serializable");
        } else {
            if (serializableModel) {
                builder.append(" implements Serializable");
            }
        }
        return builder.toString();
    }

    public CharSequence getJavaProperty(CodegenProperty codegenProperty, Options options) throws IOException {
        final StringBuilder builder = new StringBuilder();
        if (codegenProperty.isContainer()) {
            builder.append(codegenProperty.getDatatypeWithEnum());
            builder.append(StringUtils.SPACE);
            builder.append(codegenProperty.getName());
            builder.append(" = ");
            if (codegenProperty.isRequired()) {
                builder.append(codegenProperty.getDefaultValue());
            } else {
                builder.append("null");
            }
            return builder.toString();
        }
        return String.format("%s %s = %s", codegenProperty.getDatatypeWithEnum(), codegenProperty.getName(), codegenProperty.getDefaultValue());
    }
}
