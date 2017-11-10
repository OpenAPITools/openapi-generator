package io.swagger.codegen.languages.helpers.java;

import com.github.jknack.handlebars.Helper;
import com.github.jknack.handlebars.Options;
import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.CodegenModel;
import io.swagger.codegen.languages.JavaClientCodegen;
import org.apache.commons.lang3.StringUtils;

import java.io.IOException;

public class ClassDefinitionHelper implements Helper<CodegenModel> {

    public static final String NAME = "classDefinition";

    @Override
    public Object apply(CodegenModel codegenModel, Options options) throws IOException {
        final Boolean serializableModel = Boolean.valueOf(String.valueOf(options.get(CodegenConstants.SERIALIZABLE_MODEL)));
        final Boolean parceableModel = Boolean.valueOf(String.valueOf(options.get(JavaClientCodegen.PARCELABLE_MODEL)));
        final StringBuilder builder = new StringBuilder();
        builder.append("public class ");
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
        builder.append(" {");
        if (serializableModel) {
            builder.append("\n\n\tprivate static final long serialVersionUID = 1L;");
        }
        return builder.toString();
    }
}
