#!/usr/bin/env bash

set -euo pipefail

usage() {
    echo "Stubs out files for new generators" && \
    echo "usage:" && \
    echo "$0 [options]" && \
    echo "    Options:"
    grep "[[:space:]].)\ #" $0 | tr -d "#" | sed -r 's/( \| \*)//' | sed -r 's/([a-z])\)/-\1/';
    exit 0;
}

root=$(cd $(dirname "${BASH_SOURCE}") && pwd)
gen_type=client
tests=0
gen_name=

[ $# -eq 0 ] && usage
while getopts ":hcsdtn:" arg; do
  case ${arg} in
    n) # Required. Specify generator name.
      gen_name=${OPTARG}
      ;;
    c) # Create a client generator
        gen_type=client
        ;;
    s) # Create a server generator
        gen_type=server
        ;;
    d) # Create a documentation generator
        gen_type=documentation
        ;;
    t) # When specified, creates test file(s) for the generator.
      tests=1
      ;;
    h | *) # Display help.
      usage
      exit 0
      ;;
  esac
done

[ -z "${gen_name}" ] && usage

lang_classname=$(echo "${gen_name}-${gen_type}Codegen" | sed -r 's/(^|_|-)([a-z])/\U\2/g')
gen_name_camel=$(echo "${gen_name}" | sed -r 's/(^|_|-)([a-z])/\U\2/g' | sed 's/^./\L&/')
codegen_type_enum=$(echo "${gen_type}" | sed -r 's/(.)/\U\1/g')

# Step 1: Add Language Generator
[ -f "${root}/modules/swagger-codegen/src/main/java/io/swagger/codegen/languages/${lang_classname}.java" ] && \
    echo "${lang_classname} already exists" && exit 1;

echo "Creating modules/swagger-codegen/src/main/java/io/swagger/codegen/languages/${lang_classname}.java"
cat > "${root}/modules/swagger-codegen/src/main/java/io/swagger/codegen/languages/${lang_classname}.java" <<EOF
package io.swagger.codegen.languages;

import io.swagger.codegen.*;
import io.swagger.models.properties.ArrayProperty;
import io.swagger.models.properties.MapProperty;
import io.swagger.models.properties.Property;
import io.swagger.models.parameters.Parameter;

import java.io.File;
import java.util.*;

import org.apache.commons.lang3.StringUtils;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ${lang_classname} extends DefaultCodegen implements CodegenConfig {
    public static final String PROJECT_NAME = "projectName";

    static Logger LOGGER = LoggerFactory.getLogger(${lang_classname}.class);

    public CodegenType getTag() {
        return CodegenType.${codegen_type_enum};
    }

    public String getName() {
        return "${gen_name}";
    }

    public String getHelp() {
        return "Generates a ${gen_name} ${gen_type}.";
    }

    public ${lang_classname}() {
        super();

        outputFolder = "generated-code" + File.separator + "${gen_name}";
        modelTemplateFiles.put("model.mustache", ".zz");
        apiTemplateFiles.put("api.mustache", ".zz");
        embeddedTemplateDir = templateDir = "${gen_name}";
        apiPackage = File.separator + "Apis";
        modelPackage = File.separator + "Models";
        // TODO: Fill this out.
    }
}
EOF

# Step 2: Register the new class with service loader
echo -e "\nio.swagger.codegen.languages.${lang_classname}" >> "${root}/modules/swagger-codegen/src/main/resources/META-INF/services/io.swagger.codegen.CodegenConfig"

# Step 3: Create resource files
mkdir -p "${root}/modules/swagger-codegen/src/main/resources/${gen_name}-${gen_type}"
echo "Creating modules/swagger-codegen/src/main/resources/${gen_name}-${gen_type}/README.md" && \
    touch "${root}/modules/swagger-codegen/src/main/resources/${gen_name}-${gen_type}/README.md"
echo "Creating modules/swagger-codegen/src/main/resources/${gen_name}-${gen_type}/model.mustache" && \
    touch "${root}/modules/swagger-codegen/src/main/resources/${gen_name}-${gen_type}/model.mustache"
echo "Creating /modules/swagger-codegen/src/main/resources/${gen_name}-${gen_type}/api.mustache" && \
    touch "${root}/modules/swagger-codegen/src/main/resources/${gen_name}-${gen_type}/api.mustache"

# Step 4: Create bash/batch scripts

## Windows batch file
echo "Creating bin/windows/${gen_name}-${gen_type}-petstore.bat"
cat > "${root}/bin/windows/${gen_name}-${gen_type}-petstore.bat"<<EOF
set executable=.\modules\swagger-codegen-cli\target\swagger-codegen-cli.jar

If Not Exist %executable% (
  mvn clean package
)

REM set JAVA_OPTS=%JAVA_OPTS% -Xmx1024M -DloggerPath=conf/log4j.properties
set ags=generate  --artifact-id "${gen_name}-petstore-${gen_type}" -i modules\swagger-codegen\src\test\resources\2_0\petstore.yaml -l ${gen_name} -o samples\\${gen_type}\petstore\\${gen_name}

java %JAVA_OPTS% -jar %executable% %ags%
EOF

## Bash file
echo "Creating bin/${gen_name}-${gen_type}-petstore.sh"
cat > "${root}/bin/${gen_name}-${gen_type}-petstore.sh"<<EOF
#!/bin/sh

SCRIPT="\$0"

while [ -h "\$SCRIPT" ] ; do
  ls=\$(ls -ld "\$SCRIPT")
  link=\$(expr "\$ls" : '.*-> \(.*\)$')
  if expr "\$link" : '/.*' > /dev/null; then
    SCRIPT="\$link"
  else
    SCRIPT=\$(dirname "\$SCRIPT")/"\$link"
  fi
done

if [ ! -d "\${APP_DIR}" ]; then
  APP_DIR=\$(dirname "\$SCRIPT")/..
  APP_DIR=\$(cd "\${APP_DIR}"; pwd)
fi

executable="./modules/swagger-codegen-cli/target/swagger-codegen-cli.jar"

if [ ! -f "\$executable" ]
then
  mvn clean package
fi

# if you've executed sbt assembly previously it will use that instead.
export JAVA_OPTS="\${JAVA_OPTS} -XX:MaxPermSize=256M -Xmx1024M -DloggerPath=conf/log4j.properties"
ags="\$@ generate -i modules/swagger-codegen/src/test/resources/2_0/petstore.yaml -l ${gen_name} -o samples/${gen_type}/petstore/${gen_name}"

java \${JAVA_OPTS} -jar \${executable} \${ags}
EOF

# Step 5: (optional) Create Swagger Codegen test files
if [ "1" -eq "${tests}" ]; then
    mkdir -p "${root}/modules/swagger-codegen/src/test/java/io/swagger/codegen/${gen_name_camel}"
    # Codegen
    echo "Creating modules/swagger-codegen/src/test/java/io/swagger/codegen/${gen_name_camel}/${lang_classname}Test.java"
    cat > "${root}/modules/swagger-codegen/src/test/java/io/swagger/codegen/${gen_name_camel}/${lang_classname}Test.java"<<EOF
package io.swagger.codegen.${gen_name_camel};

import io.swagger.codegen.*;
import io.swagger.codegen.languages.${lang_classname};
import io.swagger.models.*;
import io.swagger.parser.SwaggerParser;
import org.testng.Assert;
import org.testng.annotations.Test;

public class ${lang_classname}Test {

    ${lang_classname} codegen = new ${lang_classname}();

    @Test
    public void shouldSucceed() throws Exception {
        // TODO: Complete this test.
        Assert.fail("Not implemented.");
    }
}
EOF

    # Model
    echo "Creating modules/swagger-codegen/src/test/java/io/swagger/codegen/${gen_name_camel}/${lang_classname}ModelTest.java"
    cat > "${root}/modules/swagger-codegen/src/test/java/io/swagger/codegen/${gen_name_camel}/${lang_classname}ModelTest.java"<<EOF
package io.swagger.codegen.${gen_name_camel};

import io.swagger.codegen.*;
import io.swagger.codegen.languages.${lang_classname};
import io.swagger.models.*;
import io.swagger.models.properties.*;

import org.testng.Assert;
import org.testng.annotations.Test;

@SuppressWarnings("static-method")
public class ${lang_classname}ModelTest {

    @Test(description = "convert a simple java model")
    public void simpleModelTest() {
        final Model model = new ModelImpl()
                .description("a sample model")
                .property("id", new LongProperty())
                .property("name", new StringProperty())
                .required("id")
                .required("name");
        final DefaultCodegen codegen = new ${lang_classname}();

        // TODO: Complete this test.
        Assert.fail("Not implemented.");
    }

}

EOF

    # Options
    echo "Creating modules/swagger-codegen/src/test/java/io/swagger/codegen/${gen_name_camel}/${lang_classname}OptionsTest.java"
    cat > "${root}/modules/swagger-codegen/src/test/java/io/swagger/codegen/${gen_name_camel}/${lang_classname}OptionsTest.java"<<EOF
package io.swagger.codegen.${gen_name_camel};

import io.swagger.codegen.AbstractOptionsTest;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.languages.${lang_classname};
import io.swagger.codegen.options.${lang_classname}OptionsProvider;

import mockit.Expectations;
import mockit.Tested;

public class ${lang_classname}OptionsTest extends AbstractOptionsTest {

    @Tested
    private ${lang_classname} codegen;

    public ${lang_classname}OptionsTest() {
        super(new ${lang_classname}OptionsProvider());
    }

    @Override
    protected CodegenConfig getCodegenConfig() {
        return codegen;
    }

    @SuppressWarnings("unused")
    @Override
    protected void setExpectations() {
        // TODO: Complete options
        new Expectations(codegen) {{

        }};
    }
}

EOF

    # Options Provider
    echo "Creating modules/swagger-codegen/src/test/java/io/swagger/codegen/options/${lang_classname}OptionsProvider.java"
    cat > "${root}/modules/swagger-codegen/src/test/java/io/swagger/codegen/options/${lang_classname}OptionsProvider.java"<<EOF
package io.swagger.codegen.options;

import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.languages.${lang_classname};

import com.google.common.collect.ImmutableMap;

import java.util.Map;

public class ${lang_classname}OptionsProvider implements OptionsProvider {
    public static final String PROJECT_NAME_VALUE = "Swagger";

    @Override
    public String getLanguage() {
        return "${gen_name_camel}";
    }

    @Override
    public Map<String, String> createOptions() {
        ImmutableMap.Builder<String, String> builder = new ImmutableMap.Builder<String, String>();
        return builder
                .put(${lang_classname}.PROJECT_NAME, PROJECT_NAME_VALUE)
                .build();
    }

    @Override
    public boolean isServer() {
        return false;
    }
}

EOF
fi

echo "Finished."
