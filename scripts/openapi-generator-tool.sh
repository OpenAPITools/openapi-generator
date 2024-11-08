#!/bin/sh

# Default values
PROJECT_PATH=".."
OUTPUT_FOLDER="../out"
GENERATOR_NAME="swift5"
INPUT_SPEC=""
TEMPLATE_FOLDER=""
IGNORE_BUILD=false

# Function to display usage
usage() {
    echo "Usage: openapi-generator-tool.sh [-p project_path] [-o output_folder] [-g generator_name] -i input_spec"
    echo "Options:"
    echo "  -p                 Path to the project (default: ..)"
    echo "  -o                 Output folder (default: ../output)"
    echo "  -g                 Generator name (default: swift5)"
    echo "  -i                 Input specification file (mandatory)"
    echo '  -t                 Template directory (defaut: empty)'
    echo "  --ignore-build     Ignore the build process (default: false)"
    echo "  --help             Display this help message"
    exit 1
}

# Parse command-line arguments
while [ "$1" != "" ]; do
    case $1 in
        -p ) shift
             PROJECT_PATH=$1
             ;;
        -o ) shift
             OUTPUT_FOLDER=$1
             ;;
        -g ) shift
             GENERATOR_NAME=$1
             ;;
        -i ) shift
             INPUT_SPEC=$1
             ;;
        -t ) shift
             TEMPLATE_FOLDER=$1
             ;;
        --ignore-build ) IGNORE_BUILD=true
             ;;
        --help ) usage
             ;;
        * ) usage
             ;;
    esac
    shift
done

# Check if the mandatory -i option is provided
if [ -z $INPUT_SPEC ]; then
    echo "Error: -i input_spec is required"
    usage
fi

# Clean and build main project if needed
if [ $IGNORE_BUILD = false ] ; then
    echo "Building the project..."
    mvn clean -f $PROJECT_PATH/pom.xml
    mvn package -f $PROJECT_PATH/pom.xml
fi

PROJECT_JAR_PATH=$PROJECT_PATH/modules/openapi-generator-cli/target/openapi-generator-cli.jar
rm -rf $OUTPUT_FOLDER

# Build the java command
JAVA_CMD="java -jar $PROJECT_JAR_PATH generate \
    --generator-name $GENERATOR_NAME \
    --input-spec $INPUT_SPEC \
    --output $OUTPUT_FOLDER/generated \
    --global-property models \
    --skip-validate-spec"

# Append --template-dir if TEMPLATE_FOLDER is not empty
if [ -n "$TEMPLATE_FOLDER" ]; then
    JAVA_CMD="$JAVA_CMD --template-dir $TEMPLATE_FOLDER"
fi

# Execute the java command
$JAVA_CMD