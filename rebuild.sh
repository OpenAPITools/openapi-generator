#!/bin/bash

set -euo pipefail

# Project configurations as associative arrays
declare -A mm_config=(
    [PROJECT]="mm-sttp4"
    [PROJECT_NAME]="mattermost-scala"
    [PROJECT_GROUP_ID]="ma.chinespirit"
    [PROJECT_ARTIFACT_ID]="mattermost-scala"
    [PROJECT_MAIN_PACKAGE]="ma.chinespirit.mm"
    [PROJECT_VERSION]="1.0.0-SNAPSHOT"
    [GENERATOR_NAME]="scala-sttp4-jsoniter"
    [SKIP_VALIDATE]="true"
    [ADDITIONAL_PROPS]=""
    [SCHEMA_MAPPINGS]=""
    [TYPE_MAPPINGS]=""
    [IMPORT_MAPPINGS]=""
)

declare -A kube_config=(
    [PROJECT]="kube-sttp4"
    [PROJECT_NAME]="kubeapi-scala"
    [PROJECT_GROUP_ID]="ma.chinespirit"
    [PROJECT_ARTIFACT_ID]="kubeapi-scala"
    [PROJECT_MAIN_PACKAGE]="ma.chinespirit.kube"
    [PROJECT_VERSION]="1.0.0-SNAPSHOT"
    [GENERATOR_NAME]="scala-sttp4-jsoniter"
    [SKIP_VALIDATE]="true"
    [ADDITIONAL_PROPS]=""
    [SCHEMA_MAPPINGS]="io.k8s.apimachinery.pkg.util.intstr.IntOrString=IntOrString"
    [TYPE_MAPPINGS]="IntOrString=IntOrString"
    [IMPORT_MAPPINGS]="IntOrString=ma.chinespirit.kube.ext.IntOrString"
)

declare -A stripe_config=(
    [PROJECT]="stripe-sttp4"
    [PROJECT_NAME]="stripe-scala"
    [PROJECT_GROUP_ID]="ma.chinespirit"
    [PROJECT_ARTIFACT_ID]="stripe-scala"
    [PROJECT_MAIN_PACKAGE]="ma.chinespirit.stripe"
    [PROJECT_VERSION]="1.0.0-SNAPSHOT"
    [GENERATOR_NAME]="scala-sttp4-jsoniter"
    [SKIP_VALIDATE]="true"
    [ADDITIONAL_PROPS]=""
    [SCHEMA_MAPPINGS]=""
    [TYPE_MAPPINGS]=""
    [IMPORT_MAPPINGS]=""
)

declare -A github_config=(
    [PROJECT]="github-sttp4"
    [PROJECT_NAME]="github-scala"
    [PROJECT_GROUP_ID]="ma.chinespirit"
    [PROJECT_ARTIFACT_ID]="github-scala"
    [PROJECT_MAIN_PACKAGE]="ma.chinespirit.github"
    [PROJECT_VERSION]="1.0.0-SNAPSHOT"
    [GENERATOR_NAME]="scala-sttp4-jsoniter"
    [SKIP_VALIDATE]="true"
    [ADDITIONAL_PROPS]=""
    [SCHEMA_MAPPINGS]=""
    [TYPE_MAPPINGS]=""
    [IMPORT_MAPPINGS]=""
)

declare -A spotify_config=(
    [PROJECT]="spotify-sttp4"
    [PROJECT_NAME]="spotify-scala"
    [PROJECT_GROUP_ID]="ma.chinespirit"
    [PROJECT_ARTIFACT_ID]="spotify-scala"
    [PROJECT_MAIN_PACKAGE]="ma.chinespirit.spotify"
    [PROJECT_VERSION]="1.0.0-SNAPSHOT"
    [GENERATOR_NAME]="scala-sttp4-jsoniter"
    [SKIP_VALIDATE]="true"
    [ADDITIONAL_PROPS]=""
    [SCHEMA_MAPPINGS]=""
    [TYPE_MAPPINGS]=""
    [IMPORT_MAPPINGS]=""
)

# Project mapping
declare -A projects=(
    [mattermost]="mm_config"
    [kubernetes]="kube_config"
    [stripe]="stripe_config"
    [github]="github_config"
    [spotify]="spotify_config"
)

# Default to mattermost project if no argument is provided
PROJECT_CONFIG=${1:-mm_config}

# If a project name is provided, look it up in the projects dictionary
if [[ -n "${projects[$1]:-}" ]]; then
    PROJECT_CONFIG=${projects[$1]}
fi

# Dynamically set project variables from the selected configuration
PROJECT="${!PROJECT_CONFIG[PROJECT]}"
PROJECT_NAME="${!PROJECT_CONFIG[PROJECT_NAME]}"
PROJECT_GROUP_ID="${!PROJECT_CONFIG[PROJECT_GROUP_ID]}"
PROJECT_ARTIFACT_ID="${!PROJECT_CONFIG[PROJECT_ARTIFACT_ID]}"
PROJECT_MAIN_PACKAGE="${!PROJECT_CONFIG[PROJECT_MAIN_PACKAGE]}"
PROJECT_VERSION="${!PROJECT_CONFIG[PROJECT_VERSION]}"

PROJECT_ROOT_PATH="../$PROJECT"

# Function to clean Maven build
clean_maven() {
    ./mvnw clean
}

# Function to install with Maven, skipping tests and javadocs
install_maven() {
    ./mvnw install -DskipTests -Dmaven.javadoc.skip=true
}

# Function to run openapi-generator-cli with default configuration
run_generator_jsoniter() {
    local generator_name="${!PROJECT_CONFIG[GENERATOR_NAME]}"
    local additional_props="mainPackage=$PROJECT_MAIN_PACKAGE,groupId=$PROJECT_GROUP_ID,artifactId=$PROJECT_ARTIFACT_ID,artifactVersion=$PROJECT_VERSION"
    
    # Append any additional properties from the project config
    if [[ -n "${!PROJECT_CONFIG[ADDITIONAL_PROPS]}" ]]; then
        additional_props+=",${!PROJECT_CONFIG[ADDITIONAL_PROPS]}"
    fi

    # Prepare optional flags
    local schema_mappings=""
    local type_mappings=""
    local import_mappings=""
    local validate_flag=""

    # Add schema mappings if specified
    if [[ -n "${!PROJECT_CONFIG[SCHEMA_MAPPINGS]}" ]]; then
        schema_mappings="--schema-mappings ${!PROJECT_CONFIG[SCHEMA_MAPPINGS]}"
    fi

    # Add type mappings if specified
    if [[ -n "${!PROJECT_CONFIG[TYPE_MAPPINGS]}" ]]; then
        type_mappings="--type-mappings ${!PROJECT_CONFIG[TYPE_MAPPINGS]}"
    fi

    # Add import mappings if specified
    if [[ -n "${!PROJECT_CONFIG[IMPORT_MAPPINGS]}" ]]; then
        import_mappings="--import-mappings ${!PROJECT_CONFIG[IMPORT_MAPPINGS]}"
    fi

    # Handle validation skipping
    if [[ "${!PROJECT_CONFIG[SKIP_VALIDATE]}" == "true" ]]; then
        validate_flag="--skip-validate-spec"
    fi

    java -jar modules/openapi-generator-cli/target/openapi-generator-cli.jar generate \
          -i $PROJECT_ROOT_PATH/openapi.json \
          --generator-name "$generator_name" \
          -o $PROJECT_ROOT_PATH \
          $validate_flag \
          --additional-properties="$additional_props" \
          $schema_mappings \
          $type_mappings \
          $import_mappings
}

run_generator_jsoniter_strict() {
    local generator_name="${!PROJECT_CONFIG[GENERATOR_NAME]}"
    local additional_props="mainPackage=$PROJECT_MAIN_PACKAGE,groupId=$PROJECT_GROUP_ID,artifactId=$PROJECT_ARTIFACT_ID,artifactVersion=$PROJECT_VERSION"
    
    # Append any additional properties from the project config
    if [[ -n "${!PROJECT_CONFIG[ADDITIONAL_PROPS]}" ]]; then
        additional_props+=",${!PROJECT_CONFIG[ADDITIONAL_PROPS]}"
    fi

    # Prepare optional flags
    local schema_mappings=""
    local type_mappings=""
    local import_mappings=""

    # Add schema mappings if specified
    if [[ -n "${!PROJECT_CONFIG[SCHEMA_MAPPINGS]}" ]]; then
        schema_mappings="--schema-mappings ${!PROJECT_CONFIG[SCHEMA_MAPPINGS]}"
    fi

    # Add type mappings if specified
    if [[ -n "${!PROJECT_CONFIG[TYPE_MAPPINGS]}" ]]; then
        type_mappings="--type-mappings ${!PROJECT_CONFIG[TYPE_MAPPINGS]}"
    fi

    # Add import mappings if specified
    if [[ -n "${!PROJECT_CONFIG[IMPORT_MAPPINGS]}" ]]; then
        import_mappings="--import-mappings ${!PROJECT_CONFIG[IMPORT_MAPPINGS]}"
    fi

    java -jar modules/openapi-generator-cli/target/openapi-generator-cli.jar generate \
          -i $PROJECT_ROOT_PATH/openapi.json \
          --generator-name "$generator_name" \
          -o $PROJECT_ROOT_PATH \
          --additional-properties="$additional_props" \
          $schema_mappings \
          $type_mappings \
          $import_mappings
}

# Function to run openapi-generator-cli with upstream configuration
run_generator_upstream() {
    local generator_name="scala-sttp4"
    local additional_props="mainPackage=$PROJECT_MAIN_PACKAGE,groupId=$PROJECT_GROUP_ID,artifactId=$PROJECT_ARTIFACT_ID,artifactVersion=$PROJECT_VERSION,jsonLibrary=circe"
    
    # Append any additional properties from the project config
    if [[ -n "${!PROJECT_CONFIG[ADDITIONAL_PROPS]}" ]]; then
        additional_props+=",${!PROJECT_CONFIG[ADDITIONAL_PROPS]}"
    fi

    # Prepare optional flags
    local schema_mappings=""
    local type_mappings=""
    local import_mappings=""
    local validate_flag=""

    # Add schema mappings if specified
    if [[ -n "${!PROJECT_CONFIG[SCHEMA_MAPPINGS]}" ]]; then
        schema_mappings="--schema-mappings ${!PROJECT_CONFIG[SCHEMA_MAPPINGS]}"
    fi

    # Add type mappings if specified
    if [[ -n "${!PROJECT_CONFIG[TYPE_MAPPINGS]}" ]]; then
        type_mappings="--type-mappings ${!PROJECT_CONFIG[TYPE_MAPPINGS]}"
    fi

    # Add import mappings if specified
    if [[ -n "${!PROJECT_CONFIG[IMPORT_MAPPINGS]}" ]]; then
        import_mappings="--import-mappings ${!PROJECT_CONFIG[IMPORT_MAPPINGS]}"
    fi

    # Handle validation skipping
    if [[ "${!PROJECT_CONFIG[SKIP_VALIDATE]}" == "true" ]]; then
        validate_flag="--skip-validate-spec"
    fi

    # Assumes the alternative openapi-generator-cli.jar is located at a specific path
    openapi-generator-cli generate \
          -i $PROJECT_ROOT_PATH/openapi.json \
          --generator-name "$generator_name" \
          -o $PROJECT_ROOT_PATH \
          $validate_flag \
          --additional-properties="$additional_props" \
          $schema_mappings \
          $type_mappings \
          $import_mappings
}

# Function to clean up specific files and directories in $PROJECT_ROOT_PATH
cleanup_generated_files() {
    rm -rf $PROJECT_ROOT_PATH/build.sbt $PROJECT_ROOT_PATH/target $PROJECT_ROOT_PATH/project $PROJECT_ROOT_PATH/README.md
    
    # Remove generated source files based on the project's main package
    local base_package=$(echo "$PROJECT_MAIN_PACKAGE" | tr '.' '/')
    rm -rf $PROJECT_ROOT_PATH/src/main/scala/$base_package/api
    rm -rf $PROJECT_ROOT_PATH/src/main/scala/$base_package/model
    rm -rf $PROJECT_ROOT_PATH/src/main/scala/$base_package/core
}

notify_failure() {
    say "You dun goofed, Mortal"
}

trap notify_failure ERR

# Function to run clean for all projects
clean_all_projects() {
    for project_name in "${!projects[@]}"; do
        PROJECT_CONFIG=${projects[$project_name]}
        
        # Dynamically set project variables from the selected configuration
        PROJECT="${!PROJECT_CONFIG[PROJECT]}"
        PROJECT_NAME="${!PROJECT_CONFIG[PROJECT_NAME]}"
        PROJECT_ROOT_PATH="../$PROJECT"
        PROJECT_MAIN_PACKAGE="${!PROJECT_CONFIG[PROJECT_MAIN_PACKAGE]}"

        echo "Cleaning project: $project_name"
        clean_maven
        cleanup_generated_files
        install_maven
        run_generator_jsoniter
    done
}

# Function to generate for all projects
generate_all_projects() {
    for project_name in "${!projects[@]}"; do
        PROJECT_CONFIG=${projects[$project_name]}
        
        # Dynamically set project variables from the selected configuration
        PROJECT="${!PROJECT_CONFIG[PROJECT]}"
        PROJECT_NAME="${!PROJECT_CONFIG[PROJECT_NAME]}"
        PROJECT_ROOT_PATH="../$PROJECT"
        PROJECT_MAIN_PACKAGE="${!PROJECT_CONFIG[PROJECT_MAIN_PACKAGE]}"

        echo "Generating project: $project_name"
        run_generator_jsoniter
    done
}

# Main script logic based on input parameters
case ${1:-} in
    clean)
        if [[ "${2:-}" == "all" ]]; then
            clean_all_projects
        else
            clean_maven
            cleanup_generated_files
            install_maven
            run_generator_jsoniter
        fi
        ;;
    generate)
        if [[ "${2:-}" == "all" ]]; then
            generate_all_projects
        else
            run_generator_jsoniter
        fi
        ;;
    generate-strict)
        if [[ "${2:-}" == "all" ]]; then
            for project_name in "${!projects[@]}"; do
                PROJECT_CONFIG=${projects[$project_name]}
                
                # Dynamically set project variables from the selected configuration
                PROJECT="${!PROJECT_CONFIG[PROJECT]}"
                PROJECT_NAME="${!PROJECT_CONFIG[PROJECT_NAME]}"
                PROJECT_ROOT_PATH="../$PROJECT"
                PROJECT_MAIN_PACKAGE="${!PROJECT_CONFIG[PROJECT_MAIN_PACKAGE]}"

                echo "Generating project (strict): $project_name"
                run_generator_jsoniter_strict
            done
        else
            run_generator_jsoniter_strict
        fi
        ;;
    upstream)
        if [[ "${2:-}" == "all" ]]; then
            for project_name in "${!projects[@]}"; do
                PROJECT_CONFIG=${projects[$project_name]}
                
                # Dynamically set project variables from the selected configuration
                PROJECT="${!PROJECT_CONFIG[PROJECT]}"
                PROJECT_NAME="${!PROJECT_CONFIG[PROJECT_NAME]}"
                PROJECT_ROOT_PATH="../$PROJECT"
                PROJECT_MAIN_PACKAGE="${!PROJECT_CONFIG[PROJECT_MAIN_PACKAGE]}"

                echo "Generating project (upstream): $project_name"
                cleanup_generated_files
                run_generator_upstream
            done
        else
            cleanup_generated_files
            run_generator_upstream
        fi
        ;;
    strict)
        if [[ "${2:-}" == "all" ]]; then
            for project_name in "${!projects[@]}"; do
                PROJECT_CONFIG=${projects[$project_name]}
                
                # Dynamically set project variables from the selected configuration
                PROJECT="${!PROJECT_CONFIG[PROJECT]}"
                PROJECT_NAME="${!PROJECT_CONFIG[PROJECT_NAME]}"
                PROJECT_ROOT_PATH="../$PROJECT"
                PROJECT_MAIN_PACKAGE="${!PROJECT_CONFIG[PROJECT_MAIN_PACKAGE]}"

                echo "Cleaning and generating project (strict): $project_name"
                clean_maven
                cleanup_generated_files
                install_maven
                run_generator_jsoniter_strict
            done
        else
            clean_maven
            cleanup_generated_files
            install_maven
            run_generator_jsoniter_strict
        fi
        ;;
    *)
        install_maven
        run_generator_jsoniter
        ;;
esac

say "It's done, Mortal"
