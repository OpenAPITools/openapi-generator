set(TARGET_NAME org.openapitools.server.apiStubs)

# Get generated filenames
file(STRINGS "${CMAKE_CURRENT_LIST_DIR}/.openapi-generator/FILES" GENERATED_SOURCES)
list(FILTER GENERATED_SOURCES INCLUDE REGEX ".*cpp$")

set(GENERATED_SOURCES_INCLUDE_DIRS
    ${CMAKE_CURRENT_LIST_DIR}/model
    ${CMAKE_CURRENT_LIST_DIR}/api
    ${CMAKE_CURRENT_LIST_DIR}
)


add_library(${TARGET_NAME} SHARED)

target_sources(${TARGET_NAME}
    PRIVATE
        ${GENERATED_SOURCES})

target_include_directories(${TARGET_NAME}
    PUBLIC
        ${GENERATED_SOURCES_INCLUDE_DIRS}
)


target_include_directories(${TARGET_NAME}
    SYSTEM PUBLIC
        ${restbed_SOURCE_DIR}/source)

target_link_libraries(${TARGET_NAME}
    PUBLIC
        Boost::system
        restbed-shared
        -lpthread)
