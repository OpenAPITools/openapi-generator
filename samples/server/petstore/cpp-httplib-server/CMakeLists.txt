cmake_minimum_required(VERSION 3.10)
project(cpp_httplib_server LANGUAGES CXX)

set(TARGET_NAME cpp_httplib_server_openapi_lib)
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
file(GLOB API_SRCS
    ${CMAKE_CURRENT_SOURCE_DIR}/api/*.h
    ${CMAKE_CURRENT_SOURCE_DIR}/api/*.cpp
)
file(GLOB MODEL_SRCS
    ${CMAKE_CURRENT_SOURCE_DIR}/models/*.h
    ${CMAKE_CURRENT_SOURCE_DIR}/models/*.cpp
)
add_library(${TARGET_NAME} ${API_SRCS} ${MODEL_SRCS})
target_include_directories (${TARGET_NAME} PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})
# Make sure these libraries/headers are available in the build environment before linking
# Required libraries/headers are httplib,ssl,nlohmann::json
target_link_libraries(${TARGET_NAME} httplib ssl nlohmann_json::nlohmann_json)