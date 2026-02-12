cmake_minimum_required(VERSION 3.14)
project(cpp_httplib_server_petstore LANGUAGES CXX)

include(FetchContent)

# Fetch nlohmann_json
FetchContent_Declare(
  nlohmann_json
  GIT_REPOSITORY https://github.com/nlohmann/json.git
  GIT_TAG v3.11.3
)
FetchContent_MakeAvailable(nlohmann_json)

# Fetch cpp-httplib
FetchContent_Declare(
  httplib
  GIT_REPOSITORY https://github.com/yhirose/cpp-httplib.git
  GIT_TAG v0.15.3
)
FetchContent_MakeAvailable(httplib)

# System libraries - install with: sudo apt-get install libssl-dev zlib1g-dev
find_package(OpenSSL REQUIRED)
find_package(ZLIB REQUIRED)

set(TARGET_NAME cpp_httplib_server_petstore_openapi_lib)
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
# Required libraries/headers are httplib,ssl,nlohmann::json,zlib
target_link_libraries(${TARGET_NAME}
                        PRIVATE
                        httplib::httplib
                        OpenSSL::SSL
                        OpenSSL::Crypto
                        nlohmann_json::nlohmann_json
                        ZLIB::ZLIB)

