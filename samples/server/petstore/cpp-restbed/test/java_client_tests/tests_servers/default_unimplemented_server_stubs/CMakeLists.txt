set(TARGET_NAME default_unimplemented_server_stubs)

add_executable(${TARGET_NAME})

target_sources(${TARGET_NAME}
        PRIVATE main.cpp)

target_link_libraries(${TARGET_NAME} PRIVATE org.openapitools.server.apiStubs)

run_java_client_test_for_cpp_server(${TARGET_NAME})