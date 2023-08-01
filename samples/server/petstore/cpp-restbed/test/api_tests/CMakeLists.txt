
add_executable(api_tests)

target_sources(api_tests
    PRIVATE
        main.cpp
        store_api_test.cpp
        beast_client.cpp
)

target_include_directories(api_tests
    SYSTEM PRIVATE
        ${Boost_INCLUDE_DIRS}
        #${restbed_SOURCE_DIR}/source
)

target_link_libraries(api_tests
    PRIVATE
        ${Boost_LIBRARIES}
        org.openapitools.server.apiStubs
)

add_test(NAME api_tests COMMAND api_tests)
