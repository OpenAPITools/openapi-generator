
add_executable(api_tests)

target_sources(api_tests
        PRIVATE
        pet_api_test.cpp
        main.cpp
)

target_include_directories(api_tests
        PRIVATE
            ${CMAKE_CURRENT_LISTS_DIR}
        SYSTEM PRIVATE
            ${Boost_INCLUDE_DIRS})

target_link_libraries(api_tests
        PRIVATE
            client
            ${Boost_LIBRARIES})


add_test(NAME api_tests COMMAND api_tests)