
add_executable(model_tests)

target_sources(model_tests
        PRIVATE
        main.cpp
        pet_model_test.cpp
        tag_model_test.cpp
        user_model_test.cpp
        category_model_test.cpp
        order_model_test.cpp
        api_response_model_test.cpp
)

target_include_directories(model_tests
        PRIVATE
            ${CMAKE_CURRENT_LISTS_DIR}
        SYSTEM PRIVATE
            ${Boost_INCLUDE_DIRS})

target_link_libraries(model_tests
        PRIVATE
            client
            ${Boost_LIBRARIES})


add_test(NAME model_tests COMMAND model_tests)