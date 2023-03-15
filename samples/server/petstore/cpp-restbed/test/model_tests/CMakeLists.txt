
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
        _foo_get_default_response_test.cpp
        _special_model_name_test.cpp
        additional_properties_class_test.cpp
        AllOfWithSingleRef_test.cpp
        Animal_test.cpp
)

target_include_directories(model_tests
    SYSTEM PRIVATE
        ${Boost_INCLUDE_DIRS}
)

target_include_directories(model_tests PRIVATE ${CMAKE_CURRENT_LIST_DIR})

target_link_libraries(model_tests
    PRIVATE
        org.openapitools.server.apiStubs
        ${Boost_LIBRARIES})


add_test(NAME model_tests COMMAND model_tests)