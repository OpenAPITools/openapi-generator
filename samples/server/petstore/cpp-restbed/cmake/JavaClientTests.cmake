
add_custom_target(run_all_java_client_test_for_cpp_server)

define_property(GLOBAL
        PROPERTY LAST_JAVA_CLIENT_TEST
        BRIEF_DOCS Used to order the tests to run after each other
        FULL_DOCS The tests are ordered by defining a dependency chain)
set_property(GLOBAL
        PROPERTY LAST_JAVA_CLIENT_TEST run_all_java_client_test_for_cpp_server)

set(RUN_CLIENT_TESTS_SHELL_TEMPLATE "run_java_client_tests_template.txt")

function(run_java_client_test_for_cpp_server TARGET_NAME)
    set(TEST_SERVER_EXECUTABLE "${CMAKE_CURRENT_BINARY_DIR}/${TARGET_NAME}")

    set(RUN_TESTS_TARGET run_${TARGET_NAME}_test)
    configure_file(${CMAKE_SOURCE_DIR}/cmake/${RUN_CLIENT_TESTS_SHELL_TEMPLATE}
            ${CMAKE_CURRENT_BINARY_DIR}/${RUN_TESTS_TARGET}.sh
            @ONLY)

    add_custom_target(${RUN_TESTS_TARGET}
            COMMAND ${CMAKE_CURRENT_BINARY_DIR}/${RUN_TESTS_TARGET}.sh
            DEPENDS ${TARGET_NAME}
            USES_TERMINAL
            COMMENT "Running tests: ${TARGET_NAME}")

    get_property(LAST_TEST
            GLOBAL
            PROPERTY LAST_JAVA_CLIENT_TEST)
    add_dependencies(run_all_java_client_test_for_cpp_server ${RUN_TESTS_TARGET})
    add_dependencies(${LAST_TEST} ${RUN_TESTS_TARGET})
    set_property(GLOBAL
            PROPERTY LAST_JAVA_CLIENT_TEST ${RUN_TESTS_TARGET})

endfunction()
