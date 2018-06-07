cmake_minimum_required (VERSION 2.6)
project (CGenerator)

file(GLOB UNIT_TESTS_C unit-tests/*.c)
file(GLOB SRC_C src/*.c)
file(GLOB MODEL_C model/*.c)
file(GLOB API_C api/*.c)

include(CTest)
include_directories(include)

find_program(VALGRIND valgrind)
if(VALGRIND)
	set(CMAKE_MEMORYCHECK_COMMAND valgrind)
	set(CMAKE_MEMORYCHECK_COMMAND_OPTIONS "--leak-check=full --track-origins=yes --read-var-info=yes --show-leak-kinds=all --error-exitcode=1")
	set(VALGRIND_LIST "")
endif()

foreach(ELEMENT ${UNIT_TESTS_C})
    get_filename_component(ELEMENT_NAME ${ELEMENT} NAME_WE)
    string(REGEX REPLACE "\\.c$" "" ELEMENT_REPLACED ${ELEMENT_NAME})
    set(EXE_NAME unit-test-${ELEMENT_REPLACED})
    add_executable(${EXE_NAME} ${ELEMENT} ${SRC_C} ${MODEL_C} ${API_C})
    add_test(NAME ${EXE_NAME} COMMAND ${CMAKE_CURRENT_BINARY_DIR}/${EXE_NAME})

    if(VALGRIND)
		set(memcheck_command "${CMAKE_MEMORYCHECK_COMMAND} ${CMAKE_MEMORYCHECK_COMMAND_OPTIONS}")
		separate_arguments(memcheck_command)

		add_test(
			NAME valgrind-test-${ELEMENT_REPLACED}
			COMMAND ${memcheck_command} ${CMAKE_CURRENT_BINARY_DIR}/${EXE_NAME}
			WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
		)
	endif()
endforeach()