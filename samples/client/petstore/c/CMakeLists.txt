cmake_minimum_required (VERSION 2.6)
project (CGenerator)

file(GLOB SRC_C src/*.c)
file(GLOB UNIT_TESTS_C unit-tests/*.c)
#file(GLOB UNIT_TEST_C unit-test/*.c)
file(GLOB MODEL_C model/*.c)
file(GLOB API_C api/*.c)
file(GLOB EXTERNAL_SRC_C external/src/*.c)
set(ALL_SRC_LIST ${SRC_C} ${UNIT_TESTS_C} ${UNIT_TEST_C} ${MODEL_C} ${API_C})
set(CMAKE_BUILD_TYPE Debug)

include(CTest)
include_directories(include)
include_directories(external/include)
include_directories(model)
include_directories(api)

find_program(VALGRIND valgrind)
if(VALGRIND)
	set(CMAKE_MEMORYCHECK_COMMAND valgrind)
	set(CMAKE_MEMORYCHECK_COMMAND_OPTIONS "--leak-check=full --track-origins=yes --read-var-info=yes --show-leak-kinds=all --error-exitcode=1")
	set(VALGRIND_LIST "")
endif()

find_package(CURL 7.61.1 REQUIRED)
if(CURL_FOUND)
	include_directories(${CURL_INCLUDE_DIR})
	set(PLATFORM_LIBRARIES ${PLATFORM_LIBRARIES} ${CURL_LIBRARIES} )
else(CURL_FOUND)
	message(FATAL_ERROR "Could not find the CURL library and development files.")
endif()

foreach(ELEMENT ${UNIT_TESTS_C})
    get_filename_component(ELEMENT_NAME ${ELEMENT} NAME_WE)
    string(REGEX REPLACE "\\.c$" "" ELEMENT_REPLACED ${ELEMENT_NAME})
    set(EXE_NAME unit-${ELEMENT_REPLACED})
	add_executable(${EXE_NAME} ${ELEMENT} ${SRC_C} ${MODEL_C} ${API_C} ${EXTERNAL_SRC_C})
	target_link_libraries(${EXE_NAME} ${CURL_LIBRARIES})
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

# comment out below as auto-generated test file is not supported at the moment
#foreach(ELEMENT ${UNIT_TEST_C})
#get_filename_component(ELEMENT_NAME ${ELEMENT} NAME_WE)
#string(REGEX REPLACE "\\.c$" "" ELEMENT_REPLACED ${ELEMENT_NAME})
#set(EXE_NAME unit-${ELEMENT_REPLACED})
#add_executable(${EXE_NAME} ${ELEMENT} ${SRC_C} ${MODEL_C} ${API_C} ${EXTERNAL_SRC_C})
#target_link_libraries(${EXE_NAME} ${CURL_LIBRARIES})
#add_test(NAME ${EXE_NAME} COMMAND ${CMAKE_CURRENT_BINARY_DIR}/${EXE_NAME})
#
#if(VALGRIND)
#set(memcheck_command "${CMAKE_MEMORYCHECK_COMMAND} ${CMAKE_MEMORYCHECK_COMMAND_OPTIONS}")
#separate_arguments(memcheck_command)
#
#add_test(
#NAME valgrind-test-${ELEMENT_REPLACED}
#COMMAND ${memcheck_command} ${CMAKE_CURRENT_BINARY_DIR}/${EXE_NAME}
#WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
#)
#endif()
#endforeach()

#For common coding standard (code beautifier/pretty printing)
find_program(UNCRUSTIFY uncrustify)
if(UNCRUSTIFY)
	add_custom_target(
		uncrustify
	)

	foreach(ELEMENT ${ALL_SRC_LIST})
		string(REGEX REPLACE "/" "_" ELEMENT_NAME ${ELEMENT})
		set(DEP_NAME "uncrustify-${ELEMENT_NAME}")
		add_custom_target(
			${DEP_NAME}
			uncrustify -c uncrustify-rules.cfg --no-backup ${ELEMENT}
			DEPENDS ${ELEMENT}
			WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
			VERBATIM
		)
		add_dependencies(uncrustify ${DEP_NAME})
	endforeach()
endif()
