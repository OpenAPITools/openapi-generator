cmake_minimum_required (VERSION 3.2)

project(cpprest-petstore)
set(CMAKE_VERBOSE_MAKEFILE ON)
set(CMAKE_INCLUDE_CURRENT_DIR ON)

if (UNIX)
    message(STATUS "Building for Linux/Unix")

    set(CMAKE_BUILD_TYPE Debug)
    set(CMAKE_C_FLAGS   "${CMAKE_C_FLAGS}   -fPIC -Wall -Wno-unused-variable -pg -g3")
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fPIC -Wall -Wno-unused-variable -pg -g3")

    find_package(cpprestsdk REQUIRED)
    find_package(Boost REQUIRED)
else()
    message(STATUS "Building for Windows")

    find_package(cpprestsdk REQUIRED)
    find_package(Boost REQUIRED)
    find_package(pthreads REQUIRED)
endif()

add_subdirectory(client)

file(GLOB SRCS
    ${CMAKE_CURRENT_SOURCE_DIR}/*.cpp
)

add_executable(${PROJECT_NAME} ${SRCS})
add_dependencies(${PROJECT_NAME} CppRestOpenAPIClient )

target_include_directories(
    ${PROJECT_NAME} PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}/client
    ${CMAKE_CURRENT_SOURCE_DIR}/client/model
    ${CMAKE_CURRENT_SOURCE_DIR}/client/api
    ${Boost_INCLUDE_DIRS}
)

target_link_directories(
    ${PROJECT_NAME} PRIVATE
    ${Boost_LIBRARY_DIRS}
)

if (UNIX)
    target_link_libraries(${PROJECT_NAME} PRIVATE CppRestOpenAPIClient cpprest pthread ${Boost_LIBRARIES} crypto)
else()
    target_link_libraries(${PROJECT_NAME} PRIVATE CppRestOpenAPIClient cpprestsdk::cpprest ${pthreads_LIBRARIES} ${Boost_LIBRARIES} bcrypt)
endif()

set_property(TARGET ${PROJECT_NAME} PROPERTY CXX_STANDARD 14)
set_property(TARGET ${PROJECT_NAME} PROPERTY CXX_STANDARD_REQUIRED ON)

if (UNIX)
    install(TARGETS ${PROJECT_NAME} RUNTIME DESTINATION bin LIBRARY DESTINATION lib ARCHIVE DESTINATION lib)
endif()
