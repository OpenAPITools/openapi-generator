cmake_minimum_required (VERSION 3.2)

project(cpprest-petstore)
set(CMAKE_VERBOSE_MAKEFILE ON)
set(CMAKE_INCLUDE_CURRENT_DIR ON)


set(CMAKE_C_FLAGS   "${CMAKE_C_FLAGS}   -fPIC -Wall -Wno-unused-variable")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fPIC -std=c++14 -Wall -Wno-unused-variable")

set(CMAKE_C_FLAGS   "${CMAKE_C_FLAGS}   -pg -g3")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -pg -g3")
set(CMAKE_BUILD_TYPE Debug)
set(CMAKE_PREFIX_PATH /usr/lib/x86_64-linux-gnu/cmake)
find_package(cpprestsdk REQUIRED)
find_package(Boost REQUIRED)
add_subdirectory(client)

file(GLOB SRCS
    ${CMAKE_CURRENT_SOURCE_DIR}/*.cpp
)
include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}/client
    ${CMAKE_CURRENT_SOURCE_DIR}/client/model
    ${CMAKE_CURRENT_SOURCE_DIR}/client/api
)

link_directories(
    ${Boost_LIBRARY_DIRS}
)
add_executable(${PROJECT_NAME} ${SRCS})
add_dependencies(${PROJECT_NAME} CppRestOpenAPIClient )
target_link_libraries(${PROJECT_NAME}  CppRestOpenAPIClient cpprest pthread boost_system crypto)
set_property(TARGET ${PROJECT_NAME} PROPERTY CXX_STANDARD 14)
set_property(TARGET ${PROJECT_NAME} PROPERTY CXX_STANDARD_REQUIRED ON)

install(TARGETS ${PROJECT_NAME} RUNTIME DESTINATION bin LIBRARY DESTINATION lib ARCHIVE DESTINATION lib)
