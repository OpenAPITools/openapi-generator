cmake_minimum_required(VERSION 3.2 FATAL_ERROR)
project(cpp-qt5-server)

set(CMAKE_VERBOSE_MAKEFILE ON)
set(CMAKE_INCLUDE_CURRENT_DIR ON)
set(CMAKE_AUTOMOC ON)

set(CMAKE_C_FLAGS   "${CMAKE_C_FLAGS}   -fPIC -Wall -pg -g3 -Wno-unused-variable")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fPIC -std=c++14 -Wall -pg -g3 -Wno-unused-variable")

find_package(Qt5Core REQUIRED)
find_package(Qt5Network REQUIRED)

file(GLOB SRCS
    ${CMAKE_CURRENT_SOURCE_DIR}/models/*.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/handlers/*.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/main.cpp
)

include_directories(
    ${Qt5Core_INCLUDE_DIRS}
    ${Qt5Network_INCLUDE_DIRS}
    ${CMAKE_CURRENT_SOURCE_DIR}/models
    ${CMAKE_CURRENT_SOURCE_DIR}/handlers
)

link_directories(
    ${CMAKE_PREFIX_PATH}/lib
)

add_executable(${PROJECT_NAME} ${SRCS})
add_dependencies(${PROJECT_NAME} QHTTPENGINE)
target_link_libraries(${PROJECT_NAME} Qt5Core Qt5Network ssl crypto qhttpengine)
set_property(TARGET ${PROJECT_NAME} PROPERTY CXX_STANDARD 14)
set_property(TARGET ${PROJECT_NAME} PROPERTY CXX_STANDARD_REQUIRED ON)

install(TARGETS ${PROJECT_NAME} RUNTIME DESTINATION bin LIBRARY DESTINATION lib ARCHIVE DESTINATION lib)
