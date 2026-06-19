cmake_minimum_required (VERSION 3.2)

project(api-server)

include(ExternalProject)

set(EXTERNAL_INSTALL_LOCATION ${CMAKE_CURRENT_SOURCE_DIR}/external)

ExternalProject_Add(OATPP
    GIT_REPOSITORY https://github.com/oatpp/oatpp.git
    BUILD_IN_SOURCE true
    GIT_TAG 1.3.1
    CMAKE_ARGS -DCMAKE_INSTALL_PREFIX=${EXTERNAL_INSTALL_LOCATION}
)

include_directories(${EXTERNAL_INSTALL_LOCATION}/include/oatpp-1.3.0/oatpp)

include_directories(model)
include_directories(api)
include_directories(impl)

file(GLOB SRCS
    ${CMAKE_CURRENT_SOURCE_DIR}/impl/*.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/*.cpp
)

add_executable(${PROJECT_NAME} ${SRCS})
target_compile_features(${PROJECT_NAME} PRIVATE cxx_std_17)

add_dependencies(${PROJECT_NAME} OATPP)
if(WIN32)
target_link_libraries(${PROJECT_NAME} ${EXTERNAL_INSTALL_LOCATION}/lib/oatpp-1.3.0/oatpp.lib)
target_link_libraries(${PROJECT_NAME} ws2_32)
else()
target_link_libraries(${PROJECT_NAME} ${EXTERNAL_INSTALL_LOCATION}/lib/oatpp-1.3.0/liboatpp.a)
endif(WIN32)
