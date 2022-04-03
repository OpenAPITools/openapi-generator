cmake_minimum_required(VERSION 3.2)

project(cpp-qt5-petstore)
set(CMAKE_VERBOSE_MAKEFILE ON)
set(CMAKE_INCLUDE_CURRENT_DIR ON)
set(CMAKE_AUTOMOC ON)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fPIC -Wall -Wno-unused-variable")

find_package(Qt5Core REQUIRED)
find_package(Qt5Network REQUIRED)
find_package(Qt5Test REQUIRED)
find_package(Qt5Gui REQUIRED)

include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}/client
)

add_subdirectory(client)
add_executable(${PROJECT_NAME}
    PetStore/main.cpp
    PetStore/PetApiTests.cpp
    PetStore/StoreApiTests.cpp
    PetStore/UserApiTests.cpp
    )
target_link_libraries(${PROJECT_NAME} PRIVATE client)
target_link_libraries(${PROJECT_NAME} PRIVATE Qt5::Core Qt5::Network Qt5::Test Qt5::Gui)
set_property(TARGET ${PROJECT_NAME} PROPERTY CXX_STANDARD 14)
set_property(TARGET ${PROJECT_NAME} PROPERTY CXX_STANDARD_REQUIRED ON)
set_property(TARGET ${PROJECT_NAME} PROPERTY CXX_EXTENSIONS OFF)

install(TARGETS ${PROJECT_NAME} RUNTIME DESTINATION bin LIBRARY DESTINATION lib ARCHIVE DESTINATION lib)
