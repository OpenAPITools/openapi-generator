cmake_minimum_required(VERSION 3.2)

project(cpp-qt5-petstore)
set(CMAKE_VERBOSE_MAKEFILE ON)
set(CMAKE_INCLUDE_CURRENT_DIR ON)
set(CMAKE_AUTOMOC ON)

add_subdirectory(client)

find_package(QT NAMES Qt6 Qt5 REQUIRED COMPONENTS Test)
find_package(Qt${QT_VERSION_MAJOR} REQUIRED COMPONENTS Test)

add_executable(${PROJECT_NAME}
    PetStore/main.cpp
    PetStore/PetApiTests.cpp
    PetStore/StoreApiTests.cpp
    PetStore/UserApiTests.cpp
    )

target_link_libraries(${PROJECT_NAME} PRIVATE CppQtPetstoreClient Qt${QT_VERSION_MAJOR}::Test)
