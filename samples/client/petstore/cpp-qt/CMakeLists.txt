cmake_minimum_required(VERSION 3.2)

project(cpp-qt-petstore)
set(CMAKE_VERBOSE_MAKEFILE ON)
set(CMAKE_INCLUDE_CURRENT_DIR ON)
set(CMAKE_AUTOMOC ON)

find_package(QT NAMES Qt6 Qt5 REQUIRED COMPONENTS Core Network Gui Test)

add_subdirectory(client)

find_package(Qt${QT_VERSION_MAJOR} REQUIRED COMPONENTS Test)

add_executable(cpp-qt-petstore
    PetStore/main.cpp
    PetStore/PetApiTests.cpp
    PetStore/StoreApiTests.cpp
    PetStore/UserApiTests.cpp
    )

target_link_libraries(cpp-qt-petstore PRIVATE CppQtPetstoreClient Qt${QT_VERSION_MAJOR}::Test)

enable_testing()

add_test(NAME cpp-qt-petstore-test COMMAND cpp-qt-petstore)
