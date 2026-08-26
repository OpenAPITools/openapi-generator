cmake_minimum_required(VERSION 3.14)
project(CppBoostBeastPetstoreServer VERSION 1.0.0 LANGUAGES CXX)

include(GNUInstallDirs)

if (POLICY CMP0167)
    cmake_policy(SET CMP0167 OLD)
endif ()

set(BOOST_BOOST_TARGET_PREDEFINED FALSE)
set(BOOST_JSON_TARGET_PREDEFINED FALSE)
set(BOOST_URL_TARGET_PREDEFINED FALSE)
if (TARGET Boost::boost)
    set(BOOST_BOOST_TARGET_PREDEFINED TRUE)
endif ()
if (TARGET Boost::json)
    set(BOOST_JSON_TARGET_PREDEFINED TRUE)
endif ()
if (TARGET Boost::url)
    set(BOOST_URL_TARGET_PREDEFINED TRUE)
endif ()

find_package(Boost 1.81 REQUIRED COMPONENTS json url)
# Imported targets created in this subdirectory are otherwise invisible to
# sibling consumers when this project is included with add_subdirectory().
if (NOT BOOST_BOOST_TARGET_PREDEFINED)
    set_property(TARGET Boost::boost PROPERTY IMPORTED_GLOBAL TRUE)
endif ()
if (NOT BOOST_JSON_TARGET_PREDEFINED)
    set_property(TARGET Boost::json PROPERTY IMPORTED_GLOBAL TRUE)
endif ()
if (NOT BOOST_URL_TARGET_PREDEFINED)
    set_property(TARGET Boost::url PROPERTY IMPORTED_GLOBAL TRUE)
endif ()
set(THREADS_TARGET_PREDEFINED FALSE)
if (TARGET Threads::Threads)
    set(THREADS_TARGET_PREDEFINED TRUE)
endif ()
set(THREADS_PREFER_PTHREAD_FLAG TRUE)
find_package(Threads REQUIRED)
if (NOT THREADS_TARGET_PREDEFINED)
    set_property(TARGET Threads::Threads PROPERTY IMPORTED_GLOBAL TRUE)
endif ()

# Boost.URL is consumed in header-only mode from HttpServer.cpp.
if (MSVC)
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /W4")
else ()
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fPIC -Wall")
endif ()


add_library(CppBoostBeastPetstoreServer STATIC)

set_property(TARGET CppBoostBeastPetstoreServer PROPERTY CXX_STANDARD 17)
set_property(TARGET CppBoostBeastPetstoreServer PROPERTY CXX_STANDARD_REQUIRED ON)
set_property(TARGET CppBoostBeastPetstoreServer PROPERTY CXX_EXTENSIONS OFF)

target_sources(CppBoostBeastPetstoreServer PRIVATE
# models
    model/Error.cpp
    model/Error.h
    model/Pet.cpp
    model/Pet.h
    model/User.cpp
    model/User.h
# apis
    api/PetsApi.cpp
    api/PetsApi.h
    api/StoreApi.cpp
    api/StoreApi.h
    api/UsersApi.cpp
    api/UsersApi.h
# server runtime
    server/Authorizer.h
    server/BodyJson.h
    server/HttpServer.cpp
    server/HttpServer.h
    server/ParamCodecs.h
    server/Problem.h
    server/Responder.h
    server/Router.h
# shared model/validation support
    model/AnyType.h
    model/NullableField.h
    model/Oas31DeepEqual.h
    model/Oas31ExactNumber.cpp
    model/Oas31ExactNumber.h
    model/Oas31SchemaIr.h
    model/Oas31ExactJson.h
    model/Oas31Validator.h
    model/ValidationTypes.h
    model/schema_ir.generated.cpp
    model/Oas31SchemaRegistry.h
)

target_link_libraries(CppBoostBeastPetstoreServer
    PUBLIC Boost::boost Boost::json Boost::url Threads::Threads)

target_include_directories(CppBoostBeastPetstoreServer PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/api>
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/model>
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/server>
    $<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}/${PROJECT_NAME}>
    $<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}/${PROJECT_NAME}/api>
    $<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}/${PROJECT_NAME}/model>
    $<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}/${PROJECT_NAME}/server>)

install(TARGETS CppBoostBeastPetstoreServer
    ARCHIVE DESTINATION "${CMAKE_INSTALL_LIBDIR}")

install(DIRECTORY api model server
    DESTINATION "${CMAKE_INSTALL_INCLUDEDIR}/${PROJECT_NAME}"
    FILES_MATCHING PATTERN "*.h")

add_executable(${PROJECT_NAME}_main main.cpp)
set_property(TARGET ${PROJECT_NAME}_main PROPERTY CXX_STANDARD 17)
set_property(TARGET ${PROJECT_NAME}_main PROPERTY CXX_STANDARD_REQUIRED ON)
set_property(TARGET ${PROJECT_NAME}_main PROPERTY CXX_EXTENSIONS OFF)
target_link_libraries(${PROJECT_NAME}_main PRIVATE CppBoostBeastPetstoreServer)
