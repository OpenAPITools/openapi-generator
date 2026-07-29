cmake_minimum_required(VERSION 3.14)
project(CppBoostBeastPetstoreClient VERSION 1.0.0 LANGUAGES CXX)

include(GNUInstallDirs)

if (POLICY CMP0167)
    cmake_policy(SET CMP0167 OLD)
endif ()

find_package(Boost 1.75 REQUIRED)
set(THREADS_TARGET_PREDEFINED FALSE)
if (TARGET Threads::Threads)
    set(THREADS_TARGET_PREDEFINED TRUE)
endif ()
set(THREADS_PREFER_PTHREAD_FLAG TRUE)
find_package(Threads REQUIRED)

if (NOT THREADS_TARGET_PREDEFINED)
    set_property(TARGET Threads::Threads PROPERTY IMPORTED_GLOBAL TRUE)
endif ()

set(OPENSSL_SSL_TARGET_PREDEFINED FALSE)
set(OPENSSL_CRYPTO_TARGET_PREDEFINED FALSE)
if (TARGET OpenSSL::SSL)
    set(OPENSSL_SSL_TARGET_PREDEFINED TRUE)
endif ()
if (TARGET OpenSSL::Crypto)
    set(OPENSSL_CRYPTO_TARGET_PREDEFINED TRUE)
endif ()

find_package(OpenSSL 1.1.0 REQUIRED COMPONENTS SSL Crypto)

# Imported targets created in this subdirectory are otherwise invisible to
# sibling consumers when this project is included with add_subdirectory().
if (NOT OPENSSL_SSL_TARGET_PREDEFINED)
    set_property(TARGET OpenSSL::SSL PROPERTY IMPORTED_GLOBAL TRUE)
endif ()
if (NOT OPENSSL_CRYPTO_TARGET_PREDEFINED)
    set_property(TARGET OpenSSL::Crypto PROPERTY IMPORTED_GLOBAL TRUE)
endif ()


if (MSVC)
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /W4")
else ()
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fPIC -Wall")
endif ()


add_library(${PROJECT_NAME} SHARED)

set_property(TARGET ${PROJECT_NAME} PROPERTY CXX_STANDARD 17)
set_property(TARGET ${PROJECT_NAME} PROPERTY CXX_STANDARD_REQUIRED ON)
set_property(TARGET ${PROJECT_NAME} PROPERTY CXX_EXTENSIONS OFF)

target_sources(${PROJECT_NAME} PRIVATE
# models
    model/ApiResponse.cpp
    model/ApiResponse.h
    model/Category.cpp
    model/Category.h
    model/Order.cpp
    model/Order.h
    model/Pet.cpp
    model/Pet.h
    model/Tag.cpp
    model/Tag.h
    model/User.cpp
    model/User.h
# apis
    api/PetApi.cpp
    api/PetApi.h
    api/StoreApi.cpp
    api/StoreApi.h
    api/UserApi.cpp
    api/UserApi.h
# other
    api/HttpClient.h
    api/HttpClientImpl.cpp
    api/HttpClientImpl.h
    model/AnyType.h
)


target_link_libraries(${PROJECT_NAME}
    PUBLIC Boost::boost OpenSSL::SSL Threads::Threads)

target_include_directories(${PROJECT_NAME} PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/api>
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/model>
    $<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}/${PROJECT_NAME}>
    $<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}/${PROJECT_NAME}/api>
    $<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}/${PROJECT_NAME}/model>)

install(TARGETS ${PROJECT_NAME}
    RUNTIME DESTINATION "${CMAKE_INSTALL_BINDIR}"
    LIBRARY DESTINATION "${CMAKE_INSTALL_LIBDIR}"
    ARCHIVE DESTINATION "${CMAKE_INSTALL_LIBDIR}")

install(DIRECTORY api model
    DESTINATION "${CMAKE_INSTALL_INCLUDEDIR}/${PROJECT_NAME}"
    FILES_MATCHING PATTERN "*.h")
