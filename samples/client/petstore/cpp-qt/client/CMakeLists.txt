cmake_minimum_required(VERSION 3.2)

project(CppQtPetstoreClient)

find_package(Qt${QT_VERSION_MAJOR} REQUIRED COMPONENTS Core Network Gui)

include(GNUInstallDirs)
include(CMakePackageConfigHelpers)

add_library(${PROJECT_NAME}
  PFXApiResponse.h
  PFXCategory.h
  PFXOrder.h
  PFXPet.h
  PFXTag.h
  PFXTestAnyType.h
  PFXUser.h
  PFXPetApi.h
  PFXPrimitivesApi.h
  PFXStoreApi.h
  PFXUserApi.h
  PFXHelpers.h
  PFXHttpRequest.h
  PFXObject.h
  PFXEnum.h
  PFXHttpFileElement.h
  PFXServerConfiguration.h
  PFXServerVariable.h
  PFXOauth.h
  PFXApiResponse.cpp
  PFXCategory.cpp
  PFXOrder.cpp
  PFXPet.cpp
  PFXTag.cpp
  PFXTestAnyType.cpp
  PFXUser.cpp
  PFXPetApi.cpp
  PFXPrimitivesApi.cpp
  PFXStoreApi.cpp
  PFXUserApi.cpp
  PFXHelpers.cpp
  PFXHttpRequest.cpp
  PFXHttpFileElement.cpp
  PFXOauth.cpp
  )

target_include_directories(${PROJECT_NAME}
    PUBLIC
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
        $<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}>
)

target_link_libraries(${PROJECT_NAME}
    PUBLIC
        Qt${QT_VERSION_MAJOR}::Core
        Qt${QT_VERSION_MAJOR}::Network
        Qt${QT_VERSION_MAJOR}::Gui

)

configure_package_config_file(${CMAKE_CURRENT_SOURCE_DIR}/Config.cmake.in
    "${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}Config.cmake"
    INSTALL_DESTINATION "${CMAKE_INSTALL_LIBDIR}/cmake/${PROJECT_NAME}"
)

install(
    TARGETS ${PROJECT_NAME}
    EXPORT ${PROJECT_NAME}Targets
    LIBRARY DESTINATION "${CMAKE_INSTALL_LIBDIR}"
    ARCHIVE DESTINATION "${CMAKE_INSTALL_LIBDIR}"
    RUNTIME DESTINATION "${CMAKE_INSTALL_BINDIR}"
    INCLUDES DESTINATION "${CMAKE_INSTALL_INCLUDEDIR}/${PROJECT_NAME}"
)

install(
    FILES ${HEADER_FILES}
    DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/${PROJECT_NAME}
)

install(
    FILES "${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}Config.cmake"
    DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/${PROJECT_NAME}
)

install(
    EXPORT ${PROJECT_NAME}Targets
    DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/${PROJECT_NAME}
)
