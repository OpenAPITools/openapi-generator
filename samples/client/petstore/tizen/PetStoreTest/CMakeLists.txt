CMAKE_MINIMUM_REQUIRED(VERSION 2.6)
PROJECT("PetStoreTest")
INCLUDE(GNUInstallDirs)

SET(target ${PROJECT_NAME})

# Dependencies
SET(dependents "glib-2.0 json-glib-1.0 libcurl")
INCLUDE(FindPkgConfig)
pkg_check_modules(pkgs REQUIRED ${dependents})
FOREACH(flag ${pkgs_CFLAGS})
	SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${flag}")
ENDFOREACH(flag)

ADD_DEFINITIONS(-O2 -w -fPIC -fvisibility=default -Wno-unused-variable -Wno-shadow -Wno-cast-qual -fpermissive)
SET(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -fPIC")

FILE(GLOB_RECURSE SRCS "./src/*.cpp" "../src/*.cpp")
add_executable(${PROJECT_NAME} ${SRCS})

include_directories(${pkgs_INCLUDE_DIRS})
link_directories(${pkgs_LIBRARY_DIRS})
target_link_libraries(${PROJECT_NAME} ${pkgs_LIBRARIES})

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)
