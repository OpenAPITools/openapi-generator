

include(FetchContent)

## Restbed
FetchContent_Declare(
        restbed
        GIT_REPOSITORY https://github.com/Corvusoft/restbed
        GIT_TAG        4.7
)

set(BUILD_TESTS OFF CACHE BOOL "Disable internal restbed tests")
set(BUILD_SSL OFF CACHE BOOL "Disable SSL")
FetchContent_MakeAvailable(restbed)

FetchContent_GetProperties(restbed)
if(NOT restbed_POPULATED)

    FetchContent_Populate(restbed)
    add_subdirectory(${restbed_SOURCE_DIR} ${restbed_BINARY_DIR})
endif()


## Boost
find_package(Boost 1.7.0 COMPONENTS system unit_test_framework REQUIRED)
