cmake_minimum_required (VERSION 2.6)
project (CGenerator)

cmake_policy(SET CMP0063 NEW)

set(CMAKE_C_VISIBILITY_PRESET default)
set(CMAKE_CXX_VISIBILITY_PRESET default)
set(CMAKE_VISIBILITY_INLINES_HIDDEN OFF)
set(CMAKE_BUILD_TYPE Debug)

set(pkgName "openapi_petstore")

find_package(CURL 7.58.0 REQUIRED)
if(CURL_FOUND)
	include_directories(${CURL_INCLUDE_DIR})
	set(PLATFORM_LIBRARIES ${PLATFORM_LIBRARIES} ${CURL_LIBRARIES} )
else(CURL_FOUND)
	message(FATAL_ERROR "Could not find the CURL library and development files.")
endif()

set(SRCS
    src/list.c
    src/apiKey.c
    src/apiClient.c
    external/cJSON.c
    model/api_response.c
    model/category.c
    model/order.c
    model/pet.c
    model/tag.c
    model/user.c
    api/PetAPI.c
    api/StoreAPI.c
    api/UserAPI.c

)

set(HDRS
    include/apiClient.h
    include/list.h
    include/keyValuePair.h
    external/cJSON.h
    model/api_response.h
    model/category.h
    model/order.h
    model/pet.h
    model/tag.h
    model/user.h
    api/PetAPI.h
    api/StoreAPI.h
    api/UserAPI.h

)

add_library(${pkgName} SHARED ${SRCS} ${HDRS})
target_link_libraries(${pkgName} ${CURL_LIBRARIES} )
install(TARGETS ${pkgName} DESTINATION ${CMAKE_INSTALL_PREFIX})
