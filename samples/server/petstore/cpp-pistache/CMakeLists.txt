cmake_minimum_required (VERSION 3.2)

project(server)

set(CMAKE_CXX_FLAGS ${CMAKE_CXX_FLAGS} -std=c++11)

link_directories(/usr/local/lib/)

aux_source_directory(model MODEL_SOURCES)
file(GLOB PET_API_SOURCES
"api/PetApi.h"
"api/PetApi.cpp"
"impl/PetApiImpl.h"
"impl/PetApiImpl.cpp"
)
file(GLOB STORE_API_SOURCES
"api/StoreApi.h"
"api/StoreApi.cpp"
"impl/StoreApiImpl.h"
"impl/StoreApiImpl.cpp"
)
file(GLOB USER_API_SOURCES
"api/UserApi.h"
"api/UserApi.cpp"
"impl/UserApiImpl.h"
"impl/UserApiImpl.cpp"
)

include_directories(model)
include_directories(api)
include_directories(impl)

set(PET_API_SERVER_SOURCES
PetApiMainServer.cpp
    ${MODEL_SOURCES}
    ${PET_API_SOURCES})
set(STORE_API_SERVER_SOURCES
StoreApiMainServer.cpp
    ${MODEL_SOURCES}
    ${STORE_API_SOURCES})
set(USER_API_SERVER_SOURCES
UserApiMainServer.cpp
    ${MODEL_SOURCES}
    ${USER_API_SOURCES})

add_executable(pet_api_server
${PET_API_SERVER_SOURCES})
add_executable(store_api_server
${STORE_API_SERVER_SOURCES})
add_executable(user_api_server
${USER_API_SERVER_SOURCES})

target_link_libraries(pet_api_server pistache pthread)
target_link_libraries(store_api_server pistache pthread)
target_link_libraries(user_api_server pistache pthread)

