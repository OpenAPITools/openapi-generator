
find_package(Boost 1.66 REQUIRED
             COMPONENTS system)


if (MSVC)
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /W4")
else ()
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fPIC -Wall")
endif ()


set(LIBRARY_NAME client)

add_library(${LIBRARY_NAME} SHARED)

set_property(TARGET ${LIBRARY_NAME} PROPERTY CXX_STANDARD 11)
set_property(TARGET ${LIBRARY_NAME} PROPERTY CXX_STANDARD_REQUIRED ON)
set_property(TARGET ${LIBRARY_NAME} PROPERTY CXX_EXTENSIONS OFF)

target_sources(${LIBRARY_NAME} PRIVATE
# models
    model/ApiResponse.cpp
    model/Category.cpp
    model/Order.cpp
    model/Pet.cpp
    model/Tag.cpp
    model/User.cpp
# apis
    api/PetApi.cpp
    api/StoreApi.cpp
    api/UserApi.cpp
# other
    api/HttpClientImpl.cpp
)


target_link_libraries(${LIBRARY_NAME} PUBLIC boost_system ssl crypto)

target_include_directories(${LIBRARY_NAME} PUBLIC model api ${CMAKE_CURRENT_LIST_DIR})


install(TARGETS ${LIBRARY_NAME} RUNTIME DESTINATION bin LIBRARY DESTINATION lib ARCHIVE DESTINATION lib)
