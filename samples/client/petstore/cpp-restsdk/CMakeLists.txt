cmake_minimum_required (VERSION 3.1)

add_subdirectory(client)

project(cpprest-petstore)

find_package(Threads REQUIRED)

file(GLOB SOURCE_FILES "*.cpp")

add_executable(${PROJECT_NAME} ${SOURCE_FILES})

target_link_libraries(${PROJECT_NAME}
    PRIVATE
        CppRestPetstoreClient
        Threads::Threads
)

if (UNIX)
    install(TARGETS ${PROJECT_NAME} RUNTIME DESTINATION bin LIBRARY DESTINATION lib ARCHIVE DESTINATION lib)
endif()
