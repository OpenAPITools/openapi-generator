QT += network

HEADERS += \
# Models
    $${PWD}/OAIApiResponse.h \
    $${PWD}/OAICategory.h \
    $${PWD}/OAIInline_object.h \
    $${PWD}/OAIInline_object_1.h \
    $${PWD}/OAIOrder.h \
    $${PWD}/OAIPet.h \
    $${PWD}/OAITag.h \
    $${PWD}/OAIUser.h \
# APIs
    $${PWD}/OAIPetApi.h \
    $${PWD}/OAIStoreApi.h \
    $${PWD}/OAIUserApi.h \
# Others
    $${PWD}/OAIHelpers.h \
    $${PWD}/OAIHttpRequest.h \
    $${PWD}/OAIObject.h
    $${PWD}/OAIEnum.h

SOURCES += \
# Models
    $${PWD}/OAIApiResponse.cpp \
    $${PWD}/OAICategory.cpp \
    $${PWD}/OAIInline_object.cpp \
    $${PWD}/OAIInline_object_1.cpp \
    $${PWD}/OAIOrder.cpp \
    $${PWD}/OAIPet.cpp \
    $${PWD}/OAITag.cpp \
    $${PWD}/OAIUser.cpp \
# APIs
    $${PWD}/OAIPetApi.cpp \
    $${PWD}/OAIStoreApi.cpp \
    $${PWD}/OAIUserApi.cpp \
# Others
    $${PWD}/OAIHelpers.cpp \
    $${PWD}/OAIHttpRequest.cpp

