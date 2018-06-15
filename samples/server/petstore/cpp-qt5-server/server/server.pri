QT += network

HEADERS += \
# Models
    $${PWD}/OAIApiResponse.h \
    $${PWD}/OAICategory.h \
    $${PWD}/OAIOrder.h \
    $${PWD}/OAIPet.h \
    $${PWD}/OAITag.h \
    $${PWD}/OAIUser.h \
# APIs
    $${PWD}/OAIPetApiHandler.h \
    $${PWD}/OAIStoreApiHandler.h \
    $${PWD}/OAIUserApiHandler.h \
# Others
    $${PWD}/OAIHelpers.h \
    $${PWD}/OAIHttpRequest.h \
    $${PWD}/OAIModelFactory.h \
    $${PWD}/OAIObject.h \
    $${PWD}/OAIQObjectWrapper.h

SOURCES += \
# Models
    $${PWD}/OAIApiResponse.cpp \
    $${PWD}/OAICategory.cpp \
    $${PWD}/OAIOrder.cpp \
    $${PWD}/OAIPet.cpp \
    $${PWD}/OAITag.cpp \
    $${PWD}/OAIUser.cpp \
# APIs
    $${PWD}/OAIPetApiHandler.cpp \
    $${PWD}/OAIStoreApiHandler.cpp \
    $${PWD}/OAIUserApiHandler.cpp \
# Others
    $${PWD}/OAIHelpers.cpp \
    $${PWD}/OAIHttpRequest.cpp

