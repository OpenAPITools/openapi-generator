QT += network

HEADERS += \
# Models
    $${PWD}/SWGApiResponse.h \
    $${PWD}/SWGCategory.h \
    $${PWD}/SWGOrder.h \
    $${PWD}/SWGPet.h \
    $${PWD}/SWGTag.h \
    $${PWD}/SWGUser.h \
# APIs
    $${PWD}/SWGPetApi.h \
    $${PWD}/SWGStoreApi.h \
    $${PWD}/SWGUserApi.h \
# Others
    $${PWD}/SWGHelpers.h \
    $${PWD}/SWGHttpRequest.h \
    $${PWD}/SWGModelFactory.h \
    $${PWD}/SWGObject.h \
    $${PWD}/SWGQObjectWrapper.h

SOURCES += \
# Models
    $${PWD}/SWGApiResponse.cpp \
    $${PWD}/SWGCategory.cpp \
    $${PWD}/SWGOrder.cpp \
    $${PWD}/SWGPet.cpp \
    $${PWD}/SWGTag.cpp \
    $${PWD}/SWGUser.cpp \
# APIs
    $${PWD}/SWGPetApi.cpp \
    $${PWD}/SWGStoreApi.cpp \
    $${PWD}/SWGUserApi.cpp \
# Others
    $${PWD}/SWGHelpers.cpp \
    $${PWD}/SWGHttpRequest.cpp

