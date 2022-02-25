QT += network

HEADERS += \
# Models
    $${PWD}/PFXApiResponse.h \
    $${PWD}/PFXCategory.h \
    $${PWD}/PFXOrder.h \
    $${PWD}/PFXPet.h \
    $${PWD}/PFXTag.h \
    $${PWD}/PFXUser.h \
# APIs
    $${PWD}/PFXPetApi.h \
    $${PWD}/PFXStoreApi.h \
    $${PWD}/PFXUserApi.h \
# Others
    $${PWD}/PFXHelpers.h \
    $${PWD}/PFXHttpRequest.h \
    $${PWD}/PFXObject.h \
    $${PWD}/PFXEnum.h \
    $${PWD}/PFXHttpFileElement.h \
    $${PWD}/PFXServerConfiguration.h \
    $${PWD}/PFXServerVariable.h \
    $${PWD}/PFXOauth.h

SOURCES += \
# Models
    $${PWD}/PFXApiResponse.cpp \
    $${PWD}/PFXCategory.cpp \
    $${PWD}/PFXOrder.cpp \
    $${PWD}/PFXPet.cpp \
    $${PWD}/PFXTag.cpp \
    $${PWD}/PFXUser.cpp \
# APIs
    $${PWD}/PFXPetApi.cpp \
    $${PWD}/PFXStoreApi.cpp \
    $${PWD}/PFXUserApi.cpp \
# Others
    $${PWD}/PFXHelpers.cpp \
    $${PWD}/PFXHttpRequest.cpp \
    $${PWD}/PFXHttpFileElement.cpp \
    $${PWD}/PFXOauth.cpp
