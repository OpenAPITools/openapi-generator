QT += network

HEADERS += \
# Models
    $${PWD}/PFXCategory.h \
    $${PWD}/PFXPet.h \
    $${PWD}/PFXTag.h \
# APIs
    $${PWD}/PFXPetApi.h \
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
    $${PWD}/PFXCategory.cpp \
    $${PWD}/PFXPet.cpp \
    $${PWD}/PFXTag.cpp \
# APIs
    $${PWD}/PFXPetApi.cpp \
# Others
    $${PWD}/PFXHelpers.cpp \
    $${PWD}/PFXHttpRequest.cpp \
    $${PWD}/PFXHttpFileElement.cpp \
    $${PWD}/PFXOauth.cpp
