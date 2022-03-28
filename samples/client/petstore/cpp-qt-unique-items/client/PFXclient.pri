QT += network

HEADERS += \
# Models
    $${PWD}/PFXError.h \
    $${PWD}/PFXPet.h \
# APIs
    $${PWD}/PFXPetsApi.h \
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
    $${PWD}/PFXError.cpp \
    $${PWD}/PFXPet.cpp \
# APIs
    $${PWD}/PFXPetsApi.cpp \
# Others
    $${PWD}/PFXHelpers.cpp \
    $${PWD}/PFXHttpRequest.cpp \
    $${PWD}/PFXHttpFileElement.cpp \
    $${PWD}/PFXOauth.cpp
