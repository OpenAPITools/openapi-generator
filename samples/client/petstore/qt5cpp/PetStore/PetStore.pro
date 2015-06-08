#-------------------------------------------------
#
# Project created by QtCreator 2015-05-14T20:56:31
#
#-------------------------------------------------

QT       += core gui testlib network

QT       -= gui

TARGET = PetStore
CONFIG   += console
CONFIG   -= app_bundle

CONFIG   += c++11

TEMPLATE = app


SOURCES += main.cpp \
    ../client/SWGCategory.cpp \
    ../client/SWGHelpers.cpp \
    ../client/SWGHttpRequest.cpp \
    ../client/SWGOrder.cpp \
    ../client/SWGPet.cpp \
    ../client/SWGPetApi.cpp \
    ../client/SWGStoreApi.cpp \
    ../client/SWGTag.cpp \
    ../client/SWGUser.cpp \
    ../client/SWGUserApi.cpp \
    PetApiTests.cpp

HEADERS += \
    ../client/SWGCategory.h \
    ../client/SWGHelpers.h \
    ../client/SWGHttpRequest.h \
    ../client/SWGObject.h \
    ../client/SWGOrder.h \
    ../client/SWGPet.h \
    ../client/SWGPetApi.h \
    ../client/SWGStoreApi.h \
    ../client/SWGTag.h \
    ../client/SWGUser.h \
    ../client/SWGUserApi.h \
    PetApiTests.h \
    ../client/SWGModelFactory.h
