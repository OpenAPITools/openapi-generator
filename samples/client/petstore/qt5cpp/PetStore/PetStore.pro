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
    ../client/Category.cpp \
    ../client/SWGHelpers.cpp \
    ../client/SWGHttpRequest.cpp \
    ../client/Order.cpp \
    ../client/Pet.cpp \
    ../client/SWGPetApi.cpp \
    ../client/SWGStoreApi.cpp \
    ../client/Tag.cpp \
    ../client/User.cpp \
    ../client/SWGUserApi.cpp \
    ../client/ApiResponse.cpp \
    PetApiTests.cpp

HEADERS += \
    ../client/Category.h \
    ../client/SWGHelpers.h \
    ../client/SWGHttpRequest.h \
    ../client/SWGObject.h \
    ../client/Order.h \
    ../client/Pet.h \
    ../client/SWGPetApi.h \
    ../client/SWGStoreApi.h \
    ../client/Tag.h \
    ../client/User.h \
    ../client/SWGUserApi.h \
    PetApiTests.h \
    ../client/ApiResponse.h \
    ../client/SWGModelFactory.h
