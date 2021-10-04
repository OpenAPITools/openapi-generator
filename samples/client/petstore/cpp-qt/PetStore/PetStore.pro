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

include(../client/PFXclient.pri)

INCLUDEPATH += ../client

SOURCES += main.cpp \
           PetApiTests.cpp \
           StoreApiTests.cpp \
           UserApiTests.cpp

HEADERS += PetApiTests.h \
           StoreApiTests.h \
           UserApiTests.h

# Disable optimisation for better valgrind report
QMAKE_CXXFLAGS_DEBUG += -O0
