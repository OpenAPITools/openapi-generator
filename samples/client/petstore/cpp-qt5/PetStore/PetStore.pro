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

include(../client/client.pri)

SOURCES += main.cpp \
	PetApiTests.cpp

HEADERS += PetApiTests.h
