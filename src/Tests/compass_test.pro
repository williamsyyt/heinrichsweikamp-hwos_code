#-----------------------------------------------------------------------------
#
# Project created by QtCreator 2013-03-29T10:58:23
#
#-----------------------------------------------------------------------------

TEMPLATE = app
TARGET = compass_test

CONFIG   *= qt
CONFIG   -= app_bundle

QT       *= core
QT       -= gui

DEFINES *= UNIX

#-----------------------------------------------------------------------------
# Need the GoogleTest 1.6.0 library here:
GTEST=$$PWD/../../../../Dependencies/gtest-1.6.0
!exists($$GTEST/include): error(Requires GoogleTest 1.6.0)
INCLUDEPATH *= $$GTEST/include $$GTEST/gtest-1.6.0
SOURCES *= $$GTEST/gtest-1.6.0/src/gtest-all.cc
win32: DEFINES *= _VARIADIC_MAX=10

#-----------------------------------------------------------------------------
SOURCES += \
    $$PWD/../compass.c \
    $$PWD/../compass_calib.c \
    compass_trigo_test.cpp \
    compass_test.cpp \
    main_test.cpp

INCLUDEPATH *= $$PWD/..
HEADERS += \
    $$PWD/../compass.h
