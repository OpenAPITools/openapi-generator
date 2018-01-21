#ifndef PETAPITESTS_H
#define PETAPITESTS_H

#include <QtTest/QtTest>
#include <QTimer>

#include "../client/SWGPetApi.h"

using namespace Swagger;

class PetApiTests: public QObject {
Q_OBJECT
public:
    PetApiTests();
    virtual ~PetApiTests();

    static void runTests();

private:
    SWGPetApi* getApi();
    SWGPet* createRandomPet();

signals:
    void quit();
    bool success();

private slots:
    void findPetsByStatusTest();
    void createAndGetPetTest();
    void updatePetTest();
    void updatePetWithFormTest();
};

#endif // PETAPITESTS_H
