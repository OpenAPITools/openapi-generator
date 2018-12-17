#ifndef PETAPITESTS_H
#define PETAPITESTS_H

#include <QtTest/QtTest>
#include <QTimer>

#include "../client/OAIPetApi.h"

using namespace OpenAPI;

class PetApiTests: public QObject {
Q_OBJECT
public:
    PetApiTests();
    virtual ~PetApiTests();

    static void runTests();

private:
    OAIPetApi* getApi();
    OAIPet createRandomPet();

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
