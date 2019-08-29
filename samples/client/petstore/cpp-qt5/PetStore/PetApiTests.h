#pragma once

#include "../client/OAIPetApi.h"

using namespace OpenAPI;

class PetApiTests: public QObject {
    Q_OBJECT

    OAIPetApi* getApi();
    OAIPet createRandomPet();

private slots:
    void findPetsByStatusTest();
    void createAndGetPetTest();
    void updatePetTest();
    void updatePetWithFormTest();
};
