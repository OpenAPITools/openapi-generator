#pragma once

#include "../client/PFXPetApi.h"

using namespace test_namespace;

class PetApiTests : public QObject {
    Q_OBJECT

    PFXPet createRandomPet();

private slots:
    void findPetsByStatusTest();
    void createAndGetPetTest();
    void updatePetTest();
    void updatePetWithFormTest();
};
