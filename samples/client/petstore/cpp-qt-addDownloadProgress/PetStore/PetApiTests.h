#pragma once

#include "../client/PFXPetApi.h"

using namespace test_namespace;

class PetApiTests : public QObject {
    Q_OBJECT

    PFXPet createRandomPet();

private Q_SLOTS:
    void findPetsByStatusTest();
    void createAndGetPetTest();
    void updatePetTest();
    void updatePetWithFormTest();
};
