#pragma once

#include "../client/OAIPetApi.h"

using namespace OpenAPI;

class PetApiTests: public QObject {
    Q_OBJECT

    OAIPet createRandomPet();

private slots:
    void findPetsByStatusTest();
    void createAndGetPetTest();
    void updatePetTest();
    void updatePetWithFormTest();
private:
    const QString PetStoreHost = QStringLiteral("http://petstore.swagger.io");
};
