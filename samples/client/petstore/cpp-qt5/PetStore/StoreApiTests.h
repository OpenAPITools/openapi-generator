#pragma once

#include "../client/OAIStoreApi.h"

using namespace OpenAPI;

class StoreApiTests: public QObject {
    Q_OBJECT

private slots:
    void placeOrderTest();
    void getOrderByIdTest();
    void getInventoryTest();
private:
    const QString PetStoreHost = QStringLiteral("http://petstore.swagger.io");
};
