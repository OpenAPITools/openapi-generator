#pragma once

#include "../client/OAIStoreApi.h"

using namespace OpenAPI;

class StoreApiTests: public QObject {
    Q_OBJECT

    OAIStoreApi* getApi();

private slots:
    void placeOrderTest();
    void getOrderByIdTest();
    void getInventoryTest();
};
