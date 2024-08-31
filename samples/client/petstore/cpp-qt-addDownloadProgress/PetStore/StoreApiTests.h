#pragma once

#include "../client/PFXStoreApi.h"

using namespace test_namespace;

class StoreApiTests : public QObject {
    Q_OBJECT

private Q_SLOTS:
    void placeOrderTest();
    void getOrderByIdTest();
    void getInventoryTest();
};
