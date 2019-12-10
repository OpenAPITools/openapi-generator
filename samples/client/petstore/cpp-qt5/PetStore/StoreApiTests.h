#pragma once

#include "../client/PFXStoreApi.h"

using namespace test_namespace;

class StoreApiTests : public QObject {
    Q_OBJECT

private slots:
    void placeOrderTest();
    void getOrderByIdTest();
    void getInventoryTest();
};
