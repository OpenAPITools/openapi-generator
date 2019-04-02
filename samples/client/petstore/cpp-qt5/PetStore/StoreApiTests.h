#ifndef STOREAPITESTS_H
#define STOREAPITESTS_H
#include <QtTest/QtTest>
#include <QTimer>

#include "../client/OAIStoreApi.h"

using namespace OpenAPI;

class StoreApiTests: public QObject {
Q_OBJECT
public:
    StoreApiTests();
    virtual ~StoreApiTests();

    static void runTests();

private:
    OAIStoreApi* getApi();
signals:
    void quit();
    bool success();

private slots:
    void placeOrderTest();
    void getOrderByIdTest();
    void getInventoryTest();
};

#endif // STOREAPITESTS_H
