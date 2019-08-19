#include "StoreApiTests.h"

#include <QTest>
#include <QTimer>
#include <QDebug>

void StoreApiTests::placeOrderTest() {
    OAIStoreApi api;
    QEventLoop loop;
    bool orderPlaced = false;

    connect(&api, &OAIStoreApi::placeOrderSignal, [&](OAIOrder order) {
        orderPlaced = true;
        QVERIFY(order.getPetId() == 10000);
        QVERIFY((order.getId() == 500));
        qDebug() << order.getShipDate();
        loop.quit();
    });
    connect(&api, &OAIStoreApi::placeOrderSignalE, [&](){
        QFAIL("shouldn't trigger error");
        loop.quit();
    });

    OAIOrder order;
    order.setId(500);
    order.setQuantity(10);
    order.setPetId(10000);
    order.setComplete(false);
    order.setStatus("shipping");
    order.setShipDate(QDateTime::currentDateTime());
    api.placeOrder(order);
    QTimer::singleShot(14000, &loop, &QEventLoop::quit);
    loop.exec();
    QVERIFY2(orderPlaced, "didn't finish within timeout");
}

void StoreApiTests::getOrderByIdTest() {
    OAIStoreApi api;
    QEventLoop loop;
    bool orderFetched = false;

    connect(&api, &OAIStoreApi::getOrderByIdSignal, [&](OAIOrder order) {
        orderFetched = true;
        QVERIFY(order.getPetId() == 10000);
        QVERIFY((order.getId() == 500));
        qDebug() << order.getShipDate();
        loop.quit();
    });

    api.getOrderById(500);
    QTimer::singleShot(14000, &loop, &QEventLoop::quit);
    loop.exec();
    QVERIFY2(orderFetched, "didn't finish within timeout");
}

void StoreApiTests::getInventoryTest() {
    OAIStoreApi api;
    QEventLoop loop;
    bool inventoryFetched = false;

    connect(&api, &OAIStoreApi::getInventorySignal, [&](QMap<QString, qint32> status) {
        inventoryFetched = true;
        for(const auto& key : status.keys()) {
            qDebug() << (key) << " Quantities " << status.value(key);
        }
        loop.quit();
    });

    api.getInventory();
    QTimer::singleShot(14000, &loop, &QEventLoop::quit);
    loop.exec();
    QVERIFY2(inventoryFetched, "didn't finish within timeout");
}
