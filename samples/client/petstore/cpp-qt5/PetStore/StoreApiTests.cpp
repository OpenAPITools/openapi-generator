#include "StoreApiTests.h"

#include <QDebug>
#include <QTest>
#include <QTimer>

void StoreApiTests::placeOrderTest() {
    PFXStoreApi api;
    QEventLoop loop;
    bool orderPlaced = false;

    connect(&api, &PFXStoreApi::placeOrderSignal, [&](PFXOrder order) {
        orderPlaced = true;
        QVERIFY(order.getPetId() == 10000);
        QVERIFY((order.getId() == 500));
        qDebug() << order.getShipDate();
        loop.quit();
    });
    connect(&api, &PFXStoreApi::placeOrderSignalE, [&]() {
        QFAIL("shouldn't trigger error");
        loop.quit();
    });

    PFXOrder order;
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
    PFXStoreApi api;
    QEventLoop loop;
    bool orderFetched = false;

    connect(&api, &PFXStoreApi::getOrderByIdSignal, [&](PFXOrder order) {
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
    PFXStoreApi api;
    QEventLoop loop;
    bool inventoryFetched = false;

    connect(&api, &PFXStoreApi::getInventorySignal, [&](QMap<QString, qint32> status) {
        inventoryFetched = true;
        for (const auto &key : status.keys()) {
            qDebug() << (key) << " Quantities " << status.value(key);
        }
        loop.quit();
    });

    api.getInventory();
    QTimer::singleShot(14000, &loop, &QEventLoop::quit);
    loop.exec();
    QVERIFY2(inventoryFetched, "didn't finish within timeout");
}
