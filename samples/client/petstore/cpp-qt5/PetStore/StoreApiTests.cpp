#include "StoreApiTests.h"

#include <QDebug>
#include <QTest>
#include <QTimer>

void StoreApiTests::placeOrderTest() {
    PFXStoreApi api;
//  api.setUsername("TestName");
//  api.setPassword("TestPassword");
    QEventLoop loop;
    bool orderPlaced = false;

    connect(&api, &PFXStoreApi::placeOrderSignal, [&](PFXOrder order) {
        orderPlaced = true;
//      QVERIFY(order.getPetId() == 10000);
//      QVERIFY((order.getId() == 500));
        qDebug() << order.getShipDate();
        QTimer::singleShot(0, &loop, &QEventLoop::quit);
    });
    connect(&api, &PFXStoreApi::placeOrderSignalE, [&](PFXOrder, QNetworkReply::NetworkError, QString error_str) {
        qDebug() << "Error happened while issuing request : " << error_str;
        QTimer::singleShot(0, &loop, &QEventLoop::quit);
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
    api.setApiKey("api_key_2","testKey");
    QEventLoop loop;
    bool orderFetched = false;

    connect(&api, &PFXStoreApi::getOrderByIdSignal, [&](PFXOrder order) {
        orderFetched = true;
//      QVERIFY(order.getPetId() == 10000);
//      QVERIFY((order.getId() == 500));
        qDebug() << order.getShipDate();
        QTimer::singleShot(0, &loop, &QEventLoop::quit);
    });
    connect(&api, &PFXStoreApi::getOrderByIdSignalE, [&](PFXOrder, QNetworkReply::NetworkError, QString error_str) {
        qDebug() << "Error happened while issuing request : " << error_str;
        QTimer::singleShot(0, &loop, &QEventLoop::quit);
    });

    api.getOrderById(500);
    QTimer::singleShot(14000, &loop, &QEventLoop::quit);
    loop.exec();
    QVERIFY2(orderFetched, "didn't finish within timeout");
}

void StoreApiTests::getInventoryTest() {
    PFXStoreApi api;
    api.setApiKey("api_key","special-key");
    QEventLoop loop;
    bool inventoryFetched = false;

    connect(&api, &PFXStoreApi::getInventorySignal, [&](QMap<QString, qint32> status) {
        inventoryFetched = true;
        for (const auto &key : status.keys()) {
            qDebug() << (key) << " Quantities " << status.value(key);
        }
        QTimer::singleShot(0, &loop, &QEventLoop::quit);
    });
    connect(&api, &PFXStoreApi::getInventorySignalE, [&](QMap<QString, qint32>, QNetworkReply::NetworkError, QString error_str) {
        qDebug() << "Error happened while issuing request : " << error_str;
        QTimer::singleShot(0, &loop, &QEventLoop::quit);
    });

    api.getInventory();
    QTimer::singleShot(14000, &loop, &QEventLoop::quit);
    loop.exec();
    QVERIFY2(inventoryFetched, "didn't finish within timeout");
}
