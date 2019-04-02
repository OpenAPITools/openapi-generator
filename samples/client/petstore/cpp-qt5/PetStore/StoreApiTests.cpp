#include "StoreApiTests.h"

#include <QJsonDocument>
#include <QNetworkAccessManager>
#include <QNetworkReply>
#include <QDebug>

StoreApiTests::StoreApiTests () {}

StoreApiTests::~StoreApiTests () {

}

OAIStoreApi* StoreApiTests::getApi() {
    auto api = new OAIStoreApi();
    api->host = "http://petstore.swagger.io";
    api->basePath = "/v2";
    return api;
}

void StoreApiTests::runTests() {
    StoreApiTests* tests = new StoreApiTests();
    QTest::qExec(tests);
    delete tests;
}

void StoreApiTests::placeOrderTest() {
    auto api = getApi();

    QEventLoop loop;
    QTimer timer;
    timer.setInterval(14000);
    timer.setSingleShot(true);

    auto validator = [this](OAIOrder order) {
        QVERIFY(order.getPetId() == 10000);
        QVERIFY((order.getId() == 500));
        qDebug() << order.getShipDate();
        emit quit();
    };
    auto finalizer = [&]() {
        loop.quit();
    };
    connect(this, &StoreApiTests::quit, finalizer);
    connect(api, &OAIStoreApi::placeOrderSignal, this, validator);
    connect(&timer, &QTimer::timeout, &loop, finalizer);

    OAIOrder order;
    order.setId(500);
    order.setQuantity(10);
    order.setPetId(10000);
    order.setComplete(false);
    order.setStatus(QString("shipping"));
    order.setShipDate(QDateTime::currentDateTime());
    api->placeOrder(order);
    timer.start();
    loop.exec();
    QVERIFY2(timer.isActive(), "didn't finish within timeout");
    disconnect(this, nullptr, nullptr, nullptr);
    delete api;
}

void StoreApiTests::getOrderByIdTest() {
    auto api = getApi();

    QEventLoop loop;
    QTimer timer;
    timer.setInterval(14000);
    timer.setSingleShot(true);

    auto validator = [this](OAIOrder order) {
        QVERIFY(order.getPetId() == 10000);
        QVERIFY((order.getId() == 500));
        qDebug() << order.getShipDate();
        emit quit();
    };
    auto finalizer = [&]() {
        loop.quit();
    };
    connect(this, &StoreApiTests::quit, finalizer);
    connect(api, &OAIStoreApi::getOrderByIdSignal, this, validator);
    connect(&timer, &QTimer::timeout, &loop, finalizer);
    api->getOrderById(500);
    timer.start();
    loop.exec();
    QVERIFY2(timer.isActive(), "didn't finish within timeout");
    disconnect(this, nullptr, nullptr, nullptr);
    delete api;
}

void StoreApiTests::getInventoryTest() {
    auto api = getApi();

    QEventLoop loop;
    QTimer timer;
    timer.setInterval(14000);
    timer.setSingleShot(true);

    auto validator = [this](QMap<QString, qint32> status) {
        for(const auto& key : status.keys()) {
            qDebug() << (key) << " Quantities " << status.value(key);
        }
        emit quit();
    };
    auto finalizer = [&]() {
        loop.quit();
    };
    connect(this, &StoreApiTests::quit, finalizer);
    connect(api, &OAIStoreApi::getInventorySignal, this, validator);
    connect(&timer, &QTimer::timeout, &loop, finalizer);
    api->getInventory();
    timer.start();
    loop.exec();
    QVERIFY2(timer.isActive(), "didn't finish within timeout");
    disconnect(this, nullptr, nullptr, nullptr);
    delete api;
}
