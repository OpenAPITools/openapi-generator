#include "UserApiTests.h"

#include <QJsonDocument>
#include <QNetworkAccessManager>
#include <QNetworkReply>
#include <QDebug>
#include <QRandomGenerator>
UserApiTests::UserApiTests () {}

UserApiTests::~UserApiTests () {
    exit(0);
}

OAIUserApi* UserApiTests::getApi() {
    auto api = new OAIUserApi();
    api->host = "http://petstore.swagger.io";
    api->basePath = "/v2";
    return api;
}

void UserApiTests::runTests() {
    UserApiTests* tests = new UserApiTests();
    QTest::qExec(tests);
    delete tests;
}

OAIUser UserApiTests::createRandomUser() {
    OAIUser user;
    user.setId(QDateTime::currentMSecsSinceEpoch());
    user.setEmail(QString("Jane.Doe@openapitools.io"));
    user.setFirstName(QString("Jane"));
    user.setLastName(QString("Doe"));
    user.setPhone(QString("123456789"));
    user.setUsername(QString("janedoe"));
    user.setPassword(QString("secretPassword"));
    user.setUserStatus(static_cast<int>(QRandomGenerator::system()->generate()));
    return user;
}

void UserApiTests::createUserTest(){
    auto api = getApi();
    QEventLoop loop;
    QTimer timer;
    timer.setInterval(14000);
    timer.setSingleShot(true);

    auto validator = [this]() {
        emit quit();
    };
    auto finalizer = [&]() {
        loop.quit();
    };
    connect(this, &UserApiTests::quit, finalizer);
    connect(api, &OAIUserApi::createUserSignal, this, validator);
    connect(&timer, &QTimer::timeout, &loop, finalizer);

    api->createUser(createRandomUser());
    timer.start();
    loop.exec();
    QVERIFY2(timer.isActive(), "didn't finish within timeout");
    disconnect(this, nullptr, nullptr, nullptr);
    delete api;
}

void UserApiTests::createUsersWithArrayInputTest(){
    auto api = getApi();
    QEventLoop loop;
    QTimer timer;
    timer.setInterval(14000);
    timer.setSingleShot(true);

    auto validator = [this]() {
        emit quit();
    };
    auto finalizer = [&]() {
        loop.quit();
    };
    connect(this, &UserApiTests::quit, finalizer);
    connect(api, &OAIUserApi::createUsersWithArrayInputSignal, this, validator);
    connect(&timer, &QTimer::timeout, &loop, finalizer);
    QList<OAIUser> users;
    users.append(createRandomUser());
    users.append(createRandomUser());
    users.append(createRandomUser());
    api->createUsersWithArrayInput(users);
    timer.start();
    loop.exec();
    QVERIFY2(timer.isActive(), "didn't finish within timeout");
    disconnect(this, nullptr, nullptr, nullptr);
    delete api;
}

void UserApiTests::createUsersWithListInputTest(){
    auto api = getApi();
    QEventLoop loop;
    QTimer timer;
    timer.setInterval(14000);
    timer.setSingleShot(true);

    auto validator = [this]() {
        emit quit();
    };
    auto finalizer = [&]() {
        loop.quit();
    };
    connect(this, &UserApiTests::quit, finalizer);
    connect(api, &OAIUserApi::createUsersWithListInputSignal, this, validator);
    connect(&timer, &QTimer::timeout, &loop, finalizer);
    QList<OAIUser> users;
    auto johndoe = createRandomUser();
    johndoe.setUsername(QString("johndoe"));
    auto rambo = createRandomUser();
    rambo.setUsername(QString("rambo"));
    users.append(johndoe);
    users.append(rambo);
    users.append(createRandomUser());
    api->createUsersWithListInput(users);
    timer.start();
    loop.exec();
    QVERIFY2(timer.isActive(), "didn't finish within timeout");
    disconnect(this, nullptr, nullptr, nullptr);
    delete api;
}

void UserApiTests::deleteUserTest(){
    auto api = getApi();
    QEventLoop loop;
    QTimer timer;
    timer.setInterval(14000);
    timer.setSingleShot(true);

    auto validator = [this]() {
        emit quit();
    };
    auto finalizer = [&]() {
        loop.quit();
    };
    connect(this, &UserApiTests::quit, finalizer);
    connect(api, &OAIUserApi::deleteUserSignal, this, validator);
    connect(&timer, &QTimer::timeout, &loop, finalizer);

    api->deleteUser(QString("rambo"));
    timer.start();
    loop.exec();
    QVERIFY2(timer.isActive(), "didn't finish within timeout");
    disconnect(this, nullptr, nullptr, nullptr);
    delete api;
}

void UserApiTests::getUserByNameTest(){
    auto api = getApi();
    QEventLoop loop;
    QTimer timer;
    timer.setInterval(30000);
    timer.setSingleShot(true);

    auto validator = [this](OAIUser summary) {
        qDebug() << summary.getUsername();
        emit quit();
    };
    auto finalizer = [&]() {
        loop.quit();
    };
    connect(this, &UserApiTests::quit, finalizer);
    connect(api, &OAIUserApi::getUserByNameSignal, this, validator);
    connect(&timer, &QTimer::timeout, &loop, finalizer);

    api->getUserByName(QString("johndoe"));
    timer.start();
    loop.exec();
    QVERIFY2(timer.isActive(), "didn't finish within timeout");
    disconnect(this, nullptr, nullptr, nullptr);
    delete api;
}

void UserApiTests::loginUserTest(){
    auto api = getApi();
    QEventLoop loop;
    QTimer timer;
    timer.setInterval(30000);
    timer.setSingleShot(true);

    auto validator = [this](QString summary) {
        qDebug() << summary;
        emit quit();
    };
    auto finalizer = [&]() {
        loop.quit();
    };
    connect(this, &UserApiTests::quit, finalizer);
    connect(api, &OAIUserApi::loginUserSignal, this, validator);
    connect(&timer, &QTimer::timeout, &loop, finalizer);

    api->loginUser(QString("johndoe"), QString("123456789"));
    timer.start();
    loop.exec();
    QVERIFY2(timer.isActive(), "didn't finish within timeout");
    disconnect(this, nullptr, nullptr, nullptr);
    delete api;
}

void UserApiTests::logoutUserTest(){
    auto api = getApi();
    QEventLoop loop;
    QTimer timer;
    timer.setInterval(30000);
    timer.setSingleShot(true);

    auto validator = [this]() {
        emit quit();
    };
    auto finalizer = [&]() {
        loop.quit();
    };
    connect(this, &UserApiTests::quit, finalizer);
    connect(api, &OAIUserApi::logoutUserSignal, this, validator);
    connect(&timer, &QTimer::timeout, &loop, finalizer);

    api->logoutUser();
    timer.start();
    loop.exec();
    QVERIFY2(timer.isActive(), "didn't finish within timeout");
    disconnect(this, nullptr, nullptr, nullptr);
    delete api;
}

void UserApiTests::updateUserTest(){
    auto api = getApi();
    QEventLoop loop;
    QTimer timer;
    timer.setInterval(30000);
    timer.setSingleShot(true);

    auto validator = [this]() {
        emit quit();
    };
    auto finalizer = [&]() {
        loop.quit();
    };
    connect(this, &UserApiTests::quit, finalizer);
    connect(api, &OAIUserApi::updateUserSignal, this, validator);
    connect(&timer, &QTimer::timeout, &loop, finalizer);

    auto johndoe = createRandomUser();
    johndoe.setUsername(QString("johndoe"));
    api->updateUser(QString("johndoe"), johndoe);
    timer.start();
    loop.exec();
    QVERIFY2(timer.isActive(), "didn't finish within timeout");
    disconnect(this, nullptr, nullptr, nullptr);
    delete api;
}
