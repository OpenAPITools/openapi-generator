#include "UserApiTests.h"

#include <QDebug>
#include <QTest>
#include <QTimer>

PFXUser UserApiTests::createRandomUser() {
    PFXUser user;
    user.setId(QDateTime::currentMSecsSinceEpoch());
    user.setEmail("Jane.Doe@openapitools.io");
    user.setFirstName("Jane");
    user.setLastName("Doe");
    user.setPhone("123456789");
    user.setUsername("janedoe");
    user.setPassword("secretPassword");
    user.setUserStatus(static_cast<int>(rand()));
    return user;
}

void UserApiTests::createUserTest() {
    PFXUserApi api;
    QEventLoop loop;
    bool userCreated = false;

    connect(&api, &PFXUserApi::createUserSignal, [&]() {
        userCreated = true;
        QTimer::singleShot(0, &loop, &QEventLoop::quit);
    });
    connect(&api, &PFXUserApi::createUserSignalE, [&](QNetworkReply::NetworkError, QString error_str) {
        qDebug() << "Error happened while issuing request : " << error_str;
        QTimer::singleShot(0, &loop, &QEventLoop::quit);
    });

    api.createUser(createRandomUser());
    QTimer::singleShot(14000, &loop, &QEventLoop::quit);
    loop.exec();
    QVERIFY2(userCreated, "didn't finish within timeout");
}

void UserApiTests::createUsersWithArrayInputTest() {
    PFXUserApi api;
    QEventLoop loop;
    bool usersCreated = false;

    connect(&api, &PFXUserApi::createUsersWithArrayInputSignal, [&]() {
        usersCreated = true;
        QTimer::singleShot(0, &loop, &QEventLoop::quit);
    });
    connect(&api, &PFXUserApi::createUsersWithArrayInputSignalE, [&](QNetworkReply::NetworkError, QString error_str) {
        qDebug() << "Error happened while issuing request : " << error_str;
        QTimer::singleShot(0, &loop, &QEventLoop::quit);
    });

    QList<PFXUser> users;
    users.append(createRandomUser());
    users.append(createRandomUser());
    users.append(createRandomUser());
    api.createUsersWithArrayInput(users);
    QTimer::singleShot(14000, &loop, &QEventLoop::quit);
    loop.exec();
    QVERIFY2(usersCreated, "didn't finish within timeout");
}

void UserApiTests::createUsersWithListInputTest() {
    PFXUserApi api;
    QEventLoop loop;
    bool usersCreated = false;

    connect(&api, &PFXUserApi::createUsersWithListInputSignal, [&]() {
        usersCreated = true;
        QTimer::singleShot(0, &loop, &QEventLoop::quit);
    });
    connect(&api, &PFXUserApi::createUsersWithListInputSignalE, [&](QNetworkReply::NetworkError, QString error_str) {
        qDebug() << "Error happened while issuing request : " << error_str;
        QTimer::singleShot(0, &loop, &QEventLoop::quit);
    });

    QList<PFXUser> users;
    auto johndoe = createRandomUser();
    johndoe.setUsername("johndoe");
    auto rambo = createRandomUser();
    rambo.setUsername("rambo");
    users.append(johndoe);
    users.append(rambo);
    users.append(createRandomUser());
    api.createUsersWithListInput(users);
    QTimer::singleShot(14000, &loop, &QEventLoop::quit);
    loop.exec();
    QVERIFY2(usersCreated, "didn't finish within timeout");
}

void UserApiTests::deleteUserTest() {
    PFXUserApi api;
    QEventLoop loop;
    bool userDeleted = false;

    connect(&api, &PFXUserApi::deleteUserSignal, [&]() {
        userDeleted = true;
        QTimer::singleShot(0, &loop, &QEventLoop::quit);
    });
    connect(&api, &PFXUserApi::deleteUserSignalE, [&](QNetworkReply::NetworkError, QString error_str) {
        userDeleted = true;
        qDebug() << "Error happened while issuing request : " << error_str;
        QTimer::singleShot(0, &loop, &QEventLoop::quit);
    });

    api.deleteUser("rambo");
    QTimer::singleShot(14000, &loop, &QEventLoop::quit);
    loop.exec();
    QVERIFY2(userDeleted, "didn't finish within timeout");
}

void UserApiTests::getUserByNameTest() {
    PFXUserApi api;
    QEventLoop loop;
    bool userFetched = false;

    connect(&api, &PFXUserApi::getUserByNameSignal, [&](PFXUser summary) {
        userFetched = true;
        qDebug() << summary.getUsername();
//      QVERIFY(summary.getUsername() == "johndoe");
        QTimer::singleShot(0, &loop, &QEventLoop::quit);
    });
    connect(&api, &PFXUserApi::getUserByNameSignalE, [&](PFXUser, QNetworkReply::NetworkError, QString error_str) {
        userFetched = true;
        qDebug() << "Error happened while issuing request : " << error_str;
        QTimer::singleShot(0, &loop, &QEventLoop::quit);
    });

    api.getUserByName("johndoe");
    QTimer::singleShot(14000, &loop, &QEventLoop::quit);
    loop.exec();
    QVERIFY2(userFetched, "didn't finish within timeout");
}

void UserApiTests::loginUserTest() {
    PFXUserApi api;
    QEventLoop loop;
    bool userLogged = false;

    connect(&api, &PFXUserApi::loginUserSignal, [&](QString summary) {
        userLogged = true;
        qDebug() << summary;
        QTimer::singleShot(0, &loop, &QEventLoop::quit);
    });
    connect(&api, &PFXUserApi::loginUserSignalE, [&](QString, QNetworkReply::NetworkError, QString error_str) {
        userLogged = true;
        qDebug() << "Error happened while issuing request : " << error_str;
        QTimer::singleShot(0, &loop, &QEventLoop::quit);
    });

    api.loginUser("johndoe", "123456789");
    QTimer::singleShot(14000, &loop, &QEventLoop::quit);
    loop.exec();
    QVERIFY2(userLogged, "didn't finish within timeout");
}

void UserApiTests::logoutUserTest() {
    PFXUserApi api;
    QEventLoop loop;
    bool userLoggedOut = false;

    connect(&api, &PFXUserApi::logoutUserSignal, [&]() {
        userLoggedOut = true;
        QTimer::singleShot(0, &loop, &QEventLoop::quit);
    });
    connect(&api, &PFXUserApi::logoutUserSignalE, [&](QNetworkReply::NetworkError, QString error_str) {
        qDebug() << "Error happened while issuing request : " << error_str;
        QTimer::singleShot(0, &loop, &QEventLoop::quit);
    });

    api.logoutUser();
    QTimer::singleShot(14000, &loop, &QEventLoop::quit);
    loop.exec();
    QVERIFY2(userLoggedOut, "didn't finish within timeout");
}

void UserApiTests::updateUserTest() {
    PFXUserApi api;
    QEventLoop loop;
    bool userUpdated = false;

    connect(&api, &PFXUserApi::updateUserSignal, [&]() {
        userUpdated = true;
        QTimer::singleShot(0, &loop, &QEventLoop::quit);
    });
    connect(&api, &PFXUserApi::updateUserSignalE, [&](QNetworkReply::NetworkError, QString error_str) {
        qDebug() << "Error happened while issuing request : " << error_str;
        QTimer::singleShot(0, &loop, &QEventLoop::quit);
    });

    auto johndoe = createRandomUser();
    johndoe.setUsername("johndoe");
    api.updateUser("johndoe", johndoe);
    QTimer::singleShot(14000, &loop, &QEventLoop::quit);
    loop.exec();
    QVERIFY2(userUpdated, "didn't finish within timeout");
}

