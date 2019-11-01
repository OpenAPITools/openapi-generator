#include "UserApiTests.h"

#include <QTest>
#include <QTimer>
#include <QDebug>

OAIUser UserApiTests::createRandomUser() {
    OAIUser user;
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

void UserApiTests::createUserTest(){
    OAIUserApi api;
    api.setHost(PetStoreHost);
    QEventLoop loop;
    bool userCreated = false;

    connect(&api, &OAIUserApi::createUserSignal, [&](){
            userCreated = true;
            loop.quit();
    });

    api.createUser(createRandomUser());
    QTimer::singleShot(14000, &loop, &QEventLoop::quit);
    loop.exec();
    QVERIFY2(userCreated, "didn't finish within timeout");
}

void UserApiTests::createUsersWithArrayInputTest(){
    OAIUserApi api;
    api.setHost(PetStoreHost);
    QEventLoop loop;
    bool usersCreated = false;

    connect(&api, &OAIUserApi::createUsersWithArrayInputSignal, [&](){
            usersCreated = true;
            loop.quit();
    });

    QList<OAIUser> users;
    users.append(createRandomUser());
    users.append(createRandomUser());
    users.append(createRandomUser());
    api.createUsersWithArrayInput(users);
    QTimer::singleShot(14000, &loop, &QEventLoop::quit);
    loop.exec();
    QVERIFY2(usersCreated, "didn't finish within timeout");
}

void UserApiTests::createUsersWithListInputTest(){
    OAIUserApi api;
    api.setHost(PetStoreHost);
    QEventLoop loop;
    bool usersCreated = false;

    connect(&api, &OAIUserApi::createUsersWithListInputSignal, [&](){
            usersCreated = true;
            loop.quit();
    });

    QList<OAIUser> users;
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

void UserApiTests::deleteUserTest(){
    OAIUserApi api;
    api.setHost(PetStoreHost);
    QEventLoop loop;
    bool userDeleted = false;

    connect(&api, &OAIUserApi::deleteUserSignal, [&](){
            userDeleted = true;
            loop.quit();
    });

    api.deleteUser("rambo");
    QTimer::singleShot(14000, &loop, &QEventLoop::quit);
    loop.exec();
    QVERIFY2(userDeleted, "didn't finish within timeout");
}

void UserApiTests::getUserByNameTest(){
    OAIUserApi api;
    api.setHost(PetStoreHost);
    QEventLoop loop;
    bool userFetched = false;

    connect(&api, &OAIUserApi::getUserByNameSignal, [&](OAIUser summary) {
        userFetched = true;
        qDebug() << summary.getUsername();
        QVERIFY(summary.getUsername() == "johndoe");
        loop.quit();
    });

    api.getUserByName("johndoe");
    QTimer::singleShot(14000, &loop, &QEventLoop::quit);
    loop.exec();
    QVERIFY2(userFetched, "didn't finish within timeout");
}

void UserApiTests::loginUserTest(){
    OAIUserApi api;
    api.setHost(PetStoreHost);
    QEventLoop loop;
    bool userLogged = false;

    connect(&api, &OAIUserApi::loginUserSignal, [&](QString summary) {
        userLogged = true;
        qDebug() << summary;
        loop.quit();
    });

    api.loginUser("johndoe", "123456789");
    QTimer::singleShot(14000, &loop, &QEventLoop::quit);
    loop.exec();
    QVERIFY2(userLogged, "didn't finish within timeout");
}

void UserApiTests::logoutUserTest(){
    OAIUserApi api;
    api.setHost(PetStoreHost);
    QEventLoop loop;
    bool userLoggedOut = false;

    connect(&api, &OAIUserApi::logoutUserSignal, [&](){
        userLoggedOut = true;
        loop.quit();
    });

    api.logoutUser();
    QTimer::singleShot(14000, &loop, &QEventLoop::quit);
    loop.exec();
    QVERIFY2(userLoggedOut, "didn't finish within timeout");
}

void UserApiTests::updateUserTest(){
    OAIUserApi api;
    api.setHost(PetStoreHost);
    QEventLoop loop;
    bool userUpdated = false;

    connect(&api, &OAIUserApi::updateUserSignal, [&]() {
            userUpdated = true;
            loop.quit();
    });

    auto johndoe = createRandomUser();
    johndoe.setUsername("johndoe");
    api.updateUser("johndoe", johndoe);
    QTimer::singleShot(14000, &loop, &QEventLoop::quit);
    loop.exec();
    QVERIFY2(userUpdated, "didn't finish within timeout");
}
