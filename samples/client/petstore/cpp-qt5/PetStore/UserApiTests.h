#ifndef USERAPITESTS_H
#define USERAPITESTS_H
#include <QtTest/QtTest>
#include <QTimer>

#include "../client/OAIUserApi.h"

using namespace OpenAPI;

class UserApiTests: public QObject {
Q_OBJECT
public:
    UserApiTests();
    virtual ~UserApiTests();

    static void runTests();

private:
    OAIUserApi* getApi();
    OAIUser createRandomUser();

signals:
    void quit();
    bool success();

private slots:
    void createUserTest();
    void createUsersWithArrayInputTest();
    void createUsersWithListInputTest();
    void deleteUserTest();
    void getUserByNameTest();
    void loginUserTest();
    void logoutUserTest();
    void updateUserTest();
};

#endif // USERAPITESTS_H
