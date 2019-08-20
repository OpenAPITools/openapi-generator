#pragma once

#include "../client/OAIUserApi.h"

using namespace OpenAPI;

class UserApiTests: public QObject {
    Q_OBJECT

    OAIUserApi* getApi();
    OAIUser createRandomUser();

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
