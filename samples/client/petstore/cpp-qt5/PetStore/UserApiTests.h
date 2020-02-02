#pragma once

#include "../client/PFXUserApi.h"

using namespace test_namespace;

class UserApiTests : public QObject {
    Q_OBJECT

    PFXUser createRandomUser();

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
