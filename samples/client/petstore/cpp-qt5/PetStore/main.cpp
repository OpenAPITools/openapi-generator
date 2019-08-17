#include <QCoreApplication>
#include <QTest>

#include "PetApiTests.h"
#include "StoreApiTests.h"
#include "UserApiTests.h"

int main(int argc, char *argv[]) {
    QCoreApplication a(argc, argv);
    PetApiTests petApiTests;
    StoreApiTests storeApiTests;
    UserApiTests userApiTests;
    int failedTests = 0;

    failedTests += QTest::qExec(&petApiTests);
    failedTests += QTest::qExec(&storeApiTests);
    failedTests += QTest::qExec(&userApiTests);

    return failedTests;
}
