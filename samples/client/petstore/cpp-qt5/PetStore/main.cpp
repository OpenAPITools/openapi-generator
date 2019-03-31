#include <QCoreApplication>
#include "PetApiTests.h"
#include "StoreApiTests.h"
#include "UserApiTests.h"

int main(int argc, char *argv[]) {
    QCoreApplication a(argc, argv);
    PetApiTests::runTests();
    StoreApiTests::runTests();
    UserApiTests::runTests();
    return a.exec();
}
