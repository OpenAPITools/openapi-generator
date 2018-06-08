#include <QCoreApplication>
#include "PetApiTests.h"

int main(int argc, char *argv[]) {
    QCoreApplication a(argc, argv);

    PetApiTests::runTests();



    return a.exec();
}
