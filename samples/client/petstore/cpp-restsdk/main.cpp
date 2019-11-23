#include "PetApiTests.h"

int main(int argc, char *argv[]) {
    auto petTest = std::make_shared<OAIPetApiTests>();
    petTest->runTests();
}
