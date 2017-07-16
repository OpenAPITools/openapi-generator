#include "PetApiTests.h"

#include <QJsonDocument>
#include <QNetworkAccessManager>
#include <QNetworkReply>
#include <QDebug>

PetApiTests::PetApiTests () {}

PetApiTests::~PetApiTests () {
    exit(1);
}

SWGPetApi* PetApiTests::getApi() {
    SWGPetApi* api = new SWGPetApi();
    api->host = "http://petstore.swagger.io";
    api->basePath = "/v2";
    return api;
}

Pet* PetApiTests::createRandomPet() {
    Pet* pet = new Pet();
    qint64 id = QDateTime::currentMSecsSinceEpoch();

    pet->setName(new QString("monster"));
    pet->setId(id);
    pet->setStatus(new QString("freaky"));

    return pet;
}

void PetApiTests::runTests() {
    PetApiTests* tests = new PetApiTests();
    QTest::qExec(tests);
    delete tests;
}

void PetApiTests::findPetsByStatusTest() {
    SWGPetApi* api = getApi();

    static QEventLoop loop;
    QTimer timer;
    timer.setInterval(14000);
    timer.setSingleShot(true);

    auto validator = [](QList<Pet*>* pets) {
        foreach(Pet* pet, *pets) {
            QVERIFY(pet->getStatus()->startsWith("available") || pet->getStatus()->startsWith("sold"));
        }
        loop.quit();
    };

    connect(api, &SWGPetApi::findPetsByStatusSignal, this, validator);
    connect(&timer, &QTimer::timeout, &loop, &QEventLoop::quit);

    QList<QString*>* status = new QList<QString*>();
    status->append(new QString("available"));
    status->append(new QString("sold"));
    api->findPetsByStatus(status);
    timer.start();
    loop.exec();
    QVERIFY2(timer.isActive(), "didn't finish within timeout");
    delete api;
}

void PetApiTests::createAndGetPetTest() {
    SWGPetApi* api = getApi();

    static QEventLoop loop;
    QTimer timer;
    timer.setInterval(14000);
    timer.setSingleShot(true);

    auto validator = []() {
        // pet created
        loop.quit();
    };

    connect(api, &SWGPetApi::addPetSignal, this, validator);
    connect(&timer, &QTimer::timeout, &loop, &QEventLoop::quit);

    Pet* pet = createRandomPet();
    qint64 id = pet->getId();

    api->addPet(*pet);
    timer.start();
    loop.exec();
    QVERIFY2(timer.isActive(), "didn't finish within timeout");

    timer.setInterval(1000);
    timer.setSingleShot(true);

    auto getPetValidator = [](Pet* pet) {
        QVERIFY(pet->getId() > 0);
        QVERIFY(pet->getStatus()->compare("freaky") == 0);
        loop.quit();
    };

    connect(api, &SWGPetApi::getPetByIdSignal, this, getPetValidator);
    connect(&timer, &QTimer::timeout, &loop, &QEventLoop::quit);

    api->getPetById(id);
    timer.start();
    loop.exec();
    QVERIFY2(timer.isActive(), "didn't finish within timeout");
    delete api;
}

void PetApiTests::updatePetTest() {
    static SWGPetApi* api = getApi();

    Pet* pet = createRandomPet();
    static Pet* petToCheck;
    qint64 id = pet->getId();
    static QEventLoop loop;
    QTimer timer;
    timer.setInterval(100000);
    timer.setSingleShot(true);

    auto validator = []() {
        loop.quit();
    };

    connect(api, &SWGPetApi::addPetSignal, this, validator);
    connect(&timer, &QTimer::timeout, &loop, &QEventLoop::quit);

    // create pet
    api->addPet(*pet);
    timer.start();
    loop.exec();
    QVERIFY2(timer.isActive(), "didn't finish within timeout");

    // fetch it
    timer.setInterval(1000);
    timer.setSingleShot(true);

    auto fetchPet = [](Pet* pet) {
        petToCheck = pet;
        loop.quit();
    };
    connect(api, &SWGPetApi::getPetByIdSignal, this, fetchPet);
    connect(&timer, &QTimer::timeout, &loop, &QEventLoop::quit);

    // create pet
    api->getPetById(id);
    timer.start();
    loop.exec();
    QVERIFY2(timer.isActive(), "didn't finish within timeout");

    // update it
    timer.setInterval(1000);
    timer.setSingleShot(true);
    auto updatePetTest = []() {
        loop.quit();
    };

    connect(api, &SWGPetApi::updatePetSignal, this, updatePetTest);
    connect(&timer, &QTimer::timeout, &loop, &QEventLoop::quit);

    // update pet
    petToCheck->setStatus(new QString("scary"));
    api->updatePet(*petToCheck);
    timer.start();
    loop.exec();
    QVERIFY2(timer.isActive(), "didn't finish within timeout");

    // check it
    timer.setInterval(1000);
    timer.setSingleShot(true);

    auto fetchPet2 = [](Pet* pet) {
        QVERIFY(pet->getId() == petToCheck->getId());
        QVERIFY(pet->getStatus()->compare(petToCheck->getStatus()) == 0);
        loop.quit();
    };
    connect(api, &SWGPetApi::getPetByIdSignal, this, fetchPet2);
    connect(&timer, &QTimer::timeout, &loop, &QEventLoop::quit);
    api->getPetById(id);
    timer.start();
    loop.exec();
    QVERIFY2(timer.isActive(), "didn't finish within timeout");
}

void PetApiTests::updatePetWithFormTest() {
    static SWGPetApi* api = getApi();

    Pet* pet = createRandomPet();
    static Pet* petToCheck;
    qint64 id = pet->getId();
    static QEventLoop loop;
    QTimer timer;

    // create pet
    timer.setInterval(1000);
    timer.setSingleShot(true);

    auto validator = []() {
        loop.quit();
    };

    connect(api, &SWGPetApi::addPetSignal, this, validator);
    connect(&timer, &QTimer::timeout, &loop, &QEventLoop::quit);
    api->addPet(*pet);
    timer.start();
    loop.exec();
    QVERIFY2(timer.isActive(), "didn't finish within timeout");

    // fetch it
    timer.setInterval(1000);
    timer.setSingleShot(true);

    auto fetchPet = [](Pet* pet) {
        petToCheck = pet;
        loop.quit();
    };
    connect(api, &SWGPetApi::getPetByIdSignal, this, fetchPet);
    connect(&timer, &QTimer::timeout, &loop, &QEventLoop::quit);

    api->getPetById(id);
    timer.start();
    loop.exec();
    QVERIFY2(timer.isActive(), "didn't finish within timeout");

    // update it
    timer.setInterval(1000);
    timer.setSingleShot(true);

    connect(api, &SWGPetApi::updatePetWithFormSignal, this, [](){loop.quit();});
    connect(&timer, &QTimer::timeout, &loop, &QEventLoop::quit);

    api->updatePetWithForm(id, new QString("gorilla"), NULL);
    timer.start();
    loop.exec();
    QVERIFY2(timer.isActive(), "didn't finish within timeout");

    // fetch it
    timer.setInterval(1000);
    timer.setSingleShot(true);

    auto fetchUpdatedPet = [](Pet* pet) {
        QVERIFY(pet->getName()->compare(QString("gorilla")) == 0);
        loop.quit();
    };
    connect(api, &SWGPetApi::getPetByIdSignal, this, fetchUpdatedPet);
    connect(&timer, &QTimer::timeout, &loop, &QEventLoop::quit);

    api->getPetById(id);
    timer.start();
    loop.exec();
    QVERIFY2(timer.isActive(), "didn't finish within timeout");
}
