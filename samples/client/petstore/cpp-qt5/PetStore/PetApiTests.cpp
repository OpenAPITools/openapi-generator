#include "PetApiTests.h"

#include <QJsonDocument>
#include <QNetworkAccessManager>
#include <QNetworkReply>
#include <QDebug>

PetApiTests::PetApiTests () {}

PetApiTests::~PetApiTests () {
    exit(1);
}

OAIPetApi* PetApiTests::getApi() {
    auto api = new OAIPetApi();
    api->host = "http://petstore.swagger.io";
    api->basePath = "/v2";
    return api;
}

OAIPet* PetApiTests::createRandomPet() {
    OAIPet* pet = new OAIPet();
    qint64 id = QDateTime::currentMSecsSinceEpoch();
    pet->getName()->clear();
    pet->getName()->append(QString("monster"));
    pet->setId(id);
    pet->getStatus()->clear();
    pet->getStatus()->append(QString("freaky"));

    return pet;
}

void PetApiTests::runTests() {
    PetApiTests* tests = new PetApiTests();
    QTest::qExec(tests);
    delete tests;
}

void PetApiTests::findPetsByStatusTest() {
    OAIPetApi* api = getApi();

    QEventLoop loop;
    QTimer timer;
    timer.setInterval(14000);
    timer.setSingleShot(true);

    auto validator = [this](QList<OAIPet*>* pets) {
        foreach(OAIPet* pet, *pets) {
            QVERIFY(pet->getStatus()->startsWith("available") || pet->getStatus()->startsWith("sold"));
        }
        emit quit();
    };
    auto finalizer = [&]() {
        loop.quit();
    };
    connect(this, &PetApiTests::quit, finalizer);
    connect(api, &OAIPetApi::findPetsByStatusSignal, this, validator);
    connect(&timer, &QTimer::timeout, &loop, finalizer);

    QList<QString*>* status = new QList<QString*>();
    auto available = new QString("available");
    auto sold = new QString("sold");
    status->append(available);
    status->append(sold);
    api->findPetsByStatus(status);
    timer.start();
    loop.exec();
    QVERIFY2(timer.isActive(), "didn't finish within timeout");
    disconnect(this, 0,0,0);
    delete api;
    delete status;
    delete sold;
    delete available;
}

void PetApiTests::createAndGetPetTest() {
    OAIPetApi* api = getApi();

    QEventLoop loop;
    QTimer timer;
    timer.setInterval(14000);
    timer.setSingleShot(true);

    auto validator = [this]() {
        // pet created
        emit quit();
    };

    auto finalizer = [&]() {
        loop.quit();
    };
    connect(this, &PetApiTests::quit, finalizer);
    connect(api, &OAIPetApi::addPetSignal, this, validator);
    connect(&timer, &QTimer::timeout, &loop, finalizer);

    OAIPet* pet = createRandomPet();
    qint64 id = pet->getId();

    api->addPet(*pet);
    timer.start();
    loop.exec();
    QVERIFY2(timer.isActive(), "didn't finish within timeout");

    timer.setInterval(1000);
    timer.setSingleShot(true);

    auto getPetValidator = [this](OAIPet* pet) {
        QVERIFY(pet->getId() > 0);
        QVERIFY(pet->getStatus()->compare("freaky") == 0);
        emit quit();
    };

    connect(api, &OAIPetApi::getPetByIdSignal, this, getPetValidator);
    connect(&timer, &QTimer::timeout, &loop, finalizer);

    api->getPetById(id);
    timer.start();
    loop.exec();
    QVERIFY2(timer.isActive(), "didn't finish within timeout");
    disconnect(this, 0,0,0);
    delete api;
    delete pet;
}

void PetApiTests::updatePetTest() {
    OAIPetApi* api = getApi();

    OAIPet* pet = createRandomPet();
    OAIPet* petToCheck;
    qint64 id = pet->getId();
    QEventLoop loop;
    QTimer timer;
    timer.setInterval(100000);
    timer.setSingleShot(true);

    auto validator = [this]() {
        emit quit();
    };
    auto finalizer = [&]() {
        loop.quit();
    };
    connect(this, &PetApiTests::quit, finalizer);
    connect(api, &OAIPetApi::addPetSignal, this, validator);
    connect(&timer, &QTimer::timeout, &loop, finalizer);

    // create pet
    api->addPet(*pet);
    timer.start();
    loop.exec();
    QVERIFY2(timer.isActive(), "didn't finish within timeout");

    // fetch it
    timer.setInterval(1000);
    timer.setSingleShot(true);

    auto fetchPet = [&](OAIPet* pet) {
        petToCheck = pet;
        emit quit();
    };
    connect(api, &OAIPetApi::getPetByIdSignal, this, fetchPet);
    connect(&timer, &QTimer::timeout, &loop, finalizer);

    // create pet
    api->getPetById(id);
    timer.start();
    loop.exec();
    QVERIFY2(timer.isActive(), "didn't finish within timeout");

    // update it
    timer.setInterval(1000);
    timer.setSingleShot(true);
    auto updatePetTest = [this]() {
        emit quit();
    };

    connect(api, &OAIPetApi::updatePetSignal, this, updatePetTest);
    connect(&timer, &QTimer::timeout, &loop, finalizer);

    // update pet
    petToCheck->getStatus()->clear();
    petToCheck->getStatus()->append(QString("scary"));
    api->updatePet(*petToCheck);
    timer.start();
    loop.exec();
    QVERIFY2(timer.isActive(), "didn't finish within timeout");

    // check it
    timer.setInterval(1000);
    timer.setSingleShot(true);

    auto fetchPet2 = [&](OAIPet* pet) {
        QVERIFY(pet->getId() == petToCheck->getId());
        QVERIFY(pet->getStatus()->compare(petToCheck->getStatus()) == 0);
        emit quit();
    };
    connect(api, &OAIPetApi::getPetByIdSignal, this, fetchPet2);
    connect(&timer, &QTimer::timeout, &loop, finalizer);
    api->getPetById(id);
    timer.start();
    loop.exec();
    QVERIFY2(timer.isActive(), "didn't finish within timeout");
    disconnect(this, 0,0,0);
    delete api;
    delete pet;
}

void PetApiTests::updatePetWithFormTest() {
    OAIPetApi* api = getApi();

    OAIPet* pet = createRandomPet();
    OAIPet* petToCheck;
    qint64 id = pet->getId();
    QEventLoop loop;
    QTimer timer;

    // create pet
    timer.setInterval(1000);
    timer.setSingleShot(true);

    auto validator = [this ]() {
        emit quit();
    };
    auto finalizer = [&]() {
        loop.quit();
    };
    connect(this, &PetApiTests::quit, finalizer);
    connect(api, &OAIPetApi::addPetSignal, this, validator);
    connect(&timer, &QTimer::timeout, &loop, finalizer);
    api->addPet(*pet);
    timer.start();
    loop.exec();
    QVERIFY2(timer.isActive(), "didn't finish within timeout");

    // fetch it
    timer.setInterval(1000);
    timer.setSingleShot(true);

    auto fetchPet = [&](OAIPet* pet) {
        petToCheck = pet;
        emit quit();
    };
    connect(api, &OAIPetApi::getPetByIdSignal, this, fetchPet);
    connect(&timer, &QTimer::timeout, &loop, finalizer);

    api->getPetById(id);
    timer.start();
    loop.exec();
    QVERIFY2(timer.isActive(), "didn't finish within timeout");

    // update it
    timer.setInterval(1000);
    timer.setSingleShot(true);

    connect(api, &OAIPetApi::updatePetWithFormSignal, this, [this](){emit quit();});
    connect(&timer, &QTimer::timeout, &loop, finalizer);

    auto name = new QString("gorilla");
    api->updatePetWithForm(id, name, NULL);
    timer.start();
    loop.exec();
    QVERIFY2(timer.isActive(), "didn't finish within timeout");

    // fetch it
    timer.setInterval(1000);
    timer.setSingleShot(true);

    auto fetchUpdatedPet = [this](OAIPet* pet) {
        QVERIFY(pet->getName()->compare(QString("gorilla")) == 0);
        emit quit();
    };
    connect(api, &OAIPetApi::getPetByIdSignal, this, fetchUpdatedPet);
    connect(&timer, &QTimer::timeout, &loop, finalizer);

    api->getPetById(id);
    timer.start();
    loop.exec();
    QVERIFY2(timer.isActive(), "didn't finish within timeout");
    disconnect(this, 0,0,0);
    delete api;
    delete pet;
    delete name;
}
