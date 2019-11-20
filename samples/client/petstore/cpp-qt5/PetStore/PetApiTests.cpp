#include "PetApiTests.h"

#include <QTest>
#include <QTimer>

OAIPet PetApiTests::createRandomPet() {
    OAIPet pet;
    qint64 id = QDateTime::currentMSecsSinceEpoch();
    pet.setName("monster");
    pet.setId(id);
    pet.setStatus("freaky");
    return pet;
}

void PetApiTests::findPetsByStatusTest() {
    OAIPetApi api;
    api.setHost(PetStoreHost);
    QEventLoop loop;
    bool petFound = false;

    connect(&api, &OAIPetApi::findPetsByStatusSignal, [&](QList<OAIPet> pets) {
        petFound = true;
        foreach(OAIPet pet, pets) {
            QVERIFY(pet.getStatus().startsWith("available") || pet.getStatus().startsWith("sold"));
        }
        loop.quit();
    });

    api.findPetsByStatus({"available", "sold"});
    QTimer::singleShot(5000, &loop, &QEventLoop::quit);
    loop.exec();
    QVERIFY2(petFound, "didn't finish within timeout");
}

void PetApiTests::createAndGetPetTest() {
    OAIPetApi api;
    api.setHost(PetStoreHost);
    QEventLoop loop;
    bool petCreated = false;

    connect(&api, &OAIPetApi::addPetSignal, [&]() {
        // pet created
        petCreated = true;
        loop.quit();
    });

    OAIPet pet = createRandomPet();
    qint64 id = pet.getId();

    api.addPet(pet);
    QTimer::singleShot(14000, &loop, &QEventLoop::quit);
    loop.exec();
    QVERIFY2(petCreated, "didn't finish within timeout");

    bool petFetched = false;

    connect(&api, &OAIPetApi::getPetByIdSignal, [&](OAIPet pet) {
        QVERIFY(pet.getId() > 0);
        QVERIFY(pet.getStatus().compare("freaky") == 0);
        loop.quit();
        petFetched = true;
    });

    api.getPetById(id);
    QTimer::singleShot(14000, &loop, &QEventLoop::quit);
    loop.exec();
    QVERIFY2(petFetched, "didn't finish within timeout");

}

void PetApiTests::updatePetTest() {
    OAIPetApi api;
    api.setHost(PetStoreHost);

    OAIPet pet = createRandomPet();
    OAIPet petToCheck;
    qint64 id = pet.getId();
    QEventLoop loop;
    bool petAdded = false;

    connect(&api, &OAIPetApi::addPetSignal, [&](){
        petAdded = true;
        loop.quit();
    });

    // create pet
    api.addPet(pet);
    QTimer::singleShot(5000, &loop, &QEventLoop::quit);
    loop.exec();
    QVERIFY2(petAdded, "didn't finish within timeout");

    // fetch it

    bool petFetched = false;
    connect(&api, &OAIPetApi::getPetByIdSignal, this, [&](OAIPet pet) {
            petFetched = true;
            petToCheck = pet;
            loop.quit();
    });

    // create pet
    api.getPetById(id);
    QTimer::singleShot(5000, &loop, &QEventLoop::quit);
    loop.exec();
    QVERIFY2(petFetched, "didn't finish within timeout");

    // update it
    bool petUpdated = false;
    connect(&api, &OAIPetApi::updatePetSignal, [&]() {
        petUpdated = true;
        loop.quit();
    });

    // update pet
    petToCheck.setStatus(QString("scary"));
    api.updatePet(petToCheck);
    QTimer::singleShot(5000, &loop, &QEventLoop::quit);
    loop.exec();
    QVERIFY2(petUpdated, "didn't finish within timeout");

    // check it
    bool petFetched2 = false;
    connect(&api, &OAIPetApi::getPetByIdSignal, [&](OAIPet pet) {
        petFetched2 = true;
        QVERIFY(pet.getId() == petToCheck.getId());
        QVERIFY(pet.getStatus().compare(petToCheck.getStatus()) == 0);
        loop.quit();
    });
    api.getPetById(id);
    QTimer::singleShot(5000, &loop, &QEventLoop::quit);
    loop.exec();
    QVERIFY2(petFetched2, "didn't finish within timeout");
}

void PetApiTests::updatePetWithFormTest() {
    OAIPetApi api;
    api.setHost(PetStoreHost);

    OAIPet pet = createRandomPet();
    OAIPet petToCheck;
    qint64 id = pet.getId();
    QEventLoop loop;

    // create pet
    bool petAdded = false;
    connect(&api, &OAIPetApi::addPetSignal, [&](){
        petAdded = true;
        loop.quit();
    });

    api.addPet(pet);
    QTimer::singleShot(5000, &loop, &QEventLoop::quit);
    loop.exec();
    QVERIFY2(petAdded, "didn't finish within timeout");

    // fetch it
    bool petFetched = false;
    connect(&api, &OAIPetApi::getPetByIdSignal, [&](OAIPet pet) {
        petFetched = true;
        petToCheck = pet;
        loop.quit();
    });

    api.getPetById(id);
    QTimer::singleShot(5000, &loop, &QEventLoop::quit);
    loop.exec();
    QVERIFY2(petFetched, "didn't finish within timeout");

    // update it
    bool petUpdated = false;
    connect(&api, &OAIPetApi::updatePetWithFormSignal, [&](){
        petUpdated = true;
        loop.quit();
    });

    QString name("gorilla");
    api.updatePetWithForm(id, name, nullptr);
    QTimer::singleShot(5000, &loop, &QEventLoop::quit);
    loop.exec();
    QVERIFY2(petUpdated, "didn't finish within timeout");

    // fetch it
    bool petUpdated2 = false;
    connect(&api, &OAIPetApi::getPetByIdSignal, [&](OAIPet pet) {
        petUpdated2 = true;
        QVERIFY(pet.getName().compare(QString("gorilla")) == 0);
        loop.quit();
    });

    api.getPetById(id);
    QTimer::singleShot(5000, &loop, &QEventLoop::quit);
    loop.exec();
    QVERIFY2(petUpdated2, "didn't finish within timeout");
}
