#include "PetApiTests.h"

#include <QTest>
#include <QTimer>

PFXPet PetApiTests::createRandomPet() {
    PFXPet pet;
    qint64 id = QDateTime::currentMSecsSinceEpoch();
    pet.setName("monster");
    pet.setId(id);
    pet.setStatus("freaky");
    return pet;
}

void PetApiTests::findPetsByStatusTest() {
    PFXPetApi api;
    QEventLoop loop;
    bool petFound = false;

    connect(&api, &PFXPetApi::findPetsByStatusSignal, [&](QList<PFXPet> pets) {
        petFound = true;
        foreach(PFXPet pet, pets) {
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
    PFXPetApi api;
    QEventLoop loop;
    bool petCreated = false;

    connect(&api, &PFXPetApi::addPetSignal, [&]() {
        // pet created
        petCreated = true;
        loop.quit();
    });

    PFXPet pet = createRandomPet();
    qint64 id = pet.getId();

    api.addPet(pet);
    QTimer::singleShot(14000, &loop, &QEventLoop::quit);
    loop.exec();
    QVERIFY2(petCreated, "didn't finish within timeout");

    bool petFetched = false;

    connect(&api, &PFXPetApi::getPetByIdSignal, [&](PFXPet pet) {
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
    PFXPetApi api;

    PFXPet pet = createRandomPet();
    PFXPet petToCheck;
    qint64 id = pet.getId();
    QEventLoop loop;
    bool petAdded = false;

    connect(&api, &PFXPetApi::addPetSignal, [&](){
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
    connect(&api, &PFXPetApi::getPetByIdSignal, this, [&](PFXPet pet) {
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
    connect(&api, &PFXPetApi::updatePetSignal, [&]() {
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
    connect(&api, &PFXPetApi::getPetByIdSignal, [&](PFXPet pet) {
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
    PFXPetApi api;

    PFXPet pet = createRandomPet();
    PFXPet petToCheck;
    qint64 id = pet.getId();
    QEventLoop loop;

    // create pet
    bool petAdded = false;
    connect(&api, &PFXPetApi::addPetSignal, [&](){
        petAdded = true;
        loop.quit();
    });

    api.addPet(pet);
    QTimer::singleShot(5000, &loop, &QEventLoop::quit);
    loop.exec();
    QVERIFY2(petAdded, "didn't finish within timeout");

    // fetch it
    bool petFetched = false;
    connect(&api, &PFXPetApi::getPetByIdSignal, [&](PFXPet pet) {
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
    connect(&api, &PFXPetApi::updatePetWithFormSignal, [&](){
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
    connect(&api, &PFXPetApi::getPetByIdSignal, [&](PFXPet pet) {
        petUpdated2 = true;
        QVERIFY(pet.getName().compare(QString("gorilla")) == 0);
        loop.quit();
    });

    api.getPetById(id);
    QTimer::singleShot(5000, &loop, &QEventLoop::quit);
    loop.exec();
    QVERIFY2(petUpdated2, "didn't finish within timeout");
}
