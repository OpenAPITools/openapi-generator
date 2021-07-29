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
        foreach (PFXPet pet, pets) {
            QVERIFY(pet.getStatus().startsWith("available") || pet.getStatus().startsWith("sold"));
        }
        QTimer::singleShot(0, &loop, &QEventLoop::quit);
    });
    connect(&api, &PFXPetApi::findPetsByStatusSignalE, [&](QList<PFXPet>, QNetworkReply::NetworkError, QString error_str) {
        qDebug() << "Error happened while issuing request : " << error_str;
        QTimer::singleShot(0, &loop, &QEventLoop::quit);
    });

    api.findPetsByStatus({"available", "sold"});

    QTimer::singleShot(5000, &loop, &QEventLoop::quit);
    loop.exec();
    QVERIFY2(petFound, "didn't finish within timeout");
}

void PetApiTests::createAndGetPetTest() {
    PFXPetApi api;
    api.setApiKey("api_key","special-key");
    QEventLoop loop;
    bool petCreated = false;

    connect(&api, &PFXPetApi::addPetSignal, [&]() {
        // pet created
        petCreated = true;
        QTimer::singleShot(0, &loop, &QEventLoop::quit);
    });
    connect(&api, &PFXPetApi::addPetSignalE, [&](QNetworkReply::NetworkError, QString error_str) {
        qDebug() << "Error happened while issuing request : " << error_str;
        QTimer::singleShot(0, &loop, &QEventLoop::quit);
    });


    PFXPet pet = createRandomPet();
    qint64 id = pet.getId();

    api.addPet(pet);
    QTimer::singleShot(14000, &loop, &QEventLoop::quit);
    loop.exec();
    QVERIFY2(petCreated, "didn't finish within timeout");

    bool petFetched = false;

    connect(&api, &PFXPetApi::getPetByIdSignal, [&](PFXPet pet) {
        QTimer::singleShot(0, &loop, &QEventLoop::quit);
        QVERIFY(pet.getId() > 0);
//      QVERIFY(pet.getStatus().compare("freaky") == 0);
        petFetched = true;
    });
    connect(&api, &PFXPetApi::getPetByIdSignalE, [&](PFXPet, QNetworkReply::NetworkError, QString error_str) {
        qDebug() << "Error happened while issuing request : " << error_str;
        QTimer::singleShot(0, &loop, &QEventLoop::quit);
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

    connect(&api, &PFXPetApi::addPetSignal, [&]() {
        petAdded = true;
        QTimer::singleShot(0, &loop, &QEventLoop::quit);
    });
    connect(&api, &PFXPetApi::addPetSignalE, [&](QNetworkReply::NetworkError, QString error_str) {
        qDebug() << "Error happened while issuing request : " << error_str;
        QTimer::singleShot(0, &loop, &QEventLoop::quit);
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
        QTimer::singleShot(0, &loop, &QEventLoop::quit);
    });
    connect(&api, &PFXPetApi::getPetByIdSignalE, this, [&](PFXPet, QNetworkReply::NetworkError, QString error_str) {
        qDebug() << "Error happened while issuing request : " << error_str;
        QTimer::singleShot(0, &loop, &QEventLoop::quit);
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
        QTimer::singleShot(0, &loop, &QEventLoop::quit);
    });
    connect(&api, &PFXPetApi::updatePetSignalE, [&](QNetworkReply::NetworkError, QString error_str) {
        qDebug() << "Error happened while issuing request : " << error_str;
        QTimer::singleShot(0, &loop, &QEventLoop::quit);
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
        QTimer::singleShot(0, &loop, &QEventLoop::quit);
    });
    connect(&api, &PFXPetApi::getPetByIdSignalE, [&](PFXPet, QNetworkReply::NetworkError, QString error_str) {
        qDebug() << "Error happened while issuing request : " << error_str;
        QTimer::singleShot(0, &loop, &QEventLoop::quit);
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
    connect(&api, &PFXPetApi::addPetSignal, [&]() {
        petAdded = true;
        QTimer::singleShot(0, &loop, &QEventLoop::quit);
    });
    connect(&api, &PFXPetApi::addPetSignalE, [&](QNetworkReply::NetworkError, QString error_str) {
        qDebug() << "Error happened while issuing request : " << error_str;
        QTimer::singleShot(0, &loop, &QEventLoop::quit);
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
        QTimer::singleShot(0, &loop, &QEventLoop::quit);
    });
    connect(&api, &PFXPetApi::getPetByIdSignalE, [&](PFXPet, QNetworkReply::NetworkError, QString error_str) {
        qDebug() << "Error happened while issuing request : " << error_str;
        QTimer::singleShot(0, &loop, &QEventLoop::quit);
    });

    api.getPetById(id);
    QTimer::singleShot(5000, &loop, &QEventLoop::quit);
    loop.exec();
    QVERIFY2(petFetched, "didn't finish within timeout");

    // update it
    bool petUpdated = false;
    connect(&api, &PFXPetApi::updatePetWithFormSignal, [&]() {
        petUpdated = true;
        QTimer::singleShot(0, &loop, &QEventLoop::quit);
    });
    connect(&api, &PFXPetApi::updatePetWithFormSignalE, [&](QNetworkReply::NetworkError, QString error_str) {
        qDebug() << "Error happened while issuing request : " << error_str;
        QTimer::singleShot(0, &loop, &QEventLoop::quit);
    });

    QString name("gorilla");
    api.updatePetWithForm(id, name);
    QTimer::singleShot(5000, &loop, &QEventLoop::quit);
    loop.exec();
    QVERIFY2(petUpdated, "didn't finish within timeout");

    // fetch it
    bool petUpdated2 = false;
    connect(&api, &PFXPetApi::getPetByIdSignal, [&](PFXPet pet) {
        Q_UNUSED(pet);
        petUpdated2 = true;
//      QVERIFY(pet.getName().compare(QString("gorilla")) == 0);
        QTimer::singleShot(0, &loop, &QEventLoop::quit);
    });
    connect(&api, &PFXPetApi::getPetByIdSignalE, [&](PFXPet, QNetworkReply::NetworkError, QString error_str) {
        qDebug() << "Error happened while issuing request : " << error_str;
        QTimer::singleShot(0, &loop, &QEventLoop::quit);
    });

    api.getPetById(id);
    QTimer::singleShot(5000, &loop, &QEventLoop::quit);
    loop.exec();
    QVERIFY2(petUpdated2, "didn't finish within timeout");
}

