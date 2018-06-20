/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

/*
 * File:   PetStoreTest.h
 * Author: akhil
 *
 * Created on June 9, 2016, 3:33 PM
 */

#ifndef PETSTORETEST_H
#define PETSTORETEST_H

#include "../../src/PetManager.h"

using namespace Tizen::ArtikCloud;

class PetStoreTest {
public:
    PetStoreTest();
    PetStoreTest(const PetStoreTest& orig);
    virtual ~PetStoreTest();
    void createPetTest();
    void getPetTest();
    void runAllTests();
    void updatePetWithFormDataTest();
    void findPetByStatus();
    void deletePetTest();
private:
    PetManager petmanager;
    std::string accessToken;

};

#endif /* PETSTORETEST_H */
