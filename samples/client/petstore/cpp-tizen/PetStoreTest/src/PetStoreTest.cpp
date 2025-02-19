/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

/*
 * File:   PetStoreTest.cpp
 * Author: akhil
 *
 * Created on June 9, 2016, 3:33 PM
 */

#include "PetStoreTest.h"
#include <iostream>
#include <cassert>

using namespace std;

void addPetCallback(Error error, void* userdata){
    int code = error.getCode();
    assert(code == 200);
    cout<<"In addPetCallback \n";
}
void getPetCallback(Pet pet,Error error, void* userdata){
    int code = error.getCode();
    assert(code == 200);
    string name = pet.getName();
    assert(strcmp(name.c_str(),"myname")==0);
    cout<<"In getPetCallback \n";
}
void updatePetCallback(Error error, void* userdata){
    int code = error.getCode();
    assert(code == 200);
    cout<<"In updatePetCallback \n";
}
void findPetCallback(list<Pet> retlist, Error error, void* userdata){
    int code = error.getCode();
    assert(code == 200);
    int size = retlist.size();
    assert(size>=1);
    cout<<"In findPetCallback \n";
}
void deletePetCallback(Error error, void* userdata){
    int code = error.getCode();
    assert(code == 200);
    cout<<"In deletePetCallback \n";
}

PetStoreTest::PetStoreTest() {
}


PetStoreTest::~PetStoreTest() {
}

void PetStoreTest::runAllTests(){
    accessToken = "special-key";
    createPetTest();
    getPetTest();
    updatePetWithFormDataTest();
    findPetByStatus();
    deletePetTest();
}

void PetStoreTest::createPetTest() {
    Category category;
    category.setId(100);
    category.setName("mycategory");
    Pet body;
    body.setId(100);
    body.setCategory(category);
    body.setName("myname");
    list<string> urllist;
    urllist.push_back("myurl");
    body.setPhotoUrls(urllist);
    Tag tag;
    tag.setId(100);
    tag.setName("mytag");
    list<Tag> taglist;
    taglist.push_back(tag);
    body.setTags(taglist);
    body.setStatus("available");
    //body.setType("mytype");

    petmanager.addPetSync(accessToken.c_str(),body,addPetCallback,NULL);
}

void PetStoreTest::getPetTest() {
    petmanager.getPetByIdSync(accessToken.c_str(),100,getPetCallback,NULL);
}

void PetStoreTest::updatePetWithFormDataTest() {
    petmanager.updatePetWithFormSync(accessToken.c_str(),100,"myname2","pending",updatePetCallback,NULL);
}
void PetStoreTest::findPetByStatus(){
    list<string> mylist;
    mylist.push_back("pending");
    petmanager.findPetsByStatusSync(accessToken.c_str(),mylist,findPetCallback,NULL);
}

void PetStoreTest::deletePetTest() {
    petmanager.deletePetSync(accessToken.c_str(),100,accessToken,deletePetCallback,NULL);
}
