import api = require('./api');
import fs = require('fs');

// a should define the required properties
function deepCheck(objectA: any, objectB: any): boolean {
    let a = objectA;
    let b = objectB;
    let isString: boolean = (typeof a === "string" && typeof b === "string");
    let isBool: boolean = (typeof a === "boolean" && typeof b === "boolean");
    let isNumber: boolean = (typeof a === "number" && typeof b === "number");

    if (a instanceof Array && b instanceof Array) {
        for (let i = 0; i < a.length; i++) {
            if (!deepCheck(a[i], b[i])) {
                return false;
            }
        }
        return true;
    } else if (isString || isBool || isNumber) {
        return a === b;
    } else if (typeof a === "object" && typeof b === "object") {
        for (let key in a) {
            if (!deepCheck(a[key], b[key])) {
                return false;
            }
        }
        return true;
    } else {
        return a === b;
    }
}

var petApi = new api.PetApi();
petApi.setApiKey(api.PetApiApiKeys.api_key, 'special-key');
// petApi.setApiKey(api.PetApiApiKeys.test_api_key_header, 'query-key');

var tag1 = new api.Tag();
tag1.id = 18291;
tag1.name = 'TS tag 1';

var pet = new api.Pet();
pet.name = 'TypeScriptDoggie';
pet.id = 18291;
pet.photoUrls = ["http://url1", "http://url2"];
pet.tags = [tag1];

var petId: any;

var exitCode = 0;

// Test Object Serializer
var rewire = require("rewire");
var rewiredApi = rewire("./api");
var objectSerializer = rewiredApi.__get__("ObjectSerializer");
console.log("Checking deserialization.");
var serializedPet = {
                        "id": pet.id,
                        "category": {
                                        "id": 18291,
                                        "name": "TS category 1"
                                    },
                        "name": pet.name,
                        "photoUrls": pet.photoUrls,
                        "tags": [
                                    {
                                        "id": 18291,
                                        "name": "TS tag 1"
                                    }
                                ],
                        "status": "available"
                    };
var deserializedPet = objectSerializer.deserialize(serializedPet, "Pet");
// Check types
var petType: boolean = deserializedPet instanceof rewiredApi.Pet;
var tagType1: boolean = deserializedPet.tags[0] instanceof rewiredApi.Tag;
var categoryType: boolean = deserializedPet.category instanceof rewiredApi.Category;

let checks = {};
for (let key in deserializedPet) {
    checks[key] = {};
    checks[key]["isCorrect"] = deepCheck(deserializedPet[key], serializedPet[key]);
    checks[key]["is"] = deserializedPet[key];
    checks[key]["should"] = serializedPet[key];
}
var correctTypes: boolean = petType && tagType1 && categoryType;

if (!correctTypes) {
    exitCode = 1;
    console.log("PetType correct: ", petType);
    console.log("TagType1 correct: ", tagType1);
    console.log("CategoryType correct: ", categoryType);
}

for (let key in checks) {
    let check = checks[key];
    if (!check["isCorrect"]) {
        exitCode = 1;
        console.log(key, " incorrect ","\nis:\n ",
                                    check["is"], "\nshould:\n ", check["should"]);
    }
}

console.log("Checking serialization");
// set a category and status for the pet (will be removed after the serialization
// check)
var category = new api.Category();
category.id = 18291;
category.name = "TS category 1";
pet.category = category;
pet.status = api.Pet.StatusEnum.Available;

var reserializedData = objectSerializer.serialize(pet, "Pet");
if (!deepCheck(reserializedData, serializedPet)) {
    exitCode = 1;
    console.log("Reserialized Data incorrect! \nis:\n ", reserializedData,
                                                "\nshould:\n ", serializedPet);
}

// category and status are not used in the tests below.
pet.category = undefined;
pet.status = undefined;

// Test various API calls to the petstore
petApi.addPet(pet)
    .then((res) => {
        var newPet = <api.Pet>res.body;
        petId = newPet.id;
        console.log(`Created pet with ID ${petId}`);
        newPet.status = api.Pet.StatusEnum.Available;
        return petApi.updatePet(newPet);
    })
    .then((res) => {
        console.log('Updated pet using POST body');
        return petApi.updatePetWithForm(petId, undefined, "pending");
    })
    .then((res) => {
        console.log('Updated pet using POST form');
        return petApi.uploadFile(petId, undefined, fs.readFileSync('sample.png'));
    })
    .then((res) => {
        console.log('Uploaded image');
        return petApi.getPetById(petId);
    })
    .then((res) => {
        console.log('Got pet by ID: ' + JSON.stringify(res.body));
    console.log("EnumValue: ", api.Pet.StatusEnum.Pending);
    console.log("Typeof EnumValue:", typeof api.Pet.StatusEnum.Pending);
    console.log("Res:", res.body.status);
        if (res.body.status != api.Pet.StatusEnum.Pending) {
            throw new Error("Unexpected pet status");
        }
    })
    .catch((err: any) => {
        console.error(err);
        exitCode = 1;
    })
    .then(() => {
        return petApi.deletePet(petId);
    })
    .then((res) => {
        console.log('Deleted pet');
        process.exit(exitCode);
    });
