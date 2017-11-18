"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var api = require("./api");
var fs = require("fs");
function deepCheck(objectA, objectB) {
    var a = objectA;
    var b = objectB;
    var isString = (typeof a === "string" && typeof b === "string");
    var isBool = (typeof a === "boolean" && typeof b === "boolean");
    var isNumber = (typeof a === "number" && typeof b === "number");
    if (a instanceof Array && b instanceof Array) {
        for (var i = 0; i < a.length; i++) {
            if (!deepCheck(a[i], b[i])) {
                return false;
            }
        }
        return true;
    }
    else if (isString || isBool || isNumber) {
        return a === b;
    }
    else if (typeof a === "object" && typeof b === "object") {
        for (var key in a) {
            if (!deepCheck(a[key], b[key])) {
                return false;
            }
        }
        return true;
    }
    else {
        return a === b;
    }
}
var petApi = new api.PetApi();
petApi.setApiKey(api.PetApiApiKeys.api_key, 'special-key');
var tag1 = new api.Tag();
tag1.id = 18291;
tag1.name = 'TS tag 1';
var pet = new api.Pet();
pet.name = 'TypeScriptDoggie';
pet.id = 18291;
pet.photoUrls = ["http://url1", "http://url2"];
pet.tags = [tag1];
var petId;
var exitCode = 0;
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
var petType = deserializedPet instanceof rewiredApi.Pet;
var tagType1 = deserializedPet.tags[0] instanceof rewiredApi.Tag;
var categoryType = deserializedPet.category instanceof rewiredApi.Category;
var checks = {};
for (var key in deserializedPet) {
    checks[key] = {};
    checks[key]["isCorrect"] = deepCheck(deserializedPet[key], serializedPet[key]);
    checks[key]["is"] = deserializedPet[key];
    checks[key]["should"] = serializedPet[key];
}
var correctTypes = petType && tagType1 && categoryType;
if (!correctTypes) {
    exitCode = 1;
    console.log("PetType correct: ", petType);
    console.log("TagType1 correct: ", tagType1);
    console.log("CategoryType correct: ", categoryType);
}
for (var key in checks) {
    var check = checks[key];
    if (!check["isCorrect"]) {
        exitCode = 1;
        console.log(key, " incorrect ", "\nis:\n ", check["is"], "\nshould:\n ", check["should"]);
    }
}
console.log("Checking serialization");
var category = new api.Category();
category.id = 18291;
category.name = "TS category 1";
pet.category = category;
pet.status = api.Pet.StatusEnum.Available;
var reserializedData = objectSerializer.serialize(pet, "Pet");
if (!deepCheck(reserializedData, serializedPet)) {
    exitCode = 1;
    console.log("Reserialized Data incorrect! \nis:\n ", reserializedData, "\nshould:\n ", serializedPet);
}
petApi.addPet(pet)
    .then(function (res) {
    var newPet = res.body;
    petId = newPet.id;
    console.log("Created pet with ID " + petId);
    newPet.status = api.Pet.StatusEnum.Available;
    return petApi.updatePet(newPet);
})
    .then(function (res) {
    console.log('Updated pet using POST body');
    return petApi.updatePetWithForm(petId, undefined, "pending");
})
    .then(function (res) {
    console.log('Updated pet using POST form');
    return petApi.uploadFile(petId, undefined, fs.readFileSync('sample.png'));
})
    .then(function (res) {
    console.log('Uploaded image');
    return petApi.getPetById(petId);
})
    .then(function (res) {
    console.log('Got pet by ID: ' + JSON.stringify(res.body));
    console.log("EnumValue: ", api.Pet.StatusEnum.Pending);
    console.log("Typeof EnumValue:", typeof api.Pet.StatusEnum.Pending);
    console.log("Res:", res.body.status);
    if (res.body.status != api.Pet.StatusEnum.Pending) {
        throw new Error("Unexpected pet status");
    }
})
    .catch(function (err) {
    console.error(err);
    exitCode = 1;
})
    .then(function () {
    return petApi.deletePet(petId);
})
    .then(function (res) {
    console.log('Deleted pet');
    process.exit(exitCode);
});
//# sourceMappingURL=client.js.map