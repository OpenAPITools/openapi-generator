"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var rewire = require("rewire");
var chai_1 = require("chai");
var ts_petstore_client_1 = require("ts-petstore-client");
var objectSerializerFile = rewire("../../node_modules/ts-petstore-client/dist/models/ObjectSerializer.js");
var ObjectSerializer = objectSerializerFile.__get__("ObjectSerializer");
describe("ObjectSerializer", function () {
    describe("Serialize", function () {
        it("String", function () {
            var input = "test string";
            chai_1.expect(ObjectSerializer.serialize(input, "string")).to.equal("test string");
        });
        it("Number", function () {
            var input = 1337;
            chai_1.expect(ObjectSerializer.serialize(input, "number")).to.equal(1337);
        });
        it("String Array", function () {
            var input = ["a", "b", "c"];
            chai_1.expect(ObjectSerializer.serialize(input, "Array<string>")).to.deep.equal(["a", "b", "c"]);
        });
        it("Number Array", function () {
            var input = [1337, 42, 0];
            chai_1.expect(ObjectSerializer.serialize(input, "Array<number>")).to.deep.equal([1337, 42, 0]);
        });
        it("Date", function () {
            var input = new Date(1543777609696);
            chai_1.expect(ObjectSerializer.serialize(input, "Date")).to.equal(input.toISOString());
        });
        it("Object", function () {
            var input = { "a": "test", "b": { "test": 5 } };
            chai_1.expect(ObjectSerializer.serialize(input, "Object")).to.deep.equal({ a: "test", "b": { "test": 5 } });
        });
        it("Class", function () {
            var input = new ts_petstore_client_1.Category();
            input.id = 4;
            input.name = "Test";
            chai_1.expect(ObjectSerializer.serialize(input, "Category")).to.deep.equal({ "id": input.id, "name": input.name });
        });
        it("Enum", function () {
            var input = ts_petstore_client_1.Pet.StatusEnum.Available;
            chai_1.expect(ObjectSerializer.serialize(input, "Pet.StatusEnum")).to.equal("available");
        });
        it("Complex Class", function () {
            var tags = [];
            var tagResult = [];
            for (var i = 0; i < 2; i++) {
                var tag = new ts_petstore_client_1.Tag();
                tag.id = i;
                tag.name = "Tag" + i;
                tags.push(tag);
                tagResult.push({
                    "id": tag.id,
                    "name": tag.name
                });
            }
            var category = new ts_petstore_client_1.Category();
            category.id = 4;
            category.name = "TestCat";
            var pet = new ts_petstore_client_1.Pet();
            pet.id = 145;
            pet.category = category;
            pet.name = "PetName";
            pet.photoUrls = ["url", "other url"];
            pet.status = ts_petstore_client_1.Pet.StatusEnum.Available;
            pet.tags = tags;
            chai_1.expect(ObjectSerializer.serialize(pet, "Pet")).to.deep.equal({
                "id": pet.id,
                "name": pet.name,
                "category": {
                    "id": category.id,
                    "name": category.name
                },
                "photoUrls": ["url", "other url"],
                "status": "available",
                "tags": tagResult
            });
        });
        it("Array of Class", function () {
            var categories = [];
            var result = [];
            for (var i = 0; i < 2; i++) {
                var category = new ts_petstore_client_1.Category();
                category.id = i;
                category.name = "Cat" + i;
                categories.push(category);
                result.push({
                    "id": category.id,
                    "name": category.name
                });
            }
            chai_1.expect(ObjectSerializer.serialize(categories, "Array<Category>")).to.deep.equal(result);
        });
    });
    describe("Deserialize", function () {
        it("String", function () {
            var input = "test string";
            chai_1.expect(ObjectSerializer.deserialize(input, "string")).to.equal("test string");
        });
        it("Number", function () {
            var input = 1337;
            chai_1.expect(ObjectSerializer.deserialize(input, "number")).to.equal(1337);
        });
        it("String Array", function () {
            var input = ["a", "b", "c"];
            chai_1.expect(ObjectSerializer.deserialize(input, "Array<string>")).to.deep.equal(["a", "b", "c"]);
        });
        it("Number Array", function () {
            var input = [1337, 42, 0];
            chai_1.expect(ObjectSerializer.deserialize(input, "Array<number>")).to.deep.equal([1337, 42, 0]);
        });
        it("Date", function () {
            var input = new Date(1543777609696);
            chai_1.expect(ObjectSerializer.deserialize(input.toISOString(), "Date").getTime()).to.equal(input.getTime());
        });
        it("Object", function () {
            var input = { "a": "test", "b": { "test": 5 } };
            chai_1.expect(ObjectSerializer.deserialize(input, "Object")).to.deep.equal({ a: "test", "b": { "test": 5 } });
        });
        it("Class", function () {
            var input = new ts_petstore_client_1.Category();
            input.id = 4;
            input.name = "Test";
            var deserialized = ObjectSerializer.deserialize({ "id": 4, "name": "Test" }, "Category");
            chai_1.expect(deserialized.constructor.name).to.equal("Category");
            chai_1.expect(deserialized).to.deep.equal(input);
        });
        it("Enum", function () {
            var input = ts_petstore_client_1.Pet.StatusEnum.Available;
            chai_1.expect(ObjectSerializer.deserialize("available", "Pet.StatusEnum")).to.equal(input);
        });
        it("Complex Class", function () {
            var tags = [];
            var tagResult = [];
            for (var i = 0; i < 2; i++) {
                var tag = new ts_petstore_client_1.Tag();
                tag.id = i;
                tag.name = "Tag" + i;
                tags.push(tag);
                tagResult.push({
                    "id": tag.id,
                    "name": tag.name
                });
            }
            var category = new ts_petstore_client_1.Category();
            category.id = 4;
            category.name = "TestCat";
            var pet = new ts_petstore_client_1.Pet();
            pet.id = 145;
            pet.category = category;
            pet.name = "PetName";
            pet.photoUrls = ["url", "other url"];
            pet.status = ts_petstore_client_1.Pet.StatusEnum.Available;
            pet.tags = tags;
            var deserialized = ObjectSerializer.deserialize({
                "id": pet.id,
                "name": pet.name,
                "category": {
                    "id": category.id,
                    "name": category.name
                },
                "photoUrls": ["url", "other url"],
                "status": "available",
                "tags": tagResult
            }, "Pet");
            chai_1.expect(deserialized.constructor.name).to.equal("Pet");
            chai_1.expect(deserialized.category.constructor.name).to.equal("Category");
            for (var i = 0; i < deserialized.tags.length; i++) {
                chai_1.expect(deserialized.tags[i].constructor.name).to.equal("Tag");
            }
            chai_1.expect(deserialized).to.deep.equal(pet);
        });
        it("Array of Class", function () {
            var categories = [];
            var result = [];
            for (var i = 0; i < 2; i++) {
                var category = new ts_petstore_client_1.Category();
                category.id = i;
                category.name = "Cat" + i;
                categories.push(category);
                result.push({
                    "id": category.id,
                    "name": category.name
                });
            }
            var deserialized = ObjectSerializer.deserialize(result, "Array<Category>");
            for (var i = 0; i < categories.length; i++) {
                chai_1.expect(deserialized[i].constructor.name).to.equal("Category");
            }
            chai_1.expect(deserialized).to.deep.equal(categories);
        });
    });
});
