const rewire = require("rewire")
import { expect} from "chai";
import * as petstore from "ts-petstore-client"


const objectSerializerFile = rewire(__dirname + "/../../node_modules/ts-petstore-client/models/ObjectSerializer.ts")

const ObjectSerializer = objectSerializerFile.__get__("ObjectSerializer")


describe("ObjectSerializer", () => {
    describe("Serialize", () => {
        it("String", () => {
            const input = "test string"
            expect(ObjectSerializer.serialize(input, "string", "")).to.equal("test string")
        });

        it("Number", () => {
            const input = 1337
            expect(ObjectSerializer.serialize(input, "number", "")).to.equal(1337)
        });

        it("String Array", () => {
            const input = ["a", "b", "c"]
            expect(ObjectSerializer.serialize(input, "Array<string>", "")).to.deep.equal(["a", "b", "c"])
        })

        it("Number Array", () => {
            const input = [ 1337, 42, 0]
            expect(ObjectSerializer.serialize(input, "Array<number>", "")).to.deep.equal([1337, 42, 0]) 
        })

        it("Date-Time", () => {
            const input = new Date(1543777609696)
            expect(ObjectSerializer.serialize(input, "Date", "date-time")).to.equal(input.toISOString())
        })

        it("Date-Time", () => {
            const input = new Date(1543777609696)
            expect(ObjectSerializer.serialize(input, "Date", "date")).to.equal("2018-12-02")
        })


        it("Object", () => {
            const input = {"a": "test", "b": { "test": 5}}
            expect(ObjectSerializer.serialize(input, "Object", "")).to.deep.equal({ a: "test", "b": { "test": 5}})
        })

        it("Class", () => {
            const input = new petstore.Category()
            input.id = 4
            input.name = "Test"
            expect(ObjectSerializer.serialize(input, "Category", "")).to.deep.equal({ "id": input.id, "name": input.name})
        });

        it ("Enum", () => {
            const input = "available"
            expect(ObjectSerializer.serialize(input, "Pet.StatusEnum", "")).to.equal("available")
        })

        it("Complex Class", () => {
            const tags = []
            const tagResult = []
            for (let i = 0; i < 2; i++) {
                const tag = new petstore.Tag()
                tag.id = i
                tag.name = "Tag" + i    
                tags.push(tag)
                tagResult.push({
                    "id": tag.id,
                    "name": tag.name
                })
            }
            
            const category = new petstore.Category()
            category.id = 4
            category.name = "TestCat"
            const pet = new petstore.Pet()
            pet.id = 145
            pet.category = category
            pet.name = "PetName"
            pet.photoUrls = [ "url", "other url"] 
            pet.status = "available"
            pet.tags = tags

            expect(ObjectSerializer.serialize(pet, "Pet", "")).to.deep.equal({
                "id": pet.id,
                "name": pet.name,
                "category": {
                    "id": category.id,
                    "name": category.name
                },
                "photoUrls": [ "url", "other url"],
                "status": "available",
                "tags": tagResult
            })
        })
        it("Array of Class", () => {
            const categories = []
            const result = []
            for (let i = 0; i < 2; i++) {
                const category = new petstore.Category()
                category.id = i
                category.name = "Cat" + i
                categories.push(category)
                result.push({
                    "id": category.id,
                    "name": category.name
                })
            }

            expect(ObjectSerializer.serialize(categories, "Array<Category>", "")).to.deep.equal(result)
        })
    })

    describe("Deserialize", () => {
        it("String", () => {
            const input = "test string"
            expect(ObjectSerializer.deserialize(input, "string", "")).to.equal("test string")
        });

        it("Number", () => {
            const input = 1337
            expect(ObjectSerializer.deserialize(input, "number", "")).to.equal(1337)
        });

        it("String Array", () => {
            const input = ["a", "b", "c"]
            expect(ObjectSerializer.deserialize(input, "Array<string>", "")).to.deep.equal(["a", "b", "c"])
        })

        it("Number Array", () => {
            const input = [ 1337, 42, 0]
            expect(ObjectSerializer.deserialize(input, "Array<number>", "")).to.deep.equal([1337, 42, 0]) 
        })

        it("DateTime", () => {
            const input = new Date(1543777609696)
            expect(ObjectSerializer.deserialize(input.toISOString(), "Date", "date-time").getTime()).to.equal(input.getTime())
        })

        it("Date", () => {
            let dateString = "2019-02-01"
            const input = new Date(dateString);
            expect(ObjectSerializer.deserialize(dateString, "Date", "date").getTime()).to.equal(input.getTime())
        })

        it("Object", () => {
            const input = {"a": "test", "b": { "test": 5}}
            expect(ObjectSerializer.deserialize(input, "Object", "")).to.deep.equal({ a: "test", "b": { "test": 5}})
        })

        it("Class", () => {
            const input = new petstore.Category()
            input.id = 4
            input.name = "Test"
            const deserialized = ObjectSerializer.deserialize({ "id": 4, "name": "Test"}, "Category")

            expect(deserialized.constructor.name).to.equal("Category")
            expect(deserialized).to.deep.equal(input)
        });

        it ("Enum", () => {
            const input = "available"
            expect(ObjectSerializer.deserialize("available", "Pet.StatusEnum", "")).to.equal(input)
        })

        it("Complex Class", () => {
            const tags = []
            const tagResult = []
            for (let i = 0; i < 2; i++) {
                const tag = new petstore.Tag()
                tag.id = i
                tag.name = "Tag" + i    
                tags.push(tag)
                tagResult.push({
                    "id": tag.id,
                    "name": tag.name
                })
            }
            
            const category = new petstore.Category()
            category.id = 4
            category.name = "TestCat"
            const pet = new petstore.Pet()
            pet.id = 145
            pet.category = category
            pet.name = "PetName"
            pet.photoUrls = [ "url", "other url"] 
            pet.status = "available"
            pet.tags = tags

            const deserialized = ObjectSerializer.deserialize({
                "id": pet.id,
                "name": pet.name,
                "category": {
                    "id": category.id,
                    "name": category.name
                },
                "photoUrls": [ "url", "other url"],
                "status": "available",
                "tags": tagResult
            }, "Pet", "") as petstore.Pet

            expect(deserialized.constructor.name).to.equal("Pet")
            expect(deserialized.category.constructor.name).to.equal("Category")
            for (let i = 0; i < deserialized.tags.length; i++){ 
                expect(deserialized.tags[i].constructor.name).to.equal("Tag")
            }
            expect(deserialized).to.deep.equal(pet)
        })

        it("Array of Class", () => {
            const categories = []
            const result = []
            for (let i = 0; i < 2; i++) {
                const category = new petstore.Category()
                category.id = i
                category.name = "Cat" + i
                categories.push(category)
                result.push({
                    "id": category.id,
                    "name": category.name
                })
            }

            const deserialized = ObjectSerializer.deserialize(result, "Array<Category>", "")
            for (let i = 0; i < categories.length; i++) {
                expect(deserialized[i].constructor.name).to.equal("Category")
            }
            expect(deserialized).to.deep.equal(categories)
        })        
    })
})
