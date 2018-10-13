"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var Pet = (function () {
    function Pet() {
    }
    Pet.getAttributeTypeMap = function () {
        return Pet.attributeTypeMap;
    };
    Pet.discriminator = undefined;
    Pet.attributeTypeMap = [
        {
            "name": "id",
            "baseName": "id",
            "type": "number"
        },
        {
            "name": "category",
            "baseName": "category",
            "type": "Category"
        },
        {
            "name": "name",
            "baseName": "name",
            "type": "string"
        },
        {
            "name": "photoUrls",
            "baseName": "photoUrls",
            "type": "Array<string>"
        },
        {
            "name": "tags",
            "baseName": "tags",
            "type": "Array<Tag>"
        },
        {
            "name": "status",
            "baseName": "status",
            "type": "Pet.StatusEnum"
        }
    ];
    return Pet;
}());
exports.Pet = Pet;
(function (Pet) {
    var StatusEnum;
    (function (StatusEnum) {
        StatusEnum[StatusEnum["Available"] = 'available'] = "Available";
        StatusEnum[StatusEnum["Pending"] = 'pending'] = "Pending";
        StatusEnum[StatusEnum["Sold"] = 'sold'] = "Sold";
    })(StatusEnum = Pet.StatusEnum || (Pet.StatusEnum = {}));
})(Pet = exports.Pet || (exports.Pet = {}));
exports.Pet = Pet;
//# sourceMappingURL=Pet.js.map