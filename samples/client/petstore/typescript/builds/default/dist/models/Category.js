"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var Category = (function () {
    function Category() {
    }
    Category.getAttributeTypeMap = function () {
        return Category.attributeTypeMap;
    };
    Category.discriminator = undefined;
    Category.attributeTypeMap = [
        {
            "name": "id",
            "baseName": "id",
            "type": "number"
        },
        {
            "name": "name",
            "baseName": "name",
            "type": "string"
        }
    ];
    return Category;
}());
exports.Category = Category;
//# sourceMappingURL=Category.js.map