"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var Tag = (function () {
    function Tag() {
    }
    Tag.getAttributeTypeMap = function () {
        return Tag.attributeTypeMap;
    };
    Tag.discriminator = undefined;
    Tag.attributeTypeMap = [
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
    return Tag;
}());
exports.Tag = Tag;
//# sourceMappingURL=Tag.js.map