"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var ApiResponse = (function () {
    function ApiResponse() {
    }
    ApiResponse.getAttributeTypeMap = function () {
        return ApiResponse.attributeTypeMap;
    };
    ApiResponse.discriminator = undefined;
    ApiResponse.attributeTypeMap = [
        {
            "name": "code",
            "baseName": "code",
            "type": "number"
        },
        {
            "name": "type",
            "baseName": "type",
            "type": "string"
        },
        {
            "name": "message",
            "baseName": "message",
            "type": "string"
        }
    ];
    return ApiResponse;
}());
exports.ApiResponse = ApiResponse;
//# sourceMappingURL=ApiResponse.js.map