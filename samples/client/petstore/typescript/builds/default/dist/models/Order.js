"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var Order = (function () {
    function Order() {
    }
    Order.getAttributeTypeMap = function () {
        return Order.attributeTypeMap;
    };
    Order.discriminator = undefined;
    Order.attributeTypeMap = [
        {
            "name": "id",
            "baseName": "id",
            "type": "number"
        },
        {
            "name": "petId",
            "baseName": "petId",
            "type": "number"
        },
        {
            "name": "quantity",
            "baseName": "quantity",
            "type": "number"
        },
        {
            "name": "shipDate",
            "baseName": "shipDate",
            "type": "Date"
        },
        {
            "name": "status",
            "baseName": "status",
            "type": "Order.StatusEnum"
        },
        {
            "name": "complete",
            "baseName": "complete",
            "type": "boolean"
        }
    ];
    return Order;
}());
exports.Order = Order;
(function (Order) {
    var StatusEnum;
    (function (StatusEnum) {
        StatusEnum[StatusEnum["Placed"] = 'placed'] = "Placed";
        StatusEnum[StatusEnum["Approved"] = 'approved'] = "Approved";
        StatusEnum[StatusEnum["Delivered"] = 'delivered'] = "Delivered";
    })(StatusEnum = Order.StatusEnum || (Order.StatusEnum = {}));
})(Order = exports.Order || (exports.Order = {}));
exports.Order = Order;
//# sourceMappingURL=Order.js.map