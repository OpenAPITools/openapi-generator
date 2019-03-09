/*
	TODO: LICENSE INFO
*/

/**
* An order for a pets from the pet store
*/
export class Order {
    'id'?: number;
    'petId'?: number;
    'quantity'?: number;
    'shipDate'?: Date;
    /**
    * Order Status
    */
    'status'?: OrderStatusEnum;
    'complete'?: boolean;

    static readonly discriminator: string | undefined = undefined;

    static readonly attributeTypeMap: Array<{name: string, baseName: string, type: string, format: string}> = [
        {
            "name": "id",
            "baseName": "id",
            "type": "number",
            "format": "int64"
        },
        {
            "name": "petId",
            "baseName": "petId",
            "type": "number",
            "format": "int64"
        },
        {
            "name": "quantity",
            "baseName": "quantity",
            "type": "number",
            "format": "int32"
        },
        {
            "name": "shipDate",
            "baseName": "shipDate",
            "type": "Date",
            "format": "date-time"
        },
        {
            "name": "status",
            "baseName": "status",
            "type": "OrderStatusEnum",
            "format": ""
        },
        {
            "name": "complete",
            "baseName": "complete",
            "type": "boolean",
            "format": ""
        }    ];

    static getAttributeTypeMap() {
        return Order.attributeTypeMap;
    }
    
    public constructor() {
    }
}


export type OrderStatusEnum = "placed" | "approved" | "delivered" ;

