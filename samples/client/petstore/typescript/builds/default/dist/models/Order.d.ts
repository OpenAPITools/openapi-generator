export declare class Order {
    'id'?: number;
    'petId'?: number;
    'quantity'?: number;
    'shipDate'?: Date;
    'status'?: Order.StatusEnum;
    'complete'?: boolean;
    static discriminator: string | undefined;
    static attributeTypeMap: Array<{
        name: string;
        baseName: string;
        type: string;
    }>;
    static getAttributeTypeMap(): {
        name: string;
        baseName: string;
        type: string;
    }[];
}
export declare namespace Order {
    enum StatusEnum {
        Placed,
        Approved,
        Delivered
    }
}
