import { Category } from './Category';
import { Tag } from './Tag';
export declare class Pet {
    'id'?: number;
    'category'?: Category;
    'name': string;
    'photoUrls': Array<string>;
    'tags'?: Array<Tag>;
    'status'?: Pet.StatusEnum;
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
export declare namespace Pet {
    enum StatusEnum {
        Available,
        Pending,
        Sold
    }
}
