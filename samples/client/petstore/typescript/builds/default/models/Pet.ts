/*
	TODO: LICENSE INFO
*/
import { Category } from './Category';
import { Tag } from './Tag';

/**
* A pet for sale in the pet store
*/
export class Pet {
    'id'?: number;
    'category'?: Category;
    'name': string;
    'photoUrls': Array<string>;
    'tags'?: Array<Tag>;
    /**
    * pet status in the store
    */
    'status'?: PetStatusEnum;

    static readonly discriminator: string | undefined = undefined;

    static readonly attributeTypeMap: Array<{name: string, baseName: string, type: string, format: string}> = [
        {
            "name": "id",
            "baseName": "id",
            "type": "number",
            "format": "int64"
        },
        {
            "name": "category",
            "baseName": "category",
            "type": "Category",
            "format": ""
        },
        {
            "name": "name",
            "baseName": "name",
            "type": "string",
            "format": ""
        },
        {
            "name": "photoUrls",
            "baseName": "photoUrls",
            "type": "Array<string>",
            "format": ""
        },
        {
            "name": "tags",
            "baseName": "tags",
            "type": "Array<Tag>",
            "format": ""
        },
        {
            "name": "status",
            "baseName": "status",
            "type": "PetStatusEnum",
            "format": ""
        }    ];

    static getAttributeTypeMap() {
        return Pet.attributeTypeMap;
    }
    
    public constructor() {
    }
}


export type PetStatusEnum = "available" | "pending" | "sold" ;

