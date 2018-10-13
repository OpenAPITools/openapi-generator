export * from './ApiResponse';
export * from './Category';
export * from './Order';
export * from './Pet';
export * from './Tag';
export * from './User';
export declare class ObjectSerializer {
    static findCorrectType(data: any, expectedType: string): any;
    static serialize(data: any, type: string): any;
    static deserialize(data: any, type: string): any;
}
