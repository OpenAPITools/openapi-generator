export class OneOfClass {
    public static instanceOf(data: any, attributeTypeMap: Array<{name: string, baseName: string, type: string, format: string, required: boolean}>): boolean {
        for (const attribute of attributeTypeMap) {
            if (attribute.required) {
                if (!(attribute.baseName in data) || data[attribute.baseName] === undefined) {
                    return false;
                }
            }
        }

        return true;
    }
}