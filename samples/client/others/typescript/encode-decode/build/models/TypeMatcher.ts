/**
 * Validates if data contains all required attributes from the attributeTypeMap.
 *
 * @param data - The data object to validate
 * @param attributeTypeMap - Array of attribute metadata including required flag
 * @returns true if all required attributes are present in data, false otherwise
 */
export function instanceOfType(data: any, attributeTypeMap: Array<{name: string, baseName: string, type: string, format: string, required: boolean}>): boolean {
    for (const attribute of attributeTypeMap) {
        if (attribute.required) {
            // Check both that the property exists AND that it's not undefined.
            // This is important because `data[attribute.baseName] === undefined` alone
            // would be true for both missing properties and properties explicitly set to undefined,
            // while `!(attribute.baseName in data)` distinguishes between these cases.
            // For proper OpenAPI validation, required fields must actually be present in the data.
            if (!(attribute.baseName in data) || data[attribute.baseName] === undefined) {
                return false;
            }
        }
    }

    return true;
}

/**
 * Attempts to find a matching type from an array of possible types by validating
 * the data against each type's attribute requirements.
 *
 * @param data - The data object to match
 * @param types - Array of possible type constructors
 * @returns The name of the matching type, or undefined if no match found
 */
export function findMatchingType(data: any, types: Array<any>): string | undefined {
    for (const type of types) {
        if (instanceOfType(data, type.getAttributeTypeMap())) {
            return type.name;
        }
    }

    return undefined;
}