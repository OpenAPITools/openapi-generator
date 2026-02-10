import { AttributeTypeMapEntry } from '../models/ModelTypes';

/**
 * Validates if data contains all required attributes from the attributeTypeMap.
 *
 * @param data - The data object to validate
 * @param attributeTypeMap - Array of attribute metadata including required flag
 * @returns true if all required attributes are present in data, false otherwise
 */
export function instanceOfType(data: any, attributeTypeMap: Array<AttributeTypeMapEntry>): boolean {
    for (const attribute of attributeTypeMap) {
        if (attribute.required && data[attribute.baseName] === undefined) {
            return false;
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