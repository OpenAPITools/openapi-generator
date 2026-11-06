import { Category } from './Category.js';
import { Tag } from './Tag.js';

export interface Pet {
    id: number,
    category: Category,
    name: string,
    photoUrls: string[],
    tags: Tag[],
    status: string,
}
