export * from './apiResponse';
export * from './category';
export * from './order';
export * from './pet';
export * from './tag';
export * from './user';

import * as fs from 'fs';

export interface RequestDetailedFile {
    value: Buffer;
    options?: {
        filename?: string;
        contentType?: string;
    }
}

export type RequestFile = string | Buffer | fs.ReadStream | RequestDetailedFile;

