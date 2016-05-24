'use strict';
import * as models from './models';

export interface ProjectEntity {
    

    id?: number;

    kind?: ProjectEntity.KindEnum;

    thumbnailUrl?: string;

    name?: string;

    state?: string;

    meta?: any;

    location?: models.ProjectEntityLocation;

    createdAt?: Date;

    updatedAt?: Date;

    publishedAt?: Date;
}
export namespace ProjectEntity {

    export enum KindEnum { 
        project = <any> 'project',
    }
}
