export * from './tag1.service';
import { Tag1Service } from './tag1.service';
export * from './tag2.service';
import { Tag2Service } from './tag2.service';
export const APIS = [Tag1Service, Tag2Service];
