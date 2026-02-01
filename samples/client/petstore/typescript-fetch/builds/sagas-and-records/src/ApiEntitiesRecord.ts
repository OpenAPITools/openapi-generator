import { Map, Record, RecordOf } from 'immutable';

import {
    CategoryRecordEntity,
} from "./models/CategoryRecord"
import {
    OrderRecordEntity,
} from "./models/OrderRecord"
import {
    PetRecordEntity,
} from "./models/PetRecord"
import {
    TagRecordEntity,
} from "./models/TagRecord"
import {
    UserRecordEntity,
} from "./models/UserRecord"

export const ApiEntitiesRecordProps = {
    recType: "ApiEntitiesRecord" as "ApiEntitiesRecord",
    category: (CategoryRecordEntity(), Map<string, CategoryRecordEntity>()),
    order: (OrderRecordEntity(), Map<string, OrderRecordEntity>()),
    pet: (PetRecordEntity(), Map<string, PetRecordEntity>()),
    tag: (TagRecordEntity(), Map<string, TagRecordEntity>()),
    user: (UserRecordEntity(), Map<string, UserRecordEntity>()),
};

export type ApiEntitiesRecordPropsType = typeof ApiEntitiesRecordProps;
export const ApiEntitiesRecord = Record(ApiEntitiesRecordProps, ApiEntitiesRecordProps.recType);
export type ApiEntitiesRecord = RecordOf<ApiEntitiesRecordPropsType>;
