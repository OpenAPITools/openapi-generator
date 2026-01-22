import { all, fork } from "redux-saga/effects";

import {
    behaviorApiAllSagas,
} from "./BehaviorApi";
import {
    petApiAllSagas,
} from "./PetApi";
import {
    petPartApiAllSagas,
} from "./PetPartApi";
import {
    storeApiAllSagas,
} from "./StoreApi";
import {
    userApiAllSagas,
} from "./UserApi";

export function *allApiSagas() {
    yield all([
        fork(behaviorApiAllSagas),
        fork(petApiAllSagas),
        fork(petPartApiAllSagas),
        fork(storeApiAllSagas),
        fork(userApiAllSagas),
    ]);
}
