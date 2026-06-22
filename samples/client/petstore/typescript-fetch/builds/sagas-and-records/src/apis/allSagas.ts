import { all, fork } from "redux-saga/effects";

import {
    behaviorApiAllSagas,
} from "./BehaviorApiSagas";
import {
    petApiAllSagas,
} from "./PetApiSagas";
import {
    petPartApiAllSagas,
} from "./PetPartApiSagas";
import {
    storeApiAllSagas,
} from "./StoreApiSagas";
import {
    userApiAllSagas,
} from "./UserApiSagas";

export function *allApiSagas() {
    yield all([
        fork(behaviorApiAllSagas),
        fork(petApiAllSagas),
        fork(petPartApiAllSagas),
        fork(storeApiAllSagas),
        fork(userApiAllSagas),
    ]);
}
