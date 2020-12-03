import {all, fork} from "redux-saga/effects";

import {
    petApiAllSagas,
    storeApiAllSagas,
    userApiAllSagas,
} from "./";

export function *allApiSagas() {
    yield all([
        fork(petApiAllSagas),
        fork(storeApiAllSagas),
        fork(userApiAllSagas),
    ]);
}
