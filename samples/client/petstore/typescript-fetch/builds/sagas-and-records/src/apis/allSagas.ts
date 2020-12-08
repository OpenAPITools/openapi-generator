import {all, fork} from "redux-saga/effects";

import {
    affiliateApiAllSagas,
    brandApiAllSagas,
    petApiAllSagas,
    storeApiAllSagas,
    userApiAllSagas,
} from "./";

export function *allApiSagas() {
    yield all([
        fork(affiliateApiAllSagas),
        fork(brandApiAllSagas),
        fork(petApiAllSagas),
        fork(storeApiAllSagas),
        fork(userApiAllSagas),
    ]);
}
