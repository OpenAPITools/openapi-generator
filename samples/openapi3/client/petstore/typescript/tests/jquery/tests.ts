declare var QUnit: any;

QUnit.test( "hello test", function( assert: any ) {
    let random = Math.random()
    assert.ok( random.toString() == "" + random, "Passed!" );
});

import './test/http/jquery.test'
import './test/api/PetApi.test'