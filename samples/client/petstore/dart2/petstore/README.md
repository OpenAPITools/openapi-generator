# To run these tests:

Simply start the dart server: `pub serve`

then open http://127.0.0.1:8080/tests.html


This already starts the tests.  There is _NO_ feedback!

Open the javascript / dart console of your browser to verify all tests 
passed successfully.

You should have the following output:
```
Observatory listening at http://127.0.0.1:39067/
unittest-suite-wait-for-done
GET http://petstore.swagger.io/v2/pet/957639 404 (Not Found)
GET http://petstore.swagger.io/v2/pet/525946 404 (Not Found)
GET http://petstore.swagger.io/v2/store/order/29756 404 (Not Found)
GET http://petstore.swagger.io/v2/user/Riddlem325 404 (Not Found)
PASS: Pet API  adds a new pet and gets it by id
PASS: Pet API  doesn't get non-existing pet by id
PASS: Pet API  deletes existing pet by id
PASS: Pet API  updates pet with form
PASS: Pet API  updates existing pet
PASS: Pet API  finds pets by status
PASS: Pet API  finds pets by tag
PASS: Pet API  uploads a pet image
PASS: Store API  places an order and gets it by id
PASS: Store API  deletes an order
PASS: Store API  gets the store inventory
PASS: User API  creates a user
PASS: User API  creates users with list input
PASS: User API  updates a user
PASS: User API  deletes a user
PASS: User API  logs a user in

All 16 tests passed.
unittest-suite-success
```


You may also run the tests in the dart vm.

Either generate the test-package for a vm:
- change bin/dart-petstore.sh and uncomment the vm options line
- run bin/dart-petstore.sh

or

- in `lib/api_client.dart` change `new BrowserClient()` to `new Client()`
- in `lib/api.dart` remove the line `import 'package:http/browser_client.dart';`



Then run `test/tests.dart`.

Have fun.