library tests;

import 'dart:async';
import 'package:http/http.dart';
import 'package:guinness/guinness.dart';
import 'package:swagger/api.dart';

part 'pet_test.dart';
part 'store_test.dart';
part 'user_test.dart';

main() {
  testPetApi();
  testStoreApi();
  testUserApi();
}