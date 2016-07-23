library tests;

import 'dart:async';
import 'dart:math';
import 'package:http/http.dart';
import 'package:guinness/guinness.dart';
import 'package:swagger/api.dart';

part 'pet_test.dart';
part 'store_test.dart';
part 'user_test.dart';

final random = new Random();

int newId() {
  return random.nextInt(999999);
}

main() {
  testPetApi();
  testStoreApi();
  testUserApi();
}
