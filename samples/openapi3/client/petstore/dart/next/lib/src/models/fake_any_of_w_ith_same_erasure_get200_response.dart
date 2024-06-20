// Model def

import 'dart:convert';
import 'dart:typed_data';
import 'package:openapi/_internal.dart';
import 'package:xml/xml.dart';

part 'fake_any_of_w_ith_same_erasure_get200_response.reflection.dart';
part 'fake_any_of_w_ith_same_erasure_get200_response.serialization.dart';

//class defination

///
mixin FakeAnyOfWIthSameErasureGet200ResponseMixin on $OpenApiObjectMixin {
  UndefinedWrapper<List<String>> get anyOf0;

  UndefinedWrapper<List<int>> get anyOf1;
}

///
class FakeAnyOfWIthSameErasureGet200Response
    with $OpenApiObjectMixin, FakeAnyOfWIthSameErasureGet200ResponseMixin {
  @override
  UndefinedWrapper<List<String>> anyOf0;

  @override
  UndefinedWrapper<List<int>> anyOf1;

  FakeAnyOfWIthSameErasureGet200Response.$all({
    required this.anyOf0,
    required this.anyOf1,
  });

  FakeAnyOfWIthSameErasureGet200Response({
    this.anyOf0 = const UndefinedWrapper.undefined(),
    this.anyOf1 = const UndefinedWrapper.undefined(),
  });
}
