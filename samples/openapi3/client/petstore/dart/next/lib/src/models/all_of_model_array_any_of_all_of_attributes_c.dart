// Model def

import 'dart:convert';
import 'dart:typed_data';
import 'package:openapi/_internal.dart';
import 'package:xml/xml.dart';

part 'all_of_model_array_any_of_all_of_attributes_c.reflection.dart';
part 'all_of_model_array_any_of_all_of_attributes_c.serialization.dart';

//class defination

///
mixin AllOfModelArrayAnyOfAllOfAttributesCMixin on $OpenApiObjectMixin {
  UndefinedWrapper<Pet> get oneOf0;

  UndefinedWrapper<Order> get oneOf1;
}

///
class AllOfModelArrayAnyOfAllOfAttributesC
    with $OpenApiObjectMixin, AllOfModelArrayAnyOfAllOfAttributesCMixin {
  @override
  UndefinedWrapper<Pet> oneOf0;

  @override
  UndefinedWrapper<Order> oneOf1;

  AllOfModelArrayAnyOfAllOfAttributesC.$all({
    required this.oneOf0,
    required this.oneOf1,
  });

  AllOfModelArrayAnyOfAllOfAttributesC({
    this.oneOf0 = const UndefinedWrapper.undefined(),
    this.oneOf1 = const UndefinedWrapper.undefined(),
  });
}
