// Model def

import 'dart:convert';
import 'dart:typed_data';
import 'package:openapi/_internal.dart';
import 'package:xml/xml.dart';

part 'gm_fruit.reflection.dart';
part 'gm_fruit.serialization.dart';

//class defination

///
mixin GmFruitMixin on $OpenApiObjectMixin {
  UndefinedWrapper<String> get color;

  UndefinedWrapper<Apple?> get anyOf0;

  UndefinedWrapper<Banana> get anyOf1;
}

///
class GmFruit with $OpenApiObjectMixin, GmFruitMixin {
  @override
  UndefinedWrapper<String> color;

  @override
  UndefinedWrapper<Apple?> anyOf0;

  @override
  UndefinedWrapper<Banana> anyOf1;

  GmFruit.$all({
    required this.color,
    required this.anyOf0,
    required this.anyOf1,
  });

  GmFruit({
    this.color = const UndefinedWrapper.undefined(),
    this.anyOf0 = const UndefinedWrapper.undefined(),
    this.anyOf1 = const UndefinedWrapper.undefined(),
  });
}
