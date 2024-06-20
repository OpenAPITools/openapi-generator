// Model def

import 'dart:convert';
import 'dart:typed_data';
import 'package:openapi/_internal.dart';
import 'package:xml/xml.dart';

part 'array_of_inline_all_of_array_allof_dog_property_inner.reflection.dart';
part 'array_of_inline_all_of_array_allof_dog_property_inner.serialization.dart';

//class defination

///
mixin ArrayOfInlineAllOfArrayAllofDogPropertyInnerMixin on $OpenApiObjectMixin {
  UndefinedWrapper<String> get breed;
  UndefinedWrapper<String> get color;
}

///
class ArrayOfInlineAllOfArrayAllofDogPropertyInner
    with
        $OpenApiObjectMixin,
        ArrayOfInlineAllOfArrayAllofDogPropertyInnerMixin {
  @override
  UndefinedWrapper<String> breed;
  @override
  UndefinedWrapper<String> color;

  ArrayOfInlineAllOfArrayAllofDogPropertyInner.$all({
    required this.breed,
    required this.color,
  });

  ArrayOfInlineAllOfArrayAllofDogPropertyInner({
    this.breed = const UndefinedWrapper.undefined(),
    this.color = const UndefinedWrapper.undefined(),
  });
}
