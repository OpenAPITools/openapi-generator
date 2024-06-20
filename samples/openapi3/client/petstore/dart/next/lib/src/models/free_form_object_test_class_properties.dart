// Model def

import 'dart:convert';
import 'dart:typed_data';
import 'package:openapi/_internal.dart';
import 'package:xml/xml.dart';

part 'free_form_object_test_class_properties.reflection.dart';
part 'free_form_object_test_class_properties.serialization.dart';

//class defination

///
mixin FreeFormObjectTestClassPropertiesMixin on $OpenApiObjectMixin {
  UndefinedWrapper<String> get oneOf0;

  UndefinedWrapper<Map<String, Object?>> get oneOf1;
}

///
class FreeFormObjectTestClassProperties
    with $OpenApiObjectMixin, FreeFormObjectTestClassPropertiesMixin {
  @override
  UndefinedWrapper<String> oneOf0;

  @override
  UndefinedWrapper<Map<String, Object?>> oneOf1;

  FreeFormObjectTestClassProperties.$all({
    required this.oneOf0,
    required this.oneOf1,
  });

  FreeFormObjectTestClassProperties({
    this.oneOf0 = const UndefinedWrapper.undefined(),
    this.oneOf1 = const UndefinedWrapper.undefined(),
  });
}
