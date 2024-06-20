// Model def

import 'dart:convert';
import 'dart:typed_data';
import 'package:openapi/_internal.dart';
import 'package:xml/xml.dart';

part 'free_form_object_test_class.reflection.dart';
part 'free_form_object_test_class.serialization.dart';

//class defination

///
mixin FreeFormObjectTestClassMixin on $OpenApiObjectMixin {
  UndefinedWrapper<String> get name;
  UndefinedWrapper<FreeFormObjectTestClassProperties> get properties;
}

///
class FreeFormObjectTestClass
    with $OpenApiObjectMixin, FreeFormObjectTestClassMixin {
  @override
  UndefinedWrapper<String> name;
  @override
  UndefinedWrapper<FreeFormObjectTestClassProperties> properties;

  FreeFormObjectTestClass.$all({
    required this.name,
    required this.properties,
  });

  FreeFormObjectTestClass({
    this.name = const UndefinedWrapper.undefined(),
    this.properties = const UndefinedWrapper.undefined(),
  });
}
