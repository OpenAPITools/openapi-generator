// Model def

import 'dart:convert';
import 'dart:typed_data';
import 'package:openapi/_internal.dart';
import 'package:xml/xml.dart';

part 'array_test.reflection.dart';
part 'array_test.serialization.dart';

//class defination

///
mixin ArrayTestMixin on $OpenApiObjectMixin {
  UndefinedWrapper<List<String>> get arrayOfString;
  UndefinedWrapper<List<List<int>>> get arrayArrayOfInteger;
  UndefinedWrapper<List<List<ReadOnlyFirst>>> get arrayArrayOfModel;
}

///
class ArrayTest with $OpenApiObjectMixin, ArrayTestMixin {
  @override
  UndefinedWrapper<List<String>> arrayOfString;
  @override
  UndefinedWrapper<List<List<int>>> arrayArrayOfInteger;
  @override
  UndefinedWrapper<List<List<ReadOnlyFirst>>> arrayArrayOfModel;

  ArrayTest.$all({
    required this.arrayOfString,
    required this.arrayArrayOfInteger,
    required this.arrayArrayOfModel,
  });

  ArrayTest({
    this.arrayOfString = const UndefinedWrapper.undefined(),
    this.arrayArrayOfInteger = const UndefinedWrapper.undefined(),
    this.arrayArrayOfModel = const UndefinedWrapper.undefined(),
  });
}
