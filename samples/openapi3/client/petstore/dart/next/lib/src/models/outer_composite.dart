// Model def

import 'dart:convert';
import 'dart:typed_data';
import 'package:openapi/_internal.dart';
import 'package:xml/xml.dart';

part 'outer_composite.reflection.dart';
part 'outer_composite.serialization.dart';

//class defination

///
mixin OuterCompositeMixin on $OpenApiObjectMixin {
  UndefinedWrapper<num> get myNumber;
  UndefinedWrapper<String> get myString;
  UndefinedWrapper<bool> get myBoolean;
}

///
class OuterComposite with $OpenApiObjectMixin, OuterCompositeMixin {
  @override
  UndefinedWrapper<num> myNumber;
  @override
  UndefinedWrapper<String> myString;
  @override
  UndefinedWrapper<bool> myBoolean;

  OuterComposite.$all({
    required this.myNumber,
    required this.myString,
    required this.myBoolean,
  });

  OuterComposite({
    this.myNumber = const UndefinedWrapper.undefined(),
    this.myString = const UndefinedWrapper.undefined(),
    this.myBoolean = const UndefinedWrapper.undefined(),
  });
}
