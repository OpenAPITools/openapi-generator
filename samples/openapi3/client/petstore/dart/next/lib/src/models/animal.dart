// Model def

import 'dart:convert';
import 'dart:typed_data';
import 'package:openapi/_internal.dart';
import 'package:xml/xml.dart';

part 'animal.reflection.dart';
part 'animal.serialization.dart';

//class defination

///
mixin AnimalMixin on $OpenApiObjectMixin {
  String get className;
  UndefinedWrapper<String> get color;
}

///
class Animal with $OpenApiObjectMixin, AnimalMixin {
  @override
  String className;
  @override
  UndefinedWrapper<String> color;

  Animal.$all({
    required this.className,
    required this.color,
  });

  Animal({
    required this.className,
    this.color = const UndefinedWrapper('red'),
  });
}
