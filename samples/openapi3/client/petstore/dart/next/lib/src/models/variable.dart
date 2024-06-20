// Model def

import 'dart:convert';
import 'dart:typed_data';
import 'package:openapi/_internal.dart';
import 'package:xml/xml.dart';

part 'variable.reflection.dart';
part 'variable.serialization.dart';

//class defination

///
mixin VariableMixin on $OpenApiObjectMixin {
  String get name;
  Value get value;
}

///
class Variable with $OpenApiObjectMixin, VariableMixin {
  @override
  String name;
  @override
  Value value;

  Variable.$all({
    required this.name,
    required this.value,
  });

  Variable({
    required this.name,
    required this.value,
  });
}
