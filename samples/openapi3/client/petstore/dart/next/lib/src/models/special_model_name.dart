// Model def

import 'dart:convert';
import 'dart:typed_data';
import 'package:openapi/_internal.dart';
import 'package:xml/xml.dart';

part 'special_model_name.reflection.dart';
part 'special_model_name.serialization.dart';

//class defination

///
mixin SpecialModelNameMixin on $OpenApiObjectMixin {
  UndefinedWrapper<int> get $specialPropertyName;
  UndefinedWrapper<String> get specialModelName;
}

///
class SpecialModelName with $OpenApiObjectMixin, SpecialModelNameMixin {
  @override
  UndefinedWrapper<int> $specialPropertyName;
  @override
  UndefinedWrapper<String> specialModelName;

  SpecialModelName.$all({
    required this.$specialPropertyName,
    required this.specialModelName,
  });

  SpecialModelName({
    this.$specialPropertyName = const UndefinedWrapper.undefined(),
    this.specialModelName = const UndefinedWrapper.undefined(),
  });
}
