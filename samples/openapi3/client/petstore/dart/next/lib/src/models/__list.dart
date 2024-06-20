// Model def

import 'dart:convert';
import 'dart:typed_data';
import 'package:openapi/_internal.dart';
import 'package:xml/xml.dart';

part '__list.reflection.dart';
part '__list.serialization.dart';

//class defination

///
mixin $ListMixin on $OpenApiObjectMixin {
  UndefinedWrapper<String> get $123list;
}

///
class $List with $OpenApiObjectMixin, $ListMixin {
  @override
  UndefinedWrapper<String> $123list;

  $List.$all({
    required this.$123list,
  });

  $List({
    this.$123list = const UndefinedWrapper.undefined(),
  });
}
