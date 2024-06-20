// Model def

import 'dart:convert';
import 'dart:typed_data';
import 'package:openapi/_internal.dart';
import 'package:xml/xml.dart';

part '__return.reflection.dart';
part '__return.serialization.dart';

//class defination

///
mixin $ReturnMixin on $OpenApiObjectMixin {
  UndefinedWrapper<int> get $return;
}

///
class $Return with $OpenApiObjectMixin, $ReturnMixin {
  @override
  UndefinedWrapper<int> $return;

  $Return.$all({
    required this.$return,
  });

  $Return({
    this.$return = const UndefinedWrapper.undefined(),
  });
}
