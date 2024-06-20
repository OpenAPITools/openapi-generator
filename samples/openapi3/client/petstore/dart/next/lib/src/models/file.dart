// Model def

import 'dart:convert';
import 'dart:typed_data';
import 'package:openapi/_internal.dart';
import 'package:xml/xml.dart';

part 'file.reflection.dart';
part 'file.serialization.dart';

//class defination

///
mixin FileMixin on $OpenApiObjectMixin {
  UndefinedWrapper<String> get sourceURI;
}

///
class File with $OpenApiObjectMixin, FileMixin {
  @override
  UndefinedWrapper<String> sourceURI;

  File.$all({
    required this.sourceURI,
  });

  File({
    this.sourceURI = const UndefinedWrapper.undefined(),
  });
}
