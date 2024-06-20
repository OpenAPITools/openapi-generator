// Model def

import 'dart:convert';
import 'dart:typed_data';
import 'package:openapi/_internal.dart';
import 'package:xml/xml.dart';

part 'file_schema_test_class.reflection.dart';
part 'file_schema_test_class.serialization.dart';

//class defination

///
mixin FileSchemaTestClassMixin on $OpenApiObjectMixin {
  UndefinedWrapper<File> get file;
  UndefinedWrapper<List<File>> get files;
}

///
class FileSchemaTestClass with $OpenApiObjectMixin, FileSchemaTestClassMixin {
  @override
  UndefinedWrapper<File> file;
  @override
  UndefinedWrapper<List<File>> files;

  FileSchemaTestClass.$all({
    required this.file,
    required this.files,
  });

  FileSchemaTestClass({
    this.file = const UndefinedWrapper.undefined(),
    this.files = const UndefinedWrapper.undefined(),
  });
}
