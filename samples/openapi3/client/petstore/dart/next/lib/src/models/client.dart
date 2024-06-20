// Model def

import 'dart:convert';
import 'dart:typed_data';
import 'package:openapi/_internal.dart';
import 'package:xml/xml.dart';

part 'client.reflection.dart';
part 'client.serialization.dart';

//class defination

///
mixin ClientMixin on $OpenApiObjectMixin {
  UndefinedWrapper<String> get client;
}

///
class Client with $OpenApiObjectMixin, ClientMixin {
  @override
  UndefinedWrapper<String> client;

  Client.$all({
    required this.client,
  });

  Client({
    this.client = const UndefinedWrapper.undefined(),
  });
}
