// Model def

import 'dart:convert';
import 'dart:typed_data';
import 'package:openapi/_internal.dart';
import 'package:xml/xml.dart';

part 'banana_req.reflection.dart';
part 'banana_req.serialization.dart';

//class defination

///
mixin BananaReqMixin on $OpenApiObjectMixin {
  num get lengthCm;
  UndefinedWrapper<bool> get sweet;
}

///
class BananaReq with $OpenApiObjectMixin, BananaReqMixin {
  @override
  num lengthCm;
  @override
  UndefinedWrapper<bool> sweet;

  BananaReq.$all({
    required this.lengthCm,
    required this.sweet,
  });

  BananaReq({
    required this.lengthCm,
    this.sweet = const UndefinedWrapper.undefined(),
  });
}
