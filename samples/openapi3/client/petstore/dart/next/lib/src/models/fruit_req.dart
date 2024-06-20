// Model def

import 'dart:convert';
import 'dart:typed_data';
import 'package:openapi/_internal.dart';
import 'package:xml/xml.dart';

part 'fruit_req.reflection.dart';
part 'fruit_req.serialization.dart';


/// FruitReqMixin
///
/// Properties:
mixin FruitReqMixin on 
  
  $OpenApiObjectMixin
 {

  UndefinedWrapper<AppleReq> get oneOf0;
  UndefinedWrapper<BananaReq> get oneOf1;
}

/// FruitReq
///
/// Properties:
class FruitReq with
$OpenApiObjectMixin,


FruitReqMixin {




  @override
  UndefinedWrapper<AppleReq> oneOf0;

  @override
  UndefinedWrapper<BananaReq> oneOf1;


  FruitReq.$all({
    
    
    required this.oneOf0,
    required this.oneOf1,
  });

  FruitReq({
    
    
    this.oneOf0 = const UndefinedWrapper.undefined(),
    this.oneOf1 = const UndefinedWrapper.undefined(),
  });
}




