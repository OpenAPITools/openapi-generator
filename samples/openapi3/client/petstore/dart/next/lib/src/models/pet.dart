// Model def

import 'dart:convert';
import 'dart:typed_data';
import 'package:openapi/_internal.dart';
import 'package:xml/xml.dart';

part 'pet.reflection.dart';
part 'pet.serialization.dart';


/// PetMixin
///
/// Properties:
/// * [id] 
/// * [category] 
/// * [name] 
/// * [photoUrls] 
/// * [tags] 
/// * [status] - pet status in the store
mixin PetMixin on 
  
  $OpenApiObjectMixin
 {
  UndefinedWrapper<int> get id;
  UndefinedWrapper<Category> get category;
  String get name;
  List<String> get photoUrls;
  UndefinedWrapper<List<Tag>> get tags;
  UndefinedWrapper<PetStatusEnum> get status;

}

/// Pet
///
/// Properties:
/// * [id] 
/// * [category] 
/// * [name] 
/// * [photoUrls] 
/// * [tags] 
/// * [status] - pet status in the store
class Pet with
$OpenApiObjectMixin,


PetMixin {
  @override
  UndefinedWrapper<int> id;
  @override
  UndefinedWrapper<Category> category;
  @override
  String name;
  @override
  List<String> photoUrls;
  @override
  UndefinedWrapper<List<Tag>> tags;
  @override
  UndefinedWrapper<PetStatusEnum> status;





  Pet.$all({
    required this.id,
    required this.category,
    required this.name,
    required this.photoUrls,
    required this.tags,
    required this.status,
    
    
  });

  Pet({
    this.id = const UndefinedWrapper.undefined(),
    this.category = const UndefinedWrapper.undefined(),
  required  this.name ,
  required  this.photoUrls ,
    this.tags = const UndefinedWrapper.undefined(),
    this.status = const UndefinedWrapper.undefined(),
    
    
  });
}




extension type const PetStatusEnum._(String value) {
  /// pet status in the store
      const PetStatusEnum.available() : this._(r'available');
  /// pet status in the store
      const PetStatusEnum.pending() : this._(r'pending');
  /// pet status in the store
      const PetStatusEnum.sold() : this._(r'sold');

  /// Creates a [PetStatusEnum] enum from a value and safely checking if it exists.
  factory PetStatusEnum.$safe(String value) {
    final res = values.where((element) => element.value == value).firstOrNull;
    if (res == null) {
      throw 'Invalid enum value $value';
    }
    return res;
  }

  /// Creates a [PetStatusEnum] enum from a value without checking if it exists.
  const PetStatusEnum.$unsafe(String value) : this._(value);

  /// All possible values of the enum.
  static const List<PetStatusEnum> values = [
    PetStatusEnum.available(),
    PetStatusEnum.pending(),
    PetStatusEnum.sold(),
    
  ];
}

