//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/banana_grandparent_disc.dart';
import 'package:openapi/src/model/apple_grandparent_disc.dart';
import 'package:json_annotation/json_annotation.dart';

part 'fruit_grandparent_disc.g.dart';


@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: false,
  explicitToJson: true,
)
class FruitGrandparentDisc {
  /// Returns a new [FruitGrandparentDisc] instance.
  FruitGrandparentDisc({

    required  this.fruitType,
  });

  @JsonKey(
    
    name: r'fruitType',
    required: true,
    includeIfNull: false
  )


  final String fruitType;



  @override
  bool operator ==(Object other) => identical(this, other) || other is FruitGrandparentDisc &&
     other.fruitType == fruitType;

  @override
  int get hashCode =>
    fruitType.hashCode;

  factory FruitGrandparentDisc.fromJson(Map<String, dynamic> json) => _$FruitGrandparentDiscFromJson(json);

  Map<String, dynamic> toJson() => _$FruitGrandparentDiscToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }

}

