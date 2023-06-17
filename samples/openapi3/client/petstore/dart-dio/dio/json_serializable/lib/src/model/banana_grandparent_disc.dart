//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:json_annotation/json_annotation.dart';

part 'banana_grandparent_disc.g.dart';


@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: false,
  explicitToJson: true,
)
class BananaGrandparentDisc {
  /// Returns a new [BananaGrandparentDisc] instance.
  BananaGrandparentDisc({

    required  this.length,

    required  this.fruitType,
  });

  @JsonKey(
    
    name: r'length',
    required: true,
    includeIfNull: false
  )


  final int length;



  @JsonKey(
    
    name: r'fruitType',
    required: true,
    includeIfNull: false
  )


  final String fruitType;



  @override
  bool operator ==(Object other) => identical(this, other) || other is BananaGrandparentDisc &&
     other.length == length &&
     other.fruitType == fruitType;

  @override
  int get hashCode =>
    length.hashCode +
    fruitType.hashCode;

  factory BananaGrandparentDisc.fromJson(Map<String, dynamic> json) => _$BananaGrandparentDiscFromJson(json);

  Map<String, dynamic> toJson() => _$BananaGrandparentDiscToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }

}

