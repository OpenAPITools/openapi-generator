//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/fruit_inline_disc_one_of.dart';
import 'package:openapi/src/model/fruit_inline_disc_one_of1.dart';
import 'package:json_annotation/json_annotation.dart';

part 'fruit_inline_disc.g.dart';


@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: false,
  explicitToJson: true,
)
class FruitInlineDisc {
  /// Returns a new [FruitInlineDisc] instance.
  FruitInlineDisc({

    required  this.seeds,

    required  this.fruitType,

    required  this.length,
  });

  @JsonKey(
    
    name: r'seeds',
    required: true,
    includeIfNull: false
  )


  final int seeds;



  @JsonKey(
    
    name: r'fruitType',
    required: true,
    includeIfNull: false
  )


  final String fruitType;



  @JsonKey(
    
    name: r'length',
    required: true,
    includeIfNull: false
  )


  final int length;



  @override
  bool operator ==(Object other) => identical(this, other) || other is FruitInlineDisc &&
     other.seeds == seeds &&
     other.fruitType == fruitType &&
     other.length == length;

  @override
  int get hashCode =>
    seeds.hashCode +
    fruitType.hashCode +
    length.hashCode;

  factory FruitInlineDisc.fromJson(Map<String, dynamic> json) => _$FruitInlineDiscFromJson(json);

  Map<String, dynamic> toJson() => _$FruitInlineDiscToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }

}

