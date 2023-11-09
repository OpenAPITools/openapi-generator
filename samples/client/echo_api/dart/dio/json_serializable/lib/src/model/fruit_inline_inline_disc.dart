//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/fruit_inline_inline_disc_one_of.dart';
import 'package:openapi/src/model/fruit_inline_inline_disc_one_of1.dart';
import 'package:json_annotation/json_annotation.dart';

part 'fruit_inline_inline_disc.g.dart';


@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: false,
  explicitToJson: true,
)
class FruitInlineInlineDisc {
  /// Returns a new [FruitInlineInlineDisc] instance.
  FruitInlineInlineDisc({

    required  this.fruitType,
  });

  @JsonKey(
    
    name: r'fruitType',
    required: true,
    includeIfNull: false
  )


  final String fruitType;



  @override
  bool operator ==(Object other) => identical(this, other) || other is FruitInlineInlineDisc &&
     other.fruitType == fruitType;

  @override
  int get hashCode =>
    fruitType.hashCode;

  factory FruitInlineInlineDisc.fromJson(Map<String, dynamic> json) => _$FruitInlineInlineDiscFromJson(json);

  Map<String, dynamic> toJson() => _$FruitInlineInlineDiscToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }

}

