//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:json_annotation/json_annotation.dart';

part 'fruit_inline_inline_disc_one_of_one_of.g.dart';


@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: false,
  explicitToJson: true,
)
class FruitInlineInlineDiscOneOfOneOf {
  /// Returns a new [FruitInlineInlineDiscOneOfOneOf] instance.
  FruitInlineInlineDiscOneOfOneOf({

    required  this.fruitType,
  });

  @JsonKey(
    
    name: r'fruitType',
    required: true,
    includeIfNull: false
  )


  final String fruitType;



  @override
  bool operator ==(Object other) => identical(this, other) || other is FruitInlineInlineDiscOneOfOneOf &&
     other.fruitType == fruitType;

  @override
  int get hashCode =>
    fruitType.hashCode;

  factory FruitInlineInlineDiscOneOfOneOf.fromJson(Map<String, dynamic> json) => _$FruitInlineInlineDiscOneOfOneOfFromJson(json);

  Map<String, dynamic> toJson() => _$FruitInlineInlineDiscOneOfOneOfToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }

}

