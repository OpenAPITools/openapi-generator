//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/fruit_type.dart';
import 'package:json_annotation/json_annotation.dart';

part 'fruit_any_of_disc.g.dart';


@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: false,
  explicitToJson: true,
)
class FruitAnyOfDisc {
  /// Returns a new [FruitAnyOfDisc] instance.
  FruitAnyOfDisc({

    required  this.fruitType,
  });

  @JsonKey(
    
    name: r'fruitType',
    required: true,
    includeIfNull: false
  )


  final String fruitType;



  @override
  bool operator ==(Object other) => identical(this, other) || other is FruitAnyOfDisc &&
     other.fruitType == fruitType;

  @override
  int get hashCode =>
    fruitType.hashCode;

  factory FruitAnyOfDisc.fromJson(Map<String, dynamic> json) => _$FruitAnyOfDiscFromJson(json);

  Map<String, dynamic> toJson() => _$FruitAnyOfDiscToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }

}

