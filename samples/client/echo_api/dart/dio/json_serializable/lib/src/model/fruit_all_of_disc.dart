//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/banana_all_of_disc.dart';
import 'package:openapi/src/model/apple_all_of_disc.dart';
import 'package:json_annotation/json_annotation.dart';

part 'fruit_all_of_disc.g.dart';


@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: false,
  explicitToJson: true,
)
class FruitAllOfDisc {
  /// Returns a new [FruitAllOfDisc] instance.
  FruitAllOfDisc({

    required  this.fruitType,
  });

  @JsonKey(
    
    name: r'fruitType',
    required: true,
    includeIfNull: false
  )


  final String fruitType;



  @override
  bool operator ==(Object other) => identical(this, other) || other is FruitAllOfDisc &&
     other.fruitType == fruitType;

  @override
  int get hashCode =>
    fruitType.hashCode;

  factory FruitAllOfDisc.fromJson(Map<String, dynamic> json) => _$FruitAllOfDiscFromJson(json);

  Map<String, dynamic> toJson() => _$FruitAllOfDiscToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }

}

