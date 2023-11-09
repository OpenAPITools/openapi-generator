//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:json_annotation/json_annotation.dart';

part 'apple_all_of_disc.g.dart';


@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: false,
  explicitToJson: true,
)
class AppleAllOfDisc {
  /// Returns a new [AppleAllOfDisc] instance.
  AppleAllOfDisc({

    required  this.seeds,

    required  this.fruitType,
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



  @override
  bool operator ==(Object other) => identical(this, other) || other is AppleAllOfDisc &&
     other.seeds == seeds &&
     other.fruitType == fruitType;

  @override
  int get hashCode =>
    seeds.hashCode +
    fruitType.hashCode;

  factory AppleAllOfDisc.fromJson(Map<String, dynamic> json) => _$AppleAllOfDiscFromJson(json);

  Map<String, dynamic> toJson() => _$AppleAllOfDiscToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }

}

