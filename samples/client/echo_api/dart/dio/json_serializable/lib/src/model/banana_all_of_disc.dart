//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:json_annotation/json_annotation.dart';

part 'banana_all_of_disc.g.dart';


@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: false,
  explicitToJson: true,
)
class BananaAllOfDisc {
  /// Returns a new [BananaAllOfDisc] instance.
  BananaAllOfDisc({

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
  bool operator ==(Object other) => identical(this, other) || other is BananaAllOfDisc &&
     other.length == length &&
     other.fruitType == fruitType;

  @override
  int get hashCode =>
    length.hashCode +
    fruitType.hashCode;

  factory BananaAllOfDisc.fromJson(Map<String, dynamic> json) => _$BananaAllOfDiscFromJson(json);

  Map<String, dynamic> toJson() => _$BananaAllOfDiscToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }

}

