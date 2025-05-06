//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:json_annotation/json_annotation.dart';

part 'number_only.g.dart';


@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: false,
  explicitToJson: true,
)
class NumberOnly {
  /// Returns a new [NumberOnly] instance.
  NumberOnly({

     this.justnumber,
  });

  @JsonKey(
    
    name: r'JustNumber',
    required: false,
    includeIfNull: false,
  )


  final num? justnumber;





    @override
    bool operator ==(Object other) => identical(this, other) || other is NumberOnly &&
      other.justnumber == justnumber;

    @override
    int get hashCode =>
        justnumber.hashCode;

  factory NumberOnly.fromJson(Map<String, dynamic> json) => _$NumberOnlyFromJson(json);

  Map<String, dynamic> toJson() => _$NumberOnlyToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }

}

