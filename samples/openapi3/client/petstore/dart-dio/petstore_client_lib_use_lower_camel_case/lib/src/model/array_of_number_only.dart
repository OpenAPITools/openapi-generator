//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:json_annotation/json_annotation.dart';

part 'array_of_number_only.g.dart';


@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: false,
  explicitToJson: true,
)
class ArrayOfNumberOnly {
  /// Returns a new [ArrayOfNumberOnly] instance.
  ArrayOfNumberOnly({

     this.arraynumber,
  });

  @JsonKey(
    
    name: r'ArrayNumber',
    required: false,
    includeIfNull: false,
  )


  final List<num>? arraynumber;





    @override
    bool operator ==(Object other) => identical(this, other) || other is ArrayOfNumberOnly &&
      other.arraynumber == arraynumber;

    @override
    int get hashCode =>
        arraynumber.hashCode;

  factory ArrayOfNumberOnly.fromJson(Map<String, dynamic> json) => _$ArrayOfNumberOnlyFromJson(json);

  Map<String, dynamic> toJson() => _$ArrayOfNumberOnlyToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }

}

