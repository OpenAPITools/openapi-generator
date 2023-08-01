//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:json_annotation/json_annotation.dart';

part 'has_only_read_only.g.dart';


@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: false,
  explicitToJson: true,
)
class HasOnlyReadOnly {
  /// Returns a new [HasOnlyReadOnly] instance.
  HasOnlyReadOnly({

     this.bar,

     this.foo,
  });

  @JsonKey(
    
    name: r'bar',
    required: false,
    includeIfNull: false
  )


  final String? bar;



  @JsonKey(
    
    name: r'foo',
    required: false,
    includeIfNull: false
  )


  final String? foo;



  @override
  bool operator ==(Object other) => identical(this, other) || other is HasOnlyReadOnly &&
     other.bar == bar &&
     other.foo == foo;

  @override
  int get hashCode =>
    bar.hashCode +
    foo.hashCode;

  factory HasOnlyReadOnly.fromJson(Map<String, dynamic> json) => _$HasOnlyReadOnlyFromJson(json);

  Map<String, dynamic> toJson() => _$HasOnlyReadOnlyToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }

}

