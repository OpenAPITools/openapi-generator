//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element, invalid_annotation_target
part of 'models.dart';

/// MapTest
    ///
    /// Properties:
        /// * [mapMapOfString] 
        /// * [mapOfEnumString] 
        /// * [directMap] 
        /// * [indirectMap] 

        @freezed
        class MapTest with _$MapTest {
        const MapTest._();
        
        const factory MapTest({
                    @JsonKey(name: r'map_map_of_string') 
    Map<String, 
    Map<String, 
    String?
>?
>?
 mapMapOfString,
                    @JsonKey(name: r'map_of_enum_string') 
    Map<String, 
    MapTestMapOfEnumStringEnum?
>?
 mapOfEnumString,
                    @JsonKey(name: r'direct_map') 
    Map<String, 
    bool?
>?
 directMap,
                    @JsonKey(name: r'indirect_map') 
    Map<String, 
    bool?
>?
 indirectMap,
        }) = _MapTest;


        factory MapTest.fromJson(Map<String, dynamic> json) => _$MapTestFromJson(json);






}



                
                @JsonEnum(valueField: 'value')
                enum MapTestMapOfEnumStringEnum {
                                            UPPER(value: r'UPPER'),
                            lower(value: r'lower'),
                            unknownDefaultOpenApi(value: r'unknown_default_open_api');
                        const MapTestMapOfEnumStringEnum({required this.value});
                        final String value;
                }
