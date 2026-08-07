/**
 * Standard parameter styles defined by OpenAPI spec
 */
export type StandardParamStyle =
  | 'matrix'
  | 'label'
  | 'form'
  | 'simple'
  | 'spaceDelimited'
  | 'pipeDelimited'
  | 'deepObject'
  ;

/**
 * The OpenAPI standard {@link StandardParamStyle}s may be extended by custom styles by the user.
 */
export type ParamStyle = StandardParamStyle | string;

/**
 * Standard parameter locations defined by OpenAPI spec
 */
export type ParamLocation = 'query' | 'header' | 'path' | 'cookie';

/**
 * Standard types as defined in <a href="https://swagger.io/specification/#data-types">OpenAPI Specification: Data Types</a>
 */
export type StandardDataType =
  | "integer"
  | "number"
  | "boolean"
  | "string"
  | "object"
  | "array"
  ;

/**
 * Standard {@link DataType}s plus your own types/classes.
 */
export type DataType = StandardDataType | string;

/**
 * Standard formats as defined in <a href="https://swagger.io/specification/#data-types">OpenAPI Specification: Data Types</a>
 */
export type StandardDataFormat =
  | "int32"
  | "int64"
  | "float"
  | "double"
  | "byte"
  | "binary"
  | "date"
  | "date-time"
  | "password"
  ;

export type DataFormat = StandardDataFormat | string;

/**
 * The parameter to encode.
 */
export interface Param {
  name: string;
  value: unknown;
  in: ParamLocation;
  style: ParamStyle,
  explode: boolean;
  dataType: DataType;
  dataFormat: DataFormat | undefined;
}
