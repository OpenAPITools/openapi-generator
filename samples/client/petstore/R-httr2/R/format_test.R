#' Create a new FormatTest
#'
#' @description
#' FormatTest Class
#'
#' @docType class
#' @title FormatTest
#' @description FormatTest Class
#' @format An \code{R6Class} generator object
#' @field integer  integer [optional]
#' @field int32  integer [optional]
#' @field int64  integer [optional]
#' @field number  numeric
#' @field float  numeric [optional]
#' @field double  numeric [optional]
#' @field string  character [optional]
#' @field byte  character
#' @field binary  data.frame [optional]
#' @field date  character
#' @field dateTime  character [optional]
#' @field uuid  character [optional]
#' @field password  character
#' @field pattern_with_digits A string that is a 10 digit number. Can have leading zeros. character [optional]
#' @field pattern_with_digits_and_delimiter A string starting with 'image_' (case insensitive) and one to three digits following i.e. Image_01. character [optional]
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
FormatTest <- R6::R6Class(
  "FormatTest",
  public = list(
    `integer` = NULL,
    `int32` = NULL,
    `int64` = NULL,
    `number` = NULL,
    `float` = NULL,
    `double` = NULL,
    `string` = NULL,
    `byte` = NULL,
    `binary` = NULL,
    `date` = NULL,
    `dateTime` = NULL,
    `uuid` = NULL,
    `password` = NULL,
    `pattern_with_digits` = NULL,
    `pattern_with_digits_and_delimiter` = NULL,
    #' Initialize a new FormatTest class.
    #'
    #' @description
    #' Initialize a new FormatTest class.
    #'
    #' @param number number
    #' @param byte byte
    #' @param date date
    #' @param password password
    #' @param integer integer
    #' @param int32 int32
    #' @param int64 int64
    #' @param float float
    #' @param double double
    #' @param string string
    #' @param binary binary
    #' @param dateTime dateTime. Default to "2015-10-28T14:38:02Z".
    #' @param uuid uuid
    #' @param pattern_with_digits A string that is a 10 digit number. Can have leading zeros.
    #' @param pattern_with_digits_and_delimiter A string starting with 'image_' (case insensitive) and one to three digits following i.e. Image_01.
    #' @param ... Other optional arguments.
    #' @export
    initialize = function(`number`, `byte`, `date`, `password`, `integer` = NULL, `int32` = NULL, `int64` = NULL, `float` = NULL, `double` = NULL, `string` = NULL, `binary` = NULL, `dateTime` = "2015-10-28T14:38:02Z", `uuid` = NULL, `pattern_with_digits` = NULL, `pattern_with_digits_and_delimiter` = NULL, ...) {
      if (!missing(`number`)) {
        self$`number` <- `number`
      }
      if (!missing(`byte`)) {
        self$`byte` <- `byte`
      }
      if (!missing(`date`)) {
        if (!(is.character(`date`) && length(`date`) == 1)) {
          stop(paste("Error! Invalid data for `date`. Must be a string:", `date`))
        }
        self$`date` <- `date`
      }
      if (!missing(`password`)) {
        if (!(is.character(`password`) && length(`password`) == 1)) {
          stop(paste("Error! Invalid data for `password`. Must be a string:", `password`))
        }
        self$`password` <- `password`
      }
      if (!is.null(`integer`)) {
        if (!(is.numeric(`integer`) && length(`integer`) == 1)) {
          stop(paste("Error! Invalid data for `integer`. Must be an integer:", `integer`))
        }
        self$`integer` <- `integer`
      }
      if (!is.null(`int32`)) {
        if (!(is.numeric(`int32`) && length(`int32`) == 1)) {
          stop(paste("Error! Invalid data for `int32`. Must be an integer:", `int32`))
        }
        self$`int32` <- `int32`
      }
      if (!is.null(`int64`)) {
        if (!(is.numeric(`int64`) && length(`int64`) == 1)) {
          stop(paste("Error! Invalid data for `int64`. Must be an integer:", `int64`))
        }
        self$`int64` <- `int64`
      }
      if (!is.null(`float`)) {
        if (!(is.numeric(`float`) && length(`float`) == 1)) {
          stop(paste("Error! Invalid data for `float`. Must be a number:", `float`))
        }
        self$`float` <- `float`
      }
      if (!is.null(`double`)) {
        if (!(is.numeric(`double`) && length(`double`) == 1)) {
          stop(paste("Error! Invalid data for `double`. Must be a number:", `double`))
        }
        self$`double` <- `double`
      }
      if (!is.null(`string`)) {
        if (!(is.character(`string`) && length(`string`) == 1)) {
          stop(paste("Error! Invalid data for `string`. Must be a string:", `string`))
        }
        self$`string` <- `string`
      }
      if (!is.null(`binary`)) {
        self$`binary` <- `binary`
      }
      if (!is.null(`dateTime`)) {
        if (!is.character(`dateTime`)) {
          stop(paste("Error! Invalid data for `dateTime`. Must be a string:", `dateTime`))
        }
        self$`dateTime` <- `dateTime`
      }
      if (!is.null(`uuid`)) {
        if (!(is.character(`uuid`) && length(`uuid`) == 1)) {
          stop(paste("Error! Invalid data for `uuid`. Must be a string:", `uuid`))
        }
        self$`uuid` <- `uuid`
      }
      if (!is.null(`pattern_with_digits`)) {
        if (!(is.character(`pattern_with_digits`) && length(`pattern_with_digits`) == 1)) {
          stop(paste("Error! Invalid data for `pattern_with_digits`. Must be a string:", `pattern_with_digits`))
        }
        self$`pattern_with_digits` <- `pattern_with_digits`
      }
      if (!is.null(`pattern_with_digits_and_delimiter`)) {
        if (!(is.character(`pattern_with_digits_and_delimiter`) && length(`pattern_with_digits_and_delimiter`) == 1)) {
          stop(paste("Error! Invalid data for `pattern_with_digits_and_delimiter`. Must be a string:", `pattern_with_digits_and_delimiter`))
        }
        self$`pattern_with_digits_and_delimiter` <- `pattern_with_digits_and_delimiter`
      }
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return FormatTest in JSON format
    #' @export
    toJSON = function() {
      FormatTestObject <- list()
      if (!is.null(self$`integer`)) {
        FormatTestObject[["integer"]] <-
          self$`integer`
      }
      if (!is.null(self$`int32`)) {
        FormatTestObject[["int32"]] <-
          self$`int32`
      }
      if (!is.null(self$`int64`)) {
        FormatTestObject[["int64"]] <-
          self$`int64`
      }
      if (!is.null(self$`number`)) {
        FormatTestObject[["number"]] <-
          self$`number`
      }
      if (!is.null(self$`float`)) {
        FormatTestObject[["float"]] <-
          self$`float`
      }
      if (!is.null(self$`double`)) {
        FormatTestObject[["double"]] <-
          self$`double`
      }
      if (!is.null(self$`string`)) {
        FormatTestObject[["string"]] <-
          self$`string`
      }
      if (!is.null(self$`byte`)) {
        FormatTestObject[["byte"]] <-
          self$`byte`
      }
      if (!is.null(self$`binary`)) {
        FormatTestObject[["binary"]] <-
          self$`binary`
      }
      if (!is.null(self$`date`)) {
        FormatTestObject[["date"]] <-
          self$`date`
      }
      if (!is.null(self$`dateTime`)) {
        FormatTestObject[["dateTime"]] <-
          self$`dateTime`
      }
      if (!is.null(self$`uuid`)) {
        FormatTestObject[["uuid"]] <-
          self$`uuid`
      }
      if (!is.null(self$`password`)) {
        FormatTestObject[["password"]] <-
          self$`password`
      }
      if (!is.null(self$`pattern_with_digits`)) {
        FormatTestObject[["pattern_with_digits"]] <-
          self$`pattern_with_digits`
      }
      if (!is.null(self$`pattern_with_digits_and_delimiter`)) {
        FormatTestObject[["pattern_with_digits_and_delimiter"]] <-
          self$`pattern_with_digits_and_delimiter`
      }
      FormatTestObject
    },
    #' Deserialize JSON string into an instance of FormatTest
    #'
    #' @description
    #' Deserialize JSON string into an instance of FormatTest
    #'
    #' @param input_json the JSON input
    #' @return the instance of FormatTest
    #' @export
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`integer`)) {
        self$`integer` <- this_object$`integer`
      }
      if (!is.null(this_object$`int32`)) {
        self$`int32` <- this_object$`int32`
      }
      if (!is.null(this_object$`int64`)) {
        self$`int64` <- this_object$`int64`
      }
      if (!is.null(this_object$`number`)) {
        self$`number` <- this_object$`number`
      }
      if (!is.null(this_object$`float`)) {
        self$`float` <- this_object$`float`
      }
      if (!is.null(this_object$`double`)) {
        self$`double` <- this_object$`double`
      }
      if (!is.null(this_object$`string`)) {
        self$`string` <- this_object$`string`
      }
      if (!is.null(this_object$`byte`)) {
        self$`byte` <- this_object$`byte`
      }
      if (!is.null(this_object$`binary`)) {
        self$`binary` <- this_object$`binary`
      }
      if (!is.null(this_object$`date`)) {
        self$`date` <- this_object$`date`
      }
      if (!is.null(this_object$`dateTime`)) {
        self$`dateTime` <- this_object$`dateTime`
      }
      if (!is.null(this_object$`uuid`)) {
        self$`uuid` <- this_object$`uuid`
      }
      if (!is.null(this_object$`password`)) {
        self$`password` <- this_object$`password`
      }
      if (!is.null(this_object$`pattern_with_digits`)) {
        self$`pattern_with_digits` <- this_object$`pattern_with_digits`
      }
      if (!is.null(this_object$`pattern_with_digits_and_delimiter`)) {
        self$`pattern_with_digits_and_delimiter` <- this_object$`pattern_with_digits_and_delimiter`
      }
      self
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return FormatTest in JSON format
    #' @export
    toJSONString = function() {
      jsoncontent <- c(
        if (!is.null(self$`integer`)) {
          sprintf(
          '"integer":
            %d
                    ',
          self$`integer`
          )
        },
        if (!is.null(self$`int32`)) {
          sprintf(
          '"int32":
            %d
                    ',
          self$`int32`
          )
        },
        if (!is.null(self$`int64`)) {
          sprintf(
          '"int64":
            %d
                    ',
          self$`int64`
          )
        },
        if (!is.null(self$`number`)) {
          sprintf(
          '"number":
            %d
                    ',
          self$`number`
          )
        },
        if (!is.null(self$`float`)) {
          sprintf(
          '"float":
            %d
                    ',
          self$`float`
          )
        },
        if (!is.null(self$`double`)) {
          sprintf(
          '"double":
            %d
                    ',
          self$`double`
          )
        },
        if (!is.null(self$`string`)) {
          sprintf(
          '"string":
            "%s"
                    ',
          self$`string`
          )
        },
        if (!is.null(self$`byte`)) {
          sprintf(
          '"byte":
            "%s"
                    ',
          self$`byte`
          )
        },
        if (!is.null(self$`binary`)) {
          sprintf(
          '"binary":
            "%s"
                    ',
          self$`binary`
          )
        },
        if (!is.null(self$`date`)) {
          sprintf(
          '"date":
            "%s"
                    ',
          self$`date`
          )
        },
        if (!is.null(self$`dateTime`)) {
          sprintf(
          '"dateTime":
            "%s"
                    ',
          self$`dateTime`
          )
        },
        if (!is.null(self$`uuid`)) {
          sprintf(
          '"uuid":
            "%s"
                    ',
          self$`uuid`
          )
        },
        if (!is.null(self$`password`)) {
          sprintf(
          '"password":
            "%s"
                    ',
          self$`password`
          )
        },
        if (!is.null(self$`pattern_with_digits`)) {
          sprintf(
          '"pattern_with_digits":
            "%s"
                    ',
          self$`pattern_with_digits`
          )
        },
        if (!is.null(self$`pattern_with_digits_and_delimiter`)) {
          sprintf(
          '"pattern_with_digits_and_delimiter":
            "%s"
                    ',
          self$`pattern_with_digits_and_delimiter`
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      json_string <- as.character(jsonlite::minify(paste("{", jsoncontent, "}", sep = "")))
    },
    #' Deserialize JSON string into an instance of FormatTest
    #'
    #' @description
    #' Deserialize JSON string into an instance of FormatTest
    #'
    #' @param input_json the JSON input
    #' @return the instance of FormatTest
    #' @export
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`integer` <- this_object$`integer`
      self$`int32` <- this_object$`int32`
      self$`int64` <- this_object$`int64`
      self$`number` <- this_object$`number`
      self$`float` <- this_object$`float`
      self$`double` <- this_object$`double`
      self$`string` <- this_object$`string`
      self$`byte` <- this_object$`byte`
      self$`binary` <- this_object$`binary`
      self$`date` <- this_object$`date`
      self$`dateTime` <- this_object$`dateTime`
      self$`uuid` <- this_object$`uuid`
      self$`password` <- this_object$`password`
      self$`pattern_with_digits` <- this_object$`pattern_with_digits`
      self$`pattern_with_digits_and_delimiter` <- this_object$`pattern_with_digits_and_delimiter`
      self
    },
    #' Validate JSON input with respect to FormatTest
    #'
    #' @description
    #' Validate JSON input with respect to FormatTest and throw an exception if invalid
    #'
    #' @param input the JSON input
    #' @export
    validateJSON = function(input) {
      input_json <- jsonlite::fromJSON(input)
      # check the required field `number`
      if (!is.null(input_json$`number`)) {
      } else {
        stop(paste("The JSON input `", input, "` is invalid for FormatTest: the required field `number` is missing."))
      }
      # check the required field `byte`
      if (!is.null(input_json$`byte`)) {
      } else {
        stop(paste("The JSON input `", input, "` is invalid for FormatTest: the required field `byte` is missing."))
      }
      # check the required field `date`
      if (!is.null(input_json$`date`)) {
        if (!(is.character(input_json$`date`) && length(input_json$`date`) == 1)) {
          stop(paste("Error! Invalid data for `date`. Must be a string:", input_json$`date`))
        }
      } else {
        stop(paste("The JSON input `", input, "` is invalid for FormatTest: the required field `date` is missing."))
      }
      # check the required field `password`
      if (!is.null(input_json$`password`)) {
        if (!(is.character(input_json$`password`) && length(input_json$`password`) == 1)) {
          stop(paste("Error! Invalid data for `password`. Must be a string:", input_json$`password`))
        }
      } else {
        stop(paste("The JSON input `", input, "` is invalid for FormatTest: the required field `password` is missing."))
      }
    },
    #' To string (JSON format)
    #'
    #' @description
    #' To string (JSON format)
    #'
    #' @return String representation of FormatTest
    #' @export
    toString = function() {
      self$toJSONString()
    },
    #' Return true if the values in all fields are valid.
    #'
    #' @description
    #' Return true if the values in all fields are valid.
    #'
    #' @return true if the values in all fields are valid.
    #' @export
    isValid = function() {
      if (self$`integer` > 100) {
        return(FALSE)
      }
      if (self$`integer` < 10) {
        return(FALSE)
      }

      if (self$`int32` > 200) {
        return(FALSE)
      }
      if (self$`int32` < 20) {
        return(FALSE)
      }

      # check if the required `number` is null
      if (is.null(self$`number`)) {
        return(FALSE)
      }

      if (self$`number` > 543.2) {
        return(FALSE)
      }
      if (self$`number` < 32.1) {
        return(FALSE)
      }

      if (self$`float` > 987.6) {
        return(FALSE)
      }
      if (self$`float` < 54.3) {
        return(FALSE)
      }

      if (self$`double` > 123.4) {
        return(FALSE)
      }
      if (self$`double` < 67.8) {
        return(FALSE)
      }

      if (!str_detect(self$`string`, "[a-z]/i")) {
        return(FALSE)
      }

      # check if the required `byte` is null
      if (is.null(self$`byte`)) {
        return(FALSE)
      }

      # check if the required `date` is null
      if (is.null(self$`date`)) {
        return(FALSE)
      }

      # check if the required `password` is null
      if (is.null(self$`password`)) {
        return(FALSE)
      }

      if (nchar(self$`password`) > 64) {
        return(FALSE)
      }
      if (nchar(self$`password`) < 10) {
        return(FALSE)
      }

      if (!str_detect(self$`pattern_with_digits`, "^\\d{10}$")) {
        return(FALSE)
      }

      if (!str_detect(self$`pattern_with_digits_and_delimiter`, "^image_\\d{1,3}$/i")) {
        return(FALSE)
      }

      TRUE
    },
    #' Return a list of invalid fields (if any).
    #'
    #' @description
    #' Return a list of invalid fields (if any).
    #'
    #' @return A list of invalid fields (if any).
    #' @export
    getInvalidFields = function() {
      invalid_fields <- list()
      if (self$`integer` > 100) {
        invalid_fields["integer"] <- "Invalid value for `integer`, must be smaller than or equal to 100."
      }
      if (self$`integer` < 10) {
        invalid_fields["integer"] <- "Invalid value for `integer`, must be bigger than or equal to 10."
      }

      if (self$`int32` > 200) {
        invalid_fields["int32"] <- "Invalid value for `int32`, must be smaller than or equal to 200."
      }
      if (self$`int32` < 20) {
        invalid_fields["int32"] <- "Invalid value for `int32`, must be bigger than or equal to 20."
      }

      # check if the required `number` is null
      if (is.null(self$`number`)) {
        invalid_fields["number"] <- "Non-nullable required field `number` cannot be null."
      }

      if (self$`number` > 543.2) {
        invalid_fields["number"] <- "Invalid value for `number`, must be smaller than or equal to 543.2."
      }
      if (self$`number` < 32.1) {
        invalid_fields["number"] <- "Invalid value for `number`, must be bigger than or equal to 32.1."
      }

      if (self$`float` > 987.6) {
        invalid_fields["float"] <- "Invalid value for `float`, must be smaller than or equal to 987.6."
      }
      if (self$`float` < 54.3) {
        invalid_fields["float"] <- "Invalid value for `float`, must be bigger than or equal to 54.3."
      }

      if (self$`double` > 123.4) {
        invalid_fields["double"] <- "Invalid value for `double`, must be smaller than or equal to 123.4."
      }
      if (self$`double` < 67.8) {
        invalid_fields["double"] <- "Invalid value for `double`, must be bigger than or equal to 67.8."
      }

      if (!str_detect(self$`string`, "[a-z]/i")) {
        invalid_fields["string"] <- "Invalid value for `string`, must conform to the pattern [a-z]/i."
      }

      # check if the required `byte` is null
      if (is.null(self$`byte`)) {
        invalid_fields["byte"] <- "Non-nullable required field `byte` cannot be null."
      }

      # check if the required `date` is null
      if (is.null(self$`date`)) {
        invalid_fields["date"] <- "Non-nullable required field `date` cannot be null."
      }

      # check if the required `password` is null
      if (is.null(self$`password`)) {
        invalid_fields["password"] <- "Non-nullable required field `password` cannot be null."
      }

      if (nchar(self$`password`) > 64) {
        invalid_fields["password"] <- "Invalid length for `password`, must be smaller than or equal to 64."
      }
      if (nchar(self$`password`) < 10) {
        invalid_fields["password"] <- "Invalid length for `password`, must be bigger than or equal to 10."
      }

      if (!str_detect(self$`pattern_with_digits`, "^\\d{10}$")) {
        invalid_fields["pattern_with_digits"] <- "Invalid value for `pattern_with_digits`, must conform to the pattern ^\\d{10}$."
      }

      if (!str_detect(self$`pattern_with_digits_and_delimiter`, "^image_\\d{1,3}$/i")) {
        invalid_fields["pattern_with_digits_and_delimiter"] <- "Invalid value for `pattern_with_digits_and_delimiter`, must conform to the pattern ^image_\\d{1,3}$/i."
      }

      invalid_fields
    },
    #' Print the object
    #'
    #' @description
    #' Print the object
    #'
    #' @export
    print = function() {
      print(jsonlite::prettify(self$toJSONString()))
      invisible(self)
    }
  ),
  # Lock the class to prevent modifications to the method or field
  lock_class = TRUE
)
## Uncomment below to unlock the class to allow modifications of the method or field
# FormatTest$unlock()
#
## Below is an example to define the print function
# FormatTest$set("public", "print", function(...) {
#   print(jsonlite::prettify(self$toJSONString()))
#   invisible(self)
# })
## Uncomment below to lock the class to prevent modifications to the method or field
# FormatTest$lock()

