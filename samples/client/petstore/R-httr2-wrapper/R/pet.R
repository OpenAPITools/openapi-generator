#' Create a new Pet
#'
#' @description
#' A pet for sale in the pet store
#'
#' @docType class
#' @title Pet
#' @description Pet Class
#' @format An \code{R6Class} generator object
#' @field id  integer [optional]
#' @field category  \link{Category} [optional]
#' @field name  character
#' @field photoUrls  list(character)
#' @field tags  list(\link{Tag}) [optional]
#' @field status pet status in the store character [optional]
#' @field _field_list a list of fields list(character)
#' @field additional_properties additional properties list(character) [optional]
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
Pet <- R6::R6Class(
  "Pet",
  public = list(
    `id` = NULL,
    `category` = NULL,
    `name` = NULL,
    `photoUrls` = NULL,
    `tags` = NULL,
    `status` = NULL,
    `_field_list` = c("id", "category", "name", "photoUrls", "tags", "status"),
    `additional_properties` = list(),
    #' Initialize a new Pet class.
    #'
    #' @description
    #' Initialize a new Pet class.
    #'
    #' @param name name
    #' @param photoUrls photoUrls
    #' @param id id
    #' @param category category
    #' @param tags tags
    #' @param status pet status in the store
    #' @param additional_properties additional properties (optional)
    #' @param ... Other optional arguments.
    #' @export
    initialize = function(`name`, `photoUrls`, `id` = NULL, `category` = NULL, `tags` = NULL, `status` = NULL, additional_properties = NULL, ...) {
      if (!missing(`name`)) {
        if (!(is.character(`name`) && length(`name`) == 1)) {
          stop(paste("Error! Invalid data for `name`. Must be a string:", `name`))
        }
        self$`name` <- `name`
      }
      if (!missing(`photoUrls`)) {
        stopifnot(is.vector(`photoUrls`), length(`photoUrls`) != 0)
        sapply(`photoUrls`, function(x) stopifnot(is.character(x)))
        self$`photoUrls` <- `photoUrls`
      }
      if (!is.null(`id`)) {
        if (!(is.numeric(`id`) && length(`id`) == 1)) {
          stop(paste("Error! Invalid data for `id`. Must be an integer:", `id`))
        }
        self$`id` <- `id`
      }
      if (!is.null(`category`)) {
        stopifnot(R6::is.R6(`category`))
        self$`category` <- `category`
      }
      if (!is.null(`tags`)) {
        stopifnot(is.vector(`tags`), length(`tags`) != 0)
        sapply(`tags`, function(x) stopifnot(R6::is.R6(x)))
        self$`tags` <- `tags`
      }
      if (!is.null(`status`)) {
        if (!(`status` %in% c("available", "pending", "sold"))) {
          stop(paste("Error! \"", `status`, "\" cannot be assigned to `status`. Must be \"available\", \"pending\", \"sold\".", sep = ""))
        }
        if (!(is.character(`status`) && length(`status`) == 1)) {
          stop(paste("Error! Invalid data for `status`. Must be a string:", `status`))
        }
        self$`status` <- `status`
      }
      if (!is.null(additional_properties)) {
        for (key in names(additional_properties)) {
          self$additional_properties[[key]] <- additional_properties[[key]]
        }
      }
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return Pet in JSON format
    #' @export
    toJSON = function() {
      PetObject <- list()
      if (!is.null(self$`id`)) {
        PetObject[["id"]] <-
          self$`id`
      }
      if (!is.null(self$`category`)) {
        PetObject[["category"]] <-
          self$`category`$toJSON()
      }
      if (!is.null(self$`name`)) {
        PetObject[["name"]] <-
          self$`name`
      }
      if (!is.null(self$`photoUrls`)) {
        PetObject[["photoUrls"]] <-
          self$`photoUrls`
      }
      if (!is.null(self$`tags`)) {
        PetObject[["tags"]] <-
          lapply(self$`tags`, function(x) x$toJSON())
      }
      if (!is.null(self$`status`)) {
        PetObject[["status"]] <-
          self$`status`
      }
      for (key in names(self$additional_properties)) {
        PetObject[[key]] <- self$additional_properties[[key]]
      }

      PetObject
    },
    #' Deserialize JSON string into an instance of Pet
    #'
    #' @description
    #' Deserialize JSON string into an instance of Pet
    #'
    #' @param input_json the JSON input
    #' @return the instance of Pet
    #' @export
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`id`)) {
        self$`id` <- this_object$`id`
      }
      if (!is.null(this_object$`category`)) {
        `category_object` <- Category$new()
        `category_object`$fromJSON(jsonlite::toJSON(this_object$`category`, auto_unbox = TRUE, digits = NA))
        self$`category` <- `category_object`
      }
      if (!is.null(this_object$`name`)) {
        self$`name` <- this_object$`name`
      }
      if (!is.null(this_object$`photoUrls`)) {
        self$`photoUrls` <- ApiClient$new()$deserializeObj(this_object$`photoUrls`, "array[character]", loadNamespace("petstore"))
      }
      if (!is.null(this_object$`tags`)) {
        self$`tags` <- ApiClient$new()$deserializeObj(this_object$`tags`, "array[Tag]", loadNamespace("petstore"))
      }
      if (!is.null(this_object$`status`)) {
        if (!is.null(this_object$`status`) && !(this_object$`status` %in% c("available", "pending", "sold"))) {
          stop(paste("Error! \"", this_object$`status`, "\" cannot be assigned to `status`. Must be \"available\", \"pending\", \"sold\".", sep = ""))
        }
        self$`status` <- this_object$`status`
      }
      # process additional properties/fields in the payload
      for (key in names(this_object)) {
        if (!(key %in% self$`_field_list`)) { # json key not in list of fields
          self$additional_properties[[key]] <- this_object[[key]]
        }
      }

      self
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return Pet in JSON format
    #' @export
    toJSONString = function() {
      jsoncontent <- c(
        if (!is.null(self$`id`)) {
          sprintf(
          '"id":
            %d
                    ',
          self$`id`
          )
        },
        if (!is.null(self$`category`)) {
          sprintf(
          '"category":
          %s
          ',
          jsonlite::toJSON(self$`category`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        },
        if (!is.null(self$`name`)) {
          sprintf(
          '"name":
            "%s"
                    ',
          self$`name`
          )
        },
        if (!is.null(self$`photoUrls`)) {
          sprintf(
          '"photoUrls":
             [%s]
          ',
          paste(unlist(lapply(self$`photoUrls`, function(x) paste0('"', x, '"'))), collapse = ",")
          )
        },
        if (!is.null(self$`tags`)) {
          sprintf(
          '"tags":
          [%s]
',
          paste(sapply(self$`tags`, function(x) jsonlite::toJSON(x$toJSON(), auto_unbox = TRUE, digits = NA)), collapse = ",")
          )
        },
        if (!is.null(self$`status`)) {
          sprintf(
          '"status":
            "%s"
                    ',
          self$`status`
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      json_string <- as.character(jsonlite::minify(paste("{", jsoncontent, "}", sep = "")))
      json_obj <- jsonlite::fromJSON(json_string)
      for (key in names(self$additional_properties)) {
        json_obj[[key]] <- self$additional_properties[[key]]
      }
      json_string <- as.character(jsonlite::minify(jsonlite::toJSON(json_obj, auto_unbox = TRUE, digits = NA)))
    },
    #' Deserialize JSON string into an instance of Pet
    #'
    #' @description
    #' Deserialize JSON string into an instance of Pet
    #'
    #' @param input_json the JSON input
    #' @return the instance of Pet
    #' @export
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`id` <- this_object$`id`
      self$`category` <- Category$new()$fromJSON(jsonlite::toJSON(this_object$`category`, auto_unbox = TRUE, digits = NA))
      self$`name` <- this_object$`name`
      self$`photoUrls` <- ApiClient$new()$deserializeObj(this_object$`photoUrls`, "array[character]", loadNamespace("petstore"))
      self$`tags` <- ApiClient$new()$deserializeObj(this_object$`tags`, "array[Tag]", loadNamespace("petstore"))
      if (!is.null(this_object$`status`) && !(this_object$`status` %in% c("available", "pending", "sold"))) {
        stop(paste("Error! \"", this_object$`status`, "\" cannot be assigned to `status`. Must be \"available\", \"pending\", \"sold\".", sep = ""))
      }
      self$`status` <- this_object$`status`
      # process additional properties/fields in the payload
      for (key in names(this_object)) {
        if (!(key %in% self$`_field_list`)) { # json key not in list of fields
          self$additional_properties[[key]] <- this_object[[key]]
        }
      }

      self
    },
    #' Validate JSON input with respect to Pet
    #'
    #' @description
    #' Validate JSON input with respect to Pet and throw an exception if invalid
    #'
    #' @param input the JSON input
    #' @export
    validateJSON = function(input) {
      input_json <- jsonlite::fromJSON(input)
      # check the required field `name`
      if (!is.null(input_json$`name`)) {
        if (!(is.character(input_json$`name`) && length(input_json$`name`) == 1)) {
          stop(paste("Error! Invalid data for `name`. Must be a string:", input_json$`name`))
        }
      } else {
        stop(paste("The JSON input `", input, "` is invalid for Pet: the required field `name` is missing."))
      }
      # check the required field `photoUrls`
      if (!is.null(input_json$`photoUrls`)) {
        stopifnot(is.vector(input_json$`photoUrls`), length(input_json$`photoUrls`) != 0)
        tmp <- sapply(input_json$`photoUrls`, function(x) stopifnot(is.character(x)))
      } else {
        stop(paste("The JSON input `", input, "` is invalid for Pet: the required field `photoUrls` is missing."))
      }
    },
    #' To string (JSON format)
    #'
    #' @description
    #' To string (JSON format)
    #'
    #' @return String representation of Pet
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
      # check if the required `name` is null
      if (is.null(self$`name`)) {
        return(FALSE)
      }

      # check if the required `photoUrls` is null
      if (is.null(self$`photoUrls`)) {
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
      # check if the required `name` is null
      if (is.null(self$`name`)) {
        invalid_fields["name"] <- "Non-nullable required field `name` cannot be null."
      }

      # check if the required `photoUrls` is null
      if (is.null(self$`photoUrls`)) {
        invalid_fields["photoUrls"] <- "Non-nullable required field `photoUrls` cannot be null."
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
# Pet$unlock()
#
## Below is an example to define the print function
# Pet$set("public", "print", function(...) {
#   print(jsonlite::prettify(self$toJSONString()))
#   invisible(self)
# })
## Uncomment below to lock the class to prevent modifications to the method or field
# Pet$lock()

