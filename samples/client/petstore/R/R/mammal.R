#' @docType class
#' @title Mammal
#'
#' @description Mammal Class
#'
#' @format An \code{R6Class} generator object
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
Mammal <- R6::R6Class(
  "Mammal",
  public = list(
    #' @field actual_instance the object stored in this instance.
    actual_instance = NULL,
    #' @field actual_type the type of the object stored in this instance.
    actual_type = NULL,
    #' @field one_of  a list of types defined in the oneOf schema.
    one_of = list("Whale", "Zebra"),
    #' Initialize a new Mammal.
    #'
    #' @description
    #' Initialize a new Mammal.
    #'
    #' @param instance an instance of the object defined in the oneOf schemas: "Whale", "Zebra"
    #' @export
    initialize = function(instance = NULL) {
      if (is.null(instance)) {
        # do nothing
      } else if (get(class(instance)[[1]], pos = -1)$classname ==  "Whale") {
        self$actual_instance <- instance
        self$actual_type <- "Whale"
      } else if (get(class(instance)[[1]], pos = -1)$classname ==  "Zebra") {
        self$actual_instance <- instance
        self$actual_type <- "Zebra"
      } else {
        stop(paste("Failed to initialize Mammal with oneOf schemas Whale, Zebra. Provided class name: ",
                   get(class(instance)[[1]], pos = -1)$classname))
      }
    },
    #' Deserialize JSON string into an instance of Mammal.
    #'
    #' @description
    #' Deserialize JSON string into an instance of Mammal.
    #' An alias to the method `fromJSON` .
    #'
    #' @param input The input JSON.
    #' @return An instance of Mammal.
    #' @export
    fromJSONString = function(input) {
      self$fromJSON(input)
    },
    #' Deserialize JSON string into an instance of Mammal.
    #'
    #' @description
    #' Deserialize JSON string into an instance of Mammal.
    #'
    #' @param input The input JSON.
    #' @return An instance of Mammal.
    #' @export
    fromJSON = function(input) {
      matched <- 0 # match counter
      matched_schemas <- list() #names of matched schemas
      error_messages <- list()
      instance <- NULL

      `Whale_result` <- tryCatch({
          `Whale`$public_methods$validateJSON(input)
          `Whale_instance` <- `Whale`$new()
          instance <- `Whale_instance`$fromJSON(input)
          instance_type <- "Whale"
          matched_schemas <- append(matched_schemas, "Whale")
          matched <- matched + 1
        },
        error = function(err) err
      )

      if (!is.null(`Whale_result`["error"])) {
        error_messages <- append(error_messages, `Whale_result`["message"])
      }

      `Zebra_result` <- tryCatch({
          `Zebra`$public_methods$validateJSON(input)
          `Zebra_instance` <- `Zebra`$new()
          instance <- `Zebra_instance`$fromJSON(input)
          instance_type <- "Zebra"
          matched_schemas <- append(matched_schemas, "Zebra")
          matched <- matched + 1
        },
        error = function(err) err
      )

      if (!is.null(`Zebra_result`["error"])) {
        error_messages <- append(error_messages, `Zebra_result`["message"])
      }

      if (matched == 1) {
        # successfully match exactly 1 schema specified in oneOf
        self$actual_instance <- instance
        self$actual_type <- instance_type
      } else if (matched > 1) {
        # more than 1 match
        stop(paste("Multiple matches found when deserializing the input into Mammal with oneOf schemas Whale, Zebra. Matched schemas: ",
                   paste(matched_schemas, collapse = ", ")))
      } else {
        # no match
        stop(paste("No match found when deserializing the input into Mammal with oneOf schemas Whale, Zebra. Details: >>",
                   paste(error_messages, collapse = " >> ")))
      }

      self
    },
    #' Serialize Mammal to JSON string.
    #'
    #' @description
    #' Serialize Mammal to JSON string.
    #'
    #' @return JSON string representation of the Mammal.
    #' @export
    toJSONString = function() {
      if (!is.null(self$actual_instance)) {
        as.character(jsonlite::minify(self$actual_instance$toJSONString()))
      } else {
        NULL
      }
    },
    #' Serialize Mammal to JSON.
    #'
    #' @description
    #' Serialize Mammal to JSON.
    #'
    #' @return JSON representation of the Mammal.
    #' @export
    toJSON = function() {
      if (!is.null(self$actual_instance)) {
        self$actual_instance$toJSON()
      } else {
        NULL
      }
    },
    #' Validate the input JSON with respect to Mammal.
    #'
    #' @description
    #' Validate the input JSON with respect to Mammal and
    #' throw exception if invalid.
    #'
    #' @param input The input JSON.
    #' @export
    validateJSON = function(input) {
      # backup current values
      actual_instance_bak <- self$actual_instance
      actual_type_bak <- self$actual_type

      # if it's not valid, an error will be thrown
      self$fromJSON(input)

      # no error thrown, restore old values
      self$actual_instance <- actual_instance_bak
      self$actual_type <- actual_type_bak
    },
    #' Returns the string representation of the instance.
    #'
    #' @description
    #' Returns the string representation of the instance.
    #'
    #' @return The string representation of the instance.
    #' @export
    toString = function() {
      jsoncontent <- c(
        sprintf('"actual_instance": %s', if (is.null(self$actual_instance)) NULL else self$actual_instance$toJSONString()),
        sprintf('"actual_type": "%s"', self$actual_type),
        sprintf('"one_of": "%s"', paste(unlist(self$one_of), collapse = ", "))
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      as.character(jsonlite::prettify(paste("{", jsoncontent, "}", sep = "")))
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
#Mammal$unlock()
#
## Below is an example to define the print function
#Mammal$set("public", "print", function(...) {
#  print(jsonlite::prettify(self$toJSONString()))
#  invisible(self)
#})
## Uncomment below to lock the class to prevent modifications to the method or field
#Mammal$lock()

