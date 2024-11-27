#' Create a new Order
#'
#' @description
#' An order for a pets from the pet store
#'
#' @docType class
#' @title Order
#' @description Order Class
#' @format An \code{R6Class} generator object
#' @field id  integer [optional]
#' @field petId  integer [optional]
#' @field quantity  integer [optional]
#' @field shipDate  character [optional]
#' @field status Order Status character [optional]
#' @field complete  character [optional]
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
Order <- R6::R6Class(
  "Order",
  public = list(
    `id` = NULL,
    `petId` = NULL,
    `quantity` = NULL,
    `shipDate` = NULL,
    `status` = NULL,
    `complete` = NULL,

    #' @description
    #' Initialize a new Order class.
    #'
    #' @param id id
    #' @param petId petId
    #' @param quantity quantity
    #' @param shipDate shipDate
    #' @param status Order Status
    #' @param complete complete. Default to FALSE.
    #' @param ... Other optional arguments.
    initialize = function(`id` = NULL, `petId` = NULL, `quantity` = NULL, `shipDate` = NULL, `status` = NULL, `complete` = FALSE, ...) {
      if (!is.null(`id`)) {
        if (!(is.numeric(`id`) && length(`id`) == 1)) {
          stop(paste("Error! Invalid data for `id`. Must be an integer:", `id`))
        }
        self$`id` <- `id`
      }
      if (!is.null(`petId`)) {
        if (!(is.numeric(`petId`) && length(`petId`) == 1)) {
          stop(paste("Error! Invalid data for `petId`. Must be an integer:", `petId`))
        }
        self$`petId` <- `petId`
      }
      if (!is.null(`quantity`)) {
        if (!(is.numeric(`quantity`) && length(`quantity`) == 1)) {
          stop(paste("Error! Invalid data for `quantity`. Must be an integer:", `quantity`))
        }
        self$`quantity` <- `quantity`
      }
      if (!is.null(`shipDate`)) {
        if (!is.character(`shipDate`)) {
          stop(paste("Error! Invalid data for `shipDate`. Must be a string:", `shipDate`))
        }
        self$`shipDate` <- `shipDate`
      }
      if (!is.null(`status`)) {
        if (!(`status` %in% c("placed", "approved", "delivered"))) {
          stop(paste("Error! \"", `status`, "\" cannot be assigned to `status`. Must be \"placed\", \"approved\", \"delivered\".", sep = ""))
        }
        if (!(is.character(`status`) && length(`status`) == 1)) {
          stop(paste("Error! Invalid data for `status`. Must be a string:", `status`))
        }
        self$`status` <- `status`
      }
      if (!is.null(`complete`)) {
        if (!(is.logical(`complete`) && length(`complete`) == 1)) {
          stop(paste("Error! Invalid data for `complete`. Must be a boolean:", `complete`))
        }
        self$`complete` <- `complete`
      }
    },

    #' @description
    #' Convert to an R object. This method is deprecated. Use `toSimpleType()` instead.
    toJSON = function() {
      .Deprecated(new = "toSimpleType", msg = "Use the '$toSimpleType()' method instead since that is more clearly named. Use '$toJSONString()' to get a JSON string")
      return(self$toSimpleType())
    },

    #' @description
    #' Convert to a List
    #'
    #' Convert the R6 object to a list to work more easily with other tooling.
    #'
    #' @return Order as a base R list.
    #' @examples
    #' # convert array of Order (x) to a data frame
    #' \dontrun{
    #' library(purrr)
    #' library(tibble)
    #' df <- x |> map(\(y)y$toList()) |> map(as_tibble) |> list_rbind()
    #' df
    #' }
    toList = function() {
      return(self$toSimpleType())
    },

    #' @description
    #' Convert Order to a base R type
    #'
    #' @return A base R type, e.g. a list or numeric/character array.
    toSimpleType = function() {
      OrderObject <- list()
      if (!is.null(self$`id`)) {
        OrderObject[["id"]] <-
          self$`id`
      }
      if (!is.null(self$`petId`)) {
        OrderObject[["petId"]] <-
          self$`petId`
      }
      if (!is.null(self$`quantity`)) {
        OrderObject[["quantity"]] <-
          self$`quantity`
      }
      if (!is.null(self$`shipDate`)) {
        OrderObject[["shipDate"]] <-
          self$`shipDate`
      }
      if (!is.null(self$`status`)) {
        OrderObject[["status"]] <-
          self$`status`
      }
      if (!is.null(self$`complete`)) {
        OrderObject[["complete"]] <-
          self$`complete`
      }
      return(OrderObject)
    },

    #' @description
    #' Deserialize JSON string into an instance of Order
    #'
    #' @param input_json the JSON input
    #' @return the instance of Order
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`id`)) {
        self$`id` <- this_object$`id`
      }
      if (!is.null(this_object$`petId`)) {
        self$`petId` <- this_object$`petId`
      }
      if (!is.null(this_object$`quantity`)) {
        self$`quantity` <- this_object$`quantity`
      }
      if (!is.null(this_object$`shipDate`)) {
        self$`shipDate` <- this_object$`shipDate`
      }
      if (!is.null(this_object$`status`)) {
        if (!is.null(this_object$`status`) && !(this_object$`status` %in% c("placed", "approved", "delivered"))) {
          stop(paste("Error! \"", this_object$`status`, "\" cannot be assigned to `status`. Must be \"placed\", \"approved\", \"delivered\".", sep = ""))
        }
        self$`status` <- this_object$`status`
      }
      if (!is.null(this_object$`complete`)) {
        self$`complete` <- this_object$`complete`
      }
      self
    },

    #' @description
    #' To JSON String
    #' 
    #' @param ... Parameters passed to `jsonlite::toJSON`
    #' @return Order in JSON format
    toJSONString = function(...) {
      simple <- self$toSimpleType()
      json <- jsonlite::toJSON(simple, auto_unbox = TRUE, digits = NA, ...)
      return(as.character(jsonlite::minify(json)))
    },

    #' @description
    #' Deserialize JSON string into an instance of Order
    #'
    #' @param input_json the JSON input
    #' @return the instance of Order
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`id` <- this_object$`id`
      self$`petId` <- this_object$`petId`
      self$`quantity` <- this_object$`quantity`
      self$`shipDate` <- this_object$`shipDate`
      if (!is.null(this_object$`status`) && !(this_object$`status` %in% c("placed", "approved", "delivered"))) {
        stop(paste("Error! \"", this_object$`status`, "\" cannot be assigned to `status`. Must be \"placed\", \"approved\", \"delivered\".", sep = ""))
      }
      self$`status` <- this_object$`status`
      self$`complete` <- this_object$`complete`
      self
    },

    #' @description
    #' Validate JSON input with respect to Order and throw an exception if invalid
    #'
    #' @param input the JSON input
    validateJSON = function(input) {
      input_json <- jsonlite::fromJSON(input)
    },

    #' @description
    #' To string (JSON format)
    #'
    #' @return String representation of Order
    toString = function() {
      self$toJSONString()
    },

    #' @description
    #' Return true if the values in all fields are valid.
    #'
    #' @return true if the values in all fields are valid.
    isValid = function() {
      TRUE
    },

    #' @description
    #' Return a list of invalid fields (if any).
    #'
    #' @return A list of invalid fields (if any).
    getInvalidFields = function() {
      invalid_fields <- list()
      invalid_fields
    },

    #' @description
    #' Print the object
    print = function() {
      print(jsonlite::prettify(self$toJSONString()))
      invisible(self)
    }
  ),
  # Lock the class to prevent modifications to the method or field
  lock_class = TRUE
)
## Uncomment below to unlock the class to allow modifications of the method or field
# Order$unlock()
#
## Below is an example to define the print function
# Order$set("public", "print", function(...) {
#   print(jsonlite::prettify(self$toJSONString()))
#   invisible(self)
# })
## Uncomment below to lock the class to prevent modifications to the method or field
# Order$lock()

