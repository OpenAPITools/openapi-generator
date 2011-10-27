/**
 * $model.description$
 */
function $className$() {

$fields:{ field |

/**
 * $field.description$
 * $if(field.required)$@Required$endif$
 * $if(field.allowableValues)$[AllowableValues(value="$field.allowedValuesString$"]$endif$
 */
 this.$field.fieldDefinition.name$ = null;$\r$}$


this.parse = function(data) {
    if(data) {
    
$fields:{ field |
$! //$field.fieldDefinition.returnType$ (primitive=$field.fieldDefinition.hasPrimitiveType$) !$
$if(field.fieldDefinition.hasListResponse)$
     $if(field.fieldDefinition.hasPrimitiveType)$
         this.$field.fieldDefinition.name$ = data.$field.fieldDefinition.name$;
     $else$
         this.$field.fieldDefinition.name$ = $field.fieldDefinition.collectionItemType$Object.parseArray(data.$field.fieldDefinition.name$);
     $endif$
$elseif(field.fieldDefinition.hasSetResponse)$
     $if(field.fieldDefinition.hasPrimitiveType)$
         this.$field.fieldDefinition.name$ = data.$field.fieldDefinition.name$;
     $else$
         this.$field.fieldDefinition.name$ = $field.fieldDefinition.collectionItemType$Collection.get(data.$field.fieldDefinition.name$);
     $endif$
$elseif(field.fieldDefinition.hasMapResponse)$
     this.$field.fieldDefinition.name$ = data.$field.fieldDefinition.name$;
$elseif(field.fieldDefinition.hasDateResponse)$
        this.$field.fieldDefinition.name$ = new Date(Date.parse(data.$field.fieldDefinition.name$));
$else$
     $if(field.fieldDefinition.hasPrimitiveType)$
         this.$field.fieldDefinition.name$ = data.$field.fieldDefinition.name$;
     $else$
         this.$field.fieldDefinition.name$ = new $field.fieldDefinition.returnType$(); this.$field.fieldDefinition.name$.parse(data.$field.fieldDefinition.name$);
     $endif$
$endif$
}$
    }

    }
}

var $className$Object = new function() {
    this.parseArray = function(arrayData) {
        var responseArray = new Array();

        if(arrayData != null && arrayData.length > 0) {
            for(i = 0; i < arrayData.length; i++) {
                var o = new $className$();
                o.parse(arrayData[i]);
                responseArray.push(o);
            }
        }

        return responseArray;
    }
}

