#!bin/bash 
grep --include=\*.{yaml,json} -lrnw $1 -e "openapi\":.*\"[0-9].[0-9].[0-9]" |\
sed -E 's/(.*\/([A-z\-]*)((\.[A-z0-9]{0,2})?\.[A-z]{4}))(.*)/\1 \2 \3/g' | xargs -n 3 sh -c 'java -jar '\
'/opt/openapi-generator/modules/openapi-generator-cli/target/openapi-generator-cli.jar generate -i "$0"'\
' --type-mappings string=types.String'\
' --type-mappings float64=types.Float64'\
' --type-mappings float32=types.Float64'\
' --type-mappings bool=types.Bool'\
' --type-mappings int=types.Int'\
' --type-mappings int32=types.Int64'\
' --type-mappings int64=types.Int64'\
' --type-mappings time.Time=types.Datetime'\
' --import-mappings types.String=github.com/AfterShip/gopkg/facility/types'\
' --import-mappings types.Float64=github.com/AfterShip/gopkg/facility/types'\
' --import-mappings types.Bool=github.com/AfterShip/gopkg/facility/types'\
' --import-mappings types.Int=github.com/AfterShip/gopkg/facility/types'\
' --import-mappings types.Int64=github.com/AfterShip/gopkg/facility/types'\
' --import-mappings types.Datetime=github.com/AfterShip/gopkg/facility/types'\
' --global-property=supportingFiles,models,apis'\
' --package-name gomodels'\
' -g go-aftership'\
' -o /output/"$1"'