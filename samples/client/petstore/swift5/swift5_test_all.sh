#!/bin/bash
set -e

DIRECTORY=`dirname $0`

# example project with unit tests
(cd $DIRECTORY/alamofireLibrary/SwaggerClientTests/ && ./run_xcodebuild.sh)
(cd $DIRECTORY/asyncAwaitLibrary/SwaggerClientTests/ && ./run_xcodebuild.sh)
(cd $DIRECTORY/combineLibrary/SwaggerClientTests/ && ./run_xcodebuild.sh)
(cd $DIRECTORY/default/SwaggerClientTests/ && ./run_xcodebuild.sh)
(cd $DIRECTORY/promisekitLibrary/SwaggerClientTests/ && ./run_xcodebuild.sh)
(cd $DIRECTORY/rxswiftLibrary/SwaggerClientTests/ && ./run_xcodebuild.sh)
(cd $DIRECTORY/urlsessionLibrary/SwaggerClientTests/ && ./run_xcodebuild.sh)

# spm build
(cd $DIRECTORY/alamofireLibrary/ && ./run_spmbuild.sh)
(cd $DIRECTORY/anycodableLibrary/ && ./run_spmbuild.sh)
(cd $DIRECTORY/asyncAwaitLibrary/ && ./run_spmbuild.sh)
(cd $DIRECTORY/combineLibrary/ && ./run_spmbuild.sh)
(cd $DIRECTORY/default/ && ./run_spmbuild.sh)
(cd $DIRECTORY/deprecated/ && ./run_spmbuild.sh)
(cd $DIRECTORY/frozenEnums/ && ./run_spmbuild.sh)
(cd $DIRECTORY/nonPublicApi/ && ./run_spmbuild.sh)
(cd $DIRECTORY/objcCompatible/ && ./run_spmbuild.sh)
(cd $DIRECTORY/oneOf/ && ./run_spmbuild.sh)
(cd $DIRECTORY/promisekitLibrary/ && ./run_spmbuild.sh)
(cd $DIRECTORY/readonlyProperties/ && ./run_spmbuild.sh)
(cd $DIRECTORY/resultLibrary/ && ./run_spmbuild.sh)
(cd $DIRECTORY/rxswiftLibrary/ && ./run_spmbuild.sh)
(cd $DIRECTORY/urlsessionLibrary/ && ./run_spmbuild.sh)
#(cd $DIRECTORY/vaporLibrary/ && ./run_spmbuild.sh)
(cd $DIRECTORY/x-swift-hashable/ && ./run_spmbuild.sh)

