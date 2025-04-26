#!/bin/bash
set -e

DIRECTORY=`dirname $0`

# spm build
(cd $DIRECTORY/alamofireLibrary/ && ./run_spmbuild.sh)
(cd $DIRECTORY/apiNonStaticMethod/ && ./run_spmbuild.sh)
(cd $DIRECTORY/asyncAwaitLibrary/ && ./run_spmbuild.sh)
(cd $DIRECTORY/combineLibrary/ && ./run_spmbuild.sh)
(cd $DIRECTORY/combineDeferredLibrary/ && ./run_spmbuild.sh)
(cd $DIRECTORY/default/ && ./run_spmbuild.sh)
(cd $DIRECTORY/objcCompatible/ && ./run_spmbuild.sh)
(cd $DIRECTORY/oneOf/ && ./run_spmbuild.sh)
# (cd $DIRECTORY/promisekitLibrary/ && ./run_spmbuild.sh) # Commented to save time in CI, building Swift5 and Swift6 is taking too much time, and making CI fail
(cd $DIRECTORY/resultLibrary/ && ./run_spmbuild.sh)
(cd $DIRECTORY/rxswiftLibrary/ && ./run_spmbuild.sh)
(cd $DIRECTORY/urlsessionLibrary/ && ./run_spmbuild.sh)
(cd $DIRECTORY/validation/ && ./run_spmbuild.sh)
# (cd $DIRECTORY/vaporLibrary/ && ./run_spmbuild.sh) # Commented because it's not working

# example project with unit tests
(cd $DIRECTORY/alamofireLibrary/SwaggerClientTests/ && ./run_xcodebuild.sh)
(cd $DIRECTORY/apiNonStaticMethod/SwaggerClientTests/ && ./run_xcodebuild.sh)
(cd $DIRECTORY/asyncAwaitLibrary/SwaggerClientTests/ && ./run_xcodebuild.sh)
# (cd $DIRECTORY/combineLibrary/SwaggerClientTests/ && ./run_xcodebuild.sh) # Commented to save time in CI, building Swift5 and Swift6 is taking too much time, and making CI fail
(cd $DIRECTORY/combineDeferredLibrary/SwaggerClientTests/ && ./run_xcodebuild.sh)
(cd $DIRECTORY/default/SwaggerClientTests/ && ./run_xcodebuild.sh)
# (cd $DIRECTORY/promisekitLibrary/SwaggerClientTests/ && ./run_xcodebuild.sh) # Commented to save time in CI, building Swift5 and Swift6 is taking too much time, and making CI fail
(cd $DIRECTORY/rxswiftLibrary/SwaggerClientTests/ && ./run_xcodebuild.sh)
(cd $DIRECTORY/urlsessionLibrary/SwaggerClientTests/ && ./run_xcodebuild.sh)
