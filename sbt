#!/usr/bin/env bash
#
# A more capable sbt runner, coincidentally also called sbt.
# Author: Paul Phillips <paulp@typesafe.com>

# todo - make this dynamic
declare -r sbt_release_version=0.12.2
declare -r sbt_snapshot_version=0.13.0-SNAPSHOT

declare sbt_jar sbt_dir sbt_create sbt_snapshot sbt_launch_dir
declare scala_version java_home sbt_explicit_version
declare verbose debug quiet noshare batch trace_level log_level

for arg in "$@"; do
  case $arg in
    -q|-quiet)  quiet=true ;;
            *)             ;;
  esac
done

build_props_sbt () {
  if [[ -r project/build.properties ]]; then
    versionLine=$(grep ^sbt.version project/build.properties)
    versionString=${versionLine##sbt.version=}
    echo "$versionString"
  fi
}

update_build_props_sbt () {
  local ver="$1"
  local old=$(build_props_sbt)

  if [[ $ver == $old ]]; then
    return
  elif [[ -r project/build.properties ]]; then
    perl -pi -e "s/^sbt\.version=.*\$/sbt.version=${ver}/" project/build.properties
    grep -q '^sbt.version=' project/build.properties || echo "sbt.version=${ver}" >> project/build.properties

    echo !!!
    echo !!! Updated file project/build.properties setting sbt.version to: $ver
    echo !!! Previous value was: $old
    echo !!!
  fi
}

sbt_version () {
  if [[ -n $sbt_explicit_version ]]; then
    echo $sbt_explicit_version
  else
    local v=$(build_props_sbt)
    if [[ -n $v ]]; then
      echo $v
    else
      echo $sbt_release_version
    fi
  fi
}

echoerr () {
  [[ -z $quiet ]] && echo "$@" >&2
}
vlog () {
  [[ $verbose || $debug ]] && echoerr "$@"
}
dlog () {
  [[ $debug ]] && echoerr "$@"
}

# this seems to cover the bases on OSX, and someone will
# have to tell me about the others.
get_script_path () {
  local path="$1"
  [[ -L "$path" ]] || { echo "$path" ; return; }

  local target=$(readlink "$path")
  if [[ "${target:0:1}" == "/" ]]; then
    echo "$target"
  else
    echo "$(dirname $path)/$target"
  fi
}

die() {
  echo "Aborting: $@"
  exit 1
}

make_url () {
  groupid="$1"
  category="$2"
  version="$3"

  echo "http://typesafe.artifactoryonline.com/typesafe/ivy-$category/$groupid/sbt-launch/$version/sbt-launch.jar"
}

declare -r default_jvm_opts="-Dfile.encoding=UTF8 -XX:MaxPermSize=256m -Xms512m -Xmx1g -XX:+CMSClassUnloadingEnabled -XX:+UseConcMarkSweepGC"
declare -r noshare_opts="-Dsbt.global.base=project/.sbtboot -Dsbt.boot.directory=project/.boot -Dsbt.ivy.home=project/.ivy"
declare -r latest_28="2.8.2"
declare -r latest_29="2.9.3"
declare -r latest_210="2.10.0"

declare -r script_path=$(get_script_path "$BASH_SOURCE")
declare -r script_dir="$(dirname $script_path)"
declare -r script_name="$(basename $script_path)"

# some non-read-onlies set with defaults
declare java_cmd=java
declare sbt_launch_dir="$script_dir/.lib"
declare sbt_universal_launcher="$script_dir/lib/sbt-launch.jar"
declare sbt_jar=$sbt_universal_launcher
declare sbt_opts_file=.sbtopts
declare jvm_opts_file=.jvmopts

# pull -J and -D options to give to java.
declare -a residual_args
declare -a java_args
declare -a scalac_args
declare -a sbt_commands

# args to jvm/sbt via files or environment variables
declare -a extra_jvm_opts extra_sbt_opts

# if set, use JAVA_HOME over java found in path
[[ -e "$JAVA_HOME/bin/java" ]] && java_cmd="$JAVA_HOME/bin/java"

# use ~/.sbt/launch to store sbt jars if script_dir is not writable
[[ -w "$sbt_launch_dir" ]] || sbt_launch_dir="$HOME/.sbt/launch"

build_props_scala () {
  if [[ -r project/build.properties ]]; then
    versionLine=$(grep ^build.scala.versions project/build.properties)
    versionString=${versionLine##build.scala.versions=}
    echo ${versionString%% .*}
  fi
}

execRunner () {
  # print the arguments one to a line, quoting any containing spaces
  [[ $verbose || $debug ]] && echo "# Executing command line:" && {
    for arg; do
      if [[ -n "$arg" ]]; then
        if printf "%s\n" "$arg" | grep -q ' '; then
          printf "\"%s\"\n" "$arg"
        else
          printf "%s\n" "$arg"
        fi
      fi
    done
    echo ""
  }

  if [[ -n $batch ]]; then
    # the only effective way I've found to avoid sbt hanging when backgrounded.
    exec 0<&-
    ( "$@" & )
    # I'm sure there's some way to get our hands on the pid and wait for it
    # but it exceeds my present level of ambition.
  else
    exec "$@"
  fi
}

sbt_groupid () {
  case $(sbt_version) in
        0.7.*) echo org.scala-tools.sbt ;;
       0.10.*) echo org.scala-tools.sbt ;;
    0.11.[12]) echo org.scala-tools.sbt ;;
            *) echo org.scala-sbt ;;
  esac
}

sbt_artifactory_list () {
  local version0=$(sbt_version)
  local version=${version0%-SNAPSHOT}
  local url="http://typesafe.artifactoryonline.com/typesafe/ivy-snapshots/$(sbt_groupid)/sbt-launch/"
  dlog "Looking for snapshot list at: $url "

  curl -s --list-only "$url" | \
    grep -F $version | \
    perl -e 'print reverse <>' | \
    perl -pe 's#^<a href="([^"/]+).*#$1#;'
}

make_release_url () {
  make_url $(sbt_groupid) releases $(sbt_version)
}

# argument is e.g. 0.13.0-SNAPSHOT
# finds the actual version (with the build id) at artifactory
make_snapshot_url () {
  for ver in $(sbt_artifactory_list); do
    local url=$(make_url $(sbt_groupid) snapshots $ver)
    dlog "Testing $url"
    curl -s --head "$url" >/dev/null
    dlog "curl returned: $?"
    echo "$url"
    return
  done
}

jar_url () {
  case $(sbt_version) in
             0.7.*) echo "http://simple-build-tool.googlecode.com/files/sbt-launch-0.7.7.jar" ;;
        *-SNAPSHOT) make_snapshot_url ;;
                 *) make_release_url ;;
  esac
}

jar_file () {
  echo "$sbt_launch_dir/$1/sbt-launch.jar"
}

download_url () {
  local url="$1"
  local jar="$2"

  echo "Downloading sbt launcher $(sbt_version):"
  echo "  From  $url"
  echo "    To  $jar"

  mkdir -p $(dirname "$jar") && {
    if which curl >/dev/null; then
      curl --fail --silent "$url" --output "$jar"
    elif which wget >/dev/null; then
      wget --quiet -O "$jar" "$url"
    fi
  } && [[ -r "$jar" ]]
}

acquire_sbt_jar () {
  sbt_url="$(jar_url)"
  sbt_jar="$(jar_file $(sbt_version))"

  [[ -r "$sbt_jar" ]] || download_url "$sbt_url" "$sbt_jar"
}

usage () {
  cat <<EOM
Usage: $script_name [options]

  -h | -help         print this message
  -v | -verbose      this runner is chattier
  -d | -debug        set sbt log level to Debug
  -q | -quiet        set sbt log level to Error
  -trace <level>     display stack traces with a max of <level> frames (default: -1, traces suppressed)
  -no-colors         disable ANSI color codes
  -sbt-create        start sbt even if current directory contains no sbt project
  -sbt-dir   <path>  path to global settings/plugins directory (default: ~/.sbt/<version>)
  -sbt-boot  <path>  path to shared boot directory (default: ~/.sbt/boot in 0.11+)
  -ivy       <path>  path to local Ivy repository (default: ~/.ivy2)
  -no-share          use all local caches; no sharing
  -offline           put sbt in offline mode
  -jvm-debug <port>  Turn on JVM debugging, open at the given port.
  -batch             Disable interactive mode
  -prompt <expr>     Set the sbt prompt; in expr, 's' is the State and 'e' is Extracted

  # sbt version (default: from project/build.properties if present, else latest release)
  !!! The only way to accomplish this pre-0.12.0 if there is a build.properties file which
  !!! contains an sbt.version property is to update the file on disk.  That's what this does.
  -sbt-version  <version>   use the specified version of sbt
  -sbt-jar      <path>      use the specified jar as the sbt launcher
  -sbt-snapshot             use a snapshot version of sbt
  -sbt-launch-dir <path>    directory to hold sbt launchers (default: $sbt_launch_dir)

  # scala version (default: as chosen by sbt)
  -28                       use $latest_28
  -29                       use $latest_29
  -210                      use $latest_210
  -scala-home <path>        use the scala build at the specified directory
  -scala-version <version>  use the specified version of scala
  -binary-version <version> use the specified scala version when searching for dependencies

  # java version (default: java from PATH, currently $(java -version |& grep version))
  -java-home <path>         alternate JAVA_HOME

  # passing options to the jvm - note it does NOT use JAVA_OPTS due to pollution
  # The default set is used if JVM_OPTS is unset and no -jvm-opts file is found
  <default>        $default_jvm_opts
  JVM_OPTS         environment variable holding jvm args
  -jvm-opts <path> file containing jvm args (if not given, .jvmopts in project root is used if present)
  -Dkey=val        pass -Dkey=val directly to the jvm
  -J-X             pass option -X directly to the jvm (-J is stripped)

  # passing options to sbt, OR to this runner
  SBT_OPTS         environment variable holding sbt args
  -sbt-opts <path> file containing sbt args (if not given, .sbtopts in project root is used if present)
  -S-X             add -X to sbt's scalacOptions (-S is stripped)
EOM
}

addJava () {
  dlog "[addJava] arg = '$1'"
  java_args=( "${java_args[@]}" "$1" )
}
addSbt () {
  dlog "[addSbt] arg = '$1'"
  sbt_commands=( "${sbt_commands[@]}" "$1" )
}
addScalac () {
  dlog "[addScalac] arg = '$1'"
  scalac_args=( "${scalac_args[@]}" "$1" )
}
addResidual () {
  dlog "[residual] arg = '$1'"
  residual_args=( "${residual_args[@]}" "$1" )
}
addResolver () {
  addSbt "set resolvers in ThisBuild += $1"
}
addDebugger () {
  addJava "-Xdebug -Xrunjdwp:transport=dt_socket,server=y,suspend=n,address=$1"
}
setScalaVersion () {
  addSbt "set scalaVersion in ThisBuild := \"$1\""
  if [[ "$1" == *SNAPSHOT* ]]; then
    addResolver Opts.resolver.sonatypeSnapshots
  fi
}

process_args ()
{
  require_arg () {
    local type="$1"
    local opt="$2"
    local arg="$3"

    if [[ -z "$arg" ]] || [[ "${arg:0:1}" == "-" ]]; then
      die "$opt requires <$type> argument"
    fi
  }
  while [[ $# -gt 0 ]]; do
    case "$1" in
       -h|-help) usage; exit 1 ;;
    -v|-verbose) verbose=true && log_level=Info && shift ;;
      -d|-debug) debug=true && log_level=Debug && shift ;;
      -q|-quiet) quiet=true && log_level=Error && shift ;;

         -trace) require_arg integer "$1" "$2" && trace_level=$2 && shift 2 ;;
           -ivy) require_arg path "$1" "$2" && addJava "-Dsbt.ivy.home=$2" && shift 2 ;;
     -no-colors) addJava "-Dsbt.log.noformat=true" && shift ;;
      -no-share) noshare=true && shift ;;
      -sbt-boot) require_arg path "$1" "$2" && addJava "-Dsbt.boot.directory=$2" && shift 2 ;;
       -sbt-dir) require_arg path "$1" "$2" && sbt_dir="$2" && shift 2 ;;
     -debug-inc) addJava "-Dxsbt.inc.debug=true" && shift ;;
       -offline) addSbt "set offline := true" && shift ;;
     -jvm-debug) require_arg port "$1" "$2" && addDebugger $2 && shift 2 ;;
         -batch) batch=true && shift ;;
        -prompt) require_arg "expr" "$1" "$2" && addSbt "set shellPrompt in ThisBuild := (s => { val e = Project.extract(s) ; $2 })" && shift 2 ;;

    -sbt-create) sbt_create=true && shift ;;
  -sbt-snapshot) sbt_explicit_version=$sbt_snapshot_version && shift ;;
       -sbt-jar) require_arg path "$1" "$2" && sbt_jar="$2" && shift 2 ;;
   -sbt-version) require_arg version "$1" "$2" && sbt_explicit_version="$2" && shift 2 ;;
-sbt-launch-dir) require_arg path "$1" "$2" && sbt_launch_dir="$2" && shift 2 ;;
 -scala-version) require_arg version "$1" "$2" && setScalaVersion "$2" && shift 2 ;;
-binary-version) require_arg version "$1" "$2" && addSbt "set scalaBinaryVersion in ThisBuild := \"$2\"" && shift 2 ;;
    -scala-home) require_arg path "$1" "$2" && addSbt "set every scalaHome := Some(file(\"$2\"))" && shift 2 ;;
     -java-home) require_arg path "$1" "$2" && java_cmd="$2/bin/java" && shift 2 ;;
      -sbt-opts) require_arg path "$1" "$2" && sbt_opts_file="$2" && shift 2 ;;
      -jvm-opts) require_arg path "$1" "$2" && jvm_opts_file="$2" && shift 2 ;;

            -D*) addJava "$1" && shift ;;
            -J*) addJava "${1:2}" && shift ;;
            -S*) addScalac "${1:2}" && shift ;;
            -28) addSbt "++ $latest_28" && shift ;;
            -29) addSbt "++ $latest_29" && shift ;;
           -210) addSbt "++ $latest_210" && shift ;;

              *) addResidual "$1" && shift ;;
    esac
  done
}


# process the direct command line arguments
process_args "$@"

# if there are file/environment sbt_opts, process again so we
# can supply args to this runner
if [[ -r "$sbt_opts_file" ]]; then
  readarray -t extra_sbt_opts < "$sbt_opts_file"
elif [[ -n "$SBT_OPTS" ]]; then
  extra_sbt_opts=( $SBT_OPTS )
fi

[[ -n $extra_sbt_opts ]] && process_args "${extra_sbt_opts[@]}"

# reset "$@" to the residual args
set -- "${residual_args[@]}"
argumentCount=$#

# only exists in 0.12+
setTraceLevel() {
  case $(sbt_version) in
     0.{7,10,11}.*) echoerr "Cannot set trace level in sbt version $(sbt_version)" ;;
                 *) addSbt "set every traceLevel := $trace_level" ;;
  esac
}

# set scalacOptions if we were given any -S opts
[[ ${#scalac_args[@]} -eq 0 ]] || addSbt "set scalacOptions in ThisBuild += \"${scalac_args[@]}\""

# Update build.properties no disk to set explicit version - sbt gives us no choice
[[ -n "$sbt_explicit_version" ]] && update_build_props_sbt "$sbt_explicit_version"
echoerr "Detected sbt version $(sbt_version)"

[[ -n "$scala_version" ]] && echo "Overriding scala version to $scala_version"

# no args - alert them there's stuff in here
(( $argumentCount > 0 )) || echo "Starting $script_name: invoke with -help for other options"

# verify this is an sbt dir or -create was given
[[ -r ./build.sbt || -d ./project || -n "$sbt_create" ]] || {
  cat <<EOM
$(pwd) doesn't appear to be an sbt project.
If you want to start sbt anyway, run:
  $0 -sbt-create

EOM
  exit 1
}

# pick up completion if present; todo
[[ -r .sbt_completion.sh ]] && source .sbt_completion.sh

# no jar? download it.
[[ -r "$sbt_jar" ]] || acquire_sbt_jar || {
  # still no jar? uh-oh.
  echo "Download failed. Obtain the jar manually and place it at $sbt_jar"
  exit 1
}

if [[ -n $noshare ]]; then
  addJava "$noshare_opts"
else
  [[ -n "$sbt_dir" ]] || {
    sbt_dir=~/.sbt/$(sbt_version)
    vlog "Using $sbt_dir as sbt dir, -sbt-dir to override."
  }
  addJava "-Dsbt.global.base=$sbt_dir"
fi

if [[ -r "$jvm_opts_file" ]]; then
  readarray -t extra_jvm_opts < "$jvm_opts_file"
elif [[ -n "$JVM_OPTS" ]]; then
  extra_jvm_opts=( $JVM_OPTS )
else
  extra_jvm_opts=( $default_jvm_opts )
fi

# since sbt 0.7 doesn't understand iflast
[[ ${#residual_args[@]} -eq 0 ]] && [[ -z "$batch" ]] && residual_args=( "shell" )

# traceLevel is 0.12+
[[ -n $trace_level ]] && setTraceLevel

[[ -n $log_level ]] && [[ $log_level != Info ]] && logLevalArg="set logLevel in Global := Level.$log_level"

# run sbt
execRunner "$java_cmd" \
  "${extra_jvm_opts[@]}" \
  "${java_args[@]}" \
  -jar "$sbt_jar" \
  "$logLevalArg" \
  "${sbt_commands[@]}" \
  "${residual_args[@]}"
