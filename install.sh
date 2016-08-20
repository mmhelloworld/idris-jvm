#!/bin/sh

ensureJavaHome () {
  if [ -z "$IDRIS_JAVA_HOME" ]; then
    if [ -z "$JAVA_HOME" ]; then
      echo "Please set environment variable IDRIS_JAVA_HOME or JAVA_HOME"
      exit 1
    else
      export IDRIS_JAVA_HOME="$JAVA_HOME"
    fi
  fi
}

downloadRuntimeJar () {
  if [ ! -f $RTJAR ]; then
    mkdir -p $HOME/.idrisjvm
    wget https://github.com/mmhelloworld/idrisjvm-runtime/releases/download/$RTVERSION/idrisjvm-runtime-$RTVERSION.jar -O $RTJAR
  fi
}

installIdrisRuntime () {
  cd runtime
  idris --install idris-jvm-runtime.ipkg
  cd ..
}

main () {
  ensureJavaHome
  downloadRuntimeJar
  stack --stack-yaml setup/stack.yaml build
  stack --stack-yaml setup/stack.yaml exec setup-exe
  stack install
  installIdrisRuntime
}

RTVERSION="1.0-SNAPSHOT"
RTJAR=$HOME/.idrisjvm/idrisjvm-runtime-$RTVERSION.jar

main
