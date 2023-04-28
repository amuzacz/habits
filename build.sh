#!/bin/bash

shadow-cljs release app

export JAVA_HOME=$(/usr/libexec/java_home -v 1.8.0_201)
export ANDROID_SDK_ROOT=~/Library/Android/sdk
export ANDROID_HOME=~/Library/Android/sdk

cordova build android --debug --device
adb install "platforms/android/app/build/outputs/apk/debug/app-debug.apk"