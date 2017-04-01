#!/usr/bin/env bash

RED=`tput setaf 1`
GREEN=`tput setaf 2`
RESET=`tput sgr0`

run_test() {
    printf "${RESET}Compiling $1... "
    cargo run $1 > /dev/null 2>&1
    if [[ $? -eq 0 ]]; then
        printf "${GREEN}Pass"
    else
        printf "${RED}Fail"
    fi
    echo "${RESET}"
}

run_tests() {
    for f in tests/*.srv; do
        run_test $f
    done
}

run_tests
