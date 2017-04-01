
WORKDIR="../serve-test"

echo "servec $@"

ERROR="$((cargo run $@ > "${WORKDIR}/src/main.rs") 2>&1)"
if [[ $? -ne 0 ]]; then
    echo "${ERROR}"
    exit 1
fi

echo "Running..."

ERROR="$((cd "${WORKDIR}" && cargo run) 2>&1)"
if [[ $? -ne 0 ]]; then
    echo "${ERROR}"
    exit 1
fi
