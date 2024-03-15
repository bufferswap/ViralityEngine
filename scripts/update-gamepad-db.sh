#!/bin/sh
# Download and install the gamepad mapping database.

set -e

SCRIPT_PATH="$(cd "$(dirname "$0")" ; pwd -P)"
REMOTE_FILE="https://raw.githubusercontent.com/gabomdq/SDL_GameControllerDB/master/gamecontrollerdb.txt"
LOCAL_FILE="${SCRIPT_PATH}/../data/gamepad-db.txt"
CUSTOM_FILE="${SCRIPT_PATH}/custom-gamepad-db.txt"

echo -n "Updating gamepad database ... "
wget -q ${REMOTE_FILE} -O ${LOCAL_FILE}
echo "DONE"

echo -n "Appending custom definitions ... "
cat < ${CUSTOM_FILE} >> ${LOCAL_FILE}
echo "DONE"

exit 0
