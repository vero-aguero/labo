#!/bin/bash

function send_message_to_telegram {
     MESSAGE=$1
     TELEGRAM_TOKEN="5416550516:AAFOsQW_FeZP4a2PXBw5pISjouysvrSWHWk"
     TELEGRAM_CHATID=-873766049
     curl -X POST \
     -H 'Content-Type: application/json' \
     -d "{\"chat_id\": \"${TELEGRAM_CHATID}\", \"text\": \"${MESSAGE}\"}" \
     https://api.telegram.org/bot${TELEGRAM_TOKEN}/sendMessage
}

function poweroff_vm {
     sleep 60
     sudo poweroff
}

function execute_script {
     SCRIPT_NAME=$1
     echo "`date`> Ejecutando ${SCRIPT_NAME}.r"
     time Rscript ${SCRIPT_NAME}.r > ${SCRIPT_NAME}.log 2>&1
     output = $?
     if [[ ${output} -ge 1 ]]; then
          send_message_to_telegram "Script Failed: ${SCRIPT_NAME}"
          poweroff_vm
     fi
}

execute_script "FE_tercera_competencia"
execute_script "Training_Strategy_tercera_competencia_20192021"
execute_script "z942_HT_lightgbm_under_modificado_20192021"
execute_script "z992_lightgbm_under_modificado_20192021"

send_message_to_telegram "Scripts finished successfully v5"

poweroff_vm
