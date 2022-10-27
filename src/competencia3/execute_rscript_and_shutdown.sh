#!/bin/bash

TELEGRAM_TOKEN="598321297:AAEPsxPEB5fw-EHz8tLy2anZcIKCYuIUls4"
TELEGRAM_CHATID=-343518064

Rscript z942_HT_lightgbm_under.r > z942_HT_lightgbm_under.log 2>&1 

curl -X POST \
     -H 'Content-Type: application/json' \
     -d "{\"chat_id\": \"${TELEGRAM_CHATID}\", \"text\": \"Script finished\"}" \
     https://api.telegram.org/bot${TELEGRAM_TOKEN}/sendMessage

sleep 60

shutdown -h now
