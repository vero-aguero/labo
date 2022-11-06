#!/bin/bash

TELEGRAM_TOKEN="598321297:AAEPsxPEB5fw-EHz8tLy2anZcIKCYuIUls4"
TELEGRAM_CHATID=-343518064

date
time Rscript z941_HT_lightgbm_sinCanaritos.r > z941_HT_lightgbm_sinCanaritos.log 2>&1
date



curl -X POST \
     -H 'Content-Type: application/json' \
     -d "{\"chat_id\": \"${TELEGRAM_CHATID}\", \"text\": \"Script finished\"}" \
     https://api,telegram,org/bot${TELEGRAM_TOKEN}/sendMessage

sleep 60

sudo poweroff
