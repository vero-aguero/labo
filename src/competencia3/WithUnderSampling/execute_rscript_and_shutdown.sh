#!/bin/bash

TELEGRAM_TOKEN="598321297:AAEPsxPEB5fw-EHz8tLy2anZcIKCYuIUls4"
TELEGRAM_CHATID=-343518064

echo "`date`> Ejecutando z906_reparar_dataset.r"
time Rscript z906_reparar_dataset.r > z906_reparar_dataset.log 2>&1
echo "`date`> Ejecutando z914_corregir_drifting.r"
time Rscript z914_corregir_drifting.r > z914_corregir_drifting.log 2>&1
echo "`date`> Ejecutando z925_FE_historia_sinCanaritos.r"
time Rscript z925_FE_historia_sinCanaritos.r > z925_FE_historia_sinCanaritos.log 2>&1
echo "`date`> Ejecutando z931_training_strategy_sinCanaritos.r"
time Rscript z931_training_strategy_sinCanaritos.r > z931_training_strategy_sinCanaritos.log 2>&1
echo "`date`> Ejecutando z941_HT_lightgbm_sinCanaritos.r"
time Rscript z941_HT_lightgbm_sinCanaritos.r > z941_HT_lightgbm_sinCanaritos.log 2>&1
echo "`date`> Ejecutando z991_ZZ_lightgbm_sinCanaritos.r"
time Rscript z991_ZZ_lightgbm_sinCanaritos.r > z991_ZZ_lightgbm_sinCanaritos.log 2>&1
echo "`date`> Ejecutando z925_FE_historia_10Canaritos.r"
time Rscript z925_FE_historia_10Canaritos.r > z925_FE_historia_10canaritos.log 2>&1
echo "`date`> Ejecutando z931_training_strategy_10Canaritos.r"
time Rscript z931_training_strategy_10Canaritos.r > 931_training_strategy_10canaritos.log 2>&1
echo "`date`> Ejecutando z941_HT_lightgbm_10canaritos.r"
time Rscript z941_HT_lightgbm_10Canaritos.r > z941_HT_lightgbm_10canaritos.log 2>&1
echo "`date`> Ejecutando z991_ZZ_lightgbm_10Canaritos.r"
time Rscript z991_ZZ_lightgbm_10Canaritos.r > z991_ZZ_lightgbm_10Canaritos.log 2>&1
echo "`date`> Ejecutando z925_FE_historia_20canaritos.r"
time Rscript z925_FE_historia_20Canaritos.r > z925_FE_historia_20canaritos.log 2>&1
echo "`date`> Ejecutando z931_training_strategy_20Canaritos.r"
time Rscript z931_training_strategy_20Canaritos.r > z931_training_strategy_20canaritos.log 2>&1
echo "`date`> Ejecutando z941_HT_lightgbm_20canaritos.r"
time Rscript z941_HT_lightgbm_20Canaritos.r > z941_HT_lightgbm_20canaritos.log 2>&1
echo "`date`> Ejecutando z991_ZZ_lightgbm_20canaritos.r"
time Rscript z991_ZZ_lightgbm_20Canaritos.r > z991_ZZ_lightgbm_20canaritos.log 2>&1
echo "`date`> Ejecutando z925_FE_historia_30Canaritos.r"
time Rscript z925_FE_historia_30Canaritos.r > z925_FE_historia_30canaritos.log 2>&1
echo "`date`> Ejecutando z931_training_strategy_30Canaritos.r"
time Rscript z931_training_strategy_30Canaritos.r > z931_training_strategy_30canaritos.log 2>&1
echo "`date`> Ejecutando z941_HT_lightgbm_30canaritos.r"
time Rscript z941_HT_lightgbm_30Canaritos.r > z941_HT_lightgbm_30canaritos.log 2>&1
echo "`date`> Ejecutando z991_ZZ_lightgbm_30canaritos.r"
time Rscript z991_ZZ_lightgbm_30Canaritos.r > z991_ZZ_lightgbm_30canaritos.log 2>&1
echo "`date` FIN"

curl -X POST \
     -H 'Content-Type: application/json' \
     -d "{\"chat_id\": \"${TELEGRAM_CHATID}\", \"text\": \"Script finished\"}" \
     https://api,telegram,org/bot${TELEGRAM_TOKEN}/sendMessage

sleep 60

sudo poweroff
