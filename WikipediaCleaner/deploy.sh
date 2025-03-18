#! /bin/bash

keyfile="resources/toolforge/toolforge"
local_dist="build/dist"
toolforge_deploy="/data/project/wpcleaner/public_html"
toolforge_host="login.toolforge.org"
toolforge_tmp="/data/project/wpcleaner/tools/tmp"
toolforge_user="nicov"

if ! [[ -f "${keyfile}" ]]; then
  echo "No file with private key found at ${keyfile}"
  exit 1
fi
if ! [[ -d "${local_dist}" ]]; then
  echo "Distribution folder is missing, please build before deploying"
  exit 1
fi
if ! [[ -d "${local_dist}/install" ]]; then
  echo "Installer folder is missing, please build before deploying"
  exit 1
fi

echo "Copy new installer in temporary folder on toolforge"
ssh -i "${keyfile}" -l "${toolforge_user}" "${toolforge_host}" \
  "rm -Rf ${toolforge_tmp}/install; mkdir ${toolforge_tmp}/install"
scp -r -i "${keyfile}" -C -q \
  "${local_dist}/install" \
  "${toolforge_user}@${toolforge_host}:${toolforge_tmp}"

echo "Copy new test version in temporary folder on toolforge"
ssh -i "${keyfile}" -l "${toolforge_user}" "${toolforge_host}" \
  "rm -Rf ${toolforge_tmp}/getdown-test; mkdir ${toolforge_tmp}/getdown-test"
scp -r -i "${keyfile}" -C -q \
  "${local_dist}/getdown-test" \
  "${toolforge_user}@${toolforge_host}:${toolforge_tmp}"

echo "Copy new version in temporary folder on toolforge"
ssh -i "${keyfile}" -l "${toolforge_user}" "${toolforge_host}" \
  "rm -Rf ${toolforge_tmp}/getdown; mkdir ${toolforge_tmp}/getdown"
scp -r -i "${keyfile}" -C -q \
  "${local_dist}/getdown" \
  "${toolforge_user}@${toolforge_host}:${toolforge_tmp}"

echo "Activate new test version on toolforge"
ssh -i "${keyfile}" -l "${toolforge_user}" "${toolforge_host}" \
  "rm -Rf ${toolforge_deploy}/wpcleaner-test; mv ${toolforge_tmp}/getdown-test/ ${toolforge_deploy}/wpcleaner-test"

echo "Activate new installer on toolforge"
ssh -i "${keyfile}" -l "${toolforge_user}" "${toolforge_host}" \
  "cp -f ${toolforge_tmp}/install/* ${toolforge_deploy}/install"

echo "Activate new version on toolforge"
answer="unknown"
read -n 1 -r -p "Do you want to deploy WPCleaner? " answer
echo
if [[ "${answer^^}" == "Y" ]]; then
  ssh -i "${keyfile}" -l "${toolforge_user}" "${toolforge_host}" \
    "rm -Rf ${toolforge_deploy}/wpcleaner; mv ${toolforge_tmp}/getdown/ ${toolforge_deploy}/wpcleaner"
fi
