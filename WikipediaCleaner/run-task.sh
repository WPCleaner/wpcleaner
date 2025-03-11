#! /bin/bash

wpcleaner_jar="build/dist/full/WikipediaCleaner.jar"
if ! [[ -f "${wpcleaner_jar}" ]]; then
  echo "WPCleaner is missing at ${wpcleaner_jar}, please build"
  exit 1
fi

credentials="resources/credentials.txt"
if ! [[ -f "${credentials}" ]]; then
  echo "Credentials file is missing at ${credentials}, please create it"
  exit 1
fi

language="$1"
if [[ "${language}" == "" ]]; then
  echo "You must select a language"
  exit 1
fi

task="$2"
if [[ "${task}" == "" ]]; then
  echo "You must select a task"
  exit 1
fi

tasks_dir="resources/tasks/${language}wiki"
if ! [[ -d "${tasks_dir}" ]]; then
  echo "Task folder not found"
  exit 1
fi
if ! [[ -f "${tasks_dir}/${task}" ]]; then
  echo "Task is missing at ${tasks_dir}/${task}, please build"
  exit 1
fi

echo "Executing task ${tasks_dir}/${task}"
java -cp "${wpcleaner_jar}" org.wikipediacleaner.Bot -credentials "${credentials}" "${language}" DoTasks "${tasks_dir}/${task}"
