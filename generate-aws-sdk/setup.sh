#!/bin/bash

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
CYAN='\033[0;36m'
NC='\033[0m'

INPUT_PATH=api-models-aws-main/models
OUTPUT_PATH=./output

SELECTED_SDK_IDS="s3"

if [ ! -d $INPUT_PATH ]; then
    echo -e "${CYAN}Download AWS models from:${NC} https://github.com/aws/api-models-aws"
    wget https://github.com/aws/api-models-aws/archive/refs/heads/main.zip -O api-models-aws.zip
    unzip -q api-models-aws.zip
    rm api-models-aws.zip
    echo "OK"
fi

function setup_project() {
    echo -e "${YELLOW}Model File:${NC} $MODEL_FILE"
    version=$(echo $MODEL_FILE|awk -F'/' '{print $5}')
    echo -e "${YELLOW}Version:${NC} $version"

    service_json=$(jq '.shapes | to_entries[] | select(.value.type == "service")' "$MODEL_FILE")
    model_key=$(echo $service_json|jq -r '.key')
    model_type=$(echo $service_json|jq -r '.value.type')
    echo -e "${YELLOW}Model:${NC} $model_key ($model_type)"

    namespace=$(echo $model_key|awk -F'#' '{print $1}')
    model_name=$(echo $model_key|awk -F'#' '{print $2}')
    echo -e "${YELLOW}- Namespace:${NC} $namespace"
    echo -e "${YELLOW}- Name:${NC} $model_name"

    # Convert CamelCase $service_name to snake_case $module_name
    module_name=$(echo "$model_name" | sed -r 's/([A-Z])/_\1/g' | sed -r 's/^_//' | awk '{print tolower($0)}'|sed 's/a_w_s/aws/')
    echo -e "${YELLOW}Erlang Module Name:${NC} $module_name"

    output_dir=$OUTPUT_PATH/$sdk_id
    echo -e "${YELLOW}Output Directory:${NC} $output_dir"

    model_dirname=model
    generated_dirname=src

    # setup project
    mkdir -p $output_dir/$model_dirname
    mkdir -p $output_dir/$generated_dirname

    # copy model file to model directory
    cp $MODEL_FILE $output_dir/model/

    # create smithy-build.json
    build_json=$output_dir/smithy-build.json

  cat <<EOT > $build_json
{
  "version": "1.0",
  "sources": ["${model_dirname}"],
  "maven": {
    "dependencies": [
      "software.amazon.smithy:smithy-aws-traits:1.64.0",
      "software.amazon.smithy:smithy-aws-endpoints:1.64.0",
      "io.smithy.erlang:smithy-erlang:0.1.0"
    ],
    "repositories": [
      {
        "url": "https://repo1.maven.org/maven2"
      },
      {
        "url": "file://\${user.home}/.m2/repository"
      }
    ]
  },
  "plugins": {
    "erlang-codegen": {
      "service": "$namespace#$model_name",
      "module": "${module_name}",
      "outputDir": "${generated_dirname}"
    }
  }
}
EOT

    # yq "." $build_json

    tree $output_dir
}

for sdk_id in $SELECTED_SDK_IDS; do
    echo
    echo -e "${CYAN}Processing:${NC} $sdk_id"

    PATH_FILTER=*/${sdk_id}/service/*
    # PATH_FILTER=*
    NAME_FILTER=${sdk_id}-*.json

    for MODEL_FILE in $(find $INPUT_PATH -path "$PATH_FILTER" -name "$NAME_FILTER"); do

        setup_project $MODEL_FILE $sdk_id

    done

done
