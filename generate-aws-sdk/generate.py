#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.12"
# dependencies = [
#   "jq"
# ]
# ///

import os
import concurrent.futures
import glob
import jq
import json
import re
import shutil
import subprocess
import time

INPUT_PATH = "api-models-aws-main/models"
OUTPUT_PATH = "output"
MODEL_DIRNAME = "model"
GENERATED_DIRNAME = "src"


def generate(sdk_id: str):

    start_time = time.time()

    def build(path: str):
        command = f"cd {path} && smithy build"
        build_log = f"{path}/smithy-build.log"
        with open(build_log, "w+") as file:
            process = subprocess.Popen(command, shell=True, stdout=file, stderr=file)
        process.wait()
        return process.returncode, build_log

    def exec(command: str):
        result = subprocess.run(
            command, encoding="utf-8", shell=True, text=True, capture_output=True
        )
        return result.returncode, result.stdout, result.stderr

    def snake_case(x):
        pattern = re.compile(r"(?<!^)(?=[A-Z])")
        return (
            pattern.sub("_", x)
            .lower()
            .replace("a_w_s", "aws")
            .replace("e_c2", "ec2")
            .replace("_d_b", "_db")
        )

    result = {}

    path = os.path.join(INPUT_PATH, sdk_id)
    json_files = glob.glob(f"{path}/**/*.json", recursive=True)
    if not json_files:
        print(f"No JSON files found for {sdk_id}")
        return

    source_path = json_files[0]
    with open(source_path, "r") as file:
        data = json.load(file)

        query = '.shapes | to_entries[] | select(.value.type == "service")|.key'
        key = jq.compile(query).input_value(data).first()
        result["model"] = source_path
        result["key"] = key

        model_name = key.split("#")[1]
        module_name = snake_case(model_name)

        build_json = {
            "version": "1.0",
            "sources": [MODEL_DIRNAME],
            "maven": {
                "dependencies": [
                    "software.amazon.smithy:smithy-aws-traits:1.64.0",
                    "software.amazon.smithy:smithy-aws-endpoints:1.64.0",
                    "software.amazon.smithy:smithy-aws-smoke-test-model:1.64.0",
                    "software.amazon.smithy:smithy-aws-iam-traits:1.64.0",
                    "io.smithy.erlang:smithy-erlang:0.1.0",
                ],
                "repositories": [
                    {"url": "https://repo1.maven.org/maven2"},
                    {"url": "file://${user.home}/.m2/repository"},
                ],
            },
            "plugins": {
                "erlang-codegen": {
                    "service": key,
                    "module": module_name,
                    "outputDir": GENERATED_DIRNAME,
                }
            },
        }

        output_path = f"{OUTPUT_PATH}/{sdk_id}"
        build_json_path = f"{output_path}/smithy-build.json"
        model_path = f"{output_path}/{MODEL_DIRNAME}"

        result["path"] = output_path

        os.makedirs(output_path, exist_ok=True)
        os.makedirs(model_path, exist_ok=True)
        shutil.copy(source_path, model_path)
        json.dump(build_json, open(build_json_path, "w"), indent=2)

    build_result, build_log = build(result["path"])
    result["build"] = "success" if build_result == 0 else "failed"

    if build_result != 0:
        _, output, _ = exec(
            "|".join(
                [
                    f'ag "ERROR" -B 1 -A 12 --max-count 0 {build_log}',
                    # "sed 's/\u2500//g'",
                    # "sed 's/--//g'",
                ]
            )
        )
        with open(f'{result["path"]}/errors.txt', "w") as file:
            file.write(output)

    shutil.rmtree(f'{result["path"]}/build')

    end_time = time.time()
    result["time"] = f"{end_time - start_time:.2f} seconds"
    print(json.dumps(result, indent=2))


def main():
    start_time = time.time()

    sdks = sorted(os.listdir(INPUT_PATH))
    sdks = ["s3", "dynamodb", "ec2", "lambda", "sns", "sqs", "iam", "sts", "route53", "cloudfront", "waf", "organizations"]
    print(f"Processing {len(sdks)} SDKs")

    # This sets the worker count to the minimum of:
    # Number of SDKs to process
    # 4× CPU cores
    # Hard cap of 64 (to avoid memory issues from too many JVM processes)
    max_workers = min(len(sdks), os.cpu_count() * 4, 64)
    print(f"Using {max_workers} workers ({os.cpu_count()} cores available, max {64} workers)")

    with concurrent.futures.ThreadPoolExecutor(max_workers=max_workers) as executor:
        futures = [executor.submit(generate, sdk) for sdk in sdks]
        # Wait for all functions to complete
        concurrent.futures.wait(futures)

    end_time = time.time()
    print(f"Total time: {end_time - start_time:.2f} seconds")

if __name__ == "__main__":
    main()
