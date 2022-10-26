#!/bin/sh

set -eou pipefail

host=$1; shift

contents=$(printf "\n[default]\naws_access_key_id = %s\naws_secret_access_key = %s\naws_session_token = %s\n" \
       $(aws-vault exec $AWS_PROFILE -j | jq -r .AccessKeyId,.SecretAccessKey,.SessionToken))
aws-vault exec $AWS_PROFILE -- ssh -T $host <<EOF
mkdir -p ~/.aws
chmod 0700 ~/.aws
echo "$contents" > ~/.aws/credentials
EOF
