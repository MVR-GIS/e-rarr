#!/usr/bin/env bash
aws-adfs login --adfs-host=sts1.cwbi.us --provider-id=urn:amazon:webservices:govcloud --region us-gov-west-1 --profile err-reports-test --role-arn arn:aws-us-gov:iam::379454761305:role/cwbi.us-DA-Planning-access-Test --no-sspi
aws --profile err-reports-test ecr get-login-password --region us-gov-west-1 | docker login --username AWS --password-stdin 379454761305.dkr.ecr.us-gov-west-1.amazonaws.com

docker build -t erarr:test .
docker tag erarr:test 379454761305.dkr.ecr.us-gov-west-1.amazonaws.com/err-reports-test:test
docker push 379454761305.dkr.ecr.us-gov-west-1.amazonaws.com/err-reports-test:test
