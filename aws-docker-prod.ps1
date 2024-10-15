#!/usr/bin/env bash
aws-adfs login --adfs-host=sts1.cwbi.us --provider-id=urn:amazon:webservices:govcloud --region us-gov-west-1 --profile err-reports-prod --role-arn arn:aws-us-gov:iam::379454761305:role/cwbi.us-DA-Planning-access-Prod --no-sspi
aws --profile err-reports-prod ecr get-login-password --region us-gov-west-1 | docker login --username AWS --password-stdin 379454761305.dkr.ecr.us-gov-west-1.amazonaws.com

docker build -t erarr:prod .
docker tag erarr:prod 379454761305.dkr.ecr.us-gov-west-1.amazonaws.com/err-reports-prod:prod
docker push 379454761305.dkr.ecr.us-gov-west-1.amazonaws.com/err-reports-prod:prod
