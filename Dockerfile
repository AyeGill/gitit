FROM fpco/stack-build:lts-12.4
ADD data data
ADD bin/gitit gitit
EXPOSE 5001
ENTRYPOINT ./gitit -f my.conf