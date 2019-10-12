FROM fpco/stack-build:lts-12.4
ADD data data
RUN stack install
EXPOSE 5001
ENTRYPOINT ./gitit