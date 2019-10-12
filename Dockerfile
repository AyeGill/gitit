FROM fpco/stack-build:lts-12.4
WORKDIR /app
COPY . .
RUN stack setup
RUN stack install
EXPOSE 5001
ENTRYPOINT ./gitit
CMD ["gitit"]