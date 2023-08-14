FROM fpco/stack-build-small:lts
RUN stack setup 
ADD . / build/
RUN (cd build/ && stack install --flag texmath:server)
EXPOSE 3000
ENTRYPOINT ["texmath-server", "-p", "3000"]
