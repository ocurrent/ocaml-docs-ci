FROM alpine:3.18
RUN apk add --no-cache \
  openssh-client \
  ca-certificates \
  bash
ADD ./docker/init/init.sh /
CMD /init.sh
