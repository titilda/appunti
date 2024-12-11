FROM pandoc/core:3.5
USER root
COPY build_docs_v2.py /root/build_docs_v2.py
RUN apk add --update sudo tar bash git python3 ttf-freefont && chmod +x /root/build_docs_v2.py
WORKDIR /
ENTRYPOINT ["/bin/bash"]