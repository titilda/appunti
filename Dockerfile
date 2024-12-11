FROM pandoc/core:3.5
ENV CHROME_BIN="/usr/bin/chromium-browser" \
    PUPPETEER_SKIP_CHROMIUM_DOWNLOAD="true"
USER root
COPY build_docs.sh /root/build_docs.sh
RUN apk add --update udev sudo bash git python3 ttf-freefont chromium npm && npm install -g mermaid-filter@1.4.5 --unsafe-perm=true && chmod +x /root/build_docs.sh
WORKDIR /
ENTRYPOINT ["/bin/bash"]