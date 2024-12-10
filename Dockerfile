FROM pandoc/core:3.5-ubuntu
RUN apt update && apt install -y language-pack-it nodejs npm git git-restore-mtime && npm install --global mermaid-filter

WORKDIR /workspace
CMD ["/bin/bash"]