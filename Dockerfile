FROM pandoc/core:3.5-ubuntu
RUN apt update && apt install -y language-pack-it nodejs npm && npm install --global mermaid-filter

CMD ["bash"]