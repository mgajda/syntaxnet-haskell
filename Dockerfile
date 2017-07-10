FROM fedora:latest
MAINTAINER sigrlami <sergey.bushnyak@sigrlami.eu>

RUN dnf -y update && dnf clean all

# TODO: Install Google TensorFlow & SyntaxNet related thing

# Install Haskell related thinds
RUN curl -sSL https://s3.amazonaws.com/download.fpcomplete.com/fedora/23/fpco.repo | tee /etc/yum.repos.d/fpco.repo
RUN dnf -y install stack

# TODO: copy everything we need

RUN stack setup
RUN stack build 