FROM alpine:3.12.0

RUN apk add --update --no-cache curl ca-certificates bash bash-completion zsh curl git vim ncurses util-linux

ADD petstore-cli /usr/bin/petstore-cli
ADD _petstore-cli /usr/local/share/zsh/site-functions/_petstore-cli
ADD petstore-cli.bash-completion /etc/bash-completion.d/petstore-cli
RUN chmod 755 /usr/bin/petstore-cli

#
# Install oh-my-zsh
#
RUN sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)" || true

#
# Enable bash completion
#
RUN echo '\n\
. /etc/bash_completion\n\
source /etc/bash-completion.d/petstore-cli\n\
' >> ~/.bashrc

#
# Setup prompt
#
RUN echo 'export PS1="[OpenAPI Petstore] \$ "' >> ~/.bashrc
RUN echo 'export PROMPT="[OpenAPI Petstore] \$ "' >> ~/.zshrc

#
# Setup a welcome message with basic instruction
#
RUN echo -e 'echo "\
\n\
This Docker provides preconfigured environment for running the command\n\
line REST client for $(tput setaf 6)OpenAPI Petstore$(tput sgr0).\n\
\n\
For convenience, you can export the following environment variables:\n\
\n\
$(tput setaf 3)PETSTORE_HOST$(tput sgr0) - server URL, e.g. https://example.com:8080\n\
$(tput setaf 3)PETSTORE_API_KEY$(tput sgr0) - access token, e.g. "ASDASHJDG63456asdASSD"\n\
$(tput setaf 3)PETSTORE_API_KEY$(tput sgr0) - access token, e.g. "ASDASHJDG63456asdASSD"\n\
$(tput setaf 3)PETSTORE_BASIC_AUTH$(tput sgr0) - basic authentication credentials, e.g.: "username:password"\n\
\n\
$(tput setaf 7)Basic usage:$(tput sgr0)\n\
\n\
$(tput setaf 3)Print the list of operations available on the service$(tput sgr0)\n\
$ petstore-cli -h\n\
\n\
$(tput setaf 3)Print the service description$(tput sgr0)\n\
$ petstore-cli --about\n\
\n\
$(tput setaf 3)Print detailed information about specific operation$(tput sgr0)\n\
$ petstore-cli <operationId> -h\n\
\n\
By default you are logged into Zsh with full autocompletion for your REST API,\n\
but you can switch to Bash, where basic autocompletion is also supported.\n\
\n\
"\
' | tee -a ~/.bashrc ~/.zshrc

#
# Poormans chsh & cleanup to make image as compact as possible
#

RUN sed -i 's/root:x:0:0:root:\/root:\/bin\/ash/root:x:0:0:root:\/root:\/bin\/zsh/' /etc/passwd
RUN apk del git vim && rm -f /var/cache/apk/*

ENTRYPOINT ["/bin/zsh"]
