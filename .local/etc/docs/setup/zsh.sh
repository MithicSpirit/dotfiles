#!/usr/bin/env sh

. "$(dirname "$0")/.imports.sh"

PLUGINPATH="$HOME/.config/zsh/plugins"

safeclone "$PLUGINPATH/zsh-autosuggestions" 'https://github.com/zsh-users/zsh-autosuggestions.git'
safeclone "$PLUGINPATH/zsh-completions" 'https://github.com/zsh-users/zsh-completions.git'
safeclone "$PLUGINPATH/zsh-history-substring-search" 'https://github.com/zsh-users/zsh-history-substring-search.git'
safeclone "$PLUGINPATH/zsh-syntax-highlighting" 'https://github.com/zsh-users/zsh-syntax-highlighting.git'
safeclone "$PLUGINPATH/powerlevel10k" 'https://github.com/romkatv/powerlevel10k.git'
