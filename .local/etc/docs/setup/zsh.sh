#!/usr/bin/env sh

. "$(dirname "$0")/.imports.sh"

PLUGINPATH="$HOME/.config/zsh/plugins"

safeclone https://github.com/zsh-users/zsh-autosuggestions.git "$PLUGINPATH/zsh-autosuggestions"
safeclone https://github.com/zsh-users/zsh-completions.git "$PLUGINPATH/zsh-completions"
safeclone https://github.com/zsh-users/zsh-history-substring-search.git "$PLUGINPATH/zsh-history-substring-search"
safeclone https://github.com/zsh-users/zsh-syntax-highlighting.git "$PLUGINPATH/zsh-syntax-highlighting"
safeclone https://github.com/romkatv/powerlevel10k.git "$PLUGINPATH/powerlevel10k"
