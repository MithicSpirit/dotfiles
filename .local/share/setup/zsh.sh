#!/usr/bin/env sh
PLUGINPATH="$HOME/.config/zsh/plugins"
git clone --depth=1 https://github.com/zsh-users/zsh-autosuggestions.git "$PLUGINPATH/zsh-autosuggestions"
git clone --depth=1 https://github.com/zsh-users/zsh-completions.git "$PLUGINPATH/zsh-completions"
git clone --depth=1 https://github.com/zsh-users/zsh-history-substring-search.git "$PLUGINPATH/zsh-history-substring-search"
git clone --depth=1 https://github.com/zsh-users/zsh-syntax-highlighting.git "$PLUGINPATH/zsh-syntax-highlighting"
