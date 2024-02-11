[{1 "mikesmithgh/kitty-scrollback.nvim"
  :lazy (let [env vim.env.KITTY_SCROLLBACK_NVIM]
          (or (= env nil) (= env "")))
  :cmd [:KittyScrollbackGenerateKittens :KittyScrollbackCheckHealth]
  :event "User KittyScrollbackLaunch"
  :config true
  :opts [{:keymaps_enabled true
          :status_window {:enabled false}}]}]
