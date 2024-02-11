[{1 "tpope/vim-fugitive"
  :cmd [:Git]
  :keys [["<leader>g" ":Git "] ; TODO: use <leader>g for gitsigns instead
         ["<leader>g" ":Git<Cr>"]]}


 {1 "lewis6991/gitsigns.nvim"
  ; :lazy false
  :event :UIEnter
  :opts
    {:signs
       {:add {:text "+"} :change {:text "~"} :changedelete {:text "="}
        :delete {:text "_"} :topdelete {:text "‾"} :untracked {:text "|"}}
     :max_file_length 40000
     :signcolumn true
     :sign_priority 6
     :numhl true
     :linehl false
     :word_diff false
     :watch_gitdir {:interval 1000 :follow_files true}
     :attach_to_untracked true
     :current_line_blame true
     :current_line_blame_opts
       {:delay 1000 :ignore_whitespace false :virt_text true
        :virt_text_pos :eol}
     :current_line_blame_formatter
       "<author>, <author_time:%Y-%m-%d> - <summary>"
     :update_debounce 100
     :status_formatter nil
     :yadm {:enable false}}}  ; TODO: look into yadm


 {1 "NeogitOrg/neogit"
  :keys [["<leader>G" #((. (require :neogit) :open))]]
  :enabled false
  :opts
    {:disable_insert_on_commit true
     :filewatcher {:enabled true :interval 1000}
     :remember_settings false
     :auto_refresh true
     :kind :replace
     :console_timeout 3000
     :auto_show_console true
     :status {:recent_commit_count 10}
     :commit_editor {:kind :auto}
     :commit_select_view {:kind :auto}
     :commit_view {:kind :auto}
     :log_view {:kind :auto}
     :merge_editor {:kind :auto}
     :popup {:kind :split}
     :preview_buffer {:kind :auto}
     :rebase_editor {:kind :auto}
     :reflog_view {:kind :auto}}}]
