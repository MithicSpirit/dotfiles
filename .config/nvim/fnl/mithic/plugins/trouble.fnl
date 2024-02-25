[{1 "folke/trouble.nvim"
  :enabled false
  :lazy false
  :opts {:position :bottom
         :icons false
         :severity vim.diagnostic.severity.WARN
         :fold_open "v"
         :fold_closed ">"
         :group true
         :padding false
         :cycle_results false
         :multiline true
         :indent_lines true
         :auto_open true
         :auto_close true
         :auto_preview true
         :use_diagnostic_signs false}
  :keys [["<leader>x" #((. (require :trouble) :open))]
         {1 "]x"
          2 #((. (require :trouble) :next)
              {:skip_groups true :jump true})
          :mode ""}
         {1 "[x"
          2 #((. (require :trouble) :previous)
              {:skip_groups true :jump true})
          :mode ""}]
  :init #(do
           (set vim.g.vimtex_quickfix_mode 0)
           (vim.api.nvim_create_autocmd :User
             {:pattern :VimtexEventCompileFailed
              :callback #((. (require :trouble) :open) :quickfix)
              :group (vim.api.nvim_create_augroup :mithic-trouble-vim {})}))}]
