[{1 "catppuccin/nvim"
  :name :catppuccin
  :priority 1000
  :opts
    {:flavour :frappe
     :transparent_background (not vim.g.neovide)
     :show_end_of_buffer true
     :term_colors false
     :dim_inactive {:enabled false :percentage 0.15 :shade :dark}
     :no_italic false
     :no_bold false
     :no_underline false
     :color_overrides {:all {:text "#eceaf0"} :frappe {:base "#333743"}}
     :custom_highlights {}
     :integrations
       {:cmp true
        :dap {:enable_ui true :enabled true}
        :gitgutter true
        :gitsigns true
        :hop true
        :markdown true
        :mason true
        :native_lsp {:enabled true}
        :neogit true
        :telescope true
        :treesitter true
        :treesitter_context false
        :vim_sneak true}
     :styles {:comments [:italic]
              :conditionals [:italic]
              :loops [:italic]
              :types [:bold]}}
  :init #(vim.cmd.colorscheme :catppuccin)}]
