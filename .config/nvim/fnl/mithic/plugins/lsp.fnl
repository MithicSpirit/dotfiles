(var lsp_setting
 {:texlab
   {:settings {:texlab {:chktex {:onOpenAndSave true :onEdit true}
                        :build {:auxDirectory "build"
                                :logDirectory "build"}
                        :auxDirectory "build"}}}
  :pylsp
   {:settings {:pylsp {:plugins {:autopep8 {:enabled false}
                                  :mccabe {:enabled false}
                                  :pycodestyle {:enabled false}
                                  :pyflakes {:enabled false}
                                  :yapf {:enabled false}}}}}
  :ltex
   {:settings {:ltex {:latex {:environments {:numcases :ignore
                                             :subnumcases :ignore}
                              :commands {"\\noeqref{}" :ignore}}}}}})

(local global_lsp [:clangd :idris2_lsp])

(fn lsp_config [name opts]
  (tset lsp_setting name
    (vim.tbl_deep_extend :force (or (. lsp_setting name) {}) opts)))

(fn lsp_setup [name]
  ((. (require :lspconfig) name :setup)
   (vim.tbl_deep_extend :force
      ((. (require :cmp_nvim_lsp) :default_capabilities))
      (or (. lsp_setting name) {}))))


[{1 "neovim/nvim-lspconfig"
  :dependencies ["hrsh7th/cmp-nvim-lsp"]
  ; :lazy false
  :event [:BufReadPre :BufNewFile]
  :priority 300
  :config #(each [_ lsp (ipairs global_lsp)] (lsp_setup lsp))}


 {1 "folke/neodev.nvim"
  :enabled false
  :dependencies ["neovim/nvim-lspconfig"]
  :lazy false
  :priority 450
  :opts
   {:library {:enabled true :plugins true :runtime true :types true}
    :pathStrict true
    :lspconfig true
    :setup_jsonls false}}


 {1 "icewind/ltex-client.nvim"
  :dependencies ["neovim/nvim-lspconfig"]
  :enabled false
  :name :ltex-client
  :init #(lsp_config :ltex
          {:on_attach #((. (require :lazy) :load) {:plugins [:ltex-client]})})
  :opts {:user_dictionaries_path (.. (vim.fn.stdpath :data) "/ltex/")}}


 {1 "mrcjkb/haskell-tools.nvim"
  :dependencies ["neovim/nvim-lspconfig" "nvim-lua/plenary.nvim"]
  :ft [:haskell :lhaskell :cabal :cabalproject]
  :init #(do
          (set vim.g.haskell_tools
             {:tools
                {:codeLens {:autoRefresh true}
                 :dap {:cmd ["haskell-debug-adapter"]}
                 :definition {:hoogle_signature_fallback true}
                 :hoogle {:mode :auto}
                 :hover {:auto_focus false
                         :enable true
                         :stylize_markdown false}
                 :repl {:auto_focus true :handler :builtin}}
              :hls
                {:default_settings
                   {:haskell {:checkProject true
                              :formattingProvider :ormolu}}}})
          (vim.api.nvim_create_autocmd :FileType
             {:pattern :haskell
              :callback #(let [ht (require :haskell-tools)
                               opts {:buffer true}]
                          (vim.keymap.set :n "<localleader>r" ht.repl.toggle opts)
                          (vim.keymap.set :n
                             "<localleader>f"
                             #(ht.repl.toggle (vim.api.nvim_buf_get_name 0))
                             opts)
                          (vim.keymap.set :n "<localleader>q" ht.repl.quit opts)
                          (ht.lsp.start))
              :group (vim.api.nvim_create_augroup :mithic-haskell {})}))}


 {1 "Julian/lean.nvim"
  :dependencies ["neovim/nvim-lspconfig" "nvim-lua/plenary.nvim"]
  :event ["BufReadPre *.lean" "BufNewFile *.lean"]
  :opts
     {:abbreviations {:enable true :extra {} :leader "\\"}
      :ft {:default :lean :nomodifiable nil}
      :infoview {:autoopen true
                 :height 20
                 :horizontal_position :bottom
                 :indicators :auto
                 :separate_tab false
                 :width 60}
      :lean3 {:mouse_events false}
      :mappings true
      :progress_bars {:enable true :priority 10}
      :stderr {:enable true :height 8 :on_lines nil}}}


 {1 "simrat39/rust-tools.nvim"
  :ft [:rust :toml]
  :dependencies ["neovim/nvim-lspconfig"]
  :opts
    #{:tools
        {:executor (. (require :rust-tools.executors) :termopen)
         :reload_workspace_from_cargo_toml true
         :inlay_hints {:auto true
                       :highlight :Comment
                       :max_len_align false
                       :max_len_align_padding 1
                       :only_current_line false
                       :other_hints_prefix "=> "
                       :parameter_hints_prefix "<- "
                       :right_align false
                       :right_align_padding 3
                       :show_parameter_hints true}
         :hover_actions {:auto_focus false :max_height nil :max_width nil}
         :crate_graph {:backend :svgz :full true :output nil}}
      :server {:standalone true}}}


 {1 "williamboman/mason.nvim"
  :build #(vim.cmd.MasonUpdate)
  ; :lazy false
  :event [:BufReadPre :BufNewFile]
  :priority 500
  :config true
  :keys [["<leader>M" #((. (require :mason.ui) :open))]]}

 {1 "williamboman/mason-lspconfig.nvim"
  ; :lazy false
  :event [:BufReadPre :BufNewFile]
  :priority 400
  :dependencies ["williamboman/mason.nvim" "neovim/nvim-lspconfig"]
  :config #(let [mlsp (require :mason-lspconfig)]
            (mlsp.setup {:ensure_installed ["lua_ls" "fennel_language_server"]})
            (mlsp.setup_handlers [lsp_setup]))}]
