[{1 "tpope/vim-sleuth"
  :lazy false
  :version false}

 {1 "tpope/vim-eunuch"
  :lazy false
  :version false}

 {1 "tpope/vim-rsi"
  :lazy false
  :version false}

 {1 "tpope/vim-repeat"
  :lazy false
  :version false}

 {1 "tpope/vim-surround"
  :version false
  :dependencies ["tpope/vim-repeat"]
  :init #(set vim.g.surround_no_insert_mappings 0)
  :keys [ "ds" "cs" "cS" "ys" "yS" "yss" "ySs" "ySS"
         {1 "S" :mode :x}
         {1 "gS" :mode :x}
         {1 "<C-S>" 2 "<Plug>Isurround" :mode :i}]}

 {1 "tpope/vim-characterize"
  :version false
  :keys [["gA" "<Plug>(characterize)"]]
  :cmd [:Characterize]}

 {1 "tpope/vim-commentary"
  :version false
  :keys [{1 "gc" :mode [:x :n :o]}]
  :cmd [:Commentary]}


 {1 "justinmk/vim-sneak"
   :init #(tset vim.g :sneak#use_ic_scs 1)
   :keys [["f" "<Plug>Sneak_f"]
          ["F" "<Plug>Sneak_F"]
          ["t" "<Plug>Sneak_t"]
          ["T" "<Plug>Sneak_T"]]}

 {1 "lukas-reineke/indent-blankline.nvim"
  :event [:BufReadPre :BufNewFile]
  :config
    #(do
       ((. (require :lazy) :load) {:plugins [:nvim-treesitter]})
       (vim.api.nvim_create_augroup :mithic-lc-lms {})
       (vim.opt_local.listchars:remove [:lead :leadmultispace])
       (vim.opt_global.listchars:remove [:lead :leadmultispace])
       ((. (require :ibl) :setup)
        {:indent {:char "▏" :tab_char "║"
                  ; :highlight [:RainbowRed :RainbowOrange :RainbowYellow
                  ;             :RainbowGreen :RainbowBlue :RainbowViolet]
                  :highlight [:RainbowRed :RainbowYellow :RainbowBlue
                              :RainbowOrange :RainbowViolet :RainbowGreen]}
         :scope {:char "▍" :show_start false :show_end false}}))}


 {1 "mbbill/undotree"
  :keys [["<leader>u" vim.cmd.UndotreeToggle]]
  :cmd [:UndotreeToggle]
  :config
    #(do (set vim.g.undotree_SetFocusWhenToggle 1)
         (set vim.g.undotree_ShortIndicators 1))}

 {1 "chrisbra/Colorizer"
  :cmd [:ColorHighlight :ColorClear :RGB2Term :HSL2RGB :Term2RGB :ColorContrast
        :ColorSwapFgBg :ColorToggle]}


 {1 "gpanders/nvim-parinfer"
  :ft [:clojure :scheme :lisp :racket :hy :fennel :janet :carp :wast :yuck]}

 {1 "whonore/Coqtail"
  :init #(set vim.g.coqtail_map_prefix "<localleader>")
  :event ["BufReadPre *.v" "BufNewFile *.v"]
  :ft :coq}

 {1 "edwinb/idris2-vim"
  :event ["BufReadPre *.idr,*.lidr" "BufNewFile *.idr,*.lidr"]
  :ft [:idris2 :lidris2]}]
